(ns closyr.web.handlers.api
  "JSON API handlers for the solver."
  (:require
    [cheshire.core :as json]
    [clojure.core.async :as async]
    [clojure.data.csv :as csv]
    [clojure.java.io :as io]
    [clojure.stacktrace :as st]
    [closyr.ga :as ga]
    [closyr.ops :as ops]
    [closyr.ops.common :as ops-common]
    [closyr.ops.initialize :as ops-init]
    [closyr.symbolic-regression :as symreg]
    [closyr.util.log :as log]
    [closyr.util.prng :as prng]
    [closyr.web.sse :as sse])
  (:import
    (java.io StringReader)
    (java.util Date)
    (org.matheclipse.core.interfaces IExpr)))


(set! *warn-on-reflection* true)


;; Job storage - maps job-id to job state
(defonce jobs* (atom {}))


(defn- generate-job-id
  []
  (str (java.util.UUID/randomUUID)))


(defn- parse-doubles
  "Parse a string or vector of numbers into a vector of doubles."
  [input]
  (cond
    (vector? input) (mapv double input)
    (string? input) (->> (clojure.string/split input #"[,\s]+")
                         (filter seq)
                         (mapv #(Double/parseDouble (clojure.string/trim %))))
    :else (throw (ex-info "Invalid input format" {:input input}))))


(defn- phenotype->solution
  "Convert a phenotype to a solution map for JSON serialization."
  [{:keys [^IExpr expr score] :as pheno}]
  (when (and expr score)
    {:formula   (str expr)
     :score     score
     :leafCount (.leafCount expr)}))


(defn- job-stopped?
  "Check if a job has been requested to stop."
  [job-id]
  (get-in @jobs* [job-id :stop-requested]))


(defn- job-paused?
  "Check if a job is paused."
  [job-id]
  (get-in @jobs* [job-id :paused]))


(defn- wait-while-paused!
  "Block while job is paused. Returns true if should continue, false if stopped."
  [job-id]
  (loop []
    (cond
      (job-stopped? job-id) false
      (job-paused? job-id) (do
                             (try
                               (Thread/sleep 100)
                               (catch InterruptedException _
                                 nil))
                             ;; Always check stop after waking, before recur
                             (if (job-stopped? job-id)
                               false
                               (recur)))
      :else true)))


(defn- interrupted-exception?
  "Check if an exception or any of its causes is an InterruptedException."
  [^Throwable e]
  (loop [ex e]
    (cond
      (nil? ex) false
      (instance? InterruptedException ex) true
      :else (recur (.getCause ex)))))


(defn- run-solver-job!
  "Run the solver in a background thread and update job state.
   Returns the future so it can be cancelled."
  [job-id {:keys [xs ys config sse-channel]}]
  (let [job-future
        (future
          (try
            ;; Reset all global state before each run to ensure clean state
            (reset! ops/test-timer* (Date.))
            (reset! ops-common/do-not-simplify-fns* {})

            (let [xs-vec (parse-doubles xs)
                  ys-vec (parse-doubles ys)
                  iterations (get config :iterations 10)
                  population-size (get config :population 20)
                  max-leafs (get config :maxLeafs 40)
                  random-seed (get config :seed)

                  ;; Progress callback that sends SSE events and checks for stop/pause
                  progress-callback (fn [progress-data]
                                      ;; Check if stop was requested
                                      (when (job-stopped? job-id)
                                        (throw (ex-info "Job stopped by user" {:type :stopped})))
                                      ;; Wait while paused (returns false if stopped during pause)
                                      (when-not (wait-while-paused! job-id)
                                        (throw (ex-info "Job stopped by user" {:type :stopped})))
                                      (when sse-channel
                                        ((:send! sse-channel) "progress" progress-data))
                                      ;; Also update job state
                                      (swap! jobs* assoc-in [job-id :progress] progress-data))

                  initial-muts (ops-init/initial-mutations)
                  run-config {:initial-phenos    (ops-init/initial-phenotypes population-size)
                              :initial-muts      initial-muts
                              :iters             iterations
                              :use-gui?          false
                              :use-flamechart    false
                              :max-leafs         max-leafs
                              :random-seed       random-seed
                              :progress-callback progress-callback
                              :input-xs-exprs    (ops-common/doubles->exprs xs-vec)
                              :input-ys-exprs    (ops-common/doubles->exprs ys-vec)}

                  _ (swap! jobs* assoc-in [job-id :status] :running)
                  result (symreg/run-find-formula run-config)

                  ;; Extract solutions from final population
                  solutions (->> (get-in result [:final-population :pop])
                                 (filter (fn [p] (and (:score p) (:expr p))))
                                 (sort-by :score)
                                 reverse
                                 (take 10)
                                 (mapv phenotype->solution)
                                 (filterv some?))

                  final-result {:iterations-done (:iters-done result)
                                :best-solution   (first solutions)
                                :all-solutions   solutions}]

              ;; Update job with final result
              (swap! jobs* assoc job-id {:status :completed
                                         :result final-result})

              ;; Send completion event via SSE
              (when sse-channel
                ((:send! sse-channel) "complete" final-result)
                ((:close! sse-channel))))

            (catch Exception e
              (let [;; Check if this is a stop - either our flag, InterruptedException, or stop-requested
                    stopped? (or (= :stopped (:type (ex-data e)))
                                 (interrupted-exception? e)
                                 (job-stopped? job-id))]
                (if stopped?
                  (do
                    (log/info "Solver job stopped by user:" job-id)
                    (swap! jobs* update job-id merge {:status :stopped})
                    (when (and sse-channel (:send! sse-channel))
                      (try
                        ((:send! sse-channel) "stopped" {:message "Job stopped by user"})
                        ((:close! sse-channel))
                        (catch Exception _ nil))))
                  (do
                    (log/error "Solver job failed:" (.getMessage e))
                    (log/error "Stack trace:" (with-out-str (st/print-stack-trace e)))
                    (swap! jobs* assoc job-id {:status :failed
                                               :error  (.getMessage e)})
                    (when (and sse-channel (:send! sse-channel))
                      (try
                        ((:send! sse-channel) "error" {:error (.getMessage e)})
                        ((:close! sse-channel))
                        (catch Exception _ nil)))))))))]
    ;; Store the future in the job state
    (swap! jobs* assoc-in [job-id :future] job-future)
    job-future))


;; ============================================================================
;; API Handlers
;; ============================================================================

(defn solve
  "POST /api/solve - Start a new solver job.
   Body: {:xs [1,2,3], :ys [1,4,9], :config {:iterations 10, :population 20}}"
  [{:keys [body-params]}]
  (try
    (let [{:keys [xs ys config]} body-params
          _ (when (or (nil? xs) (nil? ys))
              (throw (ex-info "xs and ys are required" {})))
          job-id (generate-job-id)
          sse-channel (sse/create-event-channel)]

      ;; Initialize job state with SSE channel
      (swap! jobs* assoc job-id {:status      :pending
                                 :progress    nil
                                 :result      nil
                                 :sse-channel sse-channel})

      ;; Start solver in background
      (run-solver-job! job-id {:xs          xs
                               :ys          ys
                               :config      config
                               :sse-channel sse-channel})

      {:status  202
       :headers {"Content-Type" "application/json"}
       :body    (json/encode {:jobId     job-id
                              :eventsUrl (str "/api/jobs/" job-id "/events")})})

    (catch Exception e
      {:status  400
       :headers {"Content-Type" "application/json"}
       :body    (json/encode {:error (.getMessage e)})})))


(defn get-job
  "GET /api/jobs/:id - Get job status and result."
  [{:keys [path-params]}]
  (let [job-id (:id path-params)
        job (get @jobs* job-id)]
    (if job
      {:status  200
       :headers {"Content-Type" "application/json"}
       :body    (json/encode (dissoc job :sse-channel :future))}
      {:status  404
       :headers {"Content-Type" "application/json"}
       :body    (json/encode {:error "Job not found"})})))


(defn stop-job
  "POST /api/jobs/:id/stop - Stop a running job."
  [{:keys [path-params]}]
  (let [job-id (:id path-params)
        job (get @jobs* job-id)]
    (if job
      (if (= :running (:status job))
        (let [sse-channel (:sse-channel job)]
          ;; Set the stop flag and clear paused - the progress callback will check this
          (swap! jobs* update job-id merge {:stop-requested true :paused false :status :stopped})
          ;; Send stopped event immediately via SSE
          (when (and sse-channel (:send! sse-channel))
            (try
              ((:send! sse-channel) "stopped" {:message "Job stopped by user"})
              ((:close! sse-channel))
              (catch Exception _ nil)))
          ;; Also try to cancel the future
          (when-let [f (:future job)]
            (future-cancel f))
          {:status  200
           :headers {"Content-Type" "application/json"}
           :body    (json/encode {:message "Stop requested" :jobId job-id})})
        {:status  400
         :headers {"Content-Type" "application/json"}
         :body    (json/encode {:error (str "Job is not running, status: " (name (:status job)))})})
      {:status  404
       :headers {"Content-Type" "application/json"}
       :body    (json/encode {:error "Job not found"})})))


(defn pause-job
  "POST /api/jobs/:id/pause - Pause a running job."
  [{:keys [path-params]}]
  (let [job-id (:id path-params)
        job (get @jobs* job-id)]
    (if job
      (if (= :running (:status job))
        (do
          (swap! jobs* assoc-in [job-id :paused] true)
          {:status  200
           :headers {"Content-Type" "application/json"}
           :body    (json/encode {:message "Job paused" :jobId job-id})})
        {:status  400
         :headers {"Content-Type" "application/json"}
         :body    (json/encode {:error (str "Job is not running, status: " (name (:status job)))})})
      {:status  404
       :headers {"Content-Type" "application/json"}
       :body    (json/encode {:error "Job not found"})})))


(defn resume-job
  "POST /api/jobs/:id/resume - Resume a paused job."
  [{:keys [path-params]}]
  (let [job-id (:id path-params)
        job (get @jobs* job-id)]
    (if job
      (if (and (= :running (:status job)) (:paused job))
        (do
          (swap! jobs* assoc-in [job-id :paused] false)
          {:status  200
           :headers {"Content-Type" "application/json"}
           :body    (json/encode {:message "Job resumed" :jobId job-id})})
        {:status  400
         :headers {"Content-Type" "application/json"}
         :body    (json/encode {:error "Job is not paused"})})
      {:status  404
       :headers {"Content-Type" "application/json"}
       :body    (json/encode {:error "Job not found"})})))


(defn events
  "GET /api/jobs/:id/events - SSE stream for job progress."
  [{:keys [path-params]}]
  (let [job-id (:id path-params)
        job (get @jobs* job-id)]
    (if job
      (if-let [{:keys [channel]} (:sse-channel job)]
        (cond
          ;; If job is already complete, send result immediately
          (= :completed (:status job))
          (let [new-chan (async/chan 1)]
            (async/put! new-chan {:event "complete" :data (:result job)})
            (async/close! new-chan)
            (sse/event-stream-response new-chan))

          ;; If job failed, send error
          (= :failed (:status job))
          (let [new-chan (async/chan 1)]
            (async/put! new-chan {:event "error" :data {:error (:error job)}})
            (async/close! new-chan)
            (sse/event-stream-response new-chan))

          ;; Job is pending or running, stream updates
          :else
          (sse/event-stream-response channel))
        ;; No SSE channel - job may have completed/failed, create one-shot response
        (let [new-chan (async/chan 1)]
          (cond
            (= :completed (:status job))
            (async/put! new-chan {:event "complete" :data (:result job)})
            (= :failed (:status job))
            (async/put! new-chan {:event "error" :data {:error (:error job)}})
            :else
            (async/put! new-chan {:event "error" :data {:error "Job channel not available"}}))
          (async/close! new-chan)
          (sse/event-stream-response new-chan)))
      {:status  404
       :headers {"Content-Type" "application/json"}
       :body    (json/encode {:error "Job not found"})})))


(defn datasets
  "GET /api/datasets - List available built-in datasets.
   Datasets with :formula, :xMin, :xMax can be regenerated with different point counts."
  [_]
  (let [dataset-list [{:id      "quadratic"
                       :name    "Quadratic (x^2)"
                       :formula "quadratic"
                       :xMin    1
                       :xMax    10
                       :xs      [1 2 3 4 5]
                       :ys      [1 4 9 16 25]}
                      {:id      "cubic"
                       :name    "Cubic (x^3)"
                       :formula "cubic"
                       :xMin    1
                       :xMax    5
                       :xs      [1 2 3 4 5]
                       :ys      [1 8 27 64 125]}
                      {:id      "linear"
                       :name    "Linear (2x+1)"
                       :formula "linear"
                       :xMin    0
                       :xMax    10
                       :xs      [1 2 3 4 5]
                       :ys      [3 5 7 9 11]}
                      {:id      "sine"
                       :name    "Sine"
                       :formula "sine"
                       :xMin    0
                       :xMax    (* 2 Math/PI)
                       :xs      [0 0.5 1 1.5 2 2.5 3]
                       :ys      (mapv #(Math/sin %) [0 0.5 1 1.5 2 2.5 3])}
                      {:id      "exponential"
                       :name    "Exponential (e^x)"
                       :formula "exponential"
                       :xMin    0
                       :xMax    3
                       :xs      [0 0.5 1 1.5 2]
                       :ys      (mapv #(Math/exp %) [0 0.5 1 1.5 2])}
                      {:id      "nguyen4"
                       :name    "Nguyen-4 (x^6+x^5+x^4+x^3+x^2+x)"
                       :formula "nguyen4"
                       :xMin    -1
                       :xMax    1
                       :xs      [-1 -0.5 0 0.5 1]
                       :ys      (mapv (fn [x] (+ (Math/pow x 6) (Math/pow x 5) (Math/pow x 4)
                                                 (Math/pow x 3) (Math/pow x 2) x))
                                      [-1 -0.5 0 0.5 1])}
                      {:id      "nguyen5"
                       :name    "Nguyen-5 (sin(x^2)*cos(x)-1)"
                       :formula "nguyen5"
                       :xMin    -1
                       :xMax    1
                       :xs      [-1 -0.5 0 0.5 1]
                       :ys      (mapv (fn [x] (- (* (Math/sin (* x x)) (Math/cos x)) 1))
                                      [-1 -0.5 0 0.5 1])}
                      {:id      "feynman-lorentz"
                       :name    "Feynman Lorentz (1/sqrt(1-x^2))"
                       :formula "feynman-lorentz"
                       :xMin    0
                       :xMax    0.9
                       :xs      [0 0.2 0.4 0.6 0.8]
                       :ys      (mapv (fn [x] (/ 1.0 (Math/sqrt (- 1.0 (* x x)))))
                                      [0 0.2 0.4 0.6 0.8])}
                      {:id      "feynman-wave"
                       :name    "Feynman Wave (sin(x))"
                       :formula "feynman-wave"
                       :xMin    0
                       :xMax    (* 4 Math/PI)
                       :xs      [0 1 2 3 4 5 6]
                       :ys      (mapv #(Math/sin %) [0 1 2 3 4 5 6])}]]
    {:status  200
     :headers {"Content-Type" "application/json"}
     :body    (json/encode {:datasets dataset-list})}))


(defn upload-csv
  "POST /api/upload-csv - Parse uploaded CSV file content.
   Body: {:content \"x,y\\n1,1\\n2,4\\n3,9\"}"
  [{:keys [body-params]}]
  (try
    (let [content (:content body-params)
          _ (when (nil? content)
              (throw (ex-info "content is required" {})))
          reader (StringReader. content)
          csv-data (doall (csv/read-csv reader))
          ;; Check for headers
          has-headers (or (= "x" (clojure.string/lower-case (ffirst csv-data)))
                          (= "y" (clojure.string/lower-case (ffirst csv-data))))
          data-rows (if has-headers (rest csv-data) csv-data)
          parsed (mapv (fn [row]
                         {:x (Double/parseDouble (first row))
                          :y (Double/parseDouble (second row))})
                       data-rows)
          xs (mapv :x parsed)
          ys (mapv :y parsed)]
      {:status  200
       :headers {"Content-Type" "application/json"}
       :body    (json/encode {:xs xs :ys ys :rowCount (count parsed)})})
    (catch Exception e
      {:status  400
       :headers {"Content-Type" "application/json"}
       :body    (json/encode {:error (str "Failed to parse CSV: " (.getMessage e))})})))
