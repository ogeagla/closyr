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
  "Block while job is paused. Returns true if should continue, false if stopped.
   Uses single atom reads to get consistent snapshots and avoid race conditions."
  [job-id]
  (loop []
    ;; Single atomic read to get consistent view of job state
    (let [{:keys [stop-requested paused]} (get @jobs* job-id)]
      (cond
        ;; Always check stop first - if stop requested, exit immediately
        stop-requested false

        ;; If paused, sleep and check again
        paused
        (do
          (try
            (Thread/sleep 100)
            (catch InterruptedException _ nil))
          ;; After waking, do another atomic read to check stop
          (if (:stop-requested (get @jobs* job-id))
            false
            (recur)))

        ;; Not stopped and not paused - continue execution
        :else true))))


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
   Returns the future so it can be cancelled.

   Config options:
   - :iterations - number of iterations (default 10)
   - :population - population size (default 20)
   - :maxLeafs - max leaf count (default 40)
   - :seed - random seed for reproducibility
   - :mutationsBlacklist - mutations to exclude
   - :adaptiveMode - use adaptive mutation rates
   - :quietLogs - reduce logging verbosity
   - :useEvalCache - cache evaluation results
   - :scoringMethod - scoring method (mae-max, log-cosh, r-squared)
   - :seedFormulas - vector of formula strings to seed population
   - :freshPercent - percentage of fresh phenotypes (default 0.2)"
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
                  mutations-blacklist (get config :mutationsBlacklist)
                  adaptive-mode (get config :adaptiveMode false)
                  quiet-logs (get config :quietLogs true)
                  use-eval-cache (get config :useEvalCache false)
                  scoring-method (keyword (get config :scoringMethod "mae-max"))
                  seed-formulas (get config :seedFormulas)
                  fresh-percent (get config :freshPercent 0.2)

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

                  ;; Apply mutations blacklist if provided
                  initial-muts (if (seq mutations-blacklist)
                                 (ops-init/filter-mutations {:blacklist mutations-blacklist})
                                 (ops-init/initial-mutations))

                  ;; Create initial phenotypes - either seeded from formulas or fresh
                  initial-phenos (if (seq seed-formulas)
                                   (do
                                     (log/info "Seeding population from" (count seed-formulas) "formulas,"
                                               "fresh percent:" fresh-percent)
                                     (ops-init/seeded-phenotypes seed-formulas fresh-percent population-size))
                                   (ops-init/initial-phenotypes population-size))

                  run-config {:initial-phenos    initial-phenos
                              :initial-muts      initial-muts
                              :iters             iterations
                              :use-gui?          false
                              :use-flamechart    false
                              :max-leafs         max-leafs
                              :random-seed       random-seed
                              :adaptive-mode     adaptive-mode
                              :quiet-logs        quiet-logs
                              :use-eval-cache    use-eval-cache
                              :scoring-method    scoring-method
                              :progress-callback progress-callback
                              :input-xs-exprs    (ops-common/doubles->exprs xs-vec)
                              :input-ys-exprs    (ops-common/doubles->exprs ys-vec)}

                  _ (do (log/info "Starting job" job-id "- iterations:" iterations "population:" population-size
                                  "points:" (count xs-vec) "adaptive:" adaptive-mode "quiet-logs:" quiet-logs
                                  "scoring-method:" scoring-method "random-seed:" random-seed "use-eval-cache:" use-eval-cache)
                        (swap! jobs* assoc-in [job-id :status] :running))
                  result (symreg/run-find-formula run-config)

                  ;; Extract unique solutions from final population (deduplicated by formula)
                  solutions (->> (get-in result [:final-population :pop])
                                 (filter (fn [p] (and (:score p) (:expr p))))
                                 (sort-by :score)
                                 reverse
                                 (mapv phenotype->solution)
                                 (filterv some?)
                                 ;; Deduplicate by formula, keeping first (best score)
                                 (reduce (fn [[seen results] sol]
                                           (if (seen (:formula sol))
                                             [seen results]
                                             [(conj seen (:formula sol)) (conj results sol)]))
                                         [#{} []])
                                 second
                                 (take 10)
                                 vec)

                  source-job (get-in @jobs* [job-id :source-job])
                  final-result {:iterations-done (:iters-done result)
                                :best-solution   (first solutions)
                                :scoring-method  scoring-method
                                :all-solutions   solutions
                                :source-job      source-job}]

              ;; Update job with final result and log completion (preserve source-job)
              (log/info "Job" job-id "completed. Best formula:" (:formula (first solutions))
                        "Score:" (:score (first solutions)))
              (swap! jobs* update job-id merge {:status :completed
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
                  (let [job-state (get @jobs* job-id)
                        progress (:progress job-state)
                        source-job (:source-job job-state)]
                    (log/info "Solver job stopped by user:" job-id)
                    (swap! jobs* update job-id merge {:status :stopped})
                    (when (and sse-channel (:send! sse-channel))
                      (try
                        ((:send! sse-channel) "stopped" (cond-> {:message "Job stopped by user"}
                                                          progress (assoc :last-progress progress)
                                                          source-job (assoc :source-job source-job)))
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
        (let [sse-channel (:sse-channel job)
              progress (:progress job)]
          ;; Set the stop flag - do NOT clear :paused to avoid race condition in wait-while-paused!
          ;; The pause loop checks job-stopped? first, so it will properly detect the stop.
          (swap! jobs* update job-id merge {:stop-requested true :status :stopped})
          ;; Send stopped event immediately via SSE, including last progress data
          (when (and sse-channel (:send! sse-channel))
            (try
              ((:send! sse-channel) "stopped" (merge {:message "Job stopped by user"}
                                                     (when progress
                                                       {:last-progress progress})))
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


(defn- valid-seed-formula?
  "Check if a formula string is valid for seeding.
   Rejects formulas that look corrupted or contain problematic patterns."
  [formula]
  (and (string? formula)
       (not (clojure.string/blank? formula))
       ;; Reject Symja internal wrapper patterns
       (not (.contains ^String formula "Hold("))
       (not (.contains ^String formula "Function("))
       (not (.contains ^String formula "{x}"))
       ;; Reject truncated string markers
       (not (.contains ^String formula "<<"))
       ;; Reject list patterns that shouldn't be in formulas
       (not (.contains ^String formula "List("))
       ;; Reject excessively long formulas (likely corrupted)
       (< (count formula) 500)))


(defn continue-job
  "POST /api/jobs/:id/continue - Continue evolution using results from a completed/stopped job.

   Body: {:xs [...], :ys [...], :config {...}}
   - xs/ys: new data points (optional, will use original job's data if not provided)
   - config: new configuration (optional, merges with original job's config)
     - :freshPercent - percentage of fresh phenotypes (default 0.2 = 20% fresh, 80% seeded)

   Uses formulas from the original job's results to seed the new population."
  [{:keys [path-params body-params]}]
  (let [source-job-id (:id path-params)
        source-job (get @jobs* source-job-id)]
    (if source-job
      (if (#{:completed :stopped} (:status source-job))
        (try
          (let [;; Get formulas from source job
                result (:result source-job)
                all-solutions (or (:all-solutions result) [])
                progress (:progress source-job)
                ;; For stopped jobs, best-formula may be in progress
                raw-formulas (cond
                               ;; Completed job: use all-solutions
                               (seq all-solutions)
                               (mapv :formula all-solutions)
                               ;; Stopped job: use best formula from progress
                               (:best-formula progress)
                               [(:best-formula progress)]
                               :else
                               [])
                ;; Filter out invalid/corrupted formulas
                seed-formulas (filterv valid-seed-formula? raw-formulas)
                rejected-count (- (count raw-formulas) (count seed-formulas))
                _ (when (pos? rejected-count)
                    (log/info "Filtered out" rejected-count "invalid formulas from" (count raw-formulas) "total"))

                _ (when (empty? seed-formulas)
                    (if (seq raw-formulas)
                      (do
                        (log/error "All formulas were rejected as invalid:" raw-formulas)
                        (throw (ex-info "All formulas were invalid for seeding" {:raw-formulas raw-formulas})))
                      (throw (ex-info "No formulas available to seed from" {}))))

                ;; Get config from body or use source job's config (stored in result)
                body-config (or (:config body-params) {})
                fresh-percent (get body-config :freshPercent 0.2)

                ;; Merge seedFormulas into config
                config (assoc body-config :seedFormulas seed-formulas
                                          :freshPercent fresh-percent)

                ;; Get xs/ys - use from body if provided, otherwise... we need to store them
                ;; For now, require xs/ys to be provided
                xs (or (:xs body-params)
                       (throw (ex-info "xs are required for continue" {})))
                ys (or (:ys body-params)
                       (throw (ex-info "ys are required for continue" {})))

                ;; Create new job
                new-job-id (generate-job-id)
                sse-channel (sse/create-event-channel)]

            ;; Initialize new job state
            (swap! jobs* assoc new-job-id {:status      :pending
                                           :progress    nil
                                           :result      nil
                                           :sse-channel sse-channel
                                           :source-job  source-job-id})

            ;; Start solver in background with seeded formulas
            (run-solver-job! new-job-id {:xs          xs
                                         :ys          ys
                                         :config      config
                                         :sse-channel sse-channel})

            (log/info "Created continue job" new-job-id "from source" source-job-id
                      "with" (count seed-formulas) "seed formulas, fresh percent:" fresh-percent)

            {:status  202
             :headers {"Content-Type" "application/json"}
             :body    (json/encode {:jobId       new-job-id
                                    :sourceJobId source-job-id
                                    :seedCount   (count seed-formulas)
                                    :eventsUrl   (str "/api/jobs/" new-job-id "/events")})})
          (catch Exception e
            {:status  400
             :headers {"Content-Type" "application/json"}
             :body    (json/encode {:error (.getMessage e)})}))
        {:status  400
         :headers {"Content-Type" "application/json"}
         :body    (json/encode {:error (str "Job must be completed or stopped to continue, current status: "
                                            (name (:status source-job)))})})
      {:status  404
       :headers {"Content-Type" "application/json"}
       :body    (json/encode {:error "Source job not found"})})))


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
                       :ys      (mapv #(Math/sin %) [0 1 2 3 4 5 6])}
                      {:id      "feynman-diffraction"
                       :name    "Feynman Diffraction (sin²(5x/2)/sin²(x/2))"
                       :formula "feynman-diffraction"
                       :xMin    0.1
                       :xMax    (* 2 Math/PI)
                       :xs      [0.5 1.0 1.5 2.0 2.5 3.0 4.0 5.0 6.0]
                       :ys      (mapv (fn [x]
                                        (let [sin-half (Math/sin (/ x 2.0))
                                              sin-n-half (Math/sin (* 2.5 x))]
                                          (if (< (Math/abs sin-half) 1e-10)
                                            25.0
                                            (/ (* sin-n-half sin-n-half)
                                               (* sin-half sin-half)))))
                                      [0.5 1.0 1.5 2.0 2.5 3.0 4.0 5.0 6.0])}
                      {:id      "feynman-planck"
                       :name    "Feynman Planck (x³/(e^x-1))"
                       :formula "feynman-planck"
                       :xMin    0.1
                       :xMax    5.0
                       :xs      [0.5 1.0 1.5 2.0 2.5 3.0 3.5 4.0 4.5]
                       :ys      (mapv (fn [x] (/ (* x x x) (- (Math/exp x) 1.0)))
                                      [0.5 1.0 1.5 2.0 2.5 3.0 3.5 4.0 4.5])}
                      {:id      "feynman-rutherford"
                       :name    "Feynman Rutherford (1/sin⁴(x/2))"
                       :formula "feynman-rutherford"
                       :xMin    0.3
                       :xMax    Math/PI
                       :xs      [0.5 0.8 1.0 1.3 1.6 2.0 2.5 3.0]
                       :ys      (mapv (fn [x]
                                        (let [sin-half (Math/sin (/ x 2.0))]
                                          (/ 1.0 (* sin-half sin-half sin-half sin-half))))
                                      [0.5 0.8 1.0 1.3 1.6 2.0 2.5 3.0])}
                      {:id      "feynman-ellipse"
                       :name    "Feynman Ellipse (0.64/(1+0.6*cos(x)))"
                       :formula "feynman-ellipse"
                       :xMin    0
                       :xMax    (* 2 Math/PI)
                       :xs      [0 0.8 1.6 2.4 3.2 4.0 4.8 5.6 6.2]
                       :ys      (mapv (fn [x]
                                        (/ 0.64 (+ 1.0 (* 0.6 (Math/cos x)))))
                                      [0 0.8 1.6 2.4 3.2 4.0 4.8 5.6 6.2])}
                      {:id      "feynman-transition"
                       :name    "Feynman Transition (sin²(x)/x²)"
                       :formula "feynman-transition"
                       :xMin    -9
                       :xMax    9
                       :xs      [-8 -6 -4 -2 -1 0 1 2 4 6 8]
                       :ys      (mapv (fn [^double x]
                                        (if (< (Math/abs x) 1e-10)
                                          1.0
                                          (/ (* (Math/sin x) (Math/sin x))
                                             (* x x))))
                                      [-8 -6 -4 -2 -1 0 1 2 4 6 8])}
                      ;; Prime counting function π(x) - number of primes <= x (20 points)
                      {:id   "prime-counting-20"
                       :name "Prime Counting π(x) [20 pts]"
                       :xs   [10 20 30 40 50 60 70 80 90 100
                              110 120 130 140 150 160 170 180 190 200]
                       :ys   [4 8 10 12 15 17 19 22 24 25
                              29 30 31 34 35 37 39 41 43 46]}
                      ;; Prime counting function π(x) - 40 points
                      {:id   "prime-counting-40"
                       :name "Prime Counting π(x) [40 pts]"
                       :xs   [5 10 15 20 25 30 35 40 45 50
                              55 60 65 70 75 80 85 90 95 100
                              105 110 115 120 125 130 135 140 145 150
                              155 160 165 170 175 180 185 190 195 200]
                       :ys   [3 4 6 8 9 10 11 12 14 15
                              16 17 18 19 21 22 23 24 24 25
                              27 29 30 30 30 31 32 34 34 35
                              36 37 38 39 40 41 42 43 44 46]}
                      ;; Prime counting function π(x) - 100 points
                      {:id   "prime-counting-100"
                       :name "Prime Counting π(x) [100 pts]"
                       :xs   [2 4 6 8 10 12 14 16 18 20
                              22 24 26 28 30 32 34 36 38 40
                              42 44 46 48 50 52 54 56 58 60
                              62 64 66 68 70 72 74 76 78 80
                              82 84 86 88 90 92 94 96 98 100
                              102 104 106 108 110 112 114 116 118 120
                              122 124 126 128 130 132 134 136 138 140
                              142 144 146 148 150 152 154 156 158 160
                              162 164 166 168 170 172 174 176 178 180
                              182 184 186 188 190 192 194 196 198 200]
                       :ys   [1 2 3 4 4 5 6 6 7 8
                              8 9 9 9 10 11 11 11 12 12
                              13 14 14 15 15 15 16 16 17 17
                              18 18 18 19 19 20 21 21 21 22
                              22 23 23 23 24 24 24 24 25 25
                              26 27 27 28 29 29 30 30 30 30
                              30 30 30 31 31 32 33 34 34 34
                              34 34 34 35 35 36 36 37 37 37
                              38 38 38 39 39 40 40 41 41 41
                              42 42 42 43 43 44 44 45 46 46]}
                      ;; Nth prime - Prime(n) gives the nth prime number (20 points)
                      {:id   "primes-20"
                       :name "Nth Prime P(n) [20 pts]"
                       :xs   [1 2 3 4 5 6 7 8 9 10
                              11 12 13 14 15 16 17 18 19 20]
                       :ys   [2 3 5 7 11 13 17 19 23 29
                              31 37 41 43 47 53 59 61 67 71]}
                      ;; Nth prime - 40 points
                      {:id   "primes-40"
                       :name "Nth Prime P(n) [40 pts]"
                       :xs   [1 2 3 4 5 6 7 8 9 10
                              11 12 13 14 15 16 17 18 19 20
                              21 22 23 24 25 26 27 28 29 30
                              31 32 33 34 35 36 37 38 39 40]
                       :ys   [2 3 5 7 11 13 17 19 23 29
                              31 37 41 43 47 53 59 61 67 71
                              73 79 83 89 97 101 103 107 109 113
                              127 131 137 139 149 151 157 163 167 173]}
                      ;; Nth prime - 100 points
                      {:id   "primes-100"
                       :name "Nth Prime P(n) [100 pts]"
                       :xs   (vec (range 1 101))
                       :ys   [2 3 5 7 11 13 17 19 23 29
                              31 37 41 43 47 53 59 61 67 71
                              73 79 83 89 97 101 103 107 109 113
                              127 131 137 139 149 151 157 163 167 173
                              179 181 191 193 197 199 211 223 227 229
                              233 239 241 251 257 263 269 271 277 281
                              283 293 307 311 313 317 331 337 347 349
                              353 359 367 373 379 383 389 397 401 409
                              419 421 431 433 439 443 449 457 461 463
                              467 479 487 491 499 503 509 521 523 541]}]]
    {:status  200
     :headers {"Content-Type" "application/json"}
     :body    (json/encode {:datasets dataset-list})}))


(defn mutations
  "GET /api/mutations - List all available mutation labels for filtering."
  [_]
  (let [all-labels (ops-init/mutation-labels)]
    {:status  200
     :headers {"Content-Type" "application/json"}
     :body    (json/encode {:mutations all-labels
                            :count     (count all-labels)})}))


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
