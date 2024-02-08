(ns closyr.symreg
  (:require
    [clojure.core.async :as async :refer [go go-loop timeout <!! >!! <! >! chan put! take! alts!! alts! close!]]
    [closyr.ga :as ga]
    [closyr.ops :as ops]
    [closyr.ops.common :as ops-common]
    [closyr.ops.initialize :as ops-init]
    [closyr.ui.gui :as gui]
    [flames.core :as flames]
    [seesaw.core :as ss])
  (:import
    (java.util
      Date
      List)
    (java.util.concurrent
      CopyOnWriteArrayList)
    (javax.swing
      JButton
      JLabel
      JTextField)
    (org.knowm.xchart
      XChartPanel
      XYChart)))


(set! *warn-on-reflection* true)


(def ^:dynamic *sim->gui-chan* (chan))
(def ^:dynamic *sim-stop-start-chan* (chan))
(def ^:dynamic *gui-close-chan* (chan))


(def sim-input-args* (atom nil))


(defn near-exact-solution
  [i old-scores]
  (println "Perfect score! " i " top scores: " (reverse (take-last 10 (sort old-scores))))
  0)


(def input-xs-exprs
  (->>
    (range 50)
    (map (fn [i] (* Math/PI (/ i 15.0))))
    ops-common/doubles->exprs))


(def input-ys-exprs
  (->>
    (range 50)
    (map (fn [i] 0.0))
    ops-common/doubles->exprs))


(defn clamp-infinites
  [doubles-coll]
  (mapv (fn [v]
          (cond
            (infinite? v) 0.0
            (> (abs v) 10e9) 0.0
            :else v))
        doubles-coll))


(defn close-chans!
  []
  (println "!! Got GUI exit command, see you later !!")
  (close! *sim->gui-chan*)
  (close! *sim-stop-start-chan*)
  (close! *gui-close-chan*))


(defn chart-update-loop
  "In the GUI thread, loops over data sent from the experiement to be rendered onto the GUI. Parks waiting on new data,
  and ends the loop when a command in the close chan is sent"
  [sim->gui-chan
   {:keys [^XYChart best-fn-chart
           ^XYChart scores-chart
           ^XChartPanel best-fn-chart-panel
           ^XChartPanel scores-chart-panel
           ^JTextField sim-selectable-text
           ^JLabel info-label
           ^JLabel status-label
           ^JButton ctl-start-stop-btn]}
   {:keys [^List xs-best-fn ^List xs-objective-fn ^List ys-best-fn ^List ys-objective-fn
           ^String series-best-fn-label ^String series-objective-fn-label

           ^List xs-scores-best
           ^List ys-scores-best
           ^List xs-scores-p99
           ^List ys-scores-p99
           ^List xs-scores-p95
           ^List ys-scores-p95
           ^List xs-scores-p90
           ^List ys-scores-p90

           ^String series-scores-best-label
           ^String series-scores-p99-label
           ^String series-scores-p95-label
           ^String series-scores-p90-label]
    :as   conf}]
  (println "Begin chart update loop")


  (go-loop [chart-iter 0]
    (<! (timeout 50))
    (when-let [{:keys [input-xs-vec-extended
                       best-eval-extended
                       best-eval
                       best-score
                       best-p99-score
                       best-p95-score
                       best-p90-score

                       best-f-str i iters]
                :as   sim-msg} (<! sim->gui-chan)]

      (let [{:keys [input-xs-vec input-ys-vec]} @sim-input-args*]
        (try

          (when (zero? chart-iter) (ss/set-text* status-label (str "Running")))

          (.clear ys-best-fn)
          (.addAll ys-best-fn (if best-eval-extended
                                (clamp-infinites best-eval-extended)
                                best-eval))

          (.clear ys-objective-fn)
          (.addAll ys-objective-fn input-ys-vec)

          (.clear xs-best-fn)
          (.addAll xs-best-fn (or input-xs-vec-extended input-xs-vec))

          (.clear xs-objective-fn)
          (.addAll xs-objective-fn input-xs-vec)

          (.setTitle best-fn-chart "Best vs Objective Functions")
          (.updateXYSeries best-fn-chart series-best-fn-label xs-best-fn ys-best-fn nil)
          (.updateXYSeries best-fn-chart series-objective-fn-label xs-objective-fn ys-objective-fn nil)

          (when (= 1 i)

            (.clear xs-scores-best)
            (.clear ys-scores-best)
            (.clear xs-scores-p99)
            (.clear ys-scores-p99)
            (.clear xs-scores-p95)
            (.clear ys-scores-p95)
            (.clear xs-scores-p90)
            (.clear ys-scores-p90))

          (when (= iters i)
            (let [^JButton reset-btn @gui/ctl-reset-btn*]
              (ss/set-text* ctl-start-stop-btn gui/ctl:start)
              (.setEnabled reset-btn false)

              (ss/set-text* status-label (str "Done"))))

          (.add xs-scores-best i)
          (.add xs-scores-p99 i)
          (.add xs-scores-p95 i)
          (.add xs-scores-p90 i)
          (.add ys-scores-best best-score)
          (.add ys-scores-p99 best-p99-score)
          (.add ys-scores-p95 best-p95-score)
          (.add ys-scores-p90 best-p90-score)
          (.updateXYSeries scores-chart series-scores-p90-label xs-scores-p90 ys-scores-p90 nil)
          (.updateXYSeries scores-chart series-scores-p95-label xs-scores-p95 ys-scores-p95 nil)
          (.updateXYSeries scores-chart series-scores-p99-label xs-scores-p99 ys-scores-p99 nil)
          (.updateXYSeries scores-chart series-scores-best-label xs-scores-best ys-scores-best nil)
          (.setTitle scores-chart "Population Score")

          (let [fn-str (str "y = " (ops/format-fn-str best-f-str))]
            (when (not= fn-str (.getText sim-selectable-text))
              (println "New Best Function: " fn-str)
              (ss/set-text* sim-selectable-text fn-str)))

          (ss/set-text* info-label (str "Iteration: " i "/" iters
                                        " Best Score: " best-score
                                        " P99 Score: " best-p99-score
                                        " P95 Score: " best-p95-score
                                        " p90 Score: " best-p90-score))

          (.revalidate best-fn-chart-panel)
          (.repaint best-fn-chart-panel)

          (.revalidate scores-chart-panel)
          (.repaint scores-chart-panel)
          (catch Exception e
            (println "Err in redrawing GUI: " (.getMessage e))
            (ss/set-text* status-label (str "Error!"))))


        (let [[msg ch] (alts! [*gui-close-chan*] :default :continue :priority true)]
          (if-not (= msg :continue)
            (close-chans!)
            (recur (inc chart-iter))))))))


(defn setup-gui
  []
  (let [sim->gui-chan       *sim->gui-chan*
        sim-stop-start-chan *sim-stop-start-chan*
        {:keys [input-xs-vec input-ys-vec]} @sim-input-args*]
    (gui/create-and-show-gui
      {:sim-stop-start-chan       sim-stop-start-chan
       :xs-best-fn                (doto (CopyOnWriteArrayList.) (.addAll input-xs-vec))
       :xs-objective-fn           (doto (CopyOnWriteArrayList.) (.addAll input-xs-vec))
       :ys-best-fn                (doto (CopyOnWriteArrayList.) (.addAll (repeat (count input-xs-vec) 0.0)))
       :ys-objective-fn           (doto (CopyOnWriteArrayList.) (.addAll input-ys-vec))

       :xs-scores-best            (doto (CopyOnWriteArrayList.) (.add 0.0))
       :ys-scores-best            (doto (CopyOnWriteArrayList.) (.add 0.0))

       :xs-scores-p99             (doto (CopyOnWriteArrayList.) (.add 0.0))
       :ys-scores-p99             (doto (CopyOnWriteArrayList.) (.add 0.0))
       :xs-scores-p95             (doto (CopyOnWriteArrayList.) (.add 0.0))
       :ys-scores-p95             (doto (CopyOnWriteArrayList.) (.add 0.0))
       :xs-scores-p90             (doto (CopyOnWriteArrayList.) (.add 0.0))
       :ys-scores-p90             (doto (CopyOnWriteArrayList.) (.add 0.0))

       :series-scores-best-label  "best score"
       :series-scores-p99-label   "p99 score"
       :series-scores-p95-label   "p95 score"
       :series-scores-p90-label   "p90 score"
       :series-best-fn-label      "best fn"
       :series-objective-fn-label "objective fn"
       :update-loop               (partial chart-update-loop sim->gui-chan)})
    {:sim->gui-chan       sim->gui-chan
     :sim-stop-start-chan sim-stop-start-chan}))


(defn update-plot-input-data
  [{new-state          :new-state
    reset?             :reset
    input-data-x       :input-data-x
    input-data-y       :input-data-y
    input-iters        :input-iters
    input-phenos-count :input-phenos-count}]

  (println "Got state req: "
           (if reset?
             "Reset"
             (if new-state gui/ctl:start gui/ctl:stop)))

  (let [input-xs-exprs (if input-data-x
                         (ops-common/doubles->exprs input-data-x)
                         input-xs-exprs)
        input-ys-exprs (if input-data-y
                         (ops-common/doubles->exprs input-data-y)
                         input-ys-exprs)

        input-ys-vec   (ops-common/exprs->doubles input-ys-exprs)
        input-xs-vec   (ops-common/exprs->doubles input-xs-exprs)]

    (reset! sim-input-args* {:input-xs-exprs     input-xs-exprs
                             :input-xs-vec       input-xs-vec
                             :input-ys-vec       input-ys-vec
                             :input-iters        input-iters
                             :input-phenos-count input-phenos-count})

    @sim-input-args*))


(defn restart-with-new-inputs
  [msg]
  (println "Restarting experiment! ")
  (update-plot-input-data msg)
  (<!! (timeout 200))
  true)


(defn park-if-gui-pause-and-return-if-should-restart
  "If no message from GUI is available, no-op, otherwise process stop/restart requests"
  [{:keys [input-xs-list input-xs-count input-ys-vec
           sim-stop-start-chan sim->gui-chan]
    :as   run-args}]
  (let [[msg ch] (alts!! [sim-stop-start-chan] :default :continue :priority true)]
    (when-not (= msg :continue)
      (if (:reset msg)
        (restart-with-new-inputs msg)
        (do
          (println "Parking updates due to Stop command")
          (let [msg (<!! sim-stop-start-chan)]
            (if (:reset msg)
              (restart-with-new-inputs msg)
              (do
                (println "Resuming updates")
                nil))))))))


(defn ->run-args
  [{input-xs-exprs     :input-xs-exprs
    input-xs-vec       :input-xs-vec
    input-ys-vec       :input-ys-vec
    input-iters        :input-iters
    iters              :iters
    input-phenos-count :input-phenos-count
    initial-phenos     :initial-phenos}]

  (when-not (and input-xs-exprs
                 input-xs-vec
                 input-ys-vec
                 (or input-iters iters)
                 (or input-phenos-count
                     initial-phenos))
    (throw (Exception. "Run args needs all params!")))

  {:extended-domain-args (ops-common/extend-xs input-xs-vec)
   :input-xs-list        (ops-common/exprs->exprs-list input-xs-exprs)
   :input-xs-count       (count input-xs-exprs)
   :input-xs-vec         input-xs-vec
   :input-ys-vec         input-ys-vec
   :input-iters          (or input-iters iters)
   :initial-phenos       initial-phenos
   :input-phenos-count   (when input-phenos-count
                           input-phenos-count)})


(defn wait-and-get-gui-args
  "Park and wait on GUI input, return the inputs as args to GA run"
  [sim-stop-start-chan]
  ;; wait for GUI to press Start, which submits the new xs/ys data:
  (when-let [msg (<!! sim-stop-start-chan)]
    (->run-args (update-plot-input-data msg))))


(defn start-gui-and-get-input-data
  "Initialize and show GUI, then park and wait on user input to start"
  [{:keys [iters initial-phenos initial-muts input-xs-exprs input-ys-exprs] :as run-config}]

  ;; these are the data shown in the plots before the experiment is started:
  (reset! sim-input-args* {:input-xs-vec (ops-common/exprs->doubles input-xs-exprs)
                           :input-ys-vec (ops-common/exprs->doubles input-ys-exprs)})

  (let [{sim->gui-chan       :sim->gui-chan
         sim-stop-start-chan :sim-stop-start-chan
         :as                 gui-comms} (setup-gui)]

    (merge gui-comms (wait-and-get-gui-args sim-stop-start-chan))))


(defn next-iters
  [i scores]
  (if (some #(> % -1e-3) scores)
    (near-exact-solution i scores)
    (dec i)))


(defn run-ga-iterations
  [{:keys [iters initial-phenos initial-muts use-gui?] :as run-config}
   run-args]
  (loop [pop (ga/initialize
               initial-phenos
               (partial ops/score-fn run-args)
               (partial ops/mutation-fn initial-muts)
               (partial ops/crossover-fn initial-muts))
         i   iters]
    (if (zero? i)
      {:iters-done       (- iters i)
       :final-population pop
       :next-step        :wait}
      (if (and use-gui?
               (park-if-gui-pause-and-return-if-should-restart run-args))
        {:iters-done       (- iters i)
         :final-population pop
         :next-step        :restart}
        (let [{scores :pop-scores :as ga-result} (ga/evolve pop)]
          (ops/report-iteration i iters ga-result run-args run-config)
          (recur ga-result (next-iters i scores)))))))


(defn run-from-inputs
  "Run GA as symbolic regression engine on input/output (x/y) dataset using initial functions and mutations"
  [{:keys [iters initial-phenos initial-muts use-gui?] :as run-config}
   {:keys [input-iters input-phenos-count input-xs-list input-xs-count input-ys-vec
           sim-stop-start-chan sim->gui-chan]
    :as   run-args}]
  (let [iters          (or input-iters iters)
        initial-phenos (if input-phenos-count
                         (do
                           (println "Generating pheno count: " input-phenos-count
                                    " via reps: " (/ input-phenos-count (count ops-init/initial-exprs))
                                    " from initial exprs: " (count ops-init/initial-exprs))
                           (ops-init/initial-phenotypes (/ input-phenos-count (count ops-init/initial-exprs))))
                         initial-phenos)
        run-config     (assoc run-config :initial-phenos initial-phenos :iters iters)
        start          (Date.)
        _              (do (println "Start " start "iters: " iters " pop size: " (count initial-phenos))
                           (reset! ops/test-timer* start))

        {:keys [final-population next-step iters-done]
         :as   completed-ga-data} (run-ga-iterations run-config run-args)]

    (println "Took " (/ (ops-common/start-date->diff-ms start) 1000.0) " seconds for iters: " iters-done)
    (case next-step
      :wait (if use-gui?
              (do
                (println "Experiment complete, waiting for GUI to start another")
                (when-let [new-gui-args (wait-and-get-gui-args sim-stop-start-chan)]
                  (recur run-config (merge run-args new-gui-args))))
              (do (println "Done.")
                  completed-ga-data))
      :restart (do
                 (println "Restarting...")
                 (<!! (timeout 200))
                 (recur run-config (merge run-args (->run-args @sim-input-args*)))))))


(defn run-experiment
  [{:keys [iters initial-phenos initial-muts input-xs-exprs input-ys-exprs use-gui?] :as run-config}]
  (println "initial data: iters: " iters
           "pop: " (count initial-phenos)
           "muts: " (count initial-muts))

  (run-from-inputs
    run-config
    (if use-gui?
      (start-gui-and-get-input-data run-config)
      (->run-args (merge (reset! sim-input-args*
                                 {:input-xs-exprs input-xs-exprs
                                  :input-xs-vec   (ops-common/exprs->doubles input-xs-exprs)
                                  :input-ys-vec   (ops-common/exprs->doubles input-ys-exprs)})
                         run-config)))))


(defn in-flames
  [f]
  ;; http://localhost:54321/flames.svg
  (let [flames (flames/start! {:port 54321, :host "localhost"})]
    (f)
    (flames/stop! flames)))


(def ^:dynamic *use-flamechart* false)


(defn run-with-monitoring
  [experiment-fn]
  (if *use-flamechart*
    ;; with flame graph analysis:
    (in-flames experiment-fn)
    ;; plain experiment:
    (experiment-fn)))


(defn run-app-without-gui
  []
  (run-with-monitoring
    (fn []
      (run-experiment
        {:initial-phenos (ops-init/initial-phenotypes 100)
         :initial-muts   (ops-init/initial-mutations)
         :iters          20
         :use-gui?       false
         :input-xs-exprs (->> (range 50)
                              (map (fn [i] (* Math/PI (/ i 15.0))))
                              ops-common/doubles->exprs)
         :input-ys-exprs (->> (range 50)
                              (map (fn [i]
                                     (+ 2.0
                                        (/ i 10.0)
                                        (Math/sin (* Math/PI (/ i 15.0))))))
                              ops-common/doubles->exprs)}))))


(defn run-app-with-gui
  []
  (run-with-monitoring
    (fn []
      (run-experiment
        {:initial-phenos (ops-init/initial-phenotypes 1000)
         :initial-muts   (ops-init/initial-mutations)
         :input-xs-exprs input-xs-exprs
         :input-ys-exprs input-ys-exprs
         :iters          200
         :use-gui?       true}))))


(defn run-app-from-cli-args
  [{:keys [iterations population headless xs ys] :as cli-opts}]
  (println "Running from CLI opts: " cli-opts)
  (run-with-monitoring
    (fn []
      (run-experiment
        {:initial-phenos (ops-init/initial-phenotypes (/ population (count ops-init/initial-exprs)))
         :initial-muts   (ops-init/initial-mutations)
         :iters          iterations
         :use-gui?       (not headless)
         :input-xs-exprs (if xs
                           (ops-common/doubles->exprs xs)
                           input-xs-exprs)
         :input-ys-exprs (if ys
                           (ops-common/doubles->exprs ys)
                           input-ys-exprs)})))
  (println "CLI: Done!")
  (System/exit 0))


(comment (run-app-without-gui))
(comment (run-app-with-gui))
