(ns closyr.symbolic-regression
  (:require
    [clojure.core.async :as async :refer [go go-loop timeout <!! >!! <! >! chan put! take! alts!! alts! close!]]
    [closyr.ga :as ga]
    [closyr.log :as log]
    [closyr.ops :as ops]
    [closyr.ops.common :as ops-common]
    [closyr.ops.initialize :as ops-init]
    [closyr.spec :as specs]
    [closyr.ui.gui :as gui]
    [flames.core :as flames]
    [malli.core :as m]
    [malli.generator :as mg]
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
      XYChart)
    (org.matheclipse.core.interfaces
      IExpr)))


(set! *warn-on-reflection* true)


(def ^:dynamic *sim->gui-chan*
  "Put data here during iterations to update the GUI"
  (chan))


(def ^:dynamic *sim-stop-start-chan*
  "Put data here from the GUI to start/stop/restart iterations"
  (chan))


(def ^:dynamic *gui-close-chan*
  "Put data here to shut down the application"
  (chan))


(def ^:private sim-input-args*
  "Data from GUI to use in the iterations"
  (atom nil))


(defn- config->log-steps
  "Determine how often (every n iters) to log and update charts. Returns n."
  [{:keys [iters initial-phenos]}
   {:keys [input-xs-count]}]
  (cond
    (and (< (count initial-phenos) 1000) (< input-xs-count 25) (> iters 100)) 25
    (and (< (count initial-phenos) 1000) (< input-xs-count 50) (> iters 100)) 20
    (and (< (count initial-phenos) 1000) (< input-xs-count 100) (> iters 100)) 10
    (and (< (count initial-phenos) 1000) (< input-xs-count 200) (> iters 100)) 5

    (and (< (count initial-phenos) 2000) (< input-xs-count 25) (> iters 100)) 20
    (and (< (count initial-phenos) 2000) (< input-xs-count 50) (> iters 100)) 10
    (and (< (count initial-phenos) 2000) (< input-xs-count 100) (> iters 100)) 5
    (and (< (count initial-phenos) 2000) (< input-xs-count 200) (> iters 100)) 5

    (and (< (count initial-phenos) 5000) (< input-xs-count 25) (> iters 100)) 10
    (and (< (count initial-phenos) 5000) (< input-xs-count 50) (> iters 100)) 5
    (and (< (count initial-phenos) 5000) (< input-xs-count 100) (> iters 100)) 5
    (and (< (count initial-phenos) 5000) (< input-xs-count 200) (> iters 100)) 2

    (and (< (count initial-phenos) 50000) (< input-xs-count 25) (> iters 100)) 5
    (and (< (count initial-phenos) 50000) (< input-xs-count 50) (> iters 100)) 5
    (and (< (count initial-phenos) 50000) (< input-xs-count 100) (> iters 100)) 2
    (and (< (count initial-phenos) 50000) (< input-xs-count 200) (> iters 100)) 2

    :else 1))


(defn- near-exact-solution
  [i old-scores]
  (log/info "Perfect score! " i " top scores: " (reverse (take-last 10 (sort old-scores))))
  0)


(def example-input-xs-exprs
  "Sample input xs as exprs"
  (->>
    (range 50)
    (map (fn [i] (* Math/PI (/ i 15.0))))
    ops-common/doubles->exprs))


(def example-input-ys-exprs
  "Sample input ys as exprs"
  (->>
    (range 50)
    (map (fn [i] 0.0))
    ops-common/doubles->exprs))


(defn- clamp-infinites
  [doubles-coll]
  (mapv (fn [v]
          (cond
            (infinite? v) 0.0
            (> (abs v) 10e9) 0.0
            :else v))
        doubles-coll))


(defn- close-chans!
  []
  (log/info "~~~ Got GUI exit command, see you later ~~~")
  (close! *sim->gui-chan*)
  (close! *sim-stop-start-chan*)
  (close! *gui-close-chan*))


(defn- update-chart-accumulate
  [^XYChart chart ^String series i y-value ^List xs ^List ys]

  (when (= 1 i)
    (.clear xs)
    (.clear ys))

  (.add xs i)
  (.add ys y-value)

  (.updateXYSeries chart series xs ys nil))


(defn- check-if-done
  [i iters status-label ctl-start-stop-btn]
  (when (= iters i)
    (let [^JButton reset-btn @gui/ctl-reset-btn*]
      (ss/set-text* ctl-start-stop-btn gui/ctl:start)
      (.setEnabled reset-btn false)
      (ss/set-text* status-label (str "Done")))))


(defn- check-new-best-fn
  [best-f-str ^JTextField best-fn-selectable-text]
  (let [fn-str (str "y = " (ops/format-fn-str best-f-str))]
    (when (not= fn-str (.getText best-fn-selectable-text))
      (log/info "New Best Function: " fn-str)
      (ss/set-text* best-fn-selectable-text fn-str))))


(defn- check-should-recur
  []
  (go
    (let [[msg ch] (alts! [*gui-close-chan*] :default :continue :priority true)]
      (if-not (= msg :continue)
        (do (log/warn "Closing chans")
            (close-chans!)
            false)
        true))))


(defn- repaint-gui
  [chart-iter
   {:keys [^XYChart best-fn-chart
           ^XYChart scores-chart
           ^XChartPanel best-fn-chart-panel
           ^XChartPanel scores-chart-panel
           ^JTextField best-fn-selectable-text
           ^JLabel info-label
           ^JLabel status-label
           ^JButton ctl-start-stop-btn]
    :as   ui-elements}
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
    :as   conf}
   {:keys [input-xs-vec-extended
           best-eval-extended
           best-eval
           best-score
           best-p99-score
           best-p95-score
           best-p90-score
           best-f-str i iters]
    :as   sim-msg}]
  (let [{:keys [input-xs-vec input-ys-vec]} @sim-input-args*]

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

    (update-chart-accumulate scores-chart ^String series-scores-p90-label i best-p90-score xs-scores-p90 ys-scores-p90)
    (update-chart-accumulate scores-chart ^String series-scores-p95-label i best-p95-score xs-scores-p95 ys-scores-p95)
    (update-chart-accumulate scores-chart ^String series-scores-p99-label i best-p99-score xs-scores-p99 ys-scores-p99)
    (update-chart-accumulate scores-chart ^String series-scores-best-label i best-score xs-scores-best ys-scores-best)

    (.setTitle scores-chart "Population Score")

    (check-if-done i iters status-label ctl-start-stop-btn)

    (check-new-best-fn best-f-str best-fn-selectable-text)

    (ss/set-text* info-label (str "Iteration: " i "/" iters
                                  " Best Score: " best-score
                                  " P99 Score: " best-p99-score
                                  " P95 Score: " best-p95-score
                                  " p90 Score: " best-p90-score))

    (.revalidate best-fn-chart-panel)
    (.repaint best-fn-chart-panel)

    (.revalidate scores-chart-panel)
    (.repaint scores-chart-panel)))


(defn chart-update-loop
  "In the GUI thread, loops over data sent from the experiement to be rendered onto the GUI. Parks waiting on new data,
  and ends the loop when a command in the close chan is sent"
  [sim->gui-chan
   {:keys [^JLabel status-label]
    :as   ui-elements}
   conf]

  (go-loop [chart-iter 0]

    (<! (timeout 10))

    (when-let [sim-msg (<! sim->gui-chan)]
      (try

        (repaint-gui chart-iter ui-elements conf sim-msg)

        (catch Exception e
          (log/error "Err in redrawing GUI: " (.getMessage e))
          (ss/set-text* status-label (str "Error!"))))

      (when (<! (check-should-recur))
        (recur (inc chart-iter))))))


(defn- setup-gui
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


(defn- update-plot-input-data
  [{new-state          :new-state
    input-data-x       :input-data-x
    input-data-y       :input-data-y
    input-iters        :input-iters
    input-phenos-count :input-phenos-count
    max-leafs          :max-leafs}]

  (let [input-xs-exprs (if input-data-x
                         (ops-common/doubles->exprs input-data-x)
                         example-input-xs-exprs)
        input-ys-exprs (if input-data-y
                         (ops-common/doubles->exprs input-data-y)
                         example-input-ys-exprs)

        input-ys-vec   (ops-common/exprs->doubles input-ys-exprs)
        input-xs-vec   (ops-common/exprs->doubles input-xs-exprs)]

    (reset! sim-input-args* {:input-xs-exprs     input-xs-exprs
                             :input-xs-vec       input-xs-vec
                             :input-ys-vec       input-ys-vec
                             :input-iters        input-iters
                             :input-phenos-count input-phenos-count
                             :max-leafs          max-leafs})

    @sim-input-args*))


(defn- restart-with-new-inputs
  [msg]
  (log/info "~~~ Restarting experiment! ~~~")
  (update-plot-input-data msg)
  :restart)


(defn- check-gui-command-and-maybe-park
  "If no message from GUI is available, no-op, otherwise process stop/restart requests"
  [{:keys [input-xs-list input-xs-count input-ys-vec
           sim-stop-start-chan sim->gui-chan]
    :as   run-args}]
  (let [[{:keys [new-state] :as msg} ch] (alts!! [sim-stop-start-chan] :default :continue :priority true)]
    (when (and msg (not= msg :continue))
      (case new-state
        :stop :stop
        :restart (restart-with-new-inputs msg)
        :pause (do
                 (log/info "~~~ Parking updates due to Stop command ~~~")
                 (let [{:keys [new-state] :as msg} (<!! sim-stop-start-chan)]
                   (case new-state
                     :stop :stop
                     :restart (restart-with-new-inputs msg)
                     :start (do
                              (log/info "~~~ Resuming updates ~~~")
                              nil))))))))


(defn- ->run-args
  [{input-xs-exprs     :input-xs-exprs
    input-xs-vec       :input-xs-vec
    input-ys-vec       :input-ys-vec
    input-iters        :input-iters
    iters              :iters
    input-phenos-count :input-phenos-count
    max-leafs          :max-leafs
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
   :input-phenos-count   input-phenos-count
   :max-leafs            max-leafs})


(defn- wait-and-get-gui-args
  "Park and wait on GUI input, return the inputs as args to GA run"
  [sim-stop-start-chan]
  ;; wait for GUI to press Start, which submits the new xs/ys data:
  (when-let [msg (<!! sim-stop-start-chan)]
    (->run-args (update-plot-input-data msg))))


(defn- start-gui-and-get-input-data
  "Initialize and show GUI, then park and wait on user input to start"
  [{:keys [iters initial-phenos initial-muts input-xs-exprs input-ys-exprs] :as run-config}]

  ;; these are the data shown in the plots before the experiment is started:
  (reset! sim-input-args* {:input-xs-vec (ops-common/exprs->doubles input-xs-exprs)
                           :input-ys-vec (ops-common/exprs->doubles input-ys-exprs)})

  (let [{sim->gui-chan       :sim->gui-chan
         sim-stop-start-chan :sim-stop-start-chan
         :as                 gui-comms} (setup-gui)]

    (merge gui-comms (wait-and-get-gui-args sim-stop-start-chan))))


(defn- next-iters
  "Determine how many more GA iterations are left based on score, and stop if near perfect solution."
  [i scores]
  (if (some #(> % -1e-3) scores)
    (near-exact-solution i scores)
    (dec i)))


(defn- print-end-time
  [start iters-done next-step]
  (log/info "-- Done! Next state: " next-step
            " took" (/ (ops-common/start-date->diff-ms start) 1000.0)
            " seconds for iters: " iters-done
            " --"))


(defn- print-and-save-start-time
  [iters initial-phenos]
  (let [start (Date.)]
    (log/info "-- Start " start
              "iters: " iters
              " pop size: " (count initial-phenos)
              " --")
    (reset! ops/test-timer* start)))


(def ^:private GAPhenotype
  [:map
   {:closed true}
   [:id :uuid]
   [:sym any?]
   [:expr any?]
   [:score {:optional true} number?]
   [:util any?]
   [:last-op {:optional true} :string]
   [:mods-applied {:optional true} :int]])


(def ^:private GAPopulation
  [:sequential #'GAPhenotype])


(def ^:private Mutation
  [:map
   {:closed true}
   [:op :keyword]
   [:label :string]
   [:leaf-modifier-fn {:optional true} fn?]
   [:modifier-fn {:optional true} fn?]
   [:find-expr {:optional true} some?]
   [:replace-expr {:optional true} some?]])


(def ^:private RunConfig
  [:map
   {:closed true}
   [:iters pos-int?]
   [:initial-phenos #'GAPopulation]
   [:initial-muts [:sequential #'Mutation]]
   [:use-gui? :boolean]
   [:max-leafs pos-int?]
   [:input-phenos-count {:optional true} pos-int?]
   [:log-steps pos-int?]
   [:use-flamechart [:maybe :boolean]]
   [:input-xs-exprs [:sequential some?]]
   [:input-ys-exprs [:sequential some?]]])


(def ^:private RunArgs
  [:map
   {:closed true}
   [:sim->gui-chan {:optional true} some?]
   [:sim-stop-start-chan {:optional true} some?]
   [:extended-domain-args map?]
   [:input-xs-list some?]
   [:input-xs-count pos-int?]
   [:input-xs-vec [:vector double?]]
   [:input-ys-vec [:vector double?]]
   [:input-iters pos-int?]
   [:initial-phenos [:maybe [:sequential map?]]]
   [:input-phenos-count [:maybe pos-int?]]
   [:max-leafs [:maybe pos-int?]]])


(defprotocol ISolverStateController

  "Interface which allows creation and iteration of the symbolic regression GA solver"

  (init
    [this]
    "Initialize solver state")

  (run-iteration
    [this]
    "Run 1 iteration, return stop signal under certain conditions")

  (solver-step
    [this]
    "Using current state as prior, run one GA solver evolution iteration")

  (next-state
    [this]
    "Return what state the solver is in; right now this impl contains listeners for GUI input changes,
    which is a somewhat inverted control flow")

  (end
    [this {:keys [next-step]}]
    "Report timing/perf results"))


(defrecord SolverStateController
  [;; the chans? also these names are really really ambiguous and overloaded:
   run-config
   run-args]

  ISolverStateController

  (init
    [this]
    (specs/check-schema! "run-config" RunConfig run-config)
    (specs/check-schema! "run-args" RunArgs run-args)
    (let [{:keys [iters initial-phenos initial-muts use-gui?]} run-config
          start    (print-and-save-start-time iters initial-phenos)
          init-pop (ga/initialize
                     initial-phenos
                     (partial ops/score-fn run-args run-config)
                     (partial ops/mutation-fn run-config initial-muts)
                     (partial ops/crossover-fn run-config initial-muts))]

      (log/info "Running with logging every n steps: " (:log-steps run-config))

      (assoc this :ga-result init-pop
             :iters-to-go iters
             :start-ms start)))


  (solver-step
    [this]
    (let [{:keys [iters log-steps]} run-config
          population  (:ga-result this)
          iters-to-go (:iters-to-go this)]
      (binding [ops/*log-steps* log-steps]
        (if (zero? iters-to-go)
          (assoc this :status :done :result {:iters-done       (- iters iters-to-go)
                                             :final-population population
                                             :next-step        :wait})
          (let [{scores :pop-scores :as ga-result} (ga/evolve population)]
            (specs/check-schema! "ga-population" GAPopulation (:pop ga-result))
            (ops/report-iteration iters-to-go iters ga-result run-args run-config)
            (assoc this :ga-result ga-result :iters-to-go (next-iters iters-to-go scores)))))))


  (next-state
    [this]
    (let [{:keys [iters initial-phenos initial-muts use-gui?]} run-config
          iters-to-go         (:iters-to-go this)
          population          (:ga-result this)
          should-return-state (and use-gui? (check-gui-command-and-maybe-park run-args))]
      (if (and use-gui? should-return-state)
        (case should-return-state
          :stop
          {:iters-done       (- iters iters-to-go)
           :final-population population
           :next-step        :stop}
          :restart
          {:iters-done       (- iters iters-to-go)
           :final-population population
           :next-step        :restart})
        :recur)))


  (run-iteration
    [this]
    (let [{iter-status :status iters-to-go :iters-to-go ga-result :ga-result
           done-result :result
           :as         res} (solver-step this)]
      (if (= :done iter-status)
        [false (end res done-result)]
        (let [the-next-state (next-state res)]
          (if (= :recur the-next-state)
            [:recur res]
            [false (end res the-next-state)])))))


  (end
    [this {:keys [next-step] :as return-value}]
    (print-end-time (:start-ms this) (- (:iters run-config) (:iters-to-go this)) next-step)
    return-value))


(defn- run-ga-iterations-using-record
  "Run GA evolution iterations on initial population"
  [{:keys [iters initial-phenos initial-muts use-gui?] :as run-config}
   run-args]

  (loop [solver-state (init (map->SolverStateController {:run-config run-config :run-args run-args}))]
    (let [[recur? next-solver-state] (run-iteration solver-state)]
      (if recur?
        (recur next-solver-state)
        next-solver-state))))


(defn- merge-cli-and-gui-args
  [{cli-max-leafs :max-leafs :keys [iters initial-phenos initial-muts use-gui?] :as run-config}
   {:keys [input-iters input-phenos-count max-leafs input-xs-list input-xs-count input-ys-vec
           sim-stop-start-chan sim->gui-chan]
    :as   run-args}]

  (let [max-leafs      (or max-leafs cli-max-leafs)
        iters          (or input-iters iters)
        initial-phenos (if input-phenos-count
                         (ops-init/initial-phenotypes input-phenos-count)
                         initial-phenos)

        run-config     (assoc run-config
                              :initial-phenos initial-phenos
                              :iters iters
                              :max-leafs (or max-leafs ops/default-max-leafs))

        run-config     (assoc run-config
                              :log-steps (config->log-steps run-config run-args))]
    run-config))


(defn- run-from-inputs
  "Run GA as symbolic regression engine on input/output (x/y) dataset using initial functions and mutations"
  [{:keys [iters initial-phenos initial-muts use-gui?] :as run-config}
   {:keys [input-iters input-phenos-count max-leafs input-xs-list input-xs-count input-ys-vec
           sim-stop-start-chan sim->gui-chan]
    :as   run-args}]
  (let [run-config (merge-cli-and-gui-args run-config run-args)
        {:keys [next-step] :as completed-ga-data} (run-ga-iterations-using-record run-config run-args)]

    (case next-step

      :stop
      completed-ga-data

      :wait
      (if use-gui?
        (do (log/info "-- Waiting for GUI input to start again --")
            (if-let [new-gui-args (wait-and-get-gui-args sim-stop-start-chan)]
              (recur run-config (merge run-args new-gui-args))
              completed-ga-data))
        completed-ga-data)

      :restart
      (do (log/info "-- Restarting... --")
          (recur run-config (merge run-args (->run-args @sim-input-args*)))))))


(defn- in-flames
  "Run a function wrapped in a flamegraph analysis server at http://localhost:54321/flames.svg"
  [f]
  (let [flames (flames/start! {:port 54321, :host "localhost"})
        result (f)]
    (flames/stop! flames)
    result))


(defn- get-input-data
  [{:keys [iters initial-phenos initial-muts input-xs-exprs input-ys-exprs use-gui?] :as run-config}]
  (->run-args
    (merge
      (reset! sim-input-args*
              {:input-xs-exprs input-xs-exprs
               :input-xs-vec   (ops-common/exprs->doubles input-xs-exprs)
               :input-ys-vec   (ops-common/exprs->doubles input-ys-exprs)})
      run-config)))


(defprotocol ISymbolicRegressionSolver

  "A top-level interface to start the solver using CLI or GUI args"

  (solve
    [this]
    "Run the solver on either CLI of GUI args.  When using GUI, we block on getting a signal from the
    GUI which indicates the user wants to start (and later stop/restart) the solver.  The GUI
    would also provide all the parameters and inputs to the solver, like iterations count and
    the objective data.  When running from the CLI, we use the provided inputs or some example data
    and defaults."))


(defrecord SymbolicRegressionSolver
  [iters initial-phenos initial-muts input-xs-exprs input-ys-exprs use-gui? use-flamechart max-leafs]

  ISymbolicRegressionSolver

  (solve
    [this]
    (let [symbolic-regression-solver-fn (fn []
                                          (run-from-inputs
                                            this
                                            (if use-gui?
                                              (start-gui-and-get-input-data this)
                                              (get-input-data this))))]
      (if use-gui?
        (log/info "-- Running from GUI --")
        (log/info "-- Running from CLI."
                  "iters: " iters
                  "pop: " (count initial-phenos)
                  "muts: " (count initial-muts) " --"))

      (if use-flamechart
        ;; with flame graph analysis:
        (in-flames symbolic-regression-solver-fn)
        ;; plain experiment:
        (symbolic-regression-solver-fn)))))


(defn run-solver
  "Run a GA evolution solver to search for function of best fit for input data.  The
  word experiment is used loosely here, it's more of a time-evolving best-fit method instance."
  [{:keys [iters initial-phenos initial-muts input-xs-exprs input-ys-exprs use-gui?] :as run-config}]
  (solve (map->SymbolicRegressionSolver run-config)))


(defn run-app-without-gui
  "Run app without GUI and with fake placeholder input data"
  [xs ys]
  (run-solver
    {:initial-phenos (ops-init/initial-phenotypes 100)
     :initial-muts   (ops-init/initial-mutations)
     :iters          20
     :use-gui?       false
     :use-flamechart false
     :input-xs-exprs (ops-common/doubles->exprs xs)
     :input-ys-exprs (ops-common/doubles->exprs ys)}))


(defn- run-app-with-gui
  ([]
   (run-app-with-gui {:use-flamechart false}))
  ([{:keys [use-flamechart]}]
   (run-solver
     {:initial-phenos (ops-init/initial-phenotypes 50)
      :initial-muts   (ops-init/initial-mutations)
      :iters          100
      :use-gui?       true
      :max-leafs      ops/default-max-leafs
      :use-flamechart use-flamechart
      :input-xs-exprs example-input-xs-exprs
      :input-ys-exprs example-input-ys-exprs})))


(def ^:private ^:dynamic *is-testing* false)


(defn- exit
  [{:keys [iterations population headless xs ys use-flamechart max-leafs] :as cli-opts}]
  (when (and headless (not *is-testing*))
    (log/warn "System Exit 0")
    (System/exit 0)))


(defn run-app-from-cli-args
  "Run app from CLI args"
  [{:keys [iterations population headless xs ys use-flamechart max-leafs] :as cli-opts}]
  (log/info "CLI: run from options: " cli-opts)
  (let [run-config {:initial-phenos (ops-init/initial-phenotypes population)
                    :initial-muts   (ops-init/initial-mutations)
                    :iters          iterations
                    :use-gui?       (not headless)
                    :max-leafs      max-leafs
                    :use-flamechart use-flamechart
                    :input-xs-exprs (if xs
                                      (ops-common/doubles->exprs xs)
                                      example-input-xs-exprs)
                    :input-ys-exprs (if ys
                                      (ops-common/doubles->exprs ys)
                                      example-input-ys-exprs)}
        result     (run-solver run-config)]
    (log/info "CLI: Done!")
    (exit cli-opts)
    result))


(comment (macroexpand-1 `(log/info "Hello")))
(comment (log/info "Hello"))
(comment (run-app-without-gui))
(comment (run-app-with-gui {:use-flamechart true}))
(comment (run-app-with-gui))
