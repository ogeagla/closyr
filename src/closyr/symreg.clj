(ns closyr.symreg
  (:refer-clojure :exclude [rand rand-int rand-nth shuffle])
  (:require
    [clojure.core.async :as async :refer [go go-loop timeout <!! >!! <! >! chan put! take! alts!!]]
    [clojure.string :as str]
    [closyr.dataset.prng :refer :all]
    [closyr.ga :as ga]
    [closyr.ops.common :as ops-common]
    [closyr.ops.eval :as ops-eval]
    [closyr.ops.initialize :as ops-init]
    [closyr.ops.modify :as ops-modify]
    [closyr.ui.gui :as gui]
    [flames.core :as flames]
    [seesaw.core :as ss])
  (:import
    (java.text
      DecimalFormat)
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
      IExpr
      ISymbol)))


(set! *warn-on-reflection* true)


(defn sum
  [coll]
  (reduce + 0.0 coll))


(def sim-stats* (atom {}))
(def gui-requested-reset?* (atom false))
(def test-timer* (atom nil))
(def sim-input-args* (atom nil))


(def ^:dynamic *log-steps* 1)

(def min-score -100000000)
(def max-leafs 60)
(def max-resid 1000000)


(defn tally-min-score
  [min-score]
  (swap! sim-stats* update-in [:scoring :min-scores] #(inc (or % 0)))
  min-score)


(defn not-finite?
  [n]
  (or (not (number? n))
      (and (number? n) (Double/isNaN n))))


(defn score-fn
  [{:keys [input-exprs-list input-exprs-count output-exprs-vec
           sim-stop-start-chan sim->gui-chan]
    :as   run-args}
   pheno]
  (try
    (let [leafs (.leafCount ^IExpr (:expr pheno))]
      (if (> leafs max-leafs)
        (tally-min-score min-score)
        (let [f-of-xs (ops-eval/eval-vec-pheno pheno run-args)]
          (if f-of-xs
            (let [abs-resids       (map
                                     (fn [expected actual]
                                       (let [res (if (not-finite? actual)
                                                   max-resid
                                                   (- expected actual))]
                                         (if (not-finite? res)
                                           (do
                                             (println "warning, res not a number: " res
                                                      " exp: " expected " actual: " actual)
                                             max-resid)
                                           (min max-resid (abs res)))))
                                     output-exprs-vec
                                     f-of-xs)
                  resid-sum        (sum abs-resids)
                  score            (* -1.0 (+ (* 2.0 (/ resid-sum (count abs-resids)))
                                              (reduce max abs-resids)))
                  length-deduction (* (abs score) (min 0.1 (* 0.0000001 leafs leafs)))
                  overall-score    (- score length-deduction)]


              (when (or (neg? length-deduction) (Double/isNaN length-deduction))
                (println "warning: bad/negative deduction increases score: "
                         leafs length-deduction (str (:expr pheno))))
              (swap! sim-stats* update-in [:scoring :len-deductions] #(into (or % []) [length-deduction]))

              overall-score)
            (tally-min-score min-score)))))
    (catch Exception e
      (println "Err in score fn: " (.getMessage e) ", fn: " (str (:expr pheno)) ", from: " (:expr pheno))
      (tally-min-score min-score))))


(defn fn-size-growing-too-fast?
  [v new-v]
  (let [old-count (count (str (:expr v)))
        new-count (count (str (:expr new-v)))]
    (and (< 250 old-count)
         (< old-count new-count))))


(defn mutation-fn
  [initial-muts p-winner p-discard pop]
  (try
    (let [start     (Date.)
          c         (rand-nth ops-modify/mutations-sampler)
          [new-pheno iters mods] (ops-modify/apply-modifications max-leafs c initial-muts p-winner p-discard)
          diff-ms   (ops-common/start-date->diff-ms start)
          old-leafs (.leafCount ^IExpr (:expr p-winner))
          new-leafs (.leafCount ^IExpr (:expr new-pheno))]

      (when (> diff-ms 5000)
        (println "Warning, this modification sequence took a long time: "
                 diff-ms " ms for mods: " (count mods)
                 "\n mods: " mods
                 "\n for old expr: " (:expr p-winner)
                 "\n and new expr: " (:expr new-pheno)))

      (swap! sim-stats* update-in [:mutations :counts iters] #(inc (or % 0)))
      (swap! sim-stats* update-in [:mutations :size-in] #(into (or % []) [old-leafs]))
      (swap! sim-stats* update-in [:mutations :size-out] #(into (or % []) [new-leafs]))
      (assoc new-pheno :mods-applied iters))
    (catch Exception e
      (println "Err in mutation: " e))))


(defn crossover-fn
  [initial-muts p p-discard pop]
  (let [crossover-result (ops-modify/crossover p p-discard)]
    (when crossover-result
      (swap! sim-stats* update-in [:crossovers :counts] #(inc (or % 0))))
    (or
      crossover-result
      (mutation-fn initial-muts p p-discard pop))))


(defn sort-population
  [pops]
  (->>
    (:pop pops)
    (remove #(nil? (:score %)))
    (sort-by :score)
    (reverse)))


(def ^DecimalFormat score-format (DecimalFormat. "###.#####"))


(defn format-fn-str
  [fn-str]
  (str/replace (str/trim-newline (str fn-str)) #"\n|\r" ""))


(defn reportable-phen-str
  [{:keys [^IExpr expr ^double score last-op mods-applied] p-id :id :as p}]
  (str
    " id: " (str/join (take 3 (str p-id)))
    " last mod #: " (or mods-applied "-")
    " last op: " (format "%17s" (str last-op)) #_(str/join (take 8 (str last-op)))
    " score: " (.format score-format score)
    " leafs: " (.leafCount expr)
    ;; strip newlines from here also:
    " fn: " (format-fn-str expr)))


(defn summarize-sim-stats
  []
  (let [{{xcs :counts}                :crossovers
         {cs     :counts
          sz-in  :size-in
          sz-out :size-out}           :mutations
         {len-deductions :len-deductions
          min-scores     :min-scores} :scoring
         :as                          dat} @sim-stats*]
    (let [len-deductions-sorted
          (sort len-deductions)

          sz-in-sorted
          (sort sz-in)

          sz-out-sorted
          (sort sz-out)
          summary-data
          (-> dat
              (assoc :crossovers
                     {:crossovers-count xcs})
              (assoc :scoring
                     {:len-deductions (count len-deductions)
                      :len-ded-mean   (/ (sum len-deductions) (count len-deductions))
                      :len-ded-min    (first len-deductions-sorted)
                      :len-ded-max    (last len-deductions-sorted)
                      :len-ded-med    (nth len-deductions-sorted
                                           (/ (count len-deductions-sorted) 2))})
              (assoc :mutations
                     {:counts              (reverse (sort-by second cs))
                      :sz-in-mean-max-min  [(Math/round ^double (/ (sum sz-in) (count sz-in)))
                                            (last sz-in-sorted)
                                            (first sz-in-sorted)]

                      :sz-out-mean-max-min [(Math/round ^double (/ (sum sz-out) (count sz-out)))
                                            (last sz-out-sorted)
                                            (first sz-out-sorted)]}))]
      (str "muts:" (count sz-in) " min scores: " min-scores
           " "
           (:scoring summary-data)
           "\n  "
           (:mutations summary-data)
           "\n  "
           (:crossovers summary-data)))))


(defn report-iteration
  [i
   iters
   ga-result
   {:keys [input-exprs-list input-exprs-count output-exprs-vec
           sim-stop-start-chan sim->gui-chan extended-domain-args]
    :as   run-args}
   {:keys [use-gui?] :as run-config}]
  (when (or (= 1 i) (zero? (mod i *log-steps*)))
    (let [bests    (sort-population ga-result)
          took-s   (/ (ops-common/start-date->diff-ms @test-timer*) 1000.0)
          pop-size (count (:pop ga-result))
          best-v   (first bests)
          evaled   (ops-eval/eval-vec-pheno best-v run-args)
          {evaled-extended :ys xs-extended :xs} (ops-eval/eval-vec-pheno-oversample
                                                  best-v run-args extended-domain-args)]

      (reset! test-timer* (Date.))
      (println i "-step pop size: " pop-size
               " took secs: " took-s
               " phenos/s: " (Math/round ^double (/ (* pop-size *log-steps*) took-s))
               (str "\n top 20 best:\n"
                    (->> (take 20 bests)
                         (map reportable-phen-str)
                         (str/join "\n")))
               "\n"
               (summarize-sim-stats))

      (when use-gui?
        (put! sim->gui-chan {:iters                    iters
                             :i                        (inc (- iters i))
                             :best-eval                evaled
                             :input-exprs-vec-extended xs-extended
                             :best-eval-extended       evaled-extended
                             :best-f-str               (str (:expr best-v))
                             :best-score               (:score best-v)}))))
  (reset! sim-stats* {}))


(defn near-exact-solution
  [i old-scores]
  (println "Perfect score! " i " top scores: " (take 10 (sort old-scores)))
  0)


(def input-exprs
  (->>
    (range 50)
    (map (fn [i] (* Math/PI (/ i 15.0))))
    ops-common/doubles->exprs))


(def output-exprs
  (->>
    (range 50)
    (map (fn [i] 0.0))
    ops-common/doubles->exprs))


(defn clamp-infinites
  [doubles]
  (mapv (fn [v]
          (cond
            (infinite? v) 0.0
            (> (abs v) 10e9) 0.0
            :else v))
        doubles))


(defn chart-update-loop
  [sim->gui-chan
   {:keys [^XYChart best-fn-chart
           ^XYChart scores-chart
           ^XChartPanel best-fn-chart-panel
           ^XChartPanel scores-chart-panel
           ^JTextField sim-selectable-text
           ^JLabel info-label
           ^JButton ctl-start-stop-btn]}
   {:keys [^List xs-best-fn ^List xs-objective-fn ^List ys-best-fn ^List ys-objective-fn
           ^String series-best-fn-label ^String series-objective-fn-label
           ^List xs-scores
           ^List ys-scores
           ^String series-scores-label]
    :as   conf}]
  (go-loop []
    (<! (timeout 100))
    (when-let [{:keys [input-exprs-vec-extended
                       best-eval-extended
                       best-eval best-score best-f-str i iters]
                :as   sim-msg} (<! sim->gui-chan)]

      (let [{:keys [input-exprs-vec output-exprs-vec]} @sim-input-args*]
        (.clear ys-best-fn)
        (.addAll ys-best-fn (if best-eval-extended
                              (clamp-infinites best-eval-extended)
                              best-eval))

        (.clear ys-objective-fn)
        (.addAll ys-objective-fn output-exprs-vec)

        (.clear xs-best-fn)
        (.addAll xs-best-fn (or input-exprs-vec-extended input-exprs-vec))

        (.clear xs-objective-fn)
        (.addAll xs-objective-fn input-exprs-vec)

        (.setTitle best-fn-chart "Best vs Objective Functions")
        (.updateXYSeries best-fn-chart series-best-fn-label xs-best-fn ys-best-fn nil)
        (.updateXYSeries best-fn-chart series-objective-fn-label xs-objective-fn ys-objective-fn nil)

        (when (= 1 i)
          (.clear xs-scores)
          (.clear ys-scores))

        (when (= iters i)
          (let [^JButton reset-btn @gui/ctl-reset-btn*]
            (ss/set-text* ctl-start-stop-btn gui/ctl:start)
            (.setEnabled reset-btn false)))

        (.add xs-scores i)
        (.add ys-scores best-score)
        (.updateXYSeries scores-chart series-scores-label xs-scores ys-scores nil)
        (.setTitle scores-chart "Best Score")

        (let [fn-str (str "y = " (format-fn-str best-f-str))]
          (when (not= fn-str (.getText sim-selectable-text))
            (println "New Best Function: " fn-str)
            (.setText sim-selectable-text fn-str)
            (.revalidate sim-selectable-text)
            (.repaint sim-selectable-text)))

        (.setText info-label (str
                               ;; "<html>"
                               "Iteration: " i "/" iters
                               ;; "<br>Best Function: "
                               ;; "<br><code> y = " best-f-str "</code>"
                               " Score: " best-score
                               ;; "</html>"
                               ))
        (.revalidate info-label)
        (.repaint info-label)

        (.revalidate best-fn-chart-panel)
        (.repaint best-fn-chart-panel)

        (.revalidate scores-chart-panel)
        (.repaint scores-chart-panel)

        (recur)))))


(defn setup-gui
  []
  (let [sim->gui-chan       (chan)
        sim-stop-start-chan (chan)
        {:keys [input-exprs-vec output-exprs-vec]} @sim-input-args*]
    (gui/create-and-show-gui
      {:sim-stop-start-chan       sim-stop-start-chan
       :xs-best-fn                (doto (CopyOnWriteArrayList.) (.addAll input-exprs-vec))
       :xs-objective-fn           (doto (CopyOnWriteArrayList.) (.addAll input-exprs-vec))
       :ys-best-fn                (doto (CopyOnWriteArrayList.) (.addAll (repeat (count input-exprs-vec) 0.0)))
       :ys-objective-fn           (doto (CopyOnWriteArrayList.) (.addAll output-exprs-vec))
       :xs-scores                 (doto (CopyOnWriteArrayList.) (.add 0.0))
       :ys-scores                 (doto (CopyOnWriteArrayList.) (.add 0.0))
       :series-scores-label       "best score"
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

  (let [input-exprs      (if input-data-x
                           (ops-common/doubles->exprs input-data-x)
                           input-exprs)
        output-exprs     (if input-data-y
                           (ops-common/doubles->exprs input-data-y)
                           output-exprs)

        output-exprs-vec (ops-common/exprs->doubles output-exprs)
        input-exprs-vec  (ops-common/exprs->doubles input-exprs)]

    (reset! sim-input-args* {:input-exprs        input-exprs
                             :input-exprs-vec    input-exprs-vec
                             :output-exprs-vec   output-exprs-vec
                             :input-iters        input-iters
                             :input-phenos-count input-phenos-count})

    @sim-input-args*))


(defn restart-with-new-inputs
  [msg]
  (println "Restarting experiment! ")
  (update-plot-input-data msg)
  (<!! (timeout 500))
  true)


(defn check-start-stop-state
  "If no message from GUI is available, no-op, otherwise process stop/restart requests"
  [{:keys [input-exprs-list input-exprs-count output-exprs-vec
           sim-stop-start-chan sim->gui-chan]
    :as   run-args}]
  (let [[msg ch] (alts!! [sim-stop-start-chan] :default :continue :priority true)]
    (if (= msg :continue)
      nil
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
  [{input-exprs        :input-exprs
    input-exprs-vec    :input-exprs-vec
    output-exprs-vec   :output-exprs-vec
    input-iters        :input-iters
    iters              :iters
    input-phenos-count :input-phenos-count
    initial-phenos     :initial-phenos}]

  (when-not (and input-exprs
                 input-exprs-vec
                 output-exprs-vec
                 (or input-iters iters)
                 (or input-phenos-count
                     initial-phenos))
    (throw (Exception. "Run args needs all params!")))

  {:extended-domain-args (ops-eval/extend-xs input-exprs-vec)
   :input-exprs-list     (ops-common/exprs->input-exprs-list input-exprs)
   :input-exprs-count    (count input-exprs)
   :input-exprs-vec      input-exprs-vec
   :output-exprs-vec     output-exprs-vec
   :input-iters          (or input-iters iters)
   :initial-phenos       initial-phenos
   :input-phenos-count   (when input-phenos-count
                           input-phenos-count)})


(defn wait-and-get-gui-args
  "Park and wait on GUI input, return the inputs as args to GA run"
  [sim-stop-start-chan]
  ;; wait for GUI to press Start, which submits the new xs/ys data:
  (let [msg (<!! sim-stop-start-chan)]
    (->run-args (update-plot-input-data msg))))


(defn start-gui-and-get-input-data
  "Initialize and show GUI, then park and wait on user input to start"
  [{:keys [iters initial-phenos initial-muts input-exprs output-exprs] :as run-config}]

  ;; these are the data shown in the plots before the expriement is started:
  (reset! sim-input-args* {:input-exprs-vec  (ops-common/exprs->doubles input-exprs)
                           :output-exprs-vec (ops-common/exprs->doubles output-exprs)})

  (let [{sim->gui-chan       :sim->gui-chan
         sim-stop-start-chan :sim-stop-start-chan
         :as                 gui-comms} (setup-gui)]

    (merge gui-comms (wait-and-get-gui-args sim-stop-start-chan))))


(defn run-from-inputs
  "Run GA as symbolic regression engine on input/output (x/y) dataset using initial functions and mutations"
  [{:keys [iters initial-phenos initial-muts use-gui?] :as run-config}
   {:keys [input-iters input-phenos-count input-exprs-list input-exprs-count output-exprs-vec
           sim-stop-start-chan sim->gui-chan]
    :as   run-args}]
  (let [iters          (or input-iters iters)
        initial-phenos (if input-phenos-count
                         (ops-init/initial-phenotypes (/ input-phenos-count (count ops-init/initial-exprs)))
                         initial-phenos)
        start          (Date.)
        pop1           (ga/initialize
                         initial-phenos
                         (partial score-fn run-args)
                         (partial mutation-fn initial-muts)
                         (partial crossover-fn initial-muts))]
    (println "Start " start "iters: " iters " pop size: " (count initial-phenos))
    (reset! test-timer* start)

    (loop [pop pop1
           i   iters]
      (if (zero? i)
        pop
        (if (and use-gui? (reset! gui-requested-reset?* (check-start-stop-state run-args)))
          pop
          (let [{scores :pop-scores :as ga-result} (ga/evolve pop)]
            (report-iteration i iters ga-result run-args run-config)
            (recur ga-result
                   (if (some #(> % -1e-3) scores)
                     (near-exact-solution i scores)
                     (dec i)))))))

    (println "Took " (/ (ops-common/start-date->diff-ms start) 1000.0) " seconds")

    (if @gui-requested-reset?*
      (do
        (reset! gui-requested-reset?* false)
        (println "Restarting...")
        (<!! (timeout 500))
        (run-from-inputs run-config (merge run-args (->run-args @sim-input-args*))))
      (if use-gui?
        (do
          (println "Experiment complete, waiting for GUI to start another")
          (run-from-inputs run-config (merge run-args (wait-and-get-gui-args sim-stop-start-chan))))
        (println "Done.")))))


(defn run-experiment
  [{:keys [iters initial-phenos initial-muts input-exprs output-exprs use-gui?] :as run-config}]
  (println "initial data: iters: " iters
           "pop: " (count initial-phenos)
           "muts: " (count initial-muts))

  (if use-gui?
    (do
      (let [run-args (start-gui-and-get-input-data run-config)]
        (run-from-inputs run-config run-args)))

    (do
      (reset! sim-input-args* {:input-exprs      input-exprs
                               :input-exprs-vec  (ops-common/exprs->doubles input-exprs)
                               :output-exprs-vec (ops-common/exprs->doubles output-exprs)})

      (run-from-inputs run-config (->run-args (merge @sim-input-args* run-config))))))


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
         :input-exprs    (->> (range 50)
                              (map (fn [i] (* Math/PI (/ i 15.0))))
                              ops-common/doubles->exprs)
         :output-exprs   (->> (range 50)
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
         :input-exprs    input-exprs
         :output-exprs   output-exprs
         :iters          200
         :use-gui?       true}))))


;; todo: [/] symreg ns needs clearer divisions between experiment and GUI
;;           - works without gui but the code is a little ugly
;; todo: [ ] fix can run uberjar: classload error on LogManager: turn off AOT?


(comment (run-app-without-gui))
(comment (run-app-with-gui))
