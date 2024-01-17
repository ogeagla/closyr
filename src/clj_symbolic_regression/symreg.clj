(ns clj-symbolic-regression.symreg
  (:require
    [clj-symbolic-regression.ga :as ga]
    [clj-symbolic-regression.gui :as gui]
    [clj-symbolic-regression.ops :as ops]
    [clojure.core.async :as async :refer [go go-loop timeout <!! >!! <! >! chan put! take! alts!!]]
    [clojure.string :as str]
    [flames.core :as flames])
  (:import
    (java.util
      Date
      List)
    (java.util.concurrent
      CopyOnWriteArrayList)
    (javax.swing
      JLabel)
    (org.knowm.xchart
      XChartPanel
      XYChart)
    (org.matheclipse.core.expression
      F)
    (org.matheclipse.core.interfaces
      IExpr
      ISymbol)))


(set! *warn-on-reflection* true)


(defn sum
  [coll]
  (reduce + 0.0 coll))


(def sim-stats* (atom {}))


(defn score-fn
  [{:keys [input-exprs-list input-exprs-count output-exprs-vec
           sim-stop-start-chan sim->gui-chan]
    :as   run-args}
   v]
  (try
    (let [leafs            (.leafCount ^IExpr (:expr v))
          f-of-xs          (ops/eval-vec-pheno v run-args)
          resids           (map - output-exprs-vec f-of-xs)
          resid            (->> resids
                                (map #(min 100000 (abs %)))
                                (sum))
          score            (* -1 (abs resid))
          length-deduction (* 0.000001 leafs)
          overall-score    (- score length-deduction)]

      (when (neg? length-deduction) (println "warning: negative deduction increases score: " leafs length-deduction v))
      (swap! sim-stats* update-in [:scoring :len-deductions] #(into (or % []) [length-deduction]))

      overall-score)
    (catch Exception e
      -1000000)))


(def mutations-sampler
  [1 1 1 1 1 1 1 1 1 1 1
   1 1 1 1 1 1 1 1 1 1 1
   1 1 1 1 1 1 1 1 1 1 1
   1 1 1 1 1 1 1 1 1 1 1
   2 2 2 2 2 2
   2 2 2 2 2 2
   2 2 2 2 2 2
   2 2 2 2 2 2
   3 3 3 3
   3 3 3 3
   3 3 3 3
   3 3 3 3
   4 4 4
   4 4 4
   4 4 4
   4 4 4
   5 5 5
   5 5 5
   5 5 5
   5 5
   6 6
   6 6
   6 6
   6 6
   7 7
   7 7
   7 7
   7
   8 8
   8 8
   8 8
   9 9
   9 9
   9
   10 10
   10
   11 11
   11
   12 12
   13 13
   14
   15
   16
   17
   18
   19
   20])


(defn rand-mut
  [initial-muts]
  (rand-nth initial-muts))


(defn fn-size-growing-too-fast?
  [v new-v]
  (let [old-count (count (str (:expr v)))
        new-count (count (str (:expr new-v)))]
    (and (< 250 old-count)
         (< old-count new-count))))


(defn mutation-fn
  [initial-muts v v-discard pop]
  (try
    (let [c         (rand-nth mutations-sampler)
          new-pheno (loop [c          c
                           v          v
                           first-run? true]
                      (if (zero? c)
                        v
                        (let [v     (if first-run? (assoc v :util (:util v-discard)) v)
                              m     (rand-mut initial-muts)
                              new-v (ops/modify m v)]
                          (recur
                            (if (fn-size-growing-too-fast? v new-v)
                              0
                              (dec c))
                            new-v
                            false))))
          old-leafs (.leafCount ^IExpr (:expr v))
          new-leafs (.leafCount ^IExpr (:expr new-pheno))]
      (swap! sim-stats* update-in [:mutations :counts c] #(inc (or % 0)))
      (swap! sim-stats* update-in [:mutations :size-in] #(into (or % []) [old-leafs]))
      (swap! sim-stats* update-in [:mutations :size-out] #(into (or % []) [new-leafs]))
      new-pheno)
    (catch Exception e
      (println "Err in mutation: " e))))


(defn crossover-fn
  [initial-muts v v-discard pop]
  ;; todo do something for crossover
  (mutation-fn initial-muts v v-discard pop))


(defn sort-population
  [pops]
  (->>
    (:pop pops)
    (remove #(nil? (:score %)))
    (sort-by :score)
    (reverse)))


(defn reportable-phen-str
  [p]

  (str
    " id: " (-> p :id)
    " score: " (-> p :score)
    " fn: " (-> p :expr str)))


(defn summarize-sim-stats
  []
  (let [{{cs     :counts
          sz-in  :size-in
          sz-out :size-out}               :mutations
         {len-deductions :len-deductions} :scoring
         :as                              dat} @sim-stats*]
    (let [data-str (-> dat
                       (assoc :scoring {:len-deductions (/ (sum len-deductions) (count len-deductions))})
                       (assoc :mutations {:counts        (reverse (sort-by second cs))
                                          :size-in-mean  (Math/round
                                                           ^double (/ (sum sz-in) (count sz-in)))
                                          :size-out-mean (Math/round
                                                           ^double (/ (sum sz-out) (count sz-out)))}))]
      (str "muts:" (count sz-in)
           " "
           data-str))))


(def test-timer* (atom nil))

(def log-steps 1)


(defn report-iteration
  [i
   iters
   ga-result
   {:keys [input-exprs-list input-exprs-count output-exprs-vec
           sim-stop-start-chan sim->gui-chan extended-domain-args]
    :as   run-args}]
  (when (or (= 1 i) (zero? (mod i log-steps)))
    (let [end      (Date.)
          diff     (- (.getTime end) (.getTime ^Date @test-timer*))
          bests    (sort-population ga-result)
          took-s   (/ diff 1000.0)
          pop-size (count (:pop ga-result))
          best-v   (first bests)
          evaled   (ops/eval-vec-pheno best-v run-args)
          {evaled-extended :ys xs-extended :xs} (ops/eval-vec-pheno-oversample best-v run-args extended-domain-args)]

      (reset! test-timer* end)
      (println i "-step pop size: " pop-size
               " took secs: " took-s
               " phenos/s: " (Math/round ^double (/ (* pop-size log-steps) took-s))
               "\n top best:\n"
               (->> (take 5 bests)
                    (map reportable-phen-str)
                    (str/join "\n"))
               "\n"
               (summarize-sim-stats))



      (put! sim->gui-chan {:iters                    iters
                           :i                        (- iters i)
                           :best-eval                evaled
                           :input-exprs-vec-extended xs-extended
                           :best-eval-extended       evaled-extended
                           :best-f-str               (str (:expr best-v))
                           :best-score               (:score best-v)})))
  (reset! sim-stats* {}))


(defn near-exact-solution
  [i old-score old-scores]
  (println "Perfect score! " i old-score " all scores: " old-scores)
  0)


(def input-exprs
  (->>
    (range 50)
    (map (fn [i] (* Math/PI (/ i 15.0))))
    ops/doubles->exprs))


(def output-exprs
  (->>
    (range 50)
    (map (fn [i] 0.0))
    ops/doubles->exprs))


(defn clamp-infinites
  [doubles]
  (mapv (fn [v]
          (cond
            (infinite? v) 0.0
            (> (abs v) 10e9) 0.0
            :else v))
        doubles))


(def plot-args* (atom nil))


(defn chart-update-loop
  [sim->gui-chan
   {:keys [^XYChart best-fn-chart
           ^XYChart scores-chart
           ^XChartPanel best-fn-chart-panel
           ^XChartPanel scores-chart-panel
           ^JLabel info-label]}
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

      (let [{:keys [input-exprs-vec output-exprs-vec]} @plot-args*]
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

        (when (zero? i)
          (.clear xs-scores)
          (.clear ys-scores))

        (.add xs-scores i)
        (.add ys-scores best-score)
        (.updateXYSeries scores-chart series-scores-label xs-scores ys-scores nil)
        (.setTitle scores-chart "Best Score")

        (.setText info-label (str "<html>Iteration: " i "/" iters
                                  "<br>Best Function: "
                                  "<br><code> y = " best-f-str "</code>"
                                  "<br>Score: " best-score
                                  "</html>"))
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
        {:keys [input-exprs-vec output-exprs-vec]} @plot-args*]
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
  [{new-state    :new-state
    reset?       :reset
    input-data-x :input-data-x
    input-data-y :input-data-y}]

  (println "Got state req: "
           (if reset?
             "Reset"
             (if new-state "Start" "Stop")))

  (let [input-exprs      (if input-data-x
                           (mapv (fn [^double pt-x] (F/num pt-x)) input-data-x)
                           input-exprs)
        output-exprs     (if input-data-y
                           (mapv (fn [^double pt-y] (F/num pt-y)) input-data-y)
                           output-exprs)

        output-exprs-vec (ops/exprs->doubles output-exprs)
        input-exprs-vec  (ops/exprs->doubles input-exprs)]
    (reset! plot-args* {:input-exprs-vec  input-exprs-vec
                        :output-exprs-vec output-exprs-vec})
    {:input-exprs      input-exprs
     :output-exprs     output-exprs
     :output-exprs-vec output-exprs-vec
     :input-exprs-vec  input-exprs-vec}))


(defn restart-with-new-inputs
  [msg]
  (println "Restarting experiment! ")
  (update-plot-input-data msg)
  (<!! (timeout 1000))
  true)


(defn check-start-stop-state
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


(defn start-gui-and-get-input-data
  [{:keys [iters initial-phenos initial-muts input-exprs output-exprs] :as run-config}]
  ;; to not use the GUI, pass the initial values through
  (let [input-exprs-vec  (ops/exprs->doubles input-exprs)
        output-exprs-vec (ops/exprs->doubles output-exprs)

        _                (reset! plot-args* {:input-exprs-vec  input-exprs-vec
                                             :output-exprs-vec output-exprs-vec})


        {sim->gui-chan       :sim->gui-chan
         sim-stop-start-chan :sim-stop-start-chan
         :as                 gui-comms} (setup-gui)

        ;; wait for GUI to press Start, which submits the new xs/ys data:
        {new-state    :new-state
         input-data-x :input-data-x
         input-data-y :input-data-y
         :as          msg} (<!! sim-stop-start-chan)

        {input-exprs      :input-exprs
         output-exprs     :output-exprs
         output-exprs-vec :output-exprs-vec
         input-exprs-vec  :input-exprs-vec} (update-plot-input-data msg)]

    (reset! plot-args* {:input-exprs-vec  input-exprs-vec
                        :output-exprs-vec output-exprs-vec})

    (merge gui-comms
           {:extended-domain-args (ops/extend-xs input-exprs-vec)
            :input-exprs-list     (ops/exprs->input-exprs-list input-exprs)
            :input-exprs-count    (count input-exprs)
            :input-exprs-vec      input-exprs-vec
            :output-exprs-vec     output-exprs-vec})))


(def reset?* (atom false))


(defn run-from-inputs
  [{:keys [iters initial-phenos initial-muts] :as run-config}
   {:keys [input-exprs-list input-exprs-count output-exprs-vec
           sim-stop-start-chan sim->gui-chan]
    :as   run-args}]
  (let [start (Date.)
        pop1  (ga/initialize
                initial-phenos
                (partial score-fn run-args)
                (partial mutation-fn initial-muts)
                (partial crossover-fn initial-muts))]
    (println "Start " start)
    (reset! test-timer* start)

    (loop [pop pop1
           i   iters]
      (if (zero? i)
        pop
        (let [restart? (reset! reset?* (check-start-stop-state run-args))]
          (if restart?
            pop
            (let [{old-scores :pop-old-scores old-score :pop-old-score :as ga-result} (ga/evolve pop)]
              (report-iteration i iters ga-result run-args)
              (recur ga-result
                     (if (or (zero? old-score) (some #(> % -1e-3) old-scores))
                       (near-exact-solution i old-score old-scores)
                       (dec i))))))))

    (let [end  (Date.)
          diff (- (.getTime end) (.getTime start))]

      (println "Took " (/ diff 1000.0) " seconds"))

    (when @reset?*
      (reset! reset?* false)
      (println "Restarting...")
      (<!! (timeout 1000))
      (run-from-inputs run-config run-args))))


(defn run-experiment
  [{:keys [iters initial-phenos initial-muts input-exprs output-exprs] :as run-config}]
  (println "iters: " iters)
  (println "initial pop: " (count initial-phenos))
  (println "initial muts: " (count initial-muts))
  (let [run-args (start-gui-and-get-input-data run-config)]
    (run-from-inputs run-config run-args)))


(defn in-flames
  [f]
  ;; http://localhost:54321/flames.svg
  (let [flames (flames/start! {:port 54321, :host "localhost"})]
    (f)
    (flames/stop! flames)))


(defn run-test
  []
  (let [experiment-fn (fn []
                        (run-experiment
                          {:initial-phenos (ops/initial-phenotypes ops/sym-x 1000)
                           :initial-muts   (ops/initial-mutations)
                           :input-exprs    input-exprs
                           :output-exprs   output-exprs
                           :iters          500}))]
    ;; with flame graph analysis:
    ;; (in-flames experiment-fn)
    ;; plain experiment:
    (experiment-fn)))


;; todo: pick iters / phenos count in gui

(comment (run-test))
