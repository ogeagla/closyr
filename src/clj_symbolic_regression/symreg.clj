(ns clj-symbolic-regression.symreg
  (:require
    [clj-symbolic-regression.ga :as ga]
    [clj-symbolic-regression.gui :as gui]
    [clj-symbolic-regression.ops :as ops]
    [clojure.core.async :as async :refer [go go-loop timeout <!! >!! <! >! chan put! take! alts!!]]
    [clojure.string :as str]
    [flames.core :as flames]
    [seesaw.core :as ss])
  (:import
    (java.text DecimalFormat)
    (java.util
      Date
      List)
    (java.util.concurrent
      CopyOnWriteArrayList)
    (javax.swing
      JButton
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
(def reset?* (atom false))
(def test-timer* (atom nil))
(def sim-input-args* (atom nil))


(def log-steps 1)

(def min-score -100000000)
(def max-leafs 120)


(defn tally-min-score
  []
  (swap! sim-stats* update-in [:scoring :min-scores] #(inc (or % 0))))


(defn score-fn
  [{:keys [input-exprs-list input-exprs-count output-exprs-vec
           sim-stop-start-chan sim->gui-chan]
    :as   run-args}
   v]
  (try
    (let [leafs (.leafCount ^IExpr (:expr v))]
      (if (> leafs max-leafs)
        (do
          (tally-min-score)
          min-score)
        (let [f-of-xs (ops/eval-vec-pheno v run-args)]
          (if f-of-xs
            (let [resids           (map - output-exprs-vec f-of-xs)
                  abs-resids       (map #(min 1000000 (abs %)) resids)
                  resid-sum        (sum abs-resids)
                  ;; score            (* -1 (/ (abs resid) (count abs-resids)))
                  score            (* -1.0 (+ (* 2.0 (/ resid-sum (count abs-resids)))
                                              (last (sort abs-resids))))
                  length-deduction (* (abs score) (min 0.1 (* 0.0000001 leafs leafs)))
                  overall-score    (- score length-deduction)]

              (when (neg? length-deduction)
                (println "warning: negative deduction increases score: " leafs length-deduction v))
              (swap! sim-stats* update-in [:scoring :len-deductions] #(into (or % []) [length-deduction]))

              overall-score)
            (do
              (tally-min-score)
              min-score)))))
    (catch Exception e
      (println "Err in score fn: " (.getMessage e) ", fn: " (str (:expr v)) ", from: " (:expr v))
      (do
        (tally-min-score)
        min-score))))


(defn fn-size-growing-too-fast?
  [v new-v]
  (let [old-count (count (str (:expr v)))
        new-count (count (str (:expr new-v)))]
    (and (< 250 old-count)
         (< old-count new-count))))


(defn mutation-fn
  [initial-muts p-winner p-discard pop]
  (try
    (let [c         (rand-nth ops/mutations-sampler)
          [new-pheno iters] (ops/apply-modifications max-leafs c initial-muts p-winner p-discard)
          old-leafs (.leafCount ^IExpr (:expr p-winner))
          new-leafs (.leafCount ^IExpr (:expr new-pheno))]
      (swap! sim-stats* update-in [:mutations :counts iters] #(inc (or % 0)))
      (swap! sim-stats* update-in [:mutations :size-in] #(into (or % []) [old-leafs]))
      (swap! sim-stats* update-in [:mutations :size-out] #(into (or % []) [new-leafs]))
      new-pheno)
    (catch Exception e
      (println "Err in mutation: " e))))


(defn crossover-fn
  [initial-muts p p-discard pop]
  (let [x-over-res (ops/crossover p p-discard)]
    (when x-over-res
      (swap! sim-stats* update-in [:crossovers :counts] #(inc (or % 0))))
    (or
      x-over-res
      (mutation-fn initial-muts p p-discard pop))))


(defn sort-population
  [pops]
  (->>
    (:pop pops)
    (remove #(nil? (:score %)))
    (sort-by :score)
    (reverse)))

(def ^DecimalFormat score-format (DecimalFormat. "###.#####"))

(defn reportable-phen-str
  [{:keys [^IExpr expr ^double score last-op] p-id :id :as p}]
  (str
    " id: " (str/join (take 3 (str p-id)))
    " last op: " (format "%17s" (str last-op)) #_(str/join (take 8 (str last-op)))
    " score: " (.format score-format score)
    " leafs: " (.leafCount expr)
    " fn: " (str/trim-newline (str expr))))


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
                     {:len-ded-mean (/ (sum len-deductions) (count len-deductions))
                      :len-ded-min  (first len-deductions-sorted)
                      :len-ded-max  (last len-deductions-sorted)
                      :len-ded-med  (nth len-deductions-sorted
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
               (str "\n top best:\n"
                    (->> (take 10 bests)
                         (map reportable-phen-str)
                         (str/join "\n")))
               "\n"
               (summarize-sim-stats))

      (put! sim->gui-chan {:iters                    iters
                           :i                        (inc (- iters i))
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


(defn chart-update-loop
  [sim->gui-chan
   {:keys [^XYChart best-fn-chart
           ^XYChart scores-chart
           ^XChartPanel best-fn-chart-panel
           ^XChartPanel scores-chart-panel
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
          (ss/set-text* ctl-start-stop-btn "Start"))

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
             (if new-state "Start" "Stop")))

  (let [input-exprs      (if input-data-x
                           (ops/doubles->exprs input-data-x)
                           input-exprs)
        output-exprs     (if input-data-y
                           (ops/doubles->exprs input-data-y)
                           output-exprs)

        output-exprs-vec (ops/exprs->doubles output-exprs)
        input-exprs-vec  (ops/exprs->doubles input-exprs)]

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


(defn ->run-args
  [{input-exprs        :input-exprs
    input-exprs-vec    :input-exprs-vec
    output-exprs-vec   :output-exprs-vec
    input-iters        :input-iters
    input-phenos-count :input-phenos-count}]
  {:extended-domain-args (ops/extend-xs input-exprs-vec)
   :input-exprs-list     (ops/exprs->input-exprs-list input-exprs)
   :input-exprs-count    (count input-exprs)
   :input-exprs-vec      input-exprs-vec
   :output-exprs-vec     output-exprs-vec
   :input-iters          input-iters
   :input-phenos-count   input-phenos-count})


(defn wait-and-get-gui-args
  [sim-stop-start-chan]
  ;; wait for GUI to press Start, which submits the new xs/ys data:
  (let [msg (<!! sim-stop-start-chan)]
    (->run-args (update-plot-input-data msg))))


(defn start-gui-and-get-input-data
  [{:keys [iters initial-phenos initial-muts input-exprs output-exprs] :as run-config}]

  ;; these are the data shown in the plots before the expriement is started:
  (reset! sim-input-args* {:input-exprs-vec  (ops/exprs->doubles input-exprs)
                           :output-exprs-vec (ops/exprs->doubles output-exprs)})

  (let [{sim->gui-chan       :sim->gui-chan
         sim-stop-start-chan :sim-stop-start-chan
         :as                 gui-comms} (setup-gui)]

    (merge gui-comms (wait-and-get-gui-args sim-stop-start-chan))))


(defn run-from-inputs
  [{:keys [iters initial-phenos initial-muts] :as run-config}
   {:keys [input-iters input-phenos-count input-exprs-list input-exprs-count output-exprs-vec
           sim-stop-start-chan sim->gui-chan]
    :as   run-args}]
  (let [iters          (or input-iters iters)
        initial-phenos (if input-phenos-count
                         (ops/initial-phenotypes (/ input-phenos-count (count ops/initial-exprs)))
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

    (if @reset?*
      (do
        (reset! reset?* false)
        (println "Restarting...")
        (<!! (timeout 1000))
        (run-from-inputs run-config (merge run-args (->run-args @sim-input-args*))))
      (do
        (run-from-inputs run-config (merge run-args (wait-and-get-gui-args sim-stop-start-chan)))))))


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


(def ^:dynamic *use-flamechart* false)

(defn run-test
  []
  (let [experiment-fn (fn []
                        (run-experiment
                          {:initial-phenos (ops/initial-phenotypes 4000)
                           :initial-muts   (ops/initial-mutations)
                           :input-exprs    input-exprs
                           :output-exprs   output-exprs
                           :iters          200}))]
    (if *use-flamechart*
      ;; with flame graph analysis:
      (in-flames experiment-fn)
      ;; plain experiment:
      (experiment-fn))))



(comment (run-test))
