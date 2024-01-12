(ns clj-symbolic-regression.symreg
  (:require
    [clj-symbolic-regression.ga :as ga]
    [clj-symbolic-regression.gui :as gui]
    [clj-symbolic-regression.ops :as ops]
    [clj-symbolic-regression.plot :as plot]
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
    (org.apache.log4j Level LogManager Logger)
    (org.knowm.xchart
      XChartPanel
      XYChart)
    (org.matheclipse.core.expression
      F)
    (org.matheclipse.core.interfaces
      IExpr
      ISymbol)))


(set! *warn-on-reflection* true)

(def ^ISymbol sym-x (F/Dummy "x"))


(defn eval-vec-pheno
  [p input-exprs-count input-exprs-list]
  (let [^IExpr new-expr (:expr p)
        new-is-const    (.isNumber new-expr)
        ^IExpr eval-p   (ops/eval-phenotype-on-expr-args p input-exprs-list)
        ;; ^IExpr eval-p   (ops/eval-phenotype-on-string-args p input-exprs-F-strings)
        vs              (vec (map
                               (fn [i]
                                 (try
                                   (.doubleValue
                                     (.toNumber (.getArg eval-p (inc i) F/Infinity)))
                                   (catch Exception e
                                     Double/POSITIVE_INFINITY)))
                               (range (dec (.size eval-p)))))
        vs              (if (seq vs)
                          vs
                          (vec (map
                                 (fn [i]
                                   (.doubleValue
                                     (.toNumber (if new-is-const
                                                  new-expr
                                                  (.getArg eval-p 0 F/Infinity)))))
                                 (range input-exprs-count))))]
    vs))


(defn sum
  [coll]
  (reduce + 0.0 coll))


(def sim-stats* (atom {}))


(defn score-fn
  [input-exprs-list input-exprs-count output-exprs-vec v]
  (try
    (let [leafs            (.leafCount ^IExpr (:expr v))
          ;; expr-str         (str (:expr v))
          ;; f-of-xs          (or
          ;;                   (get-cached-fn-eval expr-str input-exprs-F-strings)
          ;;                   (put-cached-fn-eval
          ;;                     expr-str
          ;;                     input-exprs-F-strings
          ;;                     (eval-vec-pheno v input-exprs input-exprs-F-strings)))
          f-of-xs          (eval-vec-pheno v input-exprs-count input-exprs-list)

          resids           (map (fn [output expted]
                                  (- expted output))
                                f-of-xs
                                output-exprs-vec)
          resid            (sum (map #(min 100000 (abs %)) resids))
          score            (* -1 (abs resid))
          length-deduction (* 0.0001 leafs)]
      (when (zero? resid) (println "warning: zero resid " resids))
      (when (neg? length-deduction) (println "warning: negative deduction increases score: " leafs length-deduction v))

      (swap! sim-stats* update-in [:scoring :len-deductions] #(into (or % []) [length-deduction]))

      (- score length-deduction))
    (catch Exception e
      -1000000)))


(def mutations-sampler
  [1 1 1 1 1 1 1 1 1
   2 2 2 2 2 2
   3 3 3 3
   4 4 4
   5 5
   6
   7
   8])


(defn rand-mut
  [initial-muts]
  (rand-nth initial-muts))


(defn mutation-fn
  [initial-muts v v-discard]
  (try
    (let [c         (rand-nth mutations-sampler)
          new-pheno (loop [c          c
                           v          v
                           first-run? true]
                      (if (zero? c)
                        v
                        (recur
                          (dec c)
                          (ops/modify (rand-mut initial-muts) (if first-run?
                                                                (assoc v :util (:util v-discard))
                                                                v))
                          false)))
          old-leafs (.leafCount ^IExpr (:expr v))
          new-leafs (.leafCount ^IExpr (:expr new-pheno))]
      (swap! sim-stats* update-in [:mutations :counts c] #(inc (or % 0)))
      (swap! sim-stats* update-in [:mutations :size-in] #(into (or % []) [old-leafs]))
      (swap! sim-stats* update-in [:mutations :size-out] #(into (or % []) [new-leafs]))
      new-pheno)
    (catch Exception e
      (println "Err in mutation: " e))))


(defn crossover-fn
  [initial-muts v v-discard]
  ;; todo do something for crossover
  (mutation-fn initial-muts v v-discard))


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
                       (assoc :mutations {:counts        cs
                                          :size-in-mean  (/ (sum sz-in) (count sz-in))
                                          :size-out-mean (/ (sum sz-out) (count sz-out))}))]
      (str " mut size: " (count sz-in) " len deduction: " (count len-deductions)
           " "
           data-str))))


(def test-timer* (atom nil))

(def log-steps 5)


(defn report-iteration
  [i ga-result sim->gui-chan input-exprs-count input-exprs-list]
  (when (or (= 1 i) (zero? (mod i log-steps)))
    (let [old-score  (:pop-old-score ga-result)
          old-scores (:pop-old-scores ga-result)
          end        (Date.)
          diff       (- (.getTime end) (.getTime ^Date @test-timer*))
          bests      (sort-population ga-result)
          took-s     (/ diff 1000.0)
          pop-size   (count (:pop ga-result))

          best-v     (first bests)
          evaled     (eval-vec-pheno best-v input-exprs-count input-exprs-list)]

      (reset! test-timer* end)
      (println i "-step pop size: " pop-size " took secs: " took-s " phenos/s: " (Math/round ^double (/ (* pop-size log-steps) took-s)))
      (println i " pop score: " old-score
               " mean: " (Math/round (float (/ old-score (count (:pop ga-result))))))
      (println i " top best: "
               (->> (take 5 bests)
                    (map reportable-phen-str)
                    (str/join "\n")))
      (println i " sim stats: " (summarize-sim-stats))
      ;; (println i " fn eval cache: " @fn-eval-cache-stats*)

      (put! sim->gui-chan {:best-eval evaled :best-f-str (str (:expr best-v))})))
  (reset! sim-stats* {}))


(defn near-exact-solution
  [i old-score old-scores]
  (println "Perfect score! " i old-score " all scores: " old-scores)
  0)


(def input-exprs
  (->>
    (range 50)
    (map (fn [i]
           (.add F/C0 (* Math/PI (/ i 15.0)))))
    vec))


(def output-exprs
  (->>
    (range 50)
    (map (fn [i]
           (let [x (* Math/PI (/ i 15.0))]
             (.add F/C0 0.0 #_(+ (* 0.5 x x) 2.0 (* 4.0 (Math/sin x)))))))
    vec))


(defn setup-gui
  [input-exprs-vec* output-exprs-vec*]
  (let [sim->gui-chan       (chan)
        sim-stop-start-chan (chan)]
    (gui/create-and-show-gui
      {:sim-stop-start-chan sim-stop-start-chan
       :xs                  (doto (CopyOnWriteArrayList.) (.addAll @input-exprs-vec*))
       :y1s                 (doto (CopyOnWriteArrayList.) (.addAll (repeat (count @input-exprs-vec*) 0.0)))
       :y2s                 (doto (CopyOnWriteArrayList.) (.addAll @output-exprs-vec*))
       :s1l                 "best fn"
       :s2l                 "objective fn"
       :update-loop         (fn [{:keys [^XYChart chart
                                         ^XChartPanel chart-panel
                                         ^JLabel info-label]}
                                 {:keys [^List xs ^List y1s ^List y2s ^String s1l ^String s2l]
                                  :as   conf}]
                              (go-loop []
                                (<! (timeout 1000))
                                (when-let [{:keys [best-eval best-f-str]
                                            :as   sim-msg} (<! sim->gui-chan)]

                                  (.clear y1s)
                                  (.addAll y1s best-eval)

                                  (.clear y2s)
                                  (.addAll y2s @output-exprs-vec*)

                                  (.clear xs)
                                  (.addAll xs @input-exprs-vec*)

                                  (.setTitle chart best-f-str)
                                  (.updateXYSeries chart s1l xs y1s nil)
                                  (.updateXYSeries chart s2l xs y2s nil)

                                  (.setText info-label (str "Best Function: " best-f-str))
                                  (.revalidate info-label)
                                  (.repaint info-label)


                                  (.revalidate chart-panel)
                                  (.repaint chart-panel)

                                  (recur))))})
    {:sim->gui-chan       sim->gui-chan
     :sim-stop-start-chan sim-stop-start-chan}))


(defn check-start-stop-state
  [sim-stop-start-chan]
  (let [[n ch] (alts!! [sim-stop-start-chan] :default :continue :priority true)]
    (if (= n :continue)
      :ok
      (do
        (println "Parking updates due to Stop command")
        (<!! sim-stop-start-chan)
        (println "Resuming updates")))))


(defn run-experiment
  [{:keys [iters initial-phenos initial-muts input-exprs output-exprs]}]
  (let [input-exprs-count (count input-exprs)
        input-exprs-vec   (mapv #(.doubleValue (.toNumber ^IExpr %)) input-exprs)
        ^"[Lorg.matheclipse.core.interfaces.IExpr;" input-exprs-arr
        (into-array IExpr input-exprs)
        ^"[Lorg.matheclipse.core.interfaces.IExpr;" input-exprs-list
        (into-array IExpr [(F/List input-exprs-arr)])
        output-exprs-vec  (mapv #(.doubleValue (.toNumber ^IExpr %)) output-exprs)

        input-exprs-vec*  (atom input-exprs-vec)
        output-exprs-vec* (atom output-exprs-vec)

        {sim->gui-chan       :sim->gui-chan
         sim-stop-start-chan :sim-stop-start-chan} (setup-gui input-exprs-vec* output-exprs-vec*)]

    (println "initial pop: " (count initial-phenos))
    (println "initial muts: " (count initial-muts))

    (let [{new-state    :new-state
           input-data-x :input-data-x
           input-data-y :input-data-y} (<!! sim-stop-start-chan)

          input-exprs       (if input-data-x
                              (mapv (fn [^double pt-x] (.add F/C0 pt-x)) input-data-x)
                              input-exprs)
          output-exprs      (if input-data-y
                              (mapv (fn [^double pt-y] (.add F/C0 pt-y)) input-data-y)
                              output-exprs)
          output-exprs-vec  (mapv #(.doubleValue (.toNumber ^IExpr %)) output-exprs)
          ^"[Lorg.matheclipse.core.interfaces.IExpr;" input-exprs-arr
          (into-array IExpr input-exprs)
          ^"[Lorg.matheclipse.core.interfaces.IExpr;" input-exprs-list
          (into-array IExpr [(F/List input-exprs-arr)])
          input-exprs-count (count input-exprs)
          input-exprs-vec   (mapv #(.doubleValue (.toNumber ^IExpr %)) input-exprs)

          _                 (do
                              (reset! input-exprs-vec* input-exprs-vec)
                              (reset! output-exprs-vec* output-exprs-vec))
          _                 (println "Got state req: " (if new-state "Start" "Stop"))

          start             (Date.)
          pop1              (ga/initialize
                              initial-phenos
                              (partial score-fn input-exprs-list input-exprs-count output-exprs-vec)
                              (partial mutation-fn initial-muts)
                              (partial crossover-fn initial-muts))]
      (println "start " start)
      (reset! test-timer* start)

      (loop [pop pop1
             i   iters]
        (if (zero? i)
          pop
          (do
            (check-start-stop-state sim-stop-start-chan)
            (let [{old-scores :pop-old-scores old-score :pop-old-score :as ga-result} (ga/evolve pop)]
              (report-iteration i ga-result sim->gui-chan input-exprs-count input-exprs-list)
              (recur ga-result
                     (if (or (zero? old-score) (some #(> % -1e-3) old-scores))
                       (near-exact-solution i old-score old-scores)
                       (dec i)))))))

      (let [end  (Date.)
            diff (- (.getTime end) (.getTime start))]

        (println "Took " (/ diff 1000.0) " seconds")))))


(defn in-flames
  [f]
  ;; http://localhost:54321/flames.svg
  (let [flames (flames/start! {:port 54321, :host "localhost"})]
    (f)
    (flames/stop! flames)))

;(def ^Logger logger (LogManager/getRootLogger))
;(println "Root logger name: " (.getName logger) (.setLevel logger Level/ERROR))

(defn run-test
  []
  (let [experiment-fn (fn []
                        (run-experiment
                          {:initial-phenos (ops/initial-phenotypes sym-x 800)
                           :initial-muts   (ops/initial-mutations)
                           :input-exprs    input-exprs
                           :output-exprs   output-exprs
                           :iters          300}))]
    ;; with flame graph analysis:
    ;; (in-flames experiment-fn)
    ;; plain experiment:
    (experiment-fn)))


(comment (run-test))
