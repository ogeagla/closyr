(ns clj-symbolic-regression.symreg
  (:require
    [clojure.core.async :as async :refer [go go-loop timeout <!! >!! <! >! chan]]
    [clj-symbolic-regression.ga :as ga]
    [clj-symbolic-regression.gui :as gui]
    [clj-symbolic-regression.ops :as ops]
    [clj-symbolic-regression.plot :as plot]
    [clojure.string :as str]
    [flames.core :as flames])
  (:import
    (java.util
      Date List)
    (java.util.concurrent CopyOnWriteArrayList)
    (org.knowm.xchart XChartPanel XYChart)
    (org.matheclipse.core.expression
      F)
    (org.matheclipse.core.interfaces
      IExpr
      ISymbol)))


(set! *warn-on-reflection* true)

(def ^ISymbol sym-x (F/Dummy "x"))


(defn eval-vec-pheno
  [p input-exprs input-exprs-list]
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
                                 (range (count input-exprs)))))]
    vs))


(defn sum
  [coll]
  (reduce + 0.0 coll))


(def sim-stats* (atom {}))


(defn score-fn
  [input-exprs-list input-exprs-arr input-exprs-F-strings input-exprs output-exprs-vec v]
  (try
    (let [leafs            (.leafCount ^IExpr (:expr v))
          ;; expr-str         (str (:expr v))
          ;; f-of-xs          (or
          ;;                   (get-cached-fn-eval expr-str input-exprs-F-strings)
          ;;                   (put-cached-fn-eval
          ;;                     expr-str
          ;;                     input-exprs-F-strings
          ;;                     (eval-vec-pheno v input-exprs input-exprs-F-strings)))
          f-of-xs          (eval-vec-pheno v input-exprs input-exprs-list)

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

(def log-steps 10)


(defn report-iteration
  [i ga-result]
  (when (zero? (mod i log-steps))
    (let [old-score  (:pop-old-score ga-result)
          old-scores (:pop-old-scores ga-result)
          end        (Date.)
          diff       (- (.getTime end) (.getTime ^Date @test-timer*))
          bests      (sort-population ga-result)
          took-s     (/ diff 1000.0)
          pop-size   (count (:pop ga-result))]

      (reset! test-timer* end)
      (println i "-step pop size: " pop-size " took secs: " took-s " phenos/s: " (Math/round ^double (/ (* pop-size log-steps) took-s)))
      (println i " pop score: " old-score
               " mean: " (Math/round (float (/ old-score (count (:pop ga-result))))))
      (println i " top best: "
               (->> (take 5 bests)
                    (map reportable-phen-str)))
      (println i " sim stats: " (summarize-sim-stats))
      ;; (println i " fn eval cache: " @fn-eval-cache-stats*)
      ))
  (reset! sim-stats* {}))


(defn near-exact-solution
  [i old-score old-scores]
  (println "Perfect score! " i old-score " all scores: " old-scores)
  0)


(def input-exprs
  (->>
    (range 30)
    (map (fn [i]
           (.add F/C0 (* Math/PI (/ i 15.0)))))
    vec))


(def output-exprs
  (->>
    (range 30)
    (map (fn [i]
           (let [x (* Math/PI (/ i 15.0))]
             (.add F/C0 (+ (* 0.5 x x) 2.0 (* 4.0 (Math/sin x)))))))
    vec))


(defn run-experiment
  [{:keys [iters initial-phenos initial-muts input-exprs output-exprs]}]

  (gui/create-and-show-gui
    {
     :xs          (doto (CopyOnWriteArrayList.) (.add 0.0) (.add 1.0))
     :y1s         (doto (CopyOnWriteArrayList.) (.add 2.0) (.add 1.0))
     :y2s         (doto (CopyOnWriteArrayList.) (.add 3.0) (.add 1.9))
     :s1l         "series 1"
     :s2l         "series 2"
     :update-loop (fn [^XYChart chart
                       ^XChartPanel chart-panel
                       {:keys [^List xs ^List y1s ^List y2s ^String s1l ^String s2l]
                        :as   conf}]
                    #_(go-loop []
                      (<! (timeout 1000))

                      (println "Draw new points " (.size xs))
                      (.add xs (.size xs))
                      (.add y1s (.size xs))
                      ;; (.remove y2s 0)
                      (.add y2s (* 10.0 (Math/random)))

                      (.updateXYSeries chart s1l xs y1s nil)
                      (.updateXYSeries chart s2l xs y2s nil)

                      (.revalidate chart-panel)
                      (.repaint chart-panel)

                      (recur)))
     })

  (let [start                 (Date.)

        input-exprs-vec       (mapv #(.doubleValue (.toNumber ^IExpr %)) input-exprs)
        ^"[Lorg.matheclipse.core.interfaces.IExpr;" input-exprs-arr
                              (into-array IExpr input-exprs)
        ^"[Lorg.matheclipse.core.interfaces.IExpr;" input-exprs-list
                              (into-array IExpr [(F/List input-exprs-arr)])
        input-exprs-F-strings (ops/->strings [(str input-exprs-list)])
        output-exprs-vec      (mapv #(.doubleValue (.toNumber ^IExpr %)) output-exprs)

        pop1                  (ga/initialize
                                initial-phenos
                                (partial score-fn input-exprs-list input-exprs-arr input-exprs-F-strings input-exprs output-exprs-vec)
                                (partial mutation-fn initial-muts)
                                (partial crossover-fn initial-muts))]
    (println "start " start)
    (println "initial pop: " (count initial-phenos))
    (println "initial muts: " (count initial-muts))

    (reset! test-timer* start)

    (let [pop    (loop [pop pop1
                        i   iters]
                   (if (zero? i)
                     pop
                     (let [{old-scores :pop-old-scores
                            old-score  :pop-old-score
                            :as        ga-result} (ga/evolve pop)]
                       (report-iteration i ga-result)
                       (recur ga-result
                              (if (or (zero? old-score) (some #(> % -1e-3) old-scores))
                                (near-exact-solution i old-score old-scores)
                                (dec i))))))
          end    (Date.)
          diff   (- (.getTime end) (.getTime start))
          bests  (take 10 (sort-population pop))
          best-v (first bests)
          evaled (eval-vec-pheno best-v input-exprs input-exprs-list)]

      (println "Took " (/ diff 1000.0) " seconds")
      (println "Bests: \n" (str/join "\n" (map reportable-phen-str bests)))
      (plot/show-plot (str (:expr best-v)) input-exprs-vec evaled output-exprs-vec))))


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
                          {:initial-phenos (ops/initial-phenotypes sym-x 100)
                           :initial-muts   (ops/initial-mutations)
                           :input-exprs    input-exprs
                           :output-exprs   output-exprs
                           :iters          20}))]
    ;; with flame graph analysis:
    ;; (in-flames experiment-fn)
    ;; plain experiment:
    (experiment-fn)))


(comment (run-test))
