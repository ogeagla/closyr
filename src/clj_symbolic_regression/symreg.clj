(ns clj-symbolic-regression.symreg
  (:require
    [clj-symbolic-regression.ga :as ga]
    [clj-symbolic-regression.ops :as ops]
    [clojure.string :as str])
  (:import
    (java.util
      Date)
    (org.matheclipse.core.expression
      F)
    (org.matheclipse.core.interfaces
      IExpr
      ISymbol)))


(def ^ISymbol sym-x (F/Dummy "x"))


(def input-exprs
  (->>
    (range 20)
    (map (fn [i]
           (.add F/C0 (* Math/PI (/ i 20.0)))))
    vec))


(def input-exprs-list
  (F/List (into-array IExpr input-exprs)))


(def output-exprs
  (->>
    (range 20)
    (map (fn [i]
           (let [x (* Math/PI (/ i 20.0))]
             (.add F/C0 (+ (* 1.5 x x) 2.0 (* 4.0 (Math/sin x)))))))
    vec))

(def output-exprs-vec (mapv #(.doubleValue (.toNumber %)) output-exprs))


(defn eval-vec-pheno
  [p input-exprs input-exprs-list]
  (let [^IExpr new-expr (:expr p)
        new-is-const    (.isNumber new-expr)
        eval-p          (ops/eval-phenotype p input-exprs-list)
        vs              (vec (pmap
                               (fn [i]
                                 (try
                                   (.doubleValue
                                     (.toNumber (.getArg eval-p (inc i) F/Infinity)))
                                   (catch Exception e
                                     Double/POSITIVE_INFINITY)))
                               (range (dec (.size eval-p)))))
        vs              (if (seq vs)
                          vs
                          (vec (pmap
                                 (fn [i]
                                   (.doubleValue
                                     (.toNumber (if new-is-const
                                                  new-expr
                                                  (.getArg eval-p 0 F/Infinity)))))
                                 (range (count input-exprs)))))]
    vs))


(defn sum
  [coll]
  (reduce + 0.0 coll)
  #_(loop [acc  0.0
           coll coll]
      (if (empty? coll)
        acc
        (recur (+ acc (first coll))
               (rest coll)))

      ))


(def sim-stats* (atom {}))


(defn score-fn
  [input-exprs input-exprs-list output-exprs-vec v]
  (try
    (let [leafs            (.leafCount (:expr v))
          resids           (pmap (fn [output expted]
                                   (- expted output))
                                 (eval-vec-pheno v input-exprs input-exprs-list)
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
   7])


(defn mutation-fn
  [initial-muts v]
  (try
    (let [c         (rand-nth mutations-sampler)
          new-pheno (loop [c c
                           v v]
                      (if (zero? c)
                        v
                        (recur
                          (dec c)
                          (ops/modify (rand-nth initial-muts) v))))
          old-leafs (.leafCount (:expr v))
          new-leafs (.leafCount (:expr new-pheno))]
      (swap! sim-stats* update-in [:mutations :counts c] #(inc (or % 0)))
      (swap! sim-stats* update-in [:mutations :size-in] #(into (or % []) [old-leafs]))
      (swap! sim-stats* update-in [:mutations :size-out] #(into (or % []) [new-leafs]))
      new-pheno)
    (catch Exception e
      (println "Err in mutation: " e))))


(defn crossover-fn
  [initial-muts v]
  ;; todo do something for crossover
  (mutation-fn initial-muts v))


(defn sort-population
  [pops]
  (->>
    (:pop pops)
    (remove #(nil? (:score %)))
    (set)
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


(defn report-iteration
  [i ga-result]
  (when (zero? (mod i 5))
    (let [old-score  (:pop-old-score ga-result)
          old-scores (:pop-old-scores ga-result)
          end        (Date.)
          diff       (- (.getTime end) (.getTime @test-timer*))]
      (reset! test-timer* end)
      (println i " step pop size: " (count (:pop ga-result)) " took secs: " (/ diff 1000.0))
      (println i " pop score: " old-score
               " mean: " (Math/round (float (/ old-score (count (:pop ga-result)))))
               "\n  top best: "
               (->> (take 5 (sort-population ga-result))
                    (map reportable-phen-str)))
      (println i " sim stats: " (summarize-sim-stats))))
  (reset! sim-stats* {}))


(defn near-exact-solution
  [i old-score old-scores]
  (println "Perfect score! " i old-score " all scores: " old-scores)
  0)


(defn run-experiment
  [{:keys [iters initial-phenos initial-muts input-exprs input-exprs-list output-exprs-vec]}]
  (let [start (Date.)
        pop1  (ga/initialize
                initial-phenos
                (partial score-fn input-exprs input-exprs-list output-exprs-vec)
                (partial mutation-fn initial-muts)
                (partial crossover-fn initial-muts))]
    (println "start " start)
    (println "initial pop: " (count initial-phenos))
    (println "initial muts: " (count initial-muts))

    (reset! test-timer* start)

    (let [pop   (loop [pop pop1
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
          end   (Date.)
          diff  (- (.getTime end) (.getTime start))
          bests (take 10 (sort-population pop))]

      (println "Took " (/ diff 1000.0) " seconds")
      (println "Bests: \n" (str/join "\n" (map reportable-phen-str bests)))
      pop)))


(defn run-test
  []
  (run-experiment
    {:initial-phenos   (ops/initial-phenotypes sym-x 1000)
     :initial-muts     (ops/initial-mutations)
     :input-exprs      input-exprs
     :input-exprs-list input-exprs-list
     :output-exprs-vec output-exprs-vec
     :iters            500}))


(comment (run-test))
