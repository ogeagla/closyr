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


(def output-exprs
  (->>
    (range 20)
    (map (fn [i]
           (let [x (* Math/PI (/ i 20.0))]
             (.add F/C0 (+ (* -1 x x) 2.0 (* 4.0 (Math/sin x)))))))
    vec))


;; (def input-exprs [(.add F/C0 1.923456) F/C1D2 F/C1D5 F/C1D4 F/C1D3 F/CN1])
;; (def output-exprs [F/CN1 F/C1D3 F/C1D5 F/C1D4 F/C1D2 (.add F/C0 1.923456)])
(def output-exprs-vec (mapv #(.doubleValue (.toNumber %)) output-exprs))


(defn eval-vec-pheno
  [p input-exprs]
  (let [^IExpr new-expr (:expr p)
        new-is-const    (.isNumber new-expr)
        eval-p          (ops/eval-phenotype p (F/List (into-array IExpr input-exprs)))
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


(def sim-stats* (atom {}))


(defn score-fn
  [input-exprs output-exprs-vec v]
  (try
    (let [leafs            (.leafCount (:expr v))
          resids           (pmap (fn [output expted]
                                  (- expted output))
                                (eval-vec-pheno v input-exprs)
                                output-exprs-vec)
          resid            (reduce + 0.0 (map #(min 100000 (abs %)) resids))
          score            (* -1 (abs resid))
          length-deduction (* 0.0001 leafs)]
      (when (zero? resid) (println "warning: zero resid " resids))
      (when (neg? length-deduction) (println "warning: negative deduction increases score: " leafs length-deduction v))

      (swap! sim-stats* update-in [:scoring :len-deductions] #(concat (or % []) [length-deduction]))

      (- score length-deduction))
    (catch Exception e
      -1000000)))


(defn mutation-fn
  [initial-muts v]
  (let [c         (rand-nth [1 1 1
                             2 2
                             3])
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
    (swap! sim-stats* update-in [:mutations :size-in] #(concat (or % []) [old-leafs]))
    (swap! sim-stats* update-in [:mutations :size-out] #(concat (or % []) [new-leafs]))
    new-pheno))


(defn crossover-fn
  [v]
  ;; todo do something
  v)


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
    "score: " (-> p :score)
    " fn: " (-> p :expr str)))


(defn summarize-sim-stats
  []
  (let [{{cs     :counts
          sz-in  :size-in
          sz-out :size-out}               :mutations
         {len-deductions :len-deductions} :scoring
         :as                              dat} @sim-stats*]
    (-> dat
        (assoc :scoring {:len-deductions (/ (reduce + 0.0 len-deductions) (count len-deductions))})
        (assoc :mutations {:counts        cs
                           :size-in-mean  (/ (reduce + 0.0 sz-in) (count sz-in))
                           :size-out-mean (/ (reduce + 0.0 sz-out) (count sz-out))}))))


(defn run-test
  []
  (let [start          (Date.)
        initial-phenos (ops/initial-phenotypes sym-x 20)
        initial-muts   (ops/initial-mutations)
        pop1           (ga/initialize initial-phenos
                                      (partial score-fn input-exprs output-exprs-vec)
                                      (partial mutation-fn initial-muts)
                                      crossover-fn)]
    (println "start " start)
    (println "initial pop: " (count initial-phenos))
    (println "initial muts: " (count initial-muts))

    (let [pop (loop [pop pop1
                     i   100]
                (if (zero? i)
                  pop
                  (let [_       (swap! sim-stats* assoc :mutations {})
                        new-pop (ga/evolve pop)
                        s       (:pop-old-score new-pop)
                        ss      (:pop-old-scores new-pop)]
                    (when (zero? (mod i 10))
                      (println i " pop score: " s
                               " mean: " (Math/round (float (/ s (count new-pop))))
                               "\n top best: "
                               (->> (take 15 (sort-population new-pop))
                                    (map reportable-phen-str))
                               "\n with sim stats: " (summarize-sim-stats)))
                    (recur new-pop
                           (if (or (zero? s) (some #(> % -1e-3) ss))
                             (do
                               (println "Perfect score! " i)
                               0)
                             (dec i))))))]
      (let [end   (Date.)
            diff  (- (.getTime end) (.getTime start))
            bests (take 15 (sort-population pop))]
        (println "Took " (/ diff 1000.0) " seconds")
        (println "Bests: "
                 (str/join "\n"
                           (map reportable-phen-str bests))
                 "\n with sim stats: " (summarize-sim-stats))))))


(comment (run-test))
