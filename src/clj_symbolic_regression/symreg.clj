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
(def initial-phenos (ops/initial-phenotypes sym-x 1000))

(def initial-muts (ops/initial-mutations))

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
           (.add F/C0 (+ 2.0 (* 2.0 (Math/sin (* Math/PI (/ i 20.0))))))))
    vec))

;(def input-exprs [(.add F/C0 1.923456) F/C1D2 F/C1D5 F/C1D4 F/C1D3 F/CN1])
;(def output-exprs [F/CN1 F/C1D3 F/C1D5 F/C1D4 F/C1D2 (.add F/C0 1.923456)])
(def output-exprs-vec (mapv #(.doubleValue (.toNumber %)) output-exprs))


(defn eval-vec-pheno
  [p input-exprs]
  (let [^IExpr new-expr (:expr p)
        new-is-const    (.isNumber new-expr)

        eval-p          (ops/eval-phenotype p (F/List (into-array IExpr input-exprs)))
        vs              (vec (pmap
                               (fn [i]
                                 (let [v (try
                                           (.doubleValue
                                             (.toNumber (.getArg eval-p (inc i) F/Infinity)))
                                           (catch Exception e
                                             Double/POSITIVE_INFINITY))]
                                   #_(println "Got output vec item: " v)
                                   v))
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


(defn demo-math-2
  []

  (let [start          (Date.)
        _              (println "start " start)
        ^ISymbol sym-x (F/Dummy "x")
        report         (->>
                         (for [p  (ops/initial-phenotypes sym-x 1)
                               m1 initial-muts
                               m2 initial-muts]
                           (let [new-p           (ops/modify m2 (ops/modify m1 p))
                                 ^IExpr new-expr (:expr new-p)
                                 new-is-const    (.isNumber new-expr)

                                 eval-p          (ops/eval-phenotype new-p (F/List (into-array IExpr input-exprs)))
                                 vs              (vec (pmap
                                                        (fn [i]
                                                          (let [v (try
                                                                    (.doubleValue
                                                                      (.toNumber (.getArg eval-p (inc i) F/Infinity)))
                                                                    (catch Exception e
                                                                      Double/POSITIVE_INFINITY))]
                                                            #_(println "Got output vec item: " v)
                                                            v))
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


                             (str
                               "\n" (:expr p) " -> " (:label m1) "." (:label m2) " :: "
                               (:expr new-p)
                               " -->> type:" (type eval-p) " size:" (.size eval-p) " " #_eval-p
                               " head: " (.getArg eval-p 0 nil)
                               " output: " vs)))
                         (sort-by count)
                         (reverse))
        end            (Date.)
        diff           (- (.getTime end) (.getTime start))]

    (println "initial muts: " (count initial-muts))
    (println "initial fn x muts: "
             (take 20 report)
             "\n...\n"


             (take 20 (take-last (/ (count report) 2) report))
             (take-last 20 (take (/ (count report) 2) report))
             "\n...\n"
             (take-last 20 report))
    (println "Took " (/ diff 1000.0) " seconds")))


(defn score-fn
  [v]
  ;; (println "Score: "  (.toString (ops/eval-phenotype v 0.3)))
  (try
    (let [resids (map (fn [output expted]
                        ;; (println output expted)
                        (- expted output))
                      (eval-vec-pheno v input-exprs)
                      output-exprs-vec)
          ;; resid (- 10 (Double/parseDouble (.toString (.toNumber (ops/eval-phenotype v 0.3)))))
          resid  (reduce + 0.0 (map #(min 10000 (abs %)) resids))]
      (when (zero? resid)
        (println "zero resid " resids))
      (* -1 (abs resid)))
    (catch Exception e
      #_(println "Error " (:expr v) " -> " e)
      -1000000)))


(defn mutation-fn
  [v]
  (ops/modify (rand-nth initial-muts) v))


(defn crossover-fn
  [v]
  v)


(defn run-test
  []
  (let [start          (Date.)
        initial-phenos (ops/initial-phenotypes sym-x 50)
        pop1           (ga/initialize initial-phenos score-fn mutation-fn crossover-fn)]
    (println "start " start)
    (println "initial pop: " (count initial-phenos))
    (println "initial muts: " (count initial-muts))

    (let [pop (loop [pop pop1
                     i   500]
                (if (zero? i)
                  pop
                  (let [new-pop (ga/evolve pop)
                        s       (:pop-old-score new-pop)
                        ss       (:pop-old-scores new-pop)
                        ]
                    (when (zero? (mod i 20))
                      (println i " pop score: " s " top best: "
                               (take 25 (reverse (sort-by :score (:pop new-pop))))))
                    (recur new-pop
                           (if (or (zero? s) (some #(> % -1e-3) ss))
                             (do
                               (println "Perfect score!")
                               0)
                             (dec i))))))]
      (let [end   (Date.)
            diff  (- (.getTime end) (.getTime start))
            bests (take 25 (reverse (sort-by :score (:pop pop))))]
        (println "Took " (/ diff 1000.0) " seconds")
        (println "Bests: "
                 (str/join "\n"
                           (map (fn [v]
                                  (println v)
                                  (str
                                    "score: " (-> v :score)
                                    " fn: " (-> v :expr str)))
                                bests)))))))


(comment (run-test))
(comment (demo-math-2))
