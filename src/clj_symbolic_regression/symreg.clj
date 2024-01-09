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
      ISymbol)))


(def ^ISymbol sym-x (F/Dummy "x"))
(def initial-phenos (ops/initial-phenotypes sym-x 1000))

(def initial-muts (ops/initial-mutations))


(defn demo-math-2
  []

  (let []
    (println "initial muts: " (count initial-muts))
    (println "initial fn x muts: "
             (->>
               (for [p  initial-phenos
                     m1 initial-muts
                     m2 initial-muts
                     m3 initial-muts]
                 (let [new-p  (ops/modify m3 (ops/modify m2 (ops/modify m1 p)))
                       eval-p (ops/eval-phenotype new-p 0.3)]
                   (str
                     "\n" (:expr p) " -> " (:label m1) "." (:label m2) "." (:label m3) " :: "
                     (:expr new-p)
                     " -->> " eval-p)))
               (sort-by count)
               (reverse)
               (take 50)))))


(defn score-fn
  [v]
  ;; (println "Score: "  (.toString (ops/eval-phenotype v 0.3)))
  (try
    (* -1 (abs (- 10 (Double/parseDouble (.toString (.toNumber (ops/eval-phenotype v 0.3)))))))
    (catch Exception e
      #_(println "Error " (:expr v) " -> " e)
      -1000)))


(defn mutation-fn
  [v]
  (ops/modify (rand-nth initial-muts) v))


(defn crossover-fn
  [v]
  v)


(defn run-test
  []
  (let [start          (Date.)
        initial-phenos (ops/initial-phenotypes sym-x 100)
        pop1           (ga/initialize initial-phenos score-fn mutation-fn crossover-fn)]
    (println "start " start)
    (println "initial pop: " (count initial-phenos))
    (println "initial muts: " (count initial-muts))

    (let [pop (loop [pop pop1
                     i   1000]
                (if (zero? i)
                  pop
                  (let [new-pop (ga/evolve pop)
                        s       (:pop-old-score new-pop)]
                    (when (zero? (mod i 100))
                      (println i " pop score: " s))
                    (recur new-pop
                           (if (zero? s)
                             (do
                               (println "Perfect score!")
                               0)
                             (dec i))))))]
      (let [end   (Date.)
            diff  (- (.getTime end) (.getTime start))
            bests (take 5 (reverse (sort-by :score (:pop pop))))]
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
