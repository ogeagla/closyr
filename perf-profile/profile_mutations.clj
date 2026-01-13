(ns profile-test
  (:require [closyr.ops.common :as ops-common]
            [closyr.ops.eval :as ops-eval]
            [closyr.ops.initialize :as ops-init]
            [closyr.ops :as ops]
            [closyr.ga :as ga]))

(defn profile-components []
  (let [phenos (ops-init/initial-phenotypes 100)
        muts (ops-init/initial-mutations)
        xs (vec (range 0.1 10.0 0.2))
        ys (mapv #(+ (* % %) (Math/sin %)) xs)
        input-xs-list (ops-common/exprs->exprs-list (ops-common/doubles->exprs xs))
        run-args {:input-xs-list  input-xs-list
                  :input-xs-count (count xs)
                  :input-xs-vec   xs
                  :input-ys-vec   ys}
        run-config {:max-leafs 40}]

    (println "\n=== Profiling Components ===\n")

    ;; Profile phenotype creation
    (println "1. Creating 1000 phenotypes:")
    (let [x ops-common/sym-x]
      (time (dotimes [_ 1000]
              (ops-common/->phenotype x (org.matheclipse.core.expression.F/Sin x) nil))))

    ;; Profile evaluation
    (println "\n2. Evaluating 100 phenotypes on" (count xs) "points:")
    (time (doseq [p phenos]
            (ops-eval/eval-vec-pheno p run-args)))

    ;; Profile scoring
    (println "\n3. Scoring 100 phenotypes:")
    (time (doseq [p phenos]
            (ops/score-fn run-args run-config p)))

    ;; Profile mutation
    (println "\n4. Applying 100 mutations:")
    (time (doseq [p phenos]
            (ops/mutation-fn run-config muts p p)))

    ;; Profile full evolution step
    (println "\n5. Full evolution step (100 pop):")
    (let [score-fn (partial ops/score-fn run-args run-config)
          mut-fn (partial ops/mutation-fn run-config muts)
          cross-fn (partial ops/crossover-fn run-config muts)
          ga-config (ga/initialize phenos score-fn mut-fn cross-fn)]
      (time (ga/evolve ga-config)))

    (println "\n=== Done ===\n")))
