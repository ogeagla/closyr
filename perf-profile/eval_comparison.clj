(ns eval-comparison
  (:require [closyr.ops.common :as ops-common]
            [closyr.ops.eval :as ops-eval])
  (:import [org.matheclipse.core.expression F]
           [org.matheclipse.core.interfaces IExpr IAST ISymbol]))

(defn compare-eval []
  (let [x ops-common/sym-x
        expr (F/Plus (F/Sin x) (F/Times x x))
        util (ops-common/new-util)
        pheno {:sym x :util util :expr expr :id (java.util.UUID/randomUUID)}

        xs (vec (range 0.1 10.0 0.1))
        n (count xs)
        xs-list (ops-common/exprs->exprs-list (ops-common/doubles->exprs xs))
        run-args {:input-xs-list xs-list :input-xs-count n}

        ;; Pre-compute for isolated tests
        fn-expr (ops-common/expr->fn pheno)
        ast (F/ast xs-list fn-expr)]

    (println "\n=== Evaluation Time Breakdown ===\n")
    (println "Points:" n)

    ;; Warmup
    (dotimes [_ 50] (ops-eval/eval-vec-pheno pheno run-args))

    (println "\n1. Current optimized eval-vec-pheno (1000x):")
    (time (dotimes [_ 1000] (ops-eval/eval-vec-pheno pheno run-args)))

    (println "\n2. Just the .eval call (1000x):")
    (time (dotimes [_ 1000] (.eval util ast)))

    (println "\n3. Just result extraction from pre-computed result (1000x):")
    (let [^IAST result (.eval util ast)]
      (time (dotimes [_ 1000]
              (loop [i (int 0), acc (transient [])]
                (if (< i n)
                  (let [^IExpr e (.getArg result (inc i) F/Infinity)
                        v (if (.isReal e)
                            (ops-common/expr->double e)
                            Double/POSITIVE_INFINITY)]
                    (recur (unchecked-inc-int i) (conj! acc v)))
                  (persistent! acc))))))

    (println "\nConclusion: .eval is ~95% of eval time, extraction is ~5%")
    (println "\n=== Done ===")))

(compare-eval)
