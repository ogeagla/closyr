(ns eval-detailed
  (:require [closyr.ops.common :as ops-common]
            [closyr.ops.eval :as ops-eval])
  (:import [org.matheclipse.core.expression F]
           [org.matheclipse.core.interfaces IExpr IAST ISymbol]
           [org.matheclipse.core.eval ExprEvaluator]))

(defn detailed-timing []
  (let [x ops-common/sym-x
        expr (F/Plus (F/Sin x) (F/Times x x))
        util (ops-common/new-util)
        pheno {:sym x :util util :expr expr :id (java.util.UUID/randomUUID)}

        xs (vec (range 0.1 10.0 0.1))
        n (count xs)
        xs-exprs (ops-common/doubles->exprs xs)
        xs-list (ops-common/exprs->exprs-list xs-exprs)
        run-args {:input-xs-list xs-list :input-xs-count n}]

    (println "\n=== Detailed Timing Breakdown ===\n")

    ;; Warmup
    (dotimes [_ 50] (ops-eval/eval-vec-pheno pheno run-args))

    (println "1. Full eval-vec-pheno (1000x):")
    (time (dotimes [_ 1000] (ops-eval/eval-vec-pheno pheno run-args)))

    (println "\n2. expr->fn creation (10000x):")
    (time (dotimes [_ 10000] (ops-common/expr->fn pheno)))

    (println "\n3. F/ast creation with pre-computed fn-expr (10000x):")
    (let [fn-expr (ops-common/expr->fn pheno)]
      (time (dotimes [_ 10000] (F/ast xs-list fn-expr))))

    (println "\n4. .eval on pre-computed ast (1000x):")
    (let [fn-expr (ops-common/expr->fn pheno)
          ast (F/ast xs-list fn-expr)]
      (time (dotimes [_ 1000] (.eval ^ExprEvaluator util ast))))

    (println "\n5. Full eval-phenotype-on-expr-args (1000x):")
    (time (dotimes [_ 1000] (ops-eval/eval-phenotype-on-expr-args pheno xs-list)))

    (println "\n6a. OLD parseable-eval-result? with string (10000x):")
    (let [fn-expr (ops-common/expr->fn pheno)
          ast (F/ast xs-list fn-expr)
          result (.eval ^ExprEvaluator util ast)]
      (time (dotimes [_ 10000]
              (not (or (nil? result) (= "Indeterminate" (str result)))))))

    (println "\n6b. NEW parseable-eval-result? with identical? (10000x):")
    (let [fn-expr (ops-common/expr->fn pheno)
          ast (F/ast xs-list fn-expr)
          result (.eval ^ExprEvaluator util ast)]
      (time (dotimes [_ 10000]
              (and (some? result) (not (identical? result F/Indeterminate))))))

    (println "\n7. Result extraction only (1000x):")
    (let [fn-expr (ops-common/expr->fn pheno)
          ast (F/ast xs-list fn-expr)
          ^IAST result (.eval ^ExprEvaluator util ast)]
      (time (dotimes [_ 1000]
              (loop [i (int 0), acc (transient [])]
                (if (< i n)
                  (let [^IExpr e (.getArg result (inc i) F/Infinity)
                        v (if (.isReal e)
                            (ops-common/expr->double e)
                            Double/POSITIVE_INFINITY)]
                    (recur (unchecked-inc-int i) (conj! acc v)))
                  (persistent! acc))))))

    (println "\n=== Reconstructed total ===")
    (println "Expected: expr->fn + F/ast + .eval + extraction")
    (println "(Values from above, scaled to 1000x)")
    (println "\n=== Done ===")))

(detailed-timing)
