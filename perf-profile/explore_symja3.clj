(ns explore-symja3
  (:require [closyr.ops.common :as ops-common]
            [closyr.ops.eval :as ops-eval])
  (:import [org.matheclipse.core.expression F]
           [org.matheclipse.core.interfaces IExpr IAST ISymbol]))

(defn benchmark-overhead []
  (let [x ops-common/sym-x
        expr (F/Plus (F/Sin x) (F/Times x x) (F/Cos (F/Times (F/num 2.0) x)))
        util (ops-common/new-util)

        xs (vec (range 0.1 10.0 0.1))
        n (count xs)
        xs-list (ops-common/exprs->exprs-list (ops-common/doubles->exprs xs))

        ;; Pre-create everything
        pheno {:sym x :util util :expr expr :id (java.util.UUID/randomUUID)}
        fn-expr (ops-common/expr->fn pheno)]

    (println "\n=== Finding the Overhead ===\n")
    (println "Expression:" (str expr))

    ;; Warmup
    (dotimes [_ 100]
      (let [ast (F/ast xs-list fn-expr)]
        (.eval util ast)))

    ;; Minimal path with pre-computed fn-expr
    (println "\n1. Minimal: pre-computed fn-expr + eval (1000x):")
    (time (dotimes [_ 1000]
            (let [ast (F/ast xs-list fn-expr)
                  ^IAST result (.eval util ast)]
              (mapv (fn [i]
                      (let [^IExpr e (.getArg result (inc i) F/Infinity)]
                        (if (.isReal e) (.evalDouble e) Double/POSITIVE_INFINITY)))
                    (range n)))))

    ;; With expr->fn called each time (like current code)
    (println "\n2. With expr->fn each time (1000x):")
    (time (dotimes [_ 1000]
            (let [fn-expr-new (ops-common/expr->fn pheno)
                  ast (F/ast xs-list fn-expr-new)
                  ^IAST result (.eval util ast)]
              (mapv (fn [i]
                      (let [^IExpr e (.getArg result (inc i) F/Infinity)]
                        (if (.isReal e) (.evalDouble e) Double/POSITIVE_INFINITY)))
                    (range n)))))

    ;; Current eval-vec-pheno
    (println "\n3. Current eval-vec-pheno (1000x):")
    (let [run-args {:input-xs-list xs-list :input-xs-count n}]
      (time (dotimes [_ 1000]
              (ops-eval/eval-vec-pheno pheno run-args))))

    ;; Check expr->fn overhead
    (println "\n4. Just expr->fn creation (10000x):")
    (time (dotimes [_ 10000]
            (ops-common/expr->fn pheno)))

    (println "\n=== Done ===")))

(benchmark-overhead)
