(ns explore-symja2
  (:require [closyr.ops.common :as ops-common]
            [closyr.ops.eval :as ops-eval]
            [closyr.ops.initialize :as ops-init])
  (:import [org.matheclipse.core.expression F]
           [org.matheclipse.core.interfaces IExpr IAST ISymbol]))

(defn benchmark-eval-paths []
  (let [x ops-common/sym-x
        ;; Create a more complex expression like GA would produce
        expr (F/Plus (F/Sin x) (F/Times x x) (F/Cos (F/Times (F/num 2.0) x)))
        pheno (ops-common/->phenotype x expr nil)
        util (ops-common/new-util)

        ;; Create input data
        xs (vec (range 0.1 10.0 0.1))  ; 100 points
        n (count xs)
        xs-exprs (ops-common/doubles->exprs xs)
        xs-list (ops-common/exprs->exprs-list xs-exprs)
        run-args {:input-xs-list xs-list :input-xs-count n}]

    (println "\n=== Full Evaluation Path Benchmark ===\n")
    (println "Expression:" (str expr))
    (println "Points:" n)

    ;; Warmup
    (dotimes [_ 10] (ops-eval/eval-vec-pheno pheno run-args))

    ;; Current implementation
    (println "\n1. Current eval-vec-pheno (1000x):")
    (time (dotimes [_ 1000]
            (ops-eval/eval-vec-pheno pheno run-args)))

    ;; Break down the components
    (println "\n2. Component breakdown (1000x each):")

    ;; Just the F/ast + eval part
    (let [fn-expr (ops-common/expr->fn pheno)]
      (print "   a. F/ast creation: ")
      (time (dotimes [_ 1000]
              (F/ast xs-list fn-expr)))

      (let [ast (F/ast xs-list fn-expr)]
        (print "   b. .eval on ast: ")
        (time (dotimes [_ 1000]
                (.eval util ast)))))

    ;; Just result extraction
    (let [fn-expr (ops-common/expr->fn pheno)
          ast (F/ast xs-list fn-expr)
          ^IAST result (.eval util ast)]
      (print "   c. Result extraction (mapv): ")
      (time (dotimes [_ 1000]
              (mapv (fn [i]
                      (let [^IExpr e (.getArg result (inc i) F/Infinity)]
                        (if (.isReal e)
                          (ops-common/expr->double e)
                          Double/POSITIVE_INFINITY)))
                    (range n)))))

    ;; Try direct double evaluation without going through IExpr
    (println "\n3. Alternative: Direct substitution approach (100x):")
    (print "   Per-point subs+eval: ")
    (time (dotimes [_ 100]
            (mapv (fn [xv]
                    (let [subst (.subs expr x (F/num xv))]
                      (.evalDouble (.eval util subst))))
                  xs)))

    (println "\n=== Done ===")))

(benchmark-eval-paths)
