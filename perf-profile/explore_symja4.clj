(ns explore-symja4
  (:require [closyr.ops.common :as ops-common]
            [closyr.ops.eval :as ops-eval])
  (:import [org.matheclipse.core.expression F]
           [org.matheclipse.core.interfaces IExpr IAST ISymbol]
           [org.matheclipse.core.eval ExprEvaluator]))

(defn benchmark-optimized []
  (let [x ops-common/sym-x
        expr (F/Plus (F/Sin x) (F/Times x x) (F/Cos (F/Times (F/num 2.0) x)))
        util (ops-common/new-util)

        xs (vec (range 0.1 10.0 0.1))
        n (count xs)
        xs-list (ops-common/exprs->exprs-list (ops-common/doubles->exprs xs))
        run-args {:input-xs-list xs-list :input-xs-count n}

        pheno {:sym x :util util :expr expr :id (java.util.UUID/randomUUID)}]

    (println "\n=== Optimized Evaluation Benchmark ===\n")

    ;; Warmup
    (dotimes [_ 100] (ops-eval/eval-vec-pheno pheno run-args))

    ;; Current
    (println "1. Current eval-vec-pheno (1000x):")
    (time (dotimes [_ 1000]
            (ops-eval/eval-vec-pheno pheno run-args)))

    ;; Optimized version - inline everything, no intermediate functions
    (println "\n2. Optimized inline version (1000x):")
    (time
      (dotimes [_ 1000]
        (let [^IAST fn-expr (F/Function
                              (F/List (into-array ISymbol [x]))
                              expr)
              ^IAST ast (F/ast xs-list fn-expr)
              ^IExpr eval-result (.eval ^ExprEvaluator util ast)]
          (when-not (or (nil? eval-result)
                        (= "Indeterminate" (str eval-result)))
            (let [result-size (dec (.size eval-result))]
              (if (= n result-size)
                ;; Normal case - one result per input
                (loop [i 0, acc (transient [])]
                  (if (< i n)
                    (let [^IExpr e (.getArg eval-result (inc i) F/Infinity)
                          v (if (.isReal e)
                              (.doubleValue (.toNumber e))
                              Double/POSITIVE_INFINITY)]
                      (recur (inc i) (conj! acc v)))
                    (persistent! acc)))
                ;; Constant result case
                (let [^IExpr arg0 (.getArg eval-result 0 F/Infinity)
                      v (.doubleValue (.toNumber (if (.isReal expr) expr arg0)))]
                  (vec (repeat n v)))))))))

    ;; With transient but using mapv structure
    (println "\n3. Transient with indexed loop (1000x):")
    (time
      (dotimes [_ 1000]
        (let [^IAST fn-expr (F/Function (F/List (into-array ISymbol [x])) expr)
              ^IAST ast (F/ast xs-list fn-expr)
              ^IAST result (.eval ^ExprEvaluator util ast)]
          (loop [i 0, acc (transient [])]
            (if (< i n)
              (let [^IExpr e (.getArg result (inc i) F/Infinity)
                    v (if (.isReal e)
                        (.doubleValue (.toNumber e))
                        Double/POSITIVE_INFINITY)]
                (recur (inc i) (conj! acc v)))
              (persistent! acc))))))

    ;; Using primitive double array
    (println "\n4. Primitive double array (1000x):")
    (time
      (dotimes [_ 1000]
        (let [^IAST fn-expr (F/Function (F/List (into-array ISymbol [x])) expr)
              ^IAST ast (F/ast xs-list fn-expr)
              ^IAST result (.eval ^ExprEvaluator util ast)
              ^doubles arr (double-array n)]
          (dotimes [i n]
            (let [^IExpr e (.getArg result (inc i) F/Infinity)]
              (aset arr i (if (.isReal e)
                            (.doubleValue (.toNumber e))
                            Double/POSITIVE_INFINITY))))
          arr)))

    (println "\n=== Done ===")))

(benchmark-optimized)
