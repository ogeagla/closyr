(ns profile-eval
  (:require [closyr.ops.common :as ops-common])
  (:import [org.matheclipse.core.expression F]
           [org.matheclipse.core.eval ExprEvaluator]))

(defn profile-eval []
  (let [x ops-common/sym-x
        util (ops-common/get-thread-local-util)
        simple-expr (F/Plus x (F/C1D2))
        complex-expr (F/D (F/Sin (F/Times x x)) x)]         ;; derivative is expensive

    (println "\n=== Eval in ->phenotype ===\n")

    (println "1. Creating expression WITHOUT .eval (1000x):")
    (time (dotimes [_ 1000]
            {:sym x :util nil :expr simple-expr}))

    (println "\n2. Creating phenotype WITH .eval on simple expr (1000x):")
    (time (dotimes [_ 1000]
            (.eval util simple-expr)))

    (println "\n3. Creating derivative expr (1000x):")
    (time (dotimes [_ 1000]
            (F/D (F/Sin x) x)))

    (println "\n4. Eval derivative result (1000x):")
    (let [deriv (F/D (F/Sin x) x)]
      (time (dotimes [_ 1000]
              (.eval util deriv))))

    (println "\n5. Complex derivative F/D(Sin(x^2), x) (100x):")
    (time (dotimes [_ 100]
            (F/D (F/Sin (F/Times x x)) x)))

    (println "\n6. Eval complex derivative (100x):")
    (let [deriv (F/D (F/Sin (F/Times x x)) x)]
      (time (dotimes [_ 100]
              (.eval util deriv))))

    (println "\n=== Done ===\n")))

(profile-eval)
