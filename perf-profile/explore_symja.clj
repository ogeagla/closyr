(ns explore-symja
  (:require [closyr.ops.common :as ops-common])
  (:import [org.matheclipse.core.expression F]
           [org.matheclipse.core.eval ExprEvaluator EvalEngine]
           [org.matheclipse.core.interfaces IExpr IAST ISymbol]))

;; Explore Symja's numeric evaluation options
(defn test-evaluation-methods []
  (let [x (F/Dummy "x")
        expr (F/Plus (F/Sin x) (F/Times x x))  ; sin(x) + x^2
        util (ops-common/new-util)
        test-vals [0.0 0.5 1.0 1.5 2.0]]

    (println "\n=== Exploring Symja Evaluation Methods ===\n")

    ;; Method 1: Current approach - evaluate function on list
    (println "1. Current F/ast approach:")
    (let [xs-exprs (mapv #(F/num %) test-vals)
          xs-arr (into-array IExpr xs-exprs)
          xs-list (into-array IExpr [(F/List xs-arr)])
          fn-expr (F/Function (F/List (into-array ISymbol [x])) expr)
          ast (F/ast xs-list fn-expr)
          result (.eval util ast)]
      (println "   Result:" (str result)))

    ;; Method 2: Try evalDouble on substituted expression
    (println "\n2. Direct evalDouble per point:")
    (doseq [v test-vals]
      (let [subst-expr (.subs expr x (F/num v))
            evaled (.eval util subst-expr)]
        (when (.isReal evaled)
          (println "   x=" v "-> y=" (.evalDouble evaled)))))

    ;; Method 3: Check if expression can compile to DoubleUnaryOperator
    (println "\n3. Check IExpr numeric methods:")
    (let [simple-expr (F/Times (F/num 2.0) x)]
      (println "   Has isNumericFunction:" (.isNumericFunction simple-expr true))
      (println "   Expr class:" (class expr)))

    ;; Method 4: Benchmark extraction from list result
    (println "\n4. Benchmark: mapv vs areduce for result extraction:")
    (let [xs-exprs (mapv #(F/num %) (range 0.1 10.0 0.1))
          xs-arr (into-array IExpr xs-exprs)
          xs-list (into-array IExpr [(F/List xs-arr)])
          fn-expr (F/Function (F/List (into-array ISymbol [x])) expr)
          ast (F/ast xs-list fn-expr)
          ^IAST result (.eval util ast)
          n (dec (.size result))]

      (println "   Result size:" n)

      ;; mapv approach (current)
      (print "   mapv extraction (1000x): ")
      (time (dotimes [_ 1000]
              (mapv (fn [i]
                      (let [^IExpr e (.getArg result (inc i) F/Infinity)]
                        (if (.isReal e) (.evalDouble e) Double/POSITIVE_INFINITY)))
                    (range n))))

      ;; Direct array approach
      (print "   array extraction (1000x): ")
      (time (dotimes [_ 1000]
              (let [arr (double-array n)]
                (dotimes [i n]
                  (let [^IExpr e (.getArg result (inc i) F/Infinity)]
                    (aset arr i (if (.isReal e) (.evalDouble e) Double/POSITIVE_INFINITY))))
                arr))))))

(test-evaluation-methods)
