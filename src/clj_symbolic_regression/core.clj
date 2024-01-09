(ns clj-symbolic-regression.core
  (:gen-class)
  (:import
    org.matheclipse.core.eval.ExprEvaluator
    (org.matheclipse.core.expression
      F)
    (org.matheclipse.core.interfaces
      IAST
      IExpr
      ISymbol)))


;; https://github.com/axkr/symja_android_library?tab=readme-ov-file#examples

(defn demo-math
  []

  (let [^ExprEvaluator util (ExprEvaluator. false 100)

        ^String java-form   (.toJavaForm util "D(sin(x)*cos(x),x)")

        ^String x-str       "x"
        _                   (println "DEBUG: "
                                     (class F))
        ^ISymbol x          (F/Dummy x-str)
        ^IAST function      (F/D (F/Times (F/Sin x) (F/Cos x)) x)
        ^IExpr result       (.eval util function)]
    (println "fn: " java-form)
    (println "res1: " result)))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (demo-math))


(-main)
