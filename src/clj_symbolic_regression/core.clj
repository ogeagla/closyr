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


(defn expr->fn
  [^ExprEvaluator util ^ISymbol variable ^IAST expr]
  (F/Function (F/List (into-array ISymbol [variable]))
              (.eval util expr)))


(defn ->iexprs
  [coll]
  (into-array IExpr coll))


(defn ->strings
  [coll]
  (into-array String coll))


(defn replace-fn
  ^IExpr [^IExpr ie]
  ie)


(defn my-replace-fn
  []
  (reify
    java.util.function.Function
    (apply
      [this arg]
      (replace-fn arg))))


(defn demo-math
  []

  (let [^ExprEvaluator util      (ExprEvaluator. false 100)

        ^String java-form        (.toJavaForm util "D((sin(x)*cos(x))+5x,x)")

        ^String x-str            "x"
        ^ISymbol x               (F/Dummy x-str)
        ^IAST function           (F/D (F/Times (F/Sin x) (F/Cos x)) x)
        ^IAST function-b         (expr->fn util
                                           x
                                           (F/Plus
                                             (->iexprs
                                               [(F/C1)
                                                (F/D (F/Times x (F/Times x x)) x)
                                                (F/D (F/Times (F/Sin x) (F/Cos x)) x)])))

        ^IExpr result-1          (.eval util function)
        ^IExpr result-1-b        (.eval util function-b)
        ^IExpr result-1-at-point (.evalFunction util function-b (->strings ["0.3"]))

        ^IExpr result-2          (.eval util java-form)
        ^IExpr result-3          (.eval util "diff(sin(x)*cos(x),x)")
        ^IExpr result-4          (.eval util "integrate(sin(x)^5,x)")]
    (println "fn: " java-form)
    (println "res1: " (.toString result-1))
    (println "res1b: " (.toString result-1-b))
    (println "res1b fn: " (.fullFormString function-b)
             "size: " (.size function-b)
             "size2: " (.size (.getArg function-b 2 nil))
             "childchild: " (.getArg (.getArg function-b 2 nil) 2 nil)
             "replace: " (.replaceAll function-b (my-replace-fn)))
    (println "res1b-pt: " (.toString result-1-at-point))
    (println "res2: " (.toString result-2))
    (println "res3: " (.toString result-3))
    (println "res4: " (.toString result-4))))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (demo-math))


(-main)
