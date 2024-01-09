(ns clj-symbolic-regression.core
  (:gen-class)
  (:import org.matheclipse.core.eval.ExprEvaluator))

;;https://github.com/axkr/symja_android_library?tab=readme-ov-file#examples

(defn do-math []

  (let [^ExprEvaluator util (ExprEvaluator. false 100)
        java-form (.toJavaForm util "D(sin(x)*cos(x),x)")
        ]
    (println java-form)
    )
  )

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))


(do-math)
