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


(defn ^IAST expr->fn
  [^ExprEvaluator util ^ISymbol variable ^IAST expr]
  (F/Function (F/List (into-array ISymbol [variable])) expr))


(defn ->iexprs
  [coll]
  (into-array IExpr coll))


(defn ->strings
  [coll]
  (into-array String coll))


(defn replace-fn
  ^IExpr [^IExpr ie]
  (println "Add 5 to " ie)
  (.plus ie (F/C5)))


(def ^ExprEvaluator util (ExprEvaluator. false 100))


(defn ^java.util.function.Function as-function
  [f]
  (reify java.util.function.Function
    (apply [this arg] (f arg))))


(defn ->phenotype
  [^ISymbol variable ^IAST expr]
  (let [^IAST expr (.eval util expr)]
    {:sym  variable
     :expr expr
     :fn   (expr->fn util variable expr)}))


(defn eval-phenotype
  [{^IAST pfn :fn} x]
  (.evalFunction util pfn (->strings [(str x)])))


(defn modify-phenotype
  [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno} modifier-fn]
  (->phenotype x-sym (modifier-fn pheno)))


(defn ^java.util.function.Function tree-modifier
  [modifier]
  (as-function (fn ^IExpr [^IExpr ie]
                 (if (instance? IAST ie)
                   (.map ^IAST ie (tree-modifier modifier))
                   (do
                     (println "Tree modify leaf: " ie)
                     (modifier ie))))))


(defmulti modify (fn [{:keys [op]} pheno] op))


(defmethod modify :substitute
  [{:keys [^IExpr find-expr ^IExpr replace-expr]} {^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
  (->phenotype x-sym (.subs expr find-expr replace-expr)))


(defmethod modify :modify-leafs
  [{:keys [leaf-modifier-fn]} {^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
  (->phenotype x-sym (.replaceAll expr (tree-modifier leaf-modifier-fn))))


(defn demo-math
  []

  (let [;; use this for testing what the IAST form should look like from a basic algebraic expression in string form:
        ^String java-form                (.toJavaForm util "D((sin(x)*cos(x))+5x,x)")

        {expr-1 :expr function-1 :fn
         :as    pheno-1} (as-> (F/Dummy "x") x
                               (->phenotype
                                 x
                                 (F/Plus
                                   (->iexprs
                                     [(F/C1)
                                      (F/D (F/Times x (F/Times x x)) x)
                                      (F/D (F/Times (F/Sin x) (F/Cos x)) x)]))))


        pheno-2                          (modify-phenotype pheno-1
                                                           (fn [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                                                             (.plus expr expr)))


        ^IExpr result-1-fn               (.eval util function-1)
        ^IExpr result-1-eval-fn-at-point (eval-phenotype pheno-1 0.3)
        ^IExpr result-2-eval-fn-at-point (eval-phenotype pheno-2 0.3)]
    (println "res1 fn: " (.toString result-1-fn))
    (println "res1 expr: " (.fullFormString expr-1))
    (println "res1 full fn: " (.fullFormString function-1)
             "\n expr size: " (.size expr-1)
             "size2: " (.size (.getArg expr-1 2 nil))
             "\n expr child: " (.getArg expr-1 2 nil)
             "\n expr childchild: " (.getArg (.getArg expr-1 2 nil) 2 nil)
             "\n expr replace1: " (:expr (modify {:op               :modify-leafs
                                                  :leaf-modifier-fn (fn ^IExpr [^IExpr ie] (.plus ie (F/C5)))}
                                                 pheno-1))

             "\n expr replace2: " (:expr (modify {:op           :substitute
                                                  :find-expr    F/Sin
                                                  :replace-expr F/Tan}
                                                 pheno-1))
             "\n expr replace3: " (:expr (modify {:op           :substitute
                                                  :find-expr    F/Power
                                                  :replace-expr F/Divide}
                                                 pheno-1))
             "\n expr replace4: " (:expr (modify {:op           :substitute
                                                  :find-expr    F/C1
                                                  :replace-expr F/C5}
                                                 pheno-1)))
    (println "res1-pt: " (.toString result-1-eval-fn-at-point))
    (println "res2-pt: " (.toString result-2-eval-fn-at-point))))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (demo-math))


(-main)
