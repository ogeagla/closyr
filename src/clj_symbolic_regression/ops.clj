(ns clj-symbolic-regression.ops
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


(defn ^java.util.function.Function tree-modifier
  [modifier]
  (as-function (fn ^IExpr [^IExpr ie]
                 (if (instance? IAST ie)
                   (.map ^IAST ie (tree-modifier modifier))
                   (modifier ie)))))


(defmulti modify (fn [{:keys [op]} pheno] op))


(defmethod modify :substitute
  [{:keys [^IExpr find-expr ^IExpr replace-expr]} {^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
  (->phenotype x-sym (.subs expr find-expr replace-expr)))


(defmethod modify :modify-leafs
  [{:keys [leaf-modifier-fn]} {^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
  (->phenotype x-sym (.replaceAll expr (tree-modifier leaf-modifier-fn))))


(defmethod modify :fn
  [{:keys [modifier-fn]} {^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
  (->phenotype x-sym (modifier-fn pheno)))


(defn initial-phenotypes
  [^ISymbol x]
  (->>
    [F/C0
     x
     (F/Times -1 (->iexprs [x]))]
    (mapv (fn [^IExpr expr] (->phenotype x expr)))))


(defn initial-mutations
  []
  [{:op          :fn
    :label       "Derivative"
    :modifier-fn (fn [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (F/D expr x-sym))}

   {:op          :fn
    :label       "+1/2"
    :modifier-fn (fn [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.plus expr F/C1D2))}

   {:op          :fn
    :label       "-1/2"
    :modifier-fn (fn [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.minus expr F/C1D2))}

   {:op          :fn
    :label       "+Sin"
    :modifier-fn (fn [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.plus expr (F/Sin x-sym)))}

   {:op          :fn
    :label       "-Sin"
    :modifier-fn (fn [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.minus expr (F/Sin x-sym)))}

   {:op          :fn
    :label       "+x"
    :modifier-fn (fn [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.plus expr x-sym))}

   {:op          :fn
    :label       "-x"
    :modifier-fn (fn [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.minus expr x-sym))}
   {:op          :fn
    :label       "*x"
    :modifier-fn (fn [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.times expr x-sym))}

   {:op          :fn
    :label       "/x"
    :modifier-fn (fn [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.divide expr x-sym))}
   {:op          :fn
    :label       "*-1"
    :modifier-fn (fn [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.times expr F/CN1))}

   {:op          :fn
    :label       "/2"
    :modifier-fn (fn [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.times expr F/C1D2))}

   {:op          :fn
    :label       "*2"
    :modifier-fn (fn [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.times expr F/C2))}



   {:op               :modify-leafs
    :label            "x+1/5"
    :leaf-modifier-fn (fn ^IExpr [^IExpr ie]
                        (if (= (.toString ie) "x")
                          (.plus ie (F/C1D5))
                          ie))}


   {:op               :modify-leafs
    :label            "x-1/5"
    :leaf-modifier-fn (fn ^IExpr [^IExpr ie]
                        (if (= (.toString ie) "x")
                          (.minus ie (F/C1D5))
                          ie))}


   {:op               :modify-leafs
    :label            "c/2"
    :leaf-modifier-fn (fn ^IExpr [^IExpr ie]
                        (if (= (.toString ie) "x")
                          ie
                          (.times ie (F/C1D2))))}


   {:op               :modify-leafs
    :label            "c*2"
    :leaf-modifier-fn (fn ^IExpr [^IExpr ie]
                        (if (= (.toString ie) "x")
                          ie
                          (.times ie (F/C2))))}

   {:op           :substitute
    :label        "Sin->Cos"
    :find-expr    F/Sin
    :replace-expr F/Cos}

   {:op           :substitute
    :label        "Cos->Sin"
    :find-expr    F/Cos
    :replace-expr F/Sin}])


(defn demo-math
  []

  (let [;; use this for testing what the IAST form should look like from a basic algebraic expression in string form:
        ^String java-form                (.toJavaForm util "D((sin(x)*cos(x))+5x,x)")

        {sym-x :sym expr-1 :expr fn-1 :fn :as pheno-1} (as->
                                                         (F/Dummy "x") x
                                                         (->phenotype
                                                           x
                                                           (F/Plus
                                                             (->iexprs
                                                               [(F/C1)
                                                                (F/D (F/Times x (F/Times x x)) x)
                                                                (F/D (F/Times (F/Sin x) (F/Cos x)) x)]))))


        pheno-2                          (modify
                                           {:op          :fn
                                            :modifier-fn (fn [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                                                           (.plus expr expr))}
                                           pheno-1)


        ^IExpr result-1-fn               (.eval util fn-1)
        ^IExpr result-1-eval-fn-at-point (eval-phenotype pheno-1 0.3)
        ^IExpr result-2-eval-fn-at-point (eval-phenotype pheno-2 0.3)]
    (println "res1 fn: " (.toString result-1-fn))
    (println "res1 expr: " (.fullFormString expr-1))
    (println "res1 full fn: " (.fullFormString fn-1)
             "\n expr size: " (.size expr-1)
             "size2: " (.size (.getArg expr-1 2 nil))
             "\n expr child: " (.getArg expr-1 2 nil)
             "\n expr childchild: " (.getArg (.getArg expr-1 2 nil) 2 nil)
             "\n expr replace1a: " (:expr
                                     (modify {:op               :modify-leafs
                                              :leaf-modifier-fn (fn ^IExpr [^IExpr ie]
                                                                  (.plus ie (F/C5)))}
                                             pheno-1))

             "\n expr replace1b: " (:expr
                                     (modify {:op               :modify-leafs
                                              :leaf-modifier-fn (fn ^IExpr [^IExpr ie]
                                                                  (if (= (.toString ie) "x")
                                                                    (.minus ie (F/C1D5))
                                                                    (.plus ie (F/C5))))}
                                             pheno-1))

             "\n expr replace2: " (:expr
                                    (modify {:op           :substitute
                                             :find-expr    F/Sin
                                             :replace-expr F/Tan}
                                            pheno-1))
             "\n expr replace3: " (:expr
                                    (modify {:op           :substitute
                                             :find-expr    F/Power
                                             :replace-expr F/Divide}
                                            pheno-1))
             "\n expr replace4: " (:expr
                                    (modify {:op           :substitute
                                             :find-expr    F/C1
                                             :replace-expr F/C5}
                                            pheno-1)))
    (println "res1-pt: " (.toString result-1-eval-fn-at-point))
    (println "res2-pt: " (.toString result-2-eval-fn-at-point))))


(defn demo-math-2
  []

  (let [^ISymbol sym-x (F/Dummy "x")
        initial-phenos (initial-phenotypes sym-x)
        initial-muts   (initial-mutations)]
    (println "initial muts: " (count initial-muts))
    (println "initial fn x muts: "
             (->>
               (for [p  initial-phenos
                     m1 initial-muts
                     m2 initial-muts
                     m3 initial-muts]
                 (str
                   "\n" (:expr p) " -> " (:label m1) "." (:label m2) "." (:label m3) " :: "
                   (:expr (modify m3 (modify m2 (modify m1 p))))
                   " "))
               (sort-by count)
               (reverse)
               (take 50)))))


(comment
  (demo-math-2))
