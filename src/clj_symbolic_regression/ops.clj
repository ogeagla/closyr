(ns clj-symbolic-regression.ops
  (:import
    (java.util
      Date
      UUID)
    org.matheclipse.core.eval.ExprEvaluator
    (org.matheclipse.core.expression
      AST
      F)
    (org.matheclipse.core.interfaces
      IAST
      IExpr
      ISymbol)
    (org.matheclipse.parser.client
      SyntaxError)
    (org.matheclipse.parser.client.math
      MathException)))


(set! *warn-on-reflection* true)


(defn ^IAST expr->fn
  [^ISymbol variable ^IAST expr]
  (F/Function
    (F/List ^"[Lorg.matheclipse.core.interfaces.ISymbol;"
     (into-array ISymbol [variable])) expr))


(defn ^"[Lorg.matheclipse.core.interfaces.IExpr;" ->iexprs
  [coll]
  ^"[Lorg.matheclipse.core.interfaces.IExpr;"
  (into-array IExpr coll))


(defn ->strings
  [coll]
  ^"[Ljava.lang.String;"
  (into-array String coll))


(defn replace-fn
  ^IExpr [^IExpr ie]
  (println "Add 5 to " ie)
  (.plus ie (F/C5)))


(defn ^ExprEvaluator new-util
  []
  ;; (println "new evaluator")
  (ExprEvaluator. true 0))


(defn ^java.util.function.Function as-function
  [f]
  (reify java.util.function.Function
    (apply [this arg] (f arg))))


(defn ->phenotype
  ([{v :sym e :expr u :util}]
   (->phenotype v e u))
  ([^ISymbol variable ^IAST expr ^ExprEvaluator util]
   (try
     (let [^ExprEvaluator util (or util (new-util))
           ^IAST expr          (.eval util expr)]
       {:sym  variable
        :util util
        :id   (UUID/randomUUID)
        :expr expr})
     (catch Exception e
       (println "Err creating pheno: " expr " , " variable " , " e)))))


(defn ^IExpr eval-phenotype-on-string-args
  [{^IAST expr :expr ^ISymbol x-sym :sym ^ExprEvaluator util :util p-id :id} string-args]
  (try
    (.evalFunction util (expr->fn x-sym expr) string-args)
    (catch SyntaxError se (println "Warning: syntax error in eval: " se))
    (catch MathException me (println "Warning: math error in eval: " me))
    (catch StackOverflowError soe (println "Warning: stack overflow error in eval: " soe))
    (catch OutOfMemoryError oome (println "Warning: OOM error in eval: " oome))))


(defn ^IExpr eval-phenotype-on-expr-args
  [{^IAST expr :expr ^ISymbol x-sym :sym ^ExprEvaluator util :util p-id :id}
   ^"[Lorg.matheclipse.core.interfaces.IExpr;" expr-args]
  (try
    (let [^IAST ast  (F/ast expr-args (expr->fn x-sym expr))
          ^IExpr res (.eval util ast)]
      res)
    (catch SyntaxError se (println "Warning: syntax error in eval: " se))
    (catch MathException me (println "Warning: math error in eval: " me))
    (catch StackOverflowError soe (println "Warning: stack overflow error in eval: " soe))
    (catch OutOfMemoryError oome (println "Warning: OOM error in eval: " oome))))


(defn ^java.util.function.Function tree-leaf-modifier
  [modifier]
  (as-function (fn ^IExpr [^IExpr ie]
                 (if (instance? IAST ie)
                   (.map ^IAST ie (tree-leaf-modifier modifier))
                   (modifier ie)))))


(defmulti modify (fn [{:keys [op]} pheno] op))


(defmethod modify :substitute
  [{:keys [^IExpr find-expr ^IExpr replace-expr]}
   {^IAST expr :expr ^ISymbol x-sym :sym ^ExprEvaluator util :util :as pheno}]
  (->phenotype x-sym (.subs expr find-expr replace-expr) util))


(defmethod modify :modify-leafs
  [{:keys [leaf-modifier-fn]}
   {^IAST expr :expr ^ISymbol x-sym :sym ^ExprEvaluator util :util :as pheno}]
  (->phenotype x-sym (.replaceAll expr (tree-leaf-modifier (partial leaf-modifier-fn pheno))) util))


(defmethod modify :fn
  [{:keys [modifier-fn]}
   {^IAST expr :expr ^ISymbol x-sym :sym ^ExprEvaluator util :util :as pheno}]
  (->phenotype x-sym (modifier-fn pheno) util))


(defn initial-phenotypes
  [^ISymbol x reps]
  (->>
    [F/C0
     x
     (F/Times -1 (->iexprs [x]))]
    (repeat reps)
    (mapcat identity)
    (mapv (fn [^IExpr expr] (->phenotype x expr nil)))))


(def modify-leafs-sampler [true false false false])


(defn initial-mutations
  []
  [{:op          :fn
    :label       "Derivative"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (F/D expr x-sym))}

   {:op          :fn
    :label       "+1/2"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.plus expr F/C1D2))}

   {:op          :fn
    :label       "-1/2"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.minus expr F/C1D2))}
   {:op          :fn
    :label       "+1/10"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.plus expr (F/Divide 1 F/C10)))}

   {:op          :fn
    :label       "-1/10"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.minus expr (F/Divide 1 F/C10)))}

   {:op          :fn
    :label       "+Sin"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.plus expr (F/Sin x-sym)))}

   {:op          :fn
    :label       "-Sin"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.minus expr (F/Sin x-sym)))}

   {:op          :fn
    :label       "+Cos"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.plus expr (F/Cos x-sym)))}

   {:op          :fn
    :label       "-Cos"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.minus expr (F/Cos x-sym)))}


   ;; {:op          :fn
   ;; :label       "+Exp"
   ;; :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
   ;;                (.plus expr (F/Exp x-sym)))}
   ;;
   ;; {:op          :fn
   ;; :label       "-Exp"
   ;; :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
   ;;                (.minus expr (F/Exp x-sym)))}
   ;;
   ;;
   ;;
   ;; {:op          :fn
   ;; :label       "+ 1/Exp"
   ;; :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
   ;;                (.plus expr (F/Divide 1 (F/Exp x-sym))))}
   ;;
   ;; {:op          :fn
   ;; :label       "- 1/Exp"
   ;; :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
   ;;                (.minus expr (F/Divide 1 (F/Exp x-sym))))}
   ;;
   ;;
   ;;
   ;; {:op          :fn
   ;; :label       "*Exp"
   ;; :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
   ;;                (.times expr (F/Exp x-sym)))}
   ;;
   ;; {:op          :fn
   ;; :label       "/Exp"
   ;; :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
   ;;                (.times expr (F/Divide 1 (F/Exp x-sym))))}


   {:op          :fn
    :label       "*Sin"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.times expr (F/Sin x-sym)))}

   {:op          :fn
    :label       "/Sin"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.times expr (F/Divide 1 (F/Sin x-sym))))}

   {:op          :fn
    :label       "*Cos"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.times expr (F/Cos x-sym)))}

   {:op          :fn
    :label       "/Cos"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.times expr (F/Divide 1 (F/Cos x-sym))))}

   {:op          :fn
    :label       "+x"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.plus expr x-sym))}

   {:op          :fn
    :label       "-x"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.minus expr x-sym))}

   {:op          :fn
    :label       "+x^2"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.plus expr (.pow x-sym 2)))}

   {:op          :fn
    :label       "-x^2"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.minus expr (.pow x-sym 2)))}

   {:op          :fn
    :label       "+x^1/2"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.plus expr (.pow x-sym 0.5)))}

   {:op          :fn
    :label       "-x^1/2"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.minus expr (.pow x-sym 0.5)))}

   {:op          :fn
    :label       "*x"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.times expr x-sym))}

   {:op          :fn
    :label       "/x"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.divide expr x-sym))}

   {:op          :fn
    :label       "*-1"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.times expr F/CN1))}

   {:op          :fn
    :label       "/2"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.times expr F/C1D2))}

   {:op          :fn
    :label       "*2"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.times expr F/C2))}

   {:op          :fn
    :label       "/10"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.times expr (F/Divide 1 F/C10)))}

   {:op          :fn
    :label       "*10"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.times expr F/C10))}



   {:op               :modify-leafs
    :label            "x+1/2"
    :leaf-modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (= (.toString ie) "x") (rand-nth modify-leafs-sampler))
                          (.plus ie (F/C1D2))
                          ie))}


   {:op               :modify-leafs
    :label            "x-1/2"
    :leaf-modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (= (.toString ie) "x") (rand-nth modify-leafs-sampler))
                          (.minus ie (F/C1D2))
                          ie))}


   {:op               :modify-leafs
    :label            "x/10"
    :leaf-modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (= (.toString ie) "x") (rand-nth modify-leafs-sampler))
                          (.divide ie (F/C10))
                          ie))}


   {:op               :modify-leafs
    :label            "10*x"
    :leaf-modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (= (.toString ie) "x") (rand-nth modify-leafs-sampler))
                          (.times ie (F/C10))
                          ie))}



   {:op               :modify-leafs
    :label            "sin(x)"
    :leaf-modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (= (.toString ie) "x") (rand-nth modify-leafs-sampler))
                          (F/Sin x-sym)
                          ie))}




   ;{:op               :modify-leafs
   ; :label            "arcsin(x)"
   ; :leaf-modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
   ;                     (if (and (= (.toString ie) "x") (rand-nth modify-leafs-sampler))
   ;                       (F/ArcSin x-sym)
   ;                       ie))}



   {:op               :modify-leafs
    :label            "cos(x)"
    :leaf-modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (= (.toString ie) "x") (rand-nth modify-leafs-sampler))
                          (F/Cos x-sym)
                          ie))}
   ;
   ;
   ;
   ;
   ;{:op               :modify-leafs
   ; :label            "arccos(x)"
   ; :leaf-modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
   ;                     (if (and (= (.toString ie) "x") (rand-nth modify-leafs-sampler))
   ;                       (F/ArcCos x-sym)
   ;                       ie))}



   {:op               :modify-leafs
    :label            "x^1/2"
    :leaf-modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (= (.toString ie) "x") (rand-nth modify-leafs-sampler))
                          (.pow ie (F/C1D2))
                          ie))}


   {:op               :modify-leafs
    :label            "x^2"
    :leaf-modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (= (.toString ie) "x") (rand-nth modify-leafs-sampler))
                          (.pow ie (F/C2))
                          ie))}


   {:op               :modify-leafs
    :label            "x+1/10"
    :leaf-modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (= (.toString ie) "x") (rand-nth modify-leafs-sampler))
                          (.plus ie (F/Divide 1 F/C10))
                          ie))}


   {:op               :modify-leafs
    :label            "x-1/10"
    :leaf-modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (= (.toString ie) "x") (rand-nth modify-leafs-sampler))
                          (.minus ie (F/Divide 1 F/C10))
                          ie))}


   {:op               :modify-leafs
    :label            "c/2"
    :leaf-modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (.isNumericArgument ie) (rand-nth modify-leafs-sampler))
                          (.times ie (F/C1D2))
                          ie))}


   {:op               :modify-leafs
    :label            "c*2"
    :leaf-modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (.isNumericArgument ie)  (rand-nth modify-leafs-sampler))
                          (.times ie (F/C2))
                          ie))}

   {:op               :modify-leafs
    :label            "c/10"
    :leaf-modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (.isNumericArgument ie) (rand-nth modify-leafs-sampler))
                          (.times ie (F/Divide 1 F/C10))
                          ie))}


   {:op               :modify-leafs
    :label            "c*10"
    :leaf-modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (.isNumericArgument ie) (rand-nth modify-leafs-sampler))
                          (.times ie F/C10)
                          ie))}


   {:op               :modify-leafs
    :label            "c+1/10"
    :leaf-modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (.isNumericArgument ie) (rand-nth modify-leafs-sampler))
                          (do
                            ;(println "1/10 + " (str ie))
                            (.plus ie (F/Divide 1 F/C10)))
                          ie))}


   {:op               :modify-leafs
    :label            "c-1/10"
    :leaf-modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (.isNumericArgument ie) (rand-nth modify-leafs-sampler))
                          (.minus ie (F/Divide 1 F/C10))
                          ie))}

   {:op               :modify-leafs
    :label            "c+1/2"
    :leaf-modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (.isNumericArgument ie) (rand-nth modify-leafs-sampler))
                          (.plus ie (F/Divide 1 F/C2))
                          ie))}


   {:op               :modify-leafs
    :label            "c-1/2"
    :leaf-modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (.isNumericArgument ie) (rand-nth modify-leafs-sampler))
                          (.minus ie (F/Divide 1 F/C2))
                          ie))}

   {:op           :substitute
    :label        "Divide->Times"
    :find-expr    F/Divide
    :replace-expr F/Times}

   {:op           :substitute
    :label        "Minus->Plus"
    :find-expr    F/Minus
    :replace-expr F/Plus}

   {:op           :substitute
    :label        "Sin->Cos"
    :find-expr    F/Sin
    :replace-expr F/Cos}

   {:op           :substitute
    :label        "Cos->Sin"
    :find-expr    F/Cos
    :replace-expr F/Sin}])

