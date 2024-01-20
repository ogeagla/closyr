(ns clj-symbolic-regression.ops
  (:import
    (java.util
      Date
      UUID)
    (org.matheclipse.core.eval
      EvalEngine
      ExprEvaluator)
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


(defn ^"[Ljava.lang.String;" ->strings
  [coll]
  (into-array String coll))


(defn ^ExprEvaluator new-util
  []
  (ExprEvaluator.
    (doto (EvalEngine. true)
      (.setQuietMode true))
    true
    0))


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


(defn is-expr-function?
  [^IExpr ie]
  (instance? IAST ie))


(def crossover-sampler [true false])


(defn crossover
  [{^IAST expr :expr ^ISymbol x-sym :sym ^ExprEvaluator util :util :as p} p-discard]
  (try
    (let [^IExpr e1       (:expr p)
          ^IExpr e2       (:expr p-discard)

          [e1-is-fn e2-is-fn] [(is-expr-function? e1) (is-expr-function? e2)]

          e1-part         (if e1-is-fn
                            (.getArg e1 (inc (rand-int (dec (.size e1)))) nil)
                            e1)

          e2-part         (if e2-is-fn
                            (.getArg e2 (inc (rand-int (dec (.size e2)))) nil)
                            e2)
          ^IExpr new-expr (if (rand-nth crossover-sampler)
                            (F/Plus e1-part e2-part)
                            (F/Times e1-part e2-part))]

      ;; (println "Cross over on: 1: sz:" (.size e1) "fn: " (str e1) "2: sz: " (.size e2) "fn: " (str e2) " --->>> " (str new-expr))

      (->phenotype x-sym new-expr (:util p-discard)))
    (catch Exception e
      (println "Error in ops/crossover: " (.getMessage e))
      nil)))


(defn initial-phenotypes
  [^ISymbol x reps]
  (->>
    (fn []
      [F/C0
       F/C1
       F/CN1
       x
       x
       x
       x
       x
       x
       (F/Times -1 (->iexprs [x]))
       (F/Times -1 (->iexprs [x]))
       (F/Times -1 (->iexprs [x]))
       (F/Times -1 (->iexprs [x]))
       (F/Times -1 (->iexprs [x]))
       (F/Times -1 (->iexprs [x]))
       ;; (F/Log x)
       ;; (F/Exp x)
       ;; (F/Sin x)
       ;; (F/Cos x)
       ;; (F/Sqr x)
       ;; (F/Times -1 (->iexprs [(F/Sqr x)]))
       ])
    (repeatedly reps)
    (mapcat identity)
    (mapv (fn [^IExpr expr] (->phenotype x expr nil)))))


(def modify-leafs-sampler [true false false false])


(defn should-modify-leaf
  [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
  (let [leaf-scalar (min 1.0
                         (max 0.005
                              (/ 1.0 (.leafCount expr))))
        r           (rand)
        do?         (< r (* 1.5 leaf-scalar))]
    #_(when do?
        (println "do mod: leafs: " (.leafCount expr) " , " r " < 1.5 *" leaf-scalar))
    do?))


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
    :label       "+1/100"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.plus expr (F/Divide 1 F/C100)))}

   {:op          :fn
    :label       "-1/100"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.minus expr (F/Divide 1 F/C100)))}

   {:op          :fn
    :label       "+Sin"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.plus expr (F/Sin x-sym)))}

   {:op          :fn
    :label       "-Sin"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.minus expr (F/Sin x-sym)))}

   {:op          :fn
    :label       "+Log"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.plus expr (F/Log x-sym)))}

   {:op          :fn
    :label       "-Log"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.minus expr (F/Log x-sym)))}

   {:op          :fn
    :label       "+Exp"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.plus expr (F/Exp x-sym)))}

   {:op          :fn
    :label       "-Exp"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.minus expr (F/Exp x-sym)))}

   {:op          :fn
    :label       "+Cos"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.plus expr (F/Cos x-sym)))}

   {:op          :fn
    :label       "-Cos"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.minus expr (F/Cos x-sym)))}

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


   {:op          :fn
    :label       "/100"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.times expr (F/Divide 1 F/C100)))}

   {:op          :fn
    :label       "*100"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.times expr F/C100))}

   {:op          :fn
    :label       "*1.1"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.times expr (F/num 1.1)))}

   {:op          :fn
    :label       "*0.9"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.times expr (F/num 0.9)))}



   {:op               :modify-leafs
    :label            "x+1/2"
    :leaf-modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (= (.toString ie) "x") (should-modify-leaf pheno))
                          (.plus ie (F/C1D2))
                          ie))}


   {:op               :modify-leafs
    :label            "x-1/2"
    :leaf-modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (= (.toString ie) "x") (should-modify-leaf pheno))
                          (.minus ie (F/C1D2))
                          ie))}


   {:op               :modify-leafs
    :label            "x/10"
    :leaf-modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (= (.toString ie) "x") (should-modify-leaf pheno))
                          (.divide ie (F/C10))
                          ie))}


   {:op               :modify-leafs
    :label            "10*x"
    :leaf-modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (= (.toString ie) "x") (should-modify-leaf pheno))
                          (.times ie (F/C10))
                          ie))}


   {:op               :modify-leafs
    :label            "x/100"
    :leaf-modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (= (.toString ie) "x") (should-modify-leaf pheno))
                          (.divide ie (F/C100))
                          ie))}


   {:op               :modify-leafs
    :label            "100*x"
    :leaf-modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (= (.toString ie) "x") (should-modify-leaf pheno))
                          (.times ie (F/C100))
                          ie))}

   {:op               :modify-leafs
    :label            "-1*x"
    :leaf-modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (= (.toString ie) "x") (should-modify-leaf pheno))
                          (.times ie (F/CN1))
                          ie))}

   {:op               :modify-leafs
    :label            "1.1*x"
    :leaf-modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (= (.toString ie) "x") (should-modify-leaf pheno))
                          (.times ie (F/num 1.1))
                          ie))}

   {:op               :modify-leafs
    :label            "0.9*x"
    :leaf-modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (= (.toString ie) "x") (should-modify-leaf pheno))
                          (.times ie (F/num 0.9))
                          ie))}



   {:op               :modify-leafs
    :label            "sin(x)"
    :leaf-modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (= (.toString ie) "x") (should-modify-leaf pheno))
                          (F/Sin x-sym)
                          ie))}

   {:op               :modify-leafs
    :label            "cos(x)"
    :leaf-modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (= (.toString ie) "x") (should-modify-leaf pheno))
                          (F/Cos x-sym)
                          ie))}




   {:op               :modify-leafs
    :label            "log(x)"
    :leaf-modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (= (.toString ie) "x") (should-modify-leaf pheno))
                          (F/Log x-sym)
                          ie))}

   {:op               :modify-leafs
    :label            "exp(x)"
    :leaf-modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (= (.toString ie) "x") (should-modify-leaf pheno))
                          (F/Exp x-sym)
                          ie))}



   {:op               :modify-leafs
    :label            "x^1/2"
    :leaf-modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (= (.toString ie) "x") (should-modify-leaf pheno))
                          (.pow ie (F/C1D2))
                          ie))}


   {:op               :modify-leafs
    :label            "x^2"
    :leaf-modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (= (.toString ie) "x") (should-modify-leaf pheno))
                          (.pow ie (F/C2))
                          ie))}


   {:op               :modify-leafs
    :label            "x+1/10"
    :leaf-modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (= (.toString ie) "x") (should-modify-leaf pheno))
                          (.plus ie (F/Divide 1 F/C10))
                          ie))}


   {:op               :modify-leafs
    :label            "x-1/10"
    :leaf-modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (= (.toString ie) "x") (should-modify-leaf pheno))
                          (.minus ie (F/Divide 1 F/C10))
                          ie))}

   {:op               :modify-leafs
    :label            "x+1/100"
    :leaf-modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (= (.toString ie) "x") (should-modify-leaf pheno))
                          (.plus ie (F/Divide 1 F/C100))
                          ie))}


   {:op               :modify-leafs
    :label            "x-1/100"
    :leaf-modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (= (.toString ie) "x") (should-modify-leaf pheno))
                          (.minus ie (F/Divide 1 F/C100))
                          ie))}


   ;; {:op               :modify-leafs
   ;; :label            "c/2"
   ;; :leaf-modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
   ;;                     (if (and (.isNumericArgument ie) (should-modify-leaf pheno))
   ;;                       (.times ie (F/C1D2))
   ;;                       ie))}
   ;;
   ;;
   ;; {:op               :modify-leafs
   ;; :label            "c*2"
   ;; :leaf-modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
   ;;                     (if (and (.isNumericArgument ie) (should-modify-leaf pheno))
   ;;                       (.times ie (F/C2))
   ;;                       ie))}
   ;;
   ;; {:op               :modify-leafs
   ;; :label            "c*-1"
   ;; :leaf-modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
   ;;                     (if (and (.isNumericArgument ie) (should-modify-leaf pheno))
   ;;                       (.times ie (F/CN1))
   ;;                       ie))}
   ;;
   ;; {:op               :modify-leafs
   ;; :label            "c/10"
   ;; :leaf-modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
   ;;                     (if (and (.isNumericArgument ie) (should-modify-leaf pheno))
   ;;                       (.times ie (F/Divide 1 F/C10))
   ;;                       ie))}
   ;;
   ;;
   ;; {:op               :modify-leafs
   ;; :label            "c*10"
   ;; :leaf-modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
   ;;                     (if (and (.isNumericArgument ie) (should-modify-leaf pheno))
   ;;                       (.times ie F/C10)
   ;;                       ie))}
   ;;
   ;;
   ;; {:op               :modify-leafs
   ;; :label            "c+1/10"
   ;; :leaf-modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
   ;;                     (if (and (.isNumericArgument ie) (should-modify-leaf pheno))
   ;;                       (do
   ;;                         ;; (println "1/10 + " (str ie))
   ;;                         (.plus ie (F/Divide 1 F/C10)))
   ;;                       ie))}
   ;;
   ;;
   ;; {:op               :modify-leafs
   ;; :label            "c-1/10"
   ;; :leaf-modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
   ;;                     (if (and (.isNumericArgument ie) (should-modify-leaf pheno))
   ;;                       (.minus ie (F/Divide 1 F/C10))
   ;;                       ie))}
   ;;
   ;; {:op               :modify-leafs
   ;; :label            "c+1/2"
   ;; :leaf-modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
   ;;                     (if (and (.isNumericArgument ie) (should-modify-leaf pheno))
   ;;                       (.plus ie (F/Divide 1 F/C2))
   ;;                       ie))}
   ;;
   ;;
   ;; {:op               :modify-leafs
   ;; :label            "c-1/2"
   ;; :leaf-modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
   ;;                     (if (and (.isNumericArgument ie) (should-modify-leaf pheno))
   ;;                       (.minus ie (F/Divide 1 F/C2))
   ;;                       ie))}
   ;;
   ;; {:op           :substitute
   ;; :label        "Divide->Times"
   ;; :find-expr    F/Divide
   ;; :replace-expr F/Times}
   ;;
   ;; {:op           :substitute
   ;; :label        "Minus->Plus"
   ;; :find-expr    F/Minus
   ;; :replace-expr F/Plus}

   {:op           :substitute
    :label        "Sin->Cos"
    :find-expr    F/Sin
    :replace-expr F/Cos}

   #_{:op           :substitute
      :label        "Sin->Plus"
      :find-expr    F/Sin
      :replace-expr F/Plus}

   {:op           :substitute
    :label        "Cos->Sin"
    :find-expr    F/Cos
    :replace-expr F/Sin}

   #_{:op           :substitute
      :label        "Cos->Plus"
      :find-expr    F/Cos
      :replace-expr F/Plus}



   #_{:op           :substitute
      :label        "Sqrt->Plus"
      :find-expr    F/Sqrt
      :replace-expr F/Plus}

   #_{:op           :substitute
      :label        "Log->Plus"
      :find-expr    F/Log
      :replace-expr F/Plus}

   #_{:op           :substitute
      :label        "Exp->Plus"
      :find-expr    F/Exp
      :replace-expr F/Plus}

   #_{:op           :substitute
      :label        "Power->Plus"
      :find-expr    F/Power
      :replace-expr F/Plus}

   #_{:op           :substitute
      :label        "Power->Times"
      :find-expr    F/Power
      :replace-expr F/Times}])


(def ^ISymbol sym-x (F/Dummy "x"))


(defn doubles->exprs
  [numbers]
  (mapv
    (fn [^double n] (F/num n))
    numbers))


(defn expr->double
  [^IExpr expr]
  (.doubleValue (.toNumber expr)))


(defn exprs->doubles
  [exprs]
  (mapv expr->double exprs))


(defn ^"[Lorg.matheclipse.core.interfaces.IExpr;" exprs->input-exprs-list
  [exprs]
  (let [^"[Lorg.matheclipse.core.interfaces.IExpr;" exprs-arr
        (into-array IExpr exprs)
        ^"[Lorg.matheclipse.core.interfaces.IExpr;" exprs-list
        (into-array IExpr [(F/List exprs-arr)])]
    exprs-list))


(defn eval-vec-pheno
  [p
   {:keys [input-exprs-list input-exprs-count output-exprs-vec]
    :as   run-args}]
  (let [^IExpr new-expr (:expr p)
        new-is-const    (.isNumber new-expr)
        ^IExpr eval-p   (eval-phenotype-on-expr-args p input-exprs-list)
        vs              (mapv
                          (fn [i]
                            (try
                              (expr->double
                                (.getArg eval-p (inc i) F/Infinity))
                              (catch Exception e
                                Double/POSITIVE_INFINITY)))
                          (range (dec (.size eval-p))))
        vs              (if (= input-exprs-count (count vs))
                          vs
                          (mapv
                            (fn [i]
                              (expr->double
                                (if new-is-const
                                  new-expr
                                  (.getArg eval-p 0 F/Infinity))))
                            (range input-exprs-count)))]
    vs))


(defn extend-xs
  [input-exprs-vec]
  (let [x-min                (first input-exprs-vec)
        x-max                (last input-exprs-vec)
        x-range-sz           (- x-max x-min)
        x-range-pct-extend   0.35
        extra-pts            (* x-range-pct-extend (count input-exprs-vec))
        x-range-extend-pt-sz (/ (* x-range-pct-extend x-range-sz) extra-pts)

        x-head               (reverse
                               (mapv
                                 (fn [i]
                                   (- x-min (* (inc i) x-range-extend-pt-sz)))
                                 (range extra-pts)))

        x-tail               (mapv
                               (fn [i]
                                 (+ x-max (* (inc i) x-range-extend-pt-sz)))
                               (range extra-pts))

        x-tail-list          (exprs->input-exprs-list (doubles->exprs x-tail))
        x-head-list          (exprs->input-exprs-list (doubles->exprs x-head))
        xs                   (concat x-head input-exprs-vec x-tail)]
    {:xs          xs
     :x-head      x-head
     :x-head-list x-head-list
     :x-tail      x-tail
     :x-tail-list x-tail-list}))


(defn eval-extended
  [p
   run-args
   {x-head      :x-head
    x-head-list :x-head-list
    x-tail      :x-tail
    x-tail-list :x-tail-list}]
  (concat
    (eval-vec-pheno p (assoc run-args :input-exprs-list x-head-list :input-exprs-count (count x-head)))
    (eval-vec-pheno p run-args)
    (eval-vec-pheno p (assoc run-args :input-exprs-list x-tail-list :input-exprs-count (count x-tail)))))


(defn eval-vec-pheno-oversample-from-orig-xs
  [p
   {:keys [input-exprs-list input-exprs-count input-exprs-vec output-exprs-vec]
    :as   run-args}]
  (let [{x-head      :x-head
         x-head-list :x-head-list
         x-tail      :x-tail
         x-tail-list :x-tail-list
         :as         ext-info} (extend-xs input-exprs-vec)
        xs           (concat x-head (:input-exprs-vec run-args) x-tail)
        evaluated-ys (eval-extended p run-args ext-info)]

    {:xs xs
     :ys evaluated-ys}))


(defn eval-vec-pheno-oversample
  [p
   {:keys [input-exprs-list input-exprs-count input-exprs-vec output-exprs-vec]
    :as   run-args}
   {xs          :xs
    x-head      :x-head
    x-head-list :x-head-list
    x-tail      :x-tail
    x-tail-list :x-tail-list
    :as         ext-info}]
  (let [evaluated-ys (eval-extended p run-args ext-info)]

    {:xs xs
     :ys evaluated-ys}))


(def mutations-sampler
  [1 1 1 1 1 1 1 1 1
   1 1 1 1 1 1 1 1 1
   1 1 1 1 1 1 1 1 1
   1 1 1 1 1 1 1 1 1
   2 2 2 2 2 2
   2 2 2 2 2 2
   2 2 2 2 2 2
   2 2 2 2 2 2
   3 3 3 3
   3 3 3 3
   3 3 3 3
   3 3 3 3
   4 4 4
   4 4 4
   4 4 4
   4 4 4
   5 5 5
   5 5 5
   5 5 5
   5 5
   6 6
   6 6
   6 6
   6 6
   7 7
   7 7
   7 7
   7
   8 8
   8 8
   8 8
   9 9
   9 9
   9
   10 10
   10
   11 11
   11
   12 12
   13 13
   14
   15
   ;; 16
   ;; 17
   ;; 18
   ;; 19
   ;; 20
   ])
