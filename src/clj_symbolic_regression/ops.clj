(ns clj-symbolic-regression.ops
  (:require
    [clojure.string :as str])
  (:import
    (java.util
      Date
      UUID)
    (org.matheclipse.core.eval
      EvalEngine
      ExprEvaluator)
    (org.matheclipse.core.eval.exception
      ArgumentTypeException)
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
    (catch ArgumentTypeException se (println "Warning: argument type error in eval: " se))
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


(defn ^java.util.function.Function tree-branch-modifier
  [modifier]
  (as-function (fn ^IExpr [^IExpr ie]
                 (if (instance? IAST ie)
                   (let [^IAST ie ie]
                     (modifier
                       (F/ast (->iexprs (.map ie (tree-branch-modifier modifier))) (.head ie))))
                   ie))))


(defn ^java.util.function.Function tree-ast-head-modifier
  [modifier]
  (as-function (fn ^IExpr [^IExpr ie]
                 (if (instance? IAST ie)
                   (let [^IAST ie ie]
                     (F/ast (->iexprs (.map ie (tree-ast-head-modifier modifier))) ^IExpr (modifier (.head ie))))
                   ie))))


(defn op-short-str_unmemo
  [{:keys [op label] :as modif}]
  (let [op-str    (name op)
        ops-short (->> (str/split op-str #"-")
                       (rest)
                       (map #(-> (take 4 %)
                                 (str/join)))
                       (str/join "-"))]
    (str ops-short ":" label)))


(def op-short-str (memoize op-short-str_unmemo))


(defn with-last-op
  [p {:keys [op label] :as modif}]
  (assoc p :last-op (op-short-str (select-keys modif [:op :label]))))


(defmulti modify (fn [{:keys [op]} pheno] op))


(defmethod modify :modify-substitute
  [{:keys [label ^IExpr find-expr ^IExpr replace-expr] :as modif}
   {^IAST expr :expr ^ISymbol x-sym :sym ^ExprEvaluator util :util :as pheno}]
  (-> (->phenotype x-sym (.subs expr find-expr replace-expr) util)
      (with-last-op modif)))


(defmethod modify :modify-leafs
  [{:keys [label leaf-modifier-fn] :as modif}
   {^IAST expr :expr ^ISymbol x-sym :sym ^ExprEvaluator util :util :as pheno}]
  (->
    x-sym
    (->phenotype (.replaceAll expr (tree-leaf-modifier (partial leaf-modifier-fn (.leafCount expr) pheno))) util)
    (with-last-op modif)))


(defmethod modify :modify-branches
  [{:keys [label leaf-modifier-fn] :as modif}
   {^IAST expr :expr ^ISymbol x-sym :sym ^ExprEvaluator util :util :as pheno}]
  (->
    x-sym
    (->phenotype (.replaceAll expr (tree-branch-modifier (partial leaf-modifier-fn (.leafCount expr) pheno))) util)
    (with-last-op modif)))


(defmethod modify :modify-ast-head
  [{:keys [label leaf-modifier-fn] :as modif}
   {^IAST expr :expr ^ISymbol x-sym :sym ^ExprEvaluator util :util :as pheno}]
  (->
    x-sym
    (->phenotype (.replaceAll expr (tree-ast-head-modifier (partial leaf-modifier-fn (.leafCount expr) pheno))) util)
    (with-last-op modif)))


(defmethod modify :modify-fn
  [{:keys [label modifier-fn] :as modif}
   {^IAST expr :expr ^ISymbol x-sym :sym ^ExprEvaluator util :util :as pheno}]
  (-> (->phenotype x-sym (modifier-fn pheno) util)
      (with-last-op modif)))


(defn is-expr-function?
  [^IExpr ie]
  (instance? IAST ie))


(def crossover-sampler [:plus :times :divide12 :divide21])


(defn crossover
  [{^IAST expr :expr ^ISymbol x-sym :sym ^ExprEvaluator util :util :as p} p-discard]
  (try
    (let [^IExpr e1        (:expr p)
          ^IExpr e2        (:expr p-discard)

          [e1-is-fn e2-is-fn] [(is-expr-function? e1) (is-expr-function? e2)]

          e1-part          (if e1-is-fn
                             (.getArg e1 (inc (rand-int (dec (.size e1)))) nil)
                             e1)

          e2-part          (if e2-is-fn
                             (.getArg e2 (inc (rand-int (dec (.size e2)))) nil)
                             e2)
          crossover-flavor (rand-nth crossover-sampler)
          ^IExpr new-expr  (case crossover-flavor
                             :divide12 (F/Divide e1-part e2-part)
                             :divide21 (F/Divide e2-part e1-part)
                             :plus (F/Plus e1-part e2-part)
                             :times (F/Times e1-part e2-part))]

      (-> (->phenotype x-sym new-expr (:util p-discard))
          (with-last-op {:label (name crossover-flavor)
                         :op    :modify-crossover})))
    (catch Exception e
      (println "Error in ops/crossover: " (.getMessage e))
      nil)))


(def ^ISymbol sym-x (F/Dummy "x"))


(def initial-exprs
  (let [^ISymbol x sym-x]
    [F/C0
     F/C1
     x
     x
     x
     x
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
     ]))


(defn initial-phenotypes
  [reps]
  (let [^ISymbol x sym-x]
    (->>
      (fn []
        initial-exprs)
      (repeatedly reps)
      (mapcat identity)
      (mapv (fn [^IExpr expr] (->phenotype x expr nil))))))


(def modify-leafs-sampler [true false false false])


(defn should-modify-leaf
  [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
  (let [leaf-scalar (min 1.0
                         (max 0.005
                              (/ 1.0 leaf-count)))
        r           (rand)
        do?         (< r (* 3.0 leaf-scalar))]
    do?))


(defn should-modify-branch
  [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
  (let [branch-scalar (min 1.0
                           (max 0.005
                                (/ 1.0 leaf-count)))
        r             (rand)
        do?           (< r (* 0.75 branch-scalar))]
    do?))


(defn should-modify-ast-head
  [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
  (let [branch-scalar (min 1.0
                           (max 0.005
                                (/ 1.0 leaf-count)))
        r             (rand)
        do?           (< r (* 0.25 branch-scalar))]
    do?))


(defn initial-mutations
  []
  [{:op          :modify-fn
    :label       "Derivative"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (F/D expr x-sym))}

   {:op          :modify-fn
    :label       "+1/2"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.plus expr F/C1D2))}

   {:op          :modify-fn
    :label       "-1/2"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.minus expr F/C1D2))}
   {:op          :modify-fn
    :label       "+1/10"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.plus expr (F/Divide 1 F/C10)))}

   {:op          :modify-fn
    :label       "-1/10"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.minus expr (F/Divide 1 F/C10)))}

   {:op          :modify-fn
    :label       "+1/100"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.plus expr (F/Divide 1 F/C100)))}

   {:op          :modify-fn
    :label       "-1/100"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.minus expr (F/Divide 1 F/C100)))}

   {:op          :modify-fn
    :label       "+Sin"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.plus expr (F/Sin x-sym)))}

   {:op          :modify-fn
    :label       "-Sin"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.minus expr (F/Sin x-sym)))}

   {:op          :modify-fn
    :label       "+Log"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.plus expr (F/Log x-sym)))}

   {:op          :modify-fn
    :label       "-Log"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.minus expr (F/Log x-sym)))}

   {:op          :modify-fn
    :label       "+Exp"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.plus expr (F/Exp x-sym)))}

   {:op          :modify-fn
    :label       "-Exp"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.minus expr (F/Exp x-sym)))}

   {:op          :modify-fn
    :label       "+Cos"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.plus expr (F/Cos x-sym)))}

   {:op          :modify-fn
    :label       "-Cos"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.minus expr (F/Cos x-sym)))}

   {:op          :modify-fn
    :label       "*Sin"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.times expr (F/Sin x-sym)))}

   {:op          :modify-fn
    :label       "/Sin"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.times expr (F/Divide 1 (F/Sin x-sym))))}

   {:op          :modify-fn
    :label       "*Cos"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.times expr (F/Cos x-sym)))}

   {:op          :modify-fn
    :label       "/Cos"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.times expr (F/Divide 1 (F/Cos x-sym))))}

   {:op          :modify-fn
    :label       "+x"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.plus expr x-sym))}

   {:op          :modify-fn
    :label       "-x"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.minus expr x-sym))}

   {:op          :modify-fn
    :label       "+x^2"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.plus expr (.pow x-sym 2)))}

   {:op          :modify-fn
    :label       "-x^2"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.minus expr (.pow x-sym 2)))}

   {:op          :modify-fn
    :label       "+x^1/2"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.plus expr (.pow x-sym 0.5)))}

   {:op          :modify-fn
    :label       "-x^1/2"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.minus expr (.pow x-sym 0.5)))}

   {:op          :modify-fn
    :label       "*x"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.times expr x-sym))}

   {:op          :modify-fn
    :label       "/x"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.divide expr x-sym))}

   {:op          :modify-fn
    :label       "1/f"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.divide F/C1 expr))}

   {:op          :modify-fn
    :label       "*-1"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.times expr F/CN1))}

   {:op          :modify-fn
    :label       "/2"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.times expr F/C1D2))}

   {:op          :modify-fn
    :label       "*2"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.times expr F/C2))}

   {:op          :modify-fn
    :label       "/10"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.times expr (F/Divide 1 F/C10)))}

   {:op          :modify-fn
    :label       "*10"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.times expr F/C10))}

   {:op          :modify-fn
    :label       "/100"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.times expr (F/Divide 1 F/C100)))}

   {:op          :modify-fn
    :label       "*100"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.times expr F/C100))}

   {:op          :modify-fn
    :label       "*1.1"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.times expr (F/num 1.1)))}

   {:op          :modify-fn
    :label       "*0.9"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.times expr (F/num 0.9)))}

   {:op               :modify-leafs
    :label            "x+1/2"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (.isSymbol ie) (should-modify-leaf leaf-count pheno))
                          (.plus ie (F/C1D2))
                          ie))}

   {:op               :modify-leafs
    :label            "x-1/2"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (.isSymbol ie) (should-modify-leaf leaf-count pheno))
                          (.minus ie (F/C1D2))
                          ie))}

   {:op               :modify-leafs
    :label            "x/10"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (.isSymbol ie) (should-modify-leaf leaf-count pheno))
                          (.divide ie (F/C10))
                          ie))}

   {:op               :modify-leafs
    :label            "10*x"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (.isSymbol ie) (should-modify-leaf leaf-count pheno))
                          (.times ie (F/C10))
                          ie))}

   {:op               :modify-leafs
    :label            "1/x"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (.isSymbol ie) (should-modify-leaf leaf-count pheno))
                          (.divide (F/C1) ie)
                          ie))}

   {:op               :modify-leafs
    :label            "x/100"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (.isSymbol ie) (should-modify-leaf leaf-count pheno))
                          (.divide ie (F/C100))
                          ie))}


   {:op               :modify-leafs
    :label            "100*x"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (.isSymbol ie) (should-modify-leaf leaf-count pheno))
                          (.times ie (F/C100))
                          ie))}

   {:op               :modify-leafs
    :label            "-1*x"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (.isSymbol ie) (should-modify-leaf leaf-count pheno))
                          (.times ie (F/CN1))
                          ie))}

   {:op               :modify-leafs
    :label            "1.1*x"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (.isSymbol ie) (should-modify-leaf leaf-count pheno))
                          (.times ie (F/num 1.1))
                          ie))}

   {:op               :modify-leafs
    :label            "0.9*x"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (.isSymbol ie) (should-modify-leaf leaf-count pheno))
                          (.times ie (F/num 0.9))
                          ie))}

   {:op               :modify-leafs
    :label            "sin(x)"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (.isSymbol ie) (should-modify-leaf leaf-count pheno))
                          (F/Sin x-sym)
                          ie))}

   {:op               :modify-leafs
    :label            "cos(x)"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (.isSymbol ie) (should-modify-leaf leaf-count pheno))
                          (F/Cos x-sym)
                          ie))}

   ;; {:op               :modify-leafs
   ;; :label            "asin(x)"
   ;; :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
   ;;                     (if (and (.isSymbol ie) (should-modify-leaf leaf-count pheno))
   ;;                       (F/ArcSin x-sym)
   ;;                       ie))}
   ;;
   ;; {:op               :modify-leafs
   ;; :label            "acos(x)"
   ;; :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
   ;;                     (if (and (.isSymbol ie) (should-modify-leaf leaf-count pheno))
   ;;                       (F/ArcCos x-sym)
   ;;                       ie))}

   {:op               :modify-leafs
    :label            "log(x)"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (.isSymbol ie) (should-modify-leaf leaf-count pheno))
                          (F/Log x-sym)
                          ie))}

   {:op               :modify-leafs
    :label            "exp(x)"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (.isSymbol ie) (should-modify-leaf leaf-count pheno))
                          (F/Exp x-sym)
                          ie))}

   {:op               :modify-leafs
    :label            "x^1/2"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (.isSymbol ie) (should-modify-leaf leaf-count pheno))
                          (.pow ie (F/C1D2))
                          ie))}

   {:op               :modify-leafs
    :label            "x^2"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (.isSymbol ie) (should-modify-leaf leaf-count pheno))
                          (.pow ie (F/C2))
                          ie))}

   {:op               :modify-leafs
    :label            "x+1/10"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (.isSymbol ie) (should-modify-leaf leaf-count pheno))
                          (.plus ie (F/Divide 1 F/C10))
                          ie))}

   {:op               :modify-leafs
    :label            "x-1/10"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (.isSymbol ie) (should-modify-leaf leaf-count pheno))
                          (.minus ie (F/Divide 1 F/C10))
                          ie))}

   {:op               :modify-leafs
    :label            "x+1/100"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (.isSymbol ie) (should-modify-leaf leaf-count pheno))
                          (.plus ie (F/Divide 1 F/C100))
                          ie))}

   {:op               :modify-leafs
    :label            "x-1/100"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (.isSymbol ie) (should-modify-leaf leaf-count pheno))
                          (.minus ie (F/Divide 1 F/C100))
                          ie))}

   {:op               :modify-leafs
    :label            "c/2"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (.isNumber ie) (should-modify-leaf leaf-count pheno))
                          (.times ie (F/C1D2))
                          ie))}

   {:op               :modify-leafs
    :label            "c*2"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (.isNumber ie) (should-modify-leaf leaf-count pheno))
                          (.times ie (F/C2))
                          ie))}

   {:op               :modify-leafs
    :label            "c*-1"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (.isNumber ie) (should-modify-leaf leaf-count pheno))
                          (.times ie (F/CN1))
                          ie))}

   {:op               :modify-leafs
    :label            "c/10"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (.isNumber ie) (should-modify-leaf leaf-count pheno))
                          (.times ie (F/Divide 1 F/C10))
                          ie))}

   {:op               :modify-leafs
    :label            "c*10"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (.isNumber ie) (should-modify-leaf leaf-count pheno))
                          (.times ie F/C10)
                          ie))}

   {:op               :modify-leafs
    :label            "c+1/10"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (.isNumber ie) (should-modify-leaf leaf-count pheno))
                          (do
                            (.plus ie (F/Divide 1 F/C10)))
                          ie))}

   {:op               :modify-leafs
    :label            "c-1/10"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (.isNumber ie) (should-modify-leaf leaf-count pheno))
                          (.minus ie (F/Divide 1 F/C10))
                          ie))}

   {:op               :modify-leafs
    :label            "c+1/100"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (.isNumber ie) (should-modify-leaf leaf-count pheno))
                          (do
                            (.plus ie (F/Divide 1 F/C100)))
                          ie))}

   {:op               :modify-leafs
    :label            "c-1/100"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (.isNumber ie) (should-modify-leaf leaf-count pheno))
                          (.minus ie (F/Divide 1 F/C100))
                          ie))}

   {:op               :modify-leafs
    :label            "c+1/2"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (.isNumber ie) (should-modify-leaf leaf-count pheno))
                          (.plus ie (F/Divide 1 F/C2))
                          ie))}

   {:op               :modify-leafs
    :label            "c-1/2"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (.isNumber ie) (should-modify-leaf leaf-count pheno))
                          (.minus ie (F/Divide 1 F/C2))
                          ie))}
   ;;
   ;; {:op           :modify-substitute
   ;; :label        "Divide->Times"
   ;; :find-expr    F/Divide
   ;; :replace-expr F/Times}
   ;;
   ;; {:op           :modify-substitute
   ;; :label        "Minus->Plus"
   ;; :find-expr    F/Minus
   ;; :replace-expr F/Plus}

   {:op           :modify-substitute
    :label        "Sin->Cos"
    :find-expr    F/Sin
    :replace-expr F/Cos}

   #_{:op           :modify-substitute
      :label        "Sin->Plus"
      :find-expr    F/Sin
      :replace-expr F/Plus}

   {:op           :modify-substitute
    :label        "Cos->Sin"
    :find-expr    F/Cos
    :replace-expr F/Sin}

   #_{:op           :modify-substitute
      :label        "Cos->Plus"
      :find-expr    F/Cos
      :replace-expr F/Plus}



   #_{:op           :modify-substitute
      :label        "Sqrt->Plus"
      :find-expr    F/Sqrt
      :replace-expr F/Plus}

   #_{:op           :modify-substitute
      :label        "Log->Plus"
      :find-expr    F/Log
      :replace-expr F/Plus}

   #_{:op           :modify-substitute
      :label        "Exp->Plus"
      :find-expr    F/Exp
      :replace-expr F/Plus}

   #_{:op           :modify-substitute
      :label        "Power->Plus"
      :find-expr    F/Power
      :replace-expr F/Plus}

   #_{:op           :modify-substitute
      :label        "Power->Times"
      :find-expr    F/Power
      :replace-expr F/Times}






   {:op               :modify-ast-head
    :label            "ast-h sin->cos"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (should-modify-ast-head leaf-count pheno)
                                 (= F/Sin ie))
                          F/Cos
                          ie))}

   {:op               :modify-ast-head
    :label            "ast-h cos->sin"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (should-modify-ast-head leaf-count pheno)
                                 (= F/Cos ie))
                          F/Sin
                          ie))}


   {:op               :modify-ast-head
    :label            "ast-h +->*"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (should-modify-ast-head leaf-count pheno)
                                 (= F/Plus ie))
                          F/Times
                          ie))}

   {:op               :modify-ast-head
    :label            "ast-h *->+"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (should-modify-ast-head leaf-count pheno)
                                 (= F/Times ie))
                          F/Plus
                          ie))}

   {:op               :modify-ast-head
    :label            "ast-h ^->*"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (should-modify-ast-head leaf-count pheno)
                                 (= F/Power ie))
                          F/Times
                          ie))}

   {:op               :modify-ast-head
    :label            "ast-h /->*"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (should-modify-ast-head leaf-count pheno)
                                 (= F/Divide ie))
                          F/Times
                          ie))}


   {:op               :modify-ast-head
    :label            "ast-h /->+"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (should-modify-ast-head leaf-count pheno)
                                 (= F/Divide ie))
                          F/Plus
                          ie))}

   {:op               :modify-branches
    :label            "b derivative"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (should-modify-branch leaf-count pheno)
                          (F/D ie x-sym)
                          ie))}

   {:op               :modify-branches
    :label            "b sin"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (should-modify-branch leaf-count pheno)
                          (F/Sin ie)
                          ie))}

   {:op               :modify-branches
    :label            "b cos"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (should-modify-branch leaf-count pheno)
                          (F/Cos ie)
                          ie))}

   {:op               :modify-branches
    :label            "b exp"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (should-modify-branch leaf-count pheno)
                          (F/Exp ie)
                          ie))}

   {:op               :modify-branches
    :label            "b log"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (should-modify-branch leaf-count pheno)
                          (F/Log ie)
                          ie))}


   {:op               :modify-branches
    :label            "b * b"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (should-modify-branch leaf-count pheno)
                          (.times ie ie)
                          ie))}

   {:op               :modify-branches
    :label            "b ^1/2"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (should-modify-branch leaf-count pheno)
                          (.pow ie F/C1D2)
                          ie))}

   {:op               :modify-branches
    :label            "b ^-2"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (should-modify-branch leaf-count pheno)
                          (.pow ie F/CN2)
                          ie))}

   {:op               :modify-branches
    :label            "b ^-1"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (should-modify-branch leaf-count pheno)
                          (.pow ie F/CN1)
                          ie))}

   {:op               :modify-branches
    :label            "b *-1"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (should-modify-branch leaf-count pheno)
                          (.times ie F/CN1)
                          ie))}

   {:op               :modify-branches
    :label            "b *1.1"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (should-modify-branch leaf-count pheno)
                          (.times ie (F/num 1.1))
                          ie))}

   {:op               :modify-branches
    :label            "b *0.9"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (should-modify-branch leaf-count pheno)
                          (.times ie (F/num 0.9))
                          ie))}
   {:op               :modify-branches
    :label            "b +0.1"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (should-modify-branch leaf-count pheno)
                          (.plus ie (F/num 0.1))
                          ie))}

   {:op               :modify-branches
    :label            "b -0.1"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (should-modify-branch leaf-count pheno)
                          (.minus ie (F/num 0.1))
                          ie))}])


(defn apply-modifications
  [max-leafs mods-count initial-muts p-winner p-discard]
  (loop [iters      0
         c          mods-count
         v          p-winner
         first-run? true]
    (if (zero? c)
      [v iters]
      (let [v     (if first-run? (assoc v :util (:util p-discard)) v)
            m     (rand-nth initial-muts)
            new-v (try
                    (modify m v)
                    (catch Exception e
                      (when-not (= "Infinite expression 1/0 encountered." (.getMessage e))
                        (println "Warning, mutation failed: " (:label m)
                                 " on: " (str (:expr v))
                                 " due to: " (.getMessage e)))
                      v))]
        (recur
          (inc iters)
          (if (> (.leafCount ^IExpr (:expr new-v)) max-leafs)
            0
            (dec c)) #_(if (fn-size-growing-too-fast? v new-v)
                         0
                         (dec c))
          new-v
          false)))))


(defn eval-vec-pheno
  [p
   {:keys [input-exprs-list input-exprs-count output-exprs-vec]
    :as   run-args}]
  (let [^IExpr new-expr (:expr p)
        ^IExpr eval-p   (eval-phenotype-on-expr-args p input-exprs-list)]
    (if (= "Indeterminate" (str eval-p))
      nil
      (let [vs (mapv
                 (fn [i]
                   (try
                     (expr->double
                       (.getArg eval-p (inc i) F/Infinity))
                     (catch Exception e
                       Double/POSITIVE_INFINITY)))
                 (range (dec (.size eval-p))))
            vs (if (= input-exprs-count (count vs))
                 vs
                 (mapv
                   (fn [i]
                     (try
                       (let [new-is-const (.isNumber new-expr)
                             ^IExpr arg0  (.getArg eval-p 0 F/Infinity)]
                         (expr->double
                           (if new-is-const
                             new-expr
                             (if (.isBuiltInSymbol arg0)
                               eval-p
                               arg0))))
                       (catch Exception e
                         (println "Error in evaling function on const xs vector: "
                                  (str eval-p) " : " (.getMessage e))
                         (throw e))))
                   (range input-exprs-count)))]
        vs))))


(defn clamp-oversampled-ys
  [max-y min-y y]
  (if (infinite? y)
    y
    (min (+ max-y 10.0)
         (max y (- min-y 10.0)))))


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

    (let [res   (eval-vec-pheno p (assoc run-args :input-exprs-list x-head-list :input-exprs-count (count x-head)))
          max-y (apply max res)
          min-y (apply min res)]
      (mapv #(clamp-oversampled-ys max-y min-y %)
            res))
    (eval-vec-pheno p run-args)

    (let [res   (eval-vec-pheno p (assoc run-args :input-exprs-list x-tail-list :input-exprs-count (count x-tail)))
          max-y (apply max res)
          min-y (apply min res)]
      (mapv #(clamp-oversampled-ys max-y min-y %)
            res))))


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
   16
   ;; 17
   ;; 18
   ;; 19
   ;; 20
   ])
