(ns closyr.ops.common
  (:refer-clojure :exclude [rand rand-int rand-nth shuffle])
  (:require
    [clojure.core.async :as async :refer [go go-loop timeout <!! >!! <! >! chan put! take! alts!! alt!! close!]]
    [closyr.dataset.prng :refer :all])
  (:import
    (java.util
      Date
      UUID)
    (java.util.function
      Function)
    (org.matheclipse.core.eval
      EvalControlledCallable
      EvalEngine
      ExprEvaluator)
    (org.matheclipse.core.expression
      AST
      F)
    (org.matheclipse.core.interfaces
      IAST
      IExpr
      ISymbol)))


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


(def ^ISymbol sym-x (F/Dummy "x"))


(defn ^EvalEngine new-eval-engine
  []
  (doto (EvalEngine. true)
    (.setQuietMode true)))


(defn ^ExprEvaluator new-util
  []
  (ExprEvaluator. (new-eval-engine) true 0))


(defn ^"[Lorg.matheclipse.core.interfaces.IExpr;" exprs->input-exprs-list
  [exprs]
  (let [^"[Lorg.matheclipse.core.interfaces.IExpr;" exprs-arr
        (into-array IExpr exprs)
        ^"[Lorg.matheclipse.core.interfaces.IExpr;" exprs-list
        (into-array IExpr [(F/List exprs-arr)])]
    exprs-list))


(def ^IExpr assume-x-gt-zero (F/Greater sym-x 0))


(def ^:dynamic *simplify-probability-sampler*
  0.5)


(def ^:dynamic *simplify-timeout*
  500)


(def ^:dynamic *simplify-max-leafs*
  5)


(defn start-date->diff-ms
  [^Date start]
  (let [end  (Date.)
        diff (- (.getTime end) (.getTime start))]
    diff))


(defn ^IAST expr->fn
  [{^IAST expr :expr ^ISymbol x-sym :sym ^ExprEvaluator util :util p-id :id :as pheno}]
  (F/Function
    (F/List ^"[Lorg.matheclipse.core.interfaces.ISymbol;"
            (into-array ISymbol [x-sym]))
    expr))


(defn ^"[Lorg.matheclipse.core.interfaces.IExpr;" ->iexprs
  [coll]
  ^"[Lorg.matheclipse.core.interfaces.IExpr;"
  (into-array IExpr coll))


(defn ^"[Ljava.lang.String;" ->strings
  [coll]
  (into-array String coll))


(defn ^Function as-function
  [f]
  (reify Function
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


(def modify-leafs-sampler [true false false false])


(defn inversely-proportional-to-leaf-size
  [leaf-count scalar]
  (let [leaf-scalar (min 1.0
                         (max 0.005
                              (/ 1.0 leaf-count)))
        r           (rand)
        do?         (< r (* scalar leaf-scalar))]
    do?))


(defn should-modify-leaf
  [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
  (inversely-proportional-to-leaf-size leaf-count 2.0))


(defn should-modify-branch
  [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
  (inversely-proportional-to-leaf-size leaf-count 0.25))


(defn should-modify-ast-head
  [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
  (inversely-proportional-to-leaf-size leaf-count 0.125))


(def do-not-simplify-fns* (atom {}))


(defn check-simplify-timing
  [^IAST expr done?*]
  (go-loop [c 0]
    (when (not @done?*)
      (do (<! (timeout (int (Math/pow 10 (+ 2 (/ c 2))))))
          (when (> c 2)
            (println "Warning: simplify taking a long time: " c " " (.leafCount expr) " : " (str expr))
            (swap! do-not-simplify-fns* assoc (str expr) true))
          (recur (inc c))))))


(defn ^IAST maybe-simplify
  [{^IAST expr :expr ^ISymbol x-sym :sym ^ExprEvaluator util :util p-id :id simple? :simple? :as pheno}]

  (if (and (<= (.leafCount expr) *simplify-max-leafs*)
           (not simple?)
           (< (rand) *simplify-probability-sampler*))
    (let [start              (Date.)
          done?*             (atom false)
          ^IAST simpled-expr (if (@do-not-simplify-fns* (str expr))
                               (do
                                 (println "Skip fn which we cannot simplify: " (str expr))
                                 expr)
                               (try
                                 (check-simplify-timing expr done?*)
                                 (let [^IAST new-expr (F/Simplify expr)
                                       #_(F/FullSimplify
                                           (F/Simplify
                                             expr
                                             assume-x-gt-zero))
                                       res            (.eval (or util (new-util)) new-expr)
                                       diff-ms        (start-date->diff-ms start)]
                                   (reset! done?* true)
                                   (when (> diff-ms 2000)
                                     (println "Long simplify: "
                                              (.leafCount expr) (str expr) " -->> "
                                              (.leafCount res) (str res)))
                                   res)

                                 (catch Exception e
                                   (println "Err in eval simplify for fn: " (str expr) " : " e)
                                   expr)))]
      (when-not util (println "Warning creating new util during simplify"))
      (assoc pheno
        :simple? true
        :expr simpled-expr))
    pheno))
