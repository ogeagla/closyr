(ns closyr.ops.common
  (:refer-clojure :exclude [rand rand-int rand-nth shuffle])
  (:require
    [clojure.core.async :as async :refer [go go-loop timeout <!! >!! <! >! chan put! take! alts!! alt!! close!]]
    [closyr.dataset.prng :refer :all]
    [closyr.log :as log])
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
  "Turn a coll of doubles to IExprs"
  [numbers]
  (mapv
    (fn [^double n] (F/num n))
    numbers))


(defn expr->double
  "Turn an IExpr to a double"
  [^IExpr expr]
  (.doubleValue (.toNumber expr)))


(defn exprs->doubles
  "Turn IExprs to vec of doubles"
  [exprs]
  (mapv expr->double exprs))


(def ^ISymbol sym-x
  "The variable to use in functions"
  (F/Dummy "x"))


(defn ^EvalEngine new-eval-engine
  "Create a new eval engine"
  []
  (doto (EvalEngine. true)
    (.setQuietMode true)))


(defn ^ExprEvaluator new-util
  "Create a new expr evaluator"
  []
  (ExprEvaluator. (new-eval-engine) true 0))


(defn ^"[Lorg.matheclipse.core.interfaces.IExpr;" exprs->exprs-list
  "Turn a coll of exprs into a primitive IExpr array List"
  [exprs]
  (let [^"[Lorg.matheclipse.core.interfaces.IExpr;" exprs-arr
        (into-array IExpr exprs)
        ^"[Lorg.matheclipse.core.interfaces.IExpr;" exprs-list
        (into-array IExpr [(F/List exprs-arr)])]
    exprs-list))


(def ^:private ^IExpr assume-x-gt-zero (F/Greater sym-x 0))


(def ^:private ^:dynamic *simplify-probability-sampler*
  0.5)


(def ^:private ^:dynamic *simplify-timeout*
  500)


(def ^:dynamic *simplify-max-leafs*
  "Max leafs allowed for a candidate IExpr to simplify"
  5)


(defn start-date->diff-ms
  "Compute ms difference from start to now"
  [^Date start]
  (let [end  (Date.)
        diff (- (.getTime end) (.getTime start))]
    diff))


(defn ^IAST expr->fn
  "Turn an IExpr into a Function IAST"
  [{^IAST expr :expr ^ISymbol x-sym :sym ^ExprEvaluator util :util p-id :id :as pheno}]
  (let [res (F/Function
              (F/List ^"[Lorg.matheclipse.core.interfaces.ISymbol;"
               (into-array ISymbol [x-sym]))
              expr)]
    res))


(defn ^"[Lorg.matheclipse.core.interfaces.IExpr;" ->iexprs
  "Turn a coll to an IExpr primitive array"
  [coll]
  ^"[Lorg.matheclipse.core.interfaces.IExpr;"
  (into-array IExpr coll))


(defn ^Function as-function
  "Wrap in clojure fn into a java.util.function.Function"
  [f]
  (reify Function
    (apply [this arg] (f arg))))


(defn ->phenotype
  "Create a GA phenotype from an expr and symbol and other args"
  ([{v :sym e :expr u :util}]
   (->phenotype v e u))
  ([^ISymbol variable ^IAST expr ^ExprEvaluator util]
   (try
     (let [^ExprEvaluator util (or util (new-util))
           ^IAST expr          (if (or (.isNIL expr)
                                       (and (.isBuiltInSymbol expr) (not (.isReal expr))))
                                 (F/Times variable F/C1)
                                 expr)
           ^IAST expr          (.eval util expr)]
       {:sym  variable
        :util util
        :id   (UUID/randomUUID)
        :expr expr})
     (catch Exception e
       (log/error "Err creating pheno: " expr " , " variable " , " (.getMessage e))))))


(defn- inversely-proportional-to-leaf-size
  [leaf-count scalar]
  (let [leaf-scalar (min 1.0
                         (max 0.005
                              (/ 1.0 leaf-count)))
        r           (rand)
        do?         (< r (* scalar leaf-scalar))]
    do?))


(defn should-modify-leaf
  "Based on probability, check if should modify leaf"
  [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
  (inversely-proportional-to-leaf-size leaf-count 1.5))


(defn should-modify-branch
  "Based on probability, check if should modify branch"
  [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
  (inversely-proportional-to-leaf-size leaf-count 0.5))


(defn should-modify-ast-head
  "Based on probability, check if should modify AST head"
  [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
  (inversely-proportional-to-leaf-size leaf-count 0.25))


(def do-not-simplify-fns*
  "Map of fns to not attempt to simplify because they've taken too long in the past"
  (atom {}))


(defn- check-simplify-timing
  [^IAST expr done?*]
  (let [report-done?* (atom false)]
    (go-loop [c 0]
      (when-not @done?*
        ;; wait sequence in ms looks like: 100, 316, 1000, ...
        (<! (timeout (int (Math/pow 10 (+ 1.5 (/ c 4))))))
        (when (> c 6)
          (reset! report-done?* true)
          (swap! do-not-simplify-fns* assoc (str expr) 1)
          (log/warn "Warning: simplify taking a long time: " c
                    " " (.leafCount expr) " : " (str expr)
                    " total slow fns: " (count @do-not-simplify-fns*)))
        (recur (inc c))))
    (when @report-done?*
      (log/warn "Warning: simplify took a long time: "
                " : " (str expr)
                " total slow fns: " (count @do-not-simplify-fns*)))))


(defn- ^IAST simplify
  [^IAST expr]
  (F/Simplify expr))

(def ^:dynamic *long-simplify-thresh-ms* 2000)

(defn- ^IAST do-simplify
  [start
   done?*
   {^IAST expr :expr ^ISymbol x-sym :sym ^ExprEvaluator util :util p-id :id simple? :simple? :as pheno}]
  (if (@do-not-simplify-fns* (str expr))
    (do
      (swap! do-not-simplify-fns* update (str expr) inc)
      (log/warn "Skip fn which we cannot simplify: " (str expr))
      expr)
    (try
      (check-simplify-timing expr done?*)
      (let [^IAST new-expr (simplify expr)
            #_(F/FullSimplify
                (F/Simplify
                  expr
                  assume-x-gt-zero))
            res            (.eval (or util (new-util)) new-expr)
            diff-ms        (start-date->diff-ms start)]
        (reset! done?* true)
        (when (> diff-ms *long-simplify-thresh-ms*)
          (log/warn "Long simplify: "
                    (.leafCount expr) (str expr) " -->> "
                    (.leafCount res) (str res)))
        res)

      (catch Exception e
        (log/error "Err in eval simplify for fn: " (str expr) " : " e)
        expr))))


(defn ^IAST maybe-simplify
  "Maybe simplify pheno expr"
  [{^IAST expr :expr ^ISymbol x-sym :sym ^ExprEvaluator util :util p-id :id simple? :simple? :as pheno}]

  (if (and (<= (.leafCount expr) *simplify-max-leafs*)
           (not simple?)
           (< (rand) *simplify-probability-sampler*))
    (let [start              (Date.)
          done?*             (atom false)
          ^IAST simpled-expr (do-simplify start done?* pheno)]
      (when-not util (log/warn "Warning creating new util during simplify"))
      (assoc pheno
             :simple? true
             :expr simpled-expr))
    pheno))


(defn extend-xs
  "Add extra xs on either side of the provided range"
  [input-xs-vec]
  (let [x-min                (first input-xs-vec)
        x-max                (last input-xs-vec)
        x-range-sz           (- x-max x-min)
        x-range-pct-extend   0.35
        extra-pts            (* x-range-pct-extend (count input-xs-vec))
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

        x-tail-list          (exprs->exprs-list (doubles->exprs x-tail))
        x-head-list          (exprs->exprs-list (doubles->exprs x-head))
        xs                   (concat x-head input-xs-vec x-tail)]
    {:xs          xs
     :x-head      x-head
     :x-head-list x-head-list
     :x-tail      x-tail
     :x-tail-list x-tail-list}))
