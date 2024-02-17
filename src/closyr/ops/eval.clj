(ns closyr.ops.eval
  (:require
    [closyr.ops.common :as ops-common]
    [closyr.util.log :as log]
    [closyr.util.spec :as specs])
  (:import
    (org.matheclipse.core.eval
      EvalControlledCallable
      EvalEngine
      ExprEvaluator)
    (org.matheclipse.core.eval.exception
      ArgumentTypeException
      TimeoutException)
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


#_(defn ^IExpr eval-phenotype-on-string-args
    [{^IAST expr :expr ^ISymbol x-sym :sym ^ExprEvaluator util :util p-id :id :as pheno} string-args]
    (try
      (.evalFunction util (ops-common/expr->fn pheno) string-args)
      (catch SyntaxError se (log/error "Warning: syntax error in eval: " se))
      (catch MathException me (log/error "Warning: math error in eval: " me))
      (catch StackOverflowError soe (log/error "Warning: stack overflow error in eval: " soe))
      (catch OutOfMemoryError oome (log/error "Warning: OOM error in eval: " oome))))


(defn ^IExpr eval-phenotype-on-expr-args
  "Eval an expr at every point in the args"
  {:malli/schema [:=> [:cat #'specs/GAPhenotype #'specs/PrimitiveArrayOfIExpr] [:maybe #'specs/SymbolicExpr]]}
  [{^IAST expr :expr ^ISymbol x-sym :sym ^ExprEvaluator util :util p-id :id :as pheno}
   ^"[Lorg.matheclipse.core.interfaces.IExpr;" expr-args]
  (try
    (when-not util
      (log/warn "*** Warning: No util provided to evaluation engine! ***"))
    (if (and expr
             expr-args
             (not (.isNIL expr)))
      (let [^IAST ast  (F/ast expr-args (ops-common/expr->fn pheno))
            ^IExpr res (.eval (or util (ops-common/new-util)) ast)]
        res)
      (log/warn "Warning: eval needs both expr and args, and expr cannot be NIL"))
    (catch Exception e (log/error "Warning: Error in eval: "
                                  (str expr) " : " (or (.getMessage e) e)))))


(defn- parseable-eval-result?
  [eval-p]
  (not (or (nil? eval-p)
           (= "Indeterminate" (str eval-p)))))


(defn- ^IExpr get-arg
  [^IExpr expr i ^IExpr default]
  (.getArg expr i default))


(defn- result-args->doubles
  [^IExpr eval-p i]
  (try
    (let [^IExpr res (get-arg eval-p (inc i) F/Infinity)]
      (if (.isReal res)
        (ops-common/expr->double res)
        Double/POSITIVE_INFINITY))
    (catch Exception e
      (log/error "Error in evaling function on input values: "
                 (str eval-p) " : " (or (.getMessage e) e))
      Double/POSITIVE_INFINITY)))


(defn- result-args->constant-input
  [^IExpr eval-p ^IExpr new-expr i]
  (try
    (let [^IExpr arg0 (get-arg eval-p 0 F/Infinity)]
      (ops-common/expr->double
        (if (.isReal new-expr)
          new-expr
          (if (.isBuiltInSymbol arg0)
            eval-p
            arg0))))
    (catch Exception e
      (log/error "Error in evaling function on const xs vector: "
                 (str eval-p) " : " (.getMessage e))
      (throw e))))


(defn eval-vec-pheno
  "Evaluate a phenotype's expr on input xs/ys vecs"
  {:malli/schema [:=> [:cat #'specs/GAPhenotype #'specs/SolverEvalArgs] [:maybe #'specs/NumberVector]]}
  [p
   {:keys [input-xs-list input-xs-count]
    :as   run-args}]
  (let [^IExpr new-expr (:expr p)
        ^IExpr eval-p   (eval-phenotype-on-expr-args p input-xs-list)]
    (when (parseable-eval-result? eval-p)
      (mapv
        (if (= input-xs-count (dec (.size eval-p)))
          (partial result-args->doubles eval-p)
          (partial result-args->constant-input eval-p new-expr))
        (range input-xs-count)))))


(defn- clamp-oversampled-ys
  [max-y min-y y]
  (if (infinite? y)
    y
    (min (+ max-y 10.0)
         (max y (- min-y 10.0)))))


(defn eval-extended
  "Eval phenotype expr on extended domain"
  [p
   run-args
   {x-head      :x-head
    x-head-list :x-head-list
    x-tail      :x-tail
    x-tail-list :x-tail-list}]
  (let [middle-section (eval-vec-pheno p run-args)
        max-y          (reduce max middle-section)
        min-y          (reduce min middle-section)]
    (concat

      (mapv #(clamp-oversampled-ys max-y min-y %)
            (eval-vec-pheno p (assoc run-args :input-xs-list x-head-list :input-xs-count (count x-head))))

      middle-section

      (mapv #(clamp-oversampled-ys max-y min-y %)
            (eval-vec-pheno p (assoc run-args :input-xs-list x-tail-list :input-xs-count (count x-tail)))))))


#_(defn eval-vec-pheno-oversample-from-orig-xs
    [p
     {:keys [input-xs-list input-xs-count input-xs-vec input-ys-vec]
      :as   run-args}]
    (let [{x-head      :x-head
           x-head-list :x-head-list
           x-tail      :x-tail
           x-tail-list :x-tail-list
           :as         ext-info} (ops-common/extend-xs input-xs-vec)
          xs           (concat x-head (:input-xs-vec run-args) x-tail)
          evaluated-ys (eval-extended p run-args ext-info)]

      {:xs xs
       :ys evaluated-ys}))


(defn eval-vec-pheno-oversample
  "Evaluate a pheno expr on an extended domain"
  [p
   {:keys [input-xs-list input-xs-count input-xs-vec input-ys-vec]
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


(specs/instrument-all!)
