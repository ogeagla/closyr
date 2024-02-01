(ns closyr.ops.eval
  (:refer-clojure :exclude [rand rand-int rand-nth shuffle])
  (:require
    [clojure.core.async :as async :refer [go go-loop timeout <!! >!! <! >! chan put! take! alts!! alt!! close!]]
    [closyr.dataset.prng :refer :all]
    [closyr.ops.common :as ops-common])
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


(defn ^IExpr eval-phenotype-on-string-args
  [{^IAST expr :expr ^ISymbol x-sym :sym ^ExprEvaluator util :util p-id :id :as pheno} string-args]
  (try
    (.evalFunction util (ops-common/expr->fn pheno) string-args)
    (catch SyntaxError se (println "Warning: syntax error in eval: " se))
    (catch MathException me (println "Warning: math error in eval: " me))
    (catch StackOverflowError soe (println "Warning: stack overflow error in eval: " soe))
    (catch OutOfMemoryError oome (println "Warning: OOM error in eval: " oome))))


(defn ^IExpr eval-phenotype-on-expr-args
  [{^IAST expr :expr ^ISymbol x-sym :sym ^ExprEvaluator util :util p-id :id :as pheno}
   ^"[Lorg.matheclipse.core.interfaces.IExpr;" expr-args]
  (try
    (when-not util (println "*** Warning: No util provided to evaluation engine! ***"))
    (if (and expr expr-args)
      (let [^IAST ast  (F/ast expr-args (ops-common/expr->fn pheno))
            ^IExpr res (.eval (or util (ops-common/new-util)) ast)]
        res)
      (println "Warning: eval needs both expr and args"))
    (catch NullPointerException npe (println "Warning: NPE error in eval: " (str expr) " : " npe))
    (catch ArgumentTypeException se (println "Warning: argument type error in eval: " (str expr) " : " se))
    (catch SyntaxError se (println "Warning: syntax error in eval: " (str expr) " : " se))
    (catch MathException me (println "Warning: math error in eval: " (str expr) " : " me))
    (catch StackOverflowError soe (println "Warning: stack overflow error in eval: " (str expr) " : " soe))
    (catch OutOfMemoryError oome (println "Warning: OOM error in eval: " (str expr) " : " oome))))


(defn eval-vec-pheno
  [p
   {:keys [input-exprs-list input-exprs-count output-exprs-vec]
    :as   run-args}]
  (let [^IExpr new-expr (:expr p)
        ^IExpr eval-p   (eval-phenotype-on-expr-args p input-exprs-list)]
    (if (or (nil? eval-p) (= "Indeterminate" (str eval-p)))
      nil
      (let [vs (mapv
                 (fn [i]
                   (try
                     (ops-common/expr->double
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
                         (ops-common/expr->double
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

        x-tail-list          (ops-common/exprs->input-exprs-list (ops-common/doubles->exprs x-tail))
        x-head-list          (ops-common/exprs->input-exprs-list (ops-common/doubles->exprs x-head))
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
  (let [middle-section (eval-vec-pheno p run-args)
        max-y          (reduce max middle-section)
        min-y          (reduce min middle-section)]
    (concat

      (mapv #(clamp-oversampled-ys max-y min-y %)
            (eval-vec-pheno p (assoc run-args :input-exprs-list x-head-list :input-exprs-count (count x-head))))

      middle-section

      (mapv #(clamp-oversampled-ys max-y min-y %)
            (eval-vec-pheno p (assoc run-args :input-exprs-list x-tail-list :input-exprs-count (count x-tail)))))))


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
