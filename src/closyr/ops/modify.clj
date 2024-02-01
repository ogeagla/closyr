(ns closyr.ops.modify
  (:refer-clojure :exclude [rand rand-int rand-nth shuffle])
  (:require
    [clojure.core.async :as async :refer [go go-loop timeout <!! >!! <! >! chan put! take! alts!! alt!! close!]]
    [clojure.string :as str]
    [closyr.dataset.prng :refer :all]
    [closyr.ops.common :as ops-common])
  (:import
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


(defn ^Function tree-leaf-modifier
  [modifier]
  (ops-common/as-function (fn ^IExpr [^IExpr ie]
                            (if (instance? IAST ie)
                              (.map ^IAST ie (tree-leaf-modifier modifier))
                              (modifier ie)))))


(defn ^Function tree-branch-modifier
  [modifier]
  (ops-common/as-function (fn ^IExpr [^IExpr ie]
                            (if (instance? IAST ie)
                              (let [^IAST ie ie]
                                (modifier
                                  (F/ast (ops-common/->iexprs (.map ie (tree-branch-modifier modifier))) (.head ie))))
                              ie))))


(defn ^Function tree-ast-head-modifier
  [modifier]
  (ops-common/as-function (fn ^IExpr [^IExpr ie]
                            (if (instance? IAST ie)
                              (let [^IAST ie ie]
                                (F/ast (ops-common/->iexprs (.map ie (tree-ast-head-modifier modifier))) ^IExpr (modifier (.head ie))))
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


(defn with-recent-mod-metadata
  [p {:keys [op label] :as modif}]
  (assoc p :last-op (op-short-str (select-keys modif [:op :label]))))


(defmulti modify (fn [{:keys [op]} pheno] op))


(defmethod modify :modify-substitute
  [{:keys [label ^IExpr find-expr ^IExpr replace-expr] :as modif}
   {^IAST expr :expr ^ISymbol x-sym :sym ^ExprEvaluator util :util :as pheno}]
  (-> (ops-common/->phenotype x-sym (.subs expr find-expr replace-expr) util)
      (with-recent-mod-metadata modif)))


(defmethod modify :modify-leafs
  [{:keys [label leaf-modifier-fn] :as modif}
   {^IAST expr :expr ^ISymbol x-sym :sym ^ExprEvaluator util :util :as pheno}]
  (->
    x-sym
    (ops-common/->phenotype (.replaceAll expr (tree-leaf-modifier (partial leaf-modifier-fn (.leafCount expr) pheno))) util)
    (with-recent-mod-metadata modif)))


(defmethod modify :modify-branches
  [{:keys [label leaf-modifier-fn] :as modif}
   {^IAST expr :expr ^ISymbol x-sym :sym ^ExprEvaluator util :util :as pheno}]
  (->
    x-sym
    (ops-common/->phenotype (.replaceAll expr (tree-branch-modifier (partial leaf-modifier-fn (.leafCount expr) pheno))) util)
    (with-recent-mod-metadata modif)))


(defmethod modify :modify-ast-head
  [{:keys [label leaf-modifier-fn] :as modif}
   {^IAST expr :expr ^ISymbol x-sym :sym ^ExprEvaluator util :util :as pheno}]
  (->
    x-sym
    (ops-common/->phenotype (.replaceAll expr (tree-ast-head-modifier (partial leaf-modifier-fn (.leafCount expr) pheno))) util)
    (with-recent-mod-metadata modif)))


(defmethod modify :modify-fn
  [{:keys [label modifier-fn] :as modif}
   {^IAST expr :expr ^ISymbol x-sym :sym ^ExprEvaluator util :util :as pheno}]
  (-> (ops-common/->phenotype x-sym (modifier-fn pheno) util)
      (with-recent-mod-metadata modif)))


(defn is-expr-function?
  [^IExpr ie]
  (instance? IAST ie))


(def crossover-sampler
  [:plus :times :divide12 :divide21 :minus12 :minus21
   ;; :exp12 :exp21
   ])


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
                             :minus12 (F/Subtract e1-part e2-part)
                             :minus21 (F/Subtract e2-part e1-part)
                             :divide12 (F/Divide e1-part e2-part)
                             :divide21 (F/Divide e2-part e1-part)
                             :plus (F/Plus e1-part e2-part)
                             :times (F/Times e1-part e2-part)
                             :exp12 (F/Power e1-part e2-part)
                             :exp21 (F/Power e2-part e1-part))]

      (-> (ops-common/->phenotype x-sym new-expr (:util p-discard))
          (with-recent-mod-metadata {:label (name crossover-flavor)
                                     :op    :modify-crossover})))
    (catch Exception e
      (println "Error in ops/crossover: " (.getMessage e))
      nil)))


(defn apply-modifications
  [max-leafs mods-count initial-muts p-winner p-discard]
  (loop [iters      0
         c          mods-count
         pheno      p-winner
         first-run? true
         mods       []]
    (if (zero? c)
      [pheno #_(maybe-simplify pheno)
       iters
       mods]
      (let [pheno        (if first-run? (assoc pheno :util (:util p-discard)) pheno)
            mod-to-apply (rand-nth initial-muts)
            new-pheno    (try
                           (modify mod-to-apply pheno)
                           (catch Exception e
                             (when-not (= "Infinite expression 1/0 encountered." (.getMessage e))
                               (println "Warning, mutation failed: " (:label mod-to-apply)
                                        " on: " (str (:expr pheno))
                                        " due to: " e))
                             pheno))

            new-leafs    (some->
                           ^IExpr (:expr new-pheno)
                           (.leafCount))

            ;; stop modification loop if too big or something went wrong:
            count-to-go  (if (or (nil? new-leafs)
                                 (> new-leafs max-leafs))
                           0
                           (dec c))]
        (recur
          (inc iters)
          count-to-go
          (if new-leafs new-pheno pheno)
          false
          (into mods [(select-keys mod-to-apply [:label :op])]))))))


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
   17
   18
   19
   20
   ])
