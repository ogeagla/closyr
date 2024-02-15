(ns closyr.ops.modify
  (:refer-clojure :exclude [rand rand-int rand-nth shuffle])
  (:require
    [clojure.core.async :as async :refer [go go-loop timeout <!! >!! <! >! chan put! take! alts!! alt!! close!]]
    [clojure.string :as str]
    [closyr.dataset.prng :refer :all]
    [closyr.log :as log]
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


(defn- ^Function tree-leaf-modifier
  [modifier]
  (ops-common/as-function
    (fn ^IExpr [^IExpr ie]
      (if (instance? IAST ie)
        (.map ^IAST ie (tree-leaf-modifier modifier))
        (modifier ie)))))


(defn- ^Function tree-branch-modifier
  [modifier]
  (ops-common/as-function
    (fn ^IExpr [^IExpr ie]
      (if (instance? IAST ie)
        (let [^IAST ie ie]
          (modifier
            (F/ast (ops-common/->iexprs (.map ie (tree-branch-modifier modifier))) (.head ie))))
        ie))))


(defn- ^Function tree-ast-head-modifier
  [modifier]
  (ops-common/as-function
    (fn ^IExpr [^IExpr ie]
      (if (instance? IAST ie)
        (let [^IAST ie ie]
          (F/ast (ops-common/->iexprs (.map ie (tree-ast-head-modifier modifier))) ^IExpr (modifier (.head ie))))
        ie))))


(defn- op-short-str_unmemo
  [{:keys [op label] :as modif}]
  (let [op-str    (name op)
        ops-short (->> (str/split op-str #"-")
                       (rest)
                       (map #(str/join (take 4 %)))
                       (str/join "-"))]
    (str ops-short ":" label)))


(def ^:private op-short-str (memoize op-short-str_unmemo))


(defn- with-recent-mod-metadata
  [p {:keys [op label] :as modif}]
  (assoc p :last-op (op-short-str (select-keys modif [:op :label]))))


(defmulti modify
  "Modify a phenotype using provided modification/mutation"
  (fn [{:keys [op]} pheno] op))


(defmethod modify :modify-substitute
  [{:keys [label ^IExpr find-expr ^IExpr replace-expr] :as modif}
   {^IAST expr :expr ^ISymbol x-sym :sym ^ExprEvaluator util :util :as pheno}]
  (-> x-sym
      (ops-common/->phenotype (.subs expr find-expr replace-expr) util)
      (with-recent-mod-metadata modif)))


(defmethod modify :modify-leafs
  [{:keys [label leaf-modifier-fn] :as modif}
   {^IAST expr :expr ^ISymbol x-sym :sym ^ExprEvaluator util :util :as pheno}]
  (->
    x-sym
    (ops-common/->phenotype
      (.replaceAll expr (tree-leaf-modifier (partial leaf-modifier-fn (.leafCount expr) pheno))) util)
    (with-recent-mod-metadata modif)))


(defmethod modify :modify-branches
  [{:keys [label leaf-modifier-fn] :as modif}
   {^IAST expr :expr ^ISymbol x-sym :sym ^ExprEvaluator util :util :as pheno}]
  (->
    x-sym
    (ops-common/->phenotype
      (.replaceAll expr (tree-branch-modifier (partial leaf-modifier-fn (.leafCount expr) pheno))) util)
    (with-recent-mod-metadata modif)))


(defmethod modify :modify-ast-head
  [{:keys [label leaf-modifier-fn] :as modif}
   {^IAST expr :expr ^ISymbol x-sym :sym ^ExprEvaluator util :util :as pheno}]
  (->
    x-sym
    (ops-common/->phenotype
      (.replaceAll expr (tree-ast-head-modifier (partial leaf-modifier-fn (.leafCount expr) pheno))) util)
    (with-recent-mod-metadata modif)))


(defmethod modify :modify-fn
  [{:keys [label modifier-fn] :as modif}
   {^IAST expr :expr ^ISymbol x-sym :sym ^ExprEvaluator util :util :as pheno}]
  (-> x-sym
      (ops-common/->phenotype (modifier-fn pheno) util)
      (with-recent-mod-metadata modif)))


(defn- is-expr-function?
  [^IExpr ie]
  (instance? IAST ie))


(def ^:private crossover-sampler
  [:plus :times :divide12 :divide21 :minus12 :minus21])


(defn crossover
  "Do phenotype crossover on their expr AST"
  [{^IAST expr :expr ^ISymbol x-sym :sym ^ExprEvaluator util :util :as p} p-discard]
  (try
    (let [^IExpr e1        (:expr p)
          ^IExpr e2        (:expr p-discard)

          ^IExpr e1-part   (if (is-expr-function? e1)
                             (.getArg e1 (inc (rand-int (dec (.size e1)))) nil)
                             e1)

          ^IExpr e2-part   (if (is-expr-function? e2)
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

      (-> x-sym
          (ops-common/->phenotype new-expr (:util p-discard))
          (with-recent-mod-metadata {:label (name crossover-flavor)
                                     :op    :modify-crossover})))
    (catch Exception e
      (log/error "Error in ops/crossover: " (.getMessage e))
      nil)))


(defn- divided-by-zero
  "If we want to measure how many times we do this during modification. For now, just a placeholder for testing."
  [])


(defn apply-modifications
  "Apply a sequence of modifications"
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
      (let [pheno           (if first-run? (assoc pheno :util (:util p-discard)) pheno)
            mod-to-apply    (rand-nth initial-muts)
            new-pheno       (try
                              (modify mod-to-apply pheno)
                              (catch Exception e
                                (if (= "Infinite expression 1/0 encountered." (.getMessage e))
                                  (divided-by-zero)
                                  (log/warn "Warning, mutation failed: " (:label mod-to-apply)
                                            " on: " (type (:expr pheno)) " / " (str (:expr pheno))
                                            " due to: " (or (.getMessage e) e)))
                                pheno))

            ^IExpr new-expr (:expr new-pheno)
            new-leafs       (some-> new-expr (.leafCount))

            ;; stop modification loop if too big or something went wrong:
            count-to-go     (if (or (nil? new-leafs)
                                    (nil? new-expr)
                                    (> new-leafs max-leafs))
                              0
                              (dec c))]
        (recur
          (inc iters)
          count-to-go
          (if (and new-leafs new-expr) new-pheno pheno)
          false
          (into mods [(select-keys mod-to-apply [:label :op])]))))))


(def mutations-sampler
  "A coll whose random element is the number of modifications to apply in succession"
  (->>
    []
    (concat (repeat 50 1))
    (concat (repeat 30 2))
    (concat (repeat 20 3))
    (concat (repeat 15 4))
    (concat (repeat 9 5))
    (concat (repeat 8 6))
    (concat (repeat 7 7))
    (concat (repeat 6 8))
    (concat (repeat 5 9))
    (concat (repeat 4 10))
    (concat (repeat 3 11))
    (concat (repeat 2 12))
    (concat (repeat 2 13))
    (concat (repeat 1 14))
    (concat (repeat 1 15))
    ;; (concat (repeat 1 16))
    ;; (concat (repeat 2 17))
    ;; (concat (repeat 1 18))
    ;; (concat (repeat 1 19))
    ;; (concat (repeat 1 20))

    vec)
  #_[1 1 1 1 1 1 1 1 1
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
