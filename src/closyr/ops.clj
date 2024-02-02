(ns closyr.ops
  (:refer-clojure :exclude [rand rand-int rand-nth shuffle])
  (:require
    [clojure.core.async :as async :refer [go go-loop timeout <!! >!! <! >! chan put! take! alts!! alts! close!]]
    [closyr.dataset.prng :refer :all]
    [closyr.ops.common :as ops-common]
    [closyr.ops.eval :as ops-eval]
    [closyr.ops.modify :as ops-modify])
  (:import
    (java.util
      Date)
    (org.matheclipse.core.interfaces
      IExpr)))


(set! *warn-on-reflection* true)


(def min-score -100000000)
(def max-leafs 60)
(def max-resid 1000000)


(def sim-stats* (atom {}))


(defn sum
  [coll]
  (reduce + 0.0 coll))


(defn tally-min-score
  [min-score]
  (swap! sim-stats* update-in [:scoring :min-scores] #(inc (or % 0)))
  min-score)


(defn not-finite?
  [n]
  (or (not (number? n))
      (and (number? n) (Double/isNaN n))))


(defn score-fn
  [{:keys [input-xs-list input-xs-count input-ys-vec
           sim-stop-start-chan sim->gui-chan]
    :as   run-args}
   pheno]
  (try
    (let [leafs (.leafCount ^IExpr (:expr pheno))]
      (if (> leafs max-leafs)
        (tally-min-score min-score)
        (let [f-of-xs (ops-eval/eval-vec-pheno pheno run-args)]
          (if f-of-xs
            (let [abs-resids       (map
                                     (fn [expected actual]
                                       (let [res (if (not-finite? actual)
                                                   max-resid
                                                   (- expected actual))]
                                         (if (not-finite? res)
                                           (do
                                             (println "warning, res not a number: " res
                                                      " exp: " expected " actual: " actual)
                                             max-resid)
                                           (min max-resid (abs res)))))
                                     input-ys-vec
                                     f-of-xs)
                  resid-sum        (sum abs-resids)
                  score            (* -1.0 (+ (* 2.0 (/ resid-sum (count abs-resids)))
                                              (reduce max abs-resids)))
                  length-deduction (* (abs score) (min 0.1 (* 0.0000001 leafs leafs)))
                  overall-score    (- score length-deduction)]


              (when (or (neg? length-deduction) (Double/isNaN length-deduction))
                (println "warning: bad/negative deduction increases score: "
                         leafs length-deduction (str (:expr pheno))))
              (swap! sim-stats* update-in [:scoring :len-deductions] #(into (or % []) [length-deduction]))

              overall-score)
            (tally-min-score min-score)))))
    (catch Exception e
      (println "Err in score fn: " (.getMessage e) ", fn: " (str (:expr pheno)) ", from: " (:expr pheno))
      (tally-min-score min-score))))


(defn mutation-fn
  [initial-muts p-winner p-discard pop]
  (try
    (let [start     (Date.)
          c         (rand-nth ops-modify/mutations-sampler)
          [new-pheno iters mods] (ops-modify/apply-modifications max-leafs c initial-muts p-winner p-discard)
          diff-ms   (ops-common/start-date->diff-ms start)
          old-leafs (.leafCount ^IExpr (:expr p-winner))
          new-leafs (.leafCount ^IExpr (:expr new-pheno))]

      (when (> diff-ms 5000)
        (println "Warning, this modification sequence took a long time: "
                 diff-ms " ms for mods: " (count mods)
                 "\n mods: " mods
                 "\n for old expr: " (:expr p-winner)
                 "\n and new expr: " (:expr new-pheno)))

      (swap! sim-stats* update-in [:mutations :counts iters] #(inc (or % 0)))
      (swap! sim-stats* update-in [:mutations :size-in] #(into (or % []) [old-leafs]))
      (swap! sim-stats* update-in [:mutations :size-out] #(into (or % []) [new-leafs]))
      (assoc new-pheno :mods-applied iters))
    (catch Exception e
      (println "Err in mutation: " e))))


(defn crossover-fn
  [initial-muts p p-discard pop]
  (let [crossover-result (ops-modify/crossover p p-discard)]
    (when crossover-result
      (swap! sim-stats* update-in [:crossovers :counts] #(inc (or % 0))))
    (or
      crossover-result
      (mutation-fn initial-muts p p-discard pop))))
