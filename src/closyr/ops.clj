(ns closyr.ops
  (:refer-clojure :exclude [rand rand-int rand-nth shuffle])
  (:require
    [clojure.core.async :as async :refer [go go-loop timeout <!! >!! <! >! chan put! take! alts!! alts! close!]]
    [clojure.string :as str]
    [closyr.dataset.prng :refer :all]
    [closyr.ops.common :as ops-common]
    [closyr.ops.eval :as ops-eval]
    [closyr.ops.modify :as ops-modify])
  (:import
    (java.text
      DecimalFormat)
    (java.util
      Date)
    (org.matheclipse.core.interfaces
      IExpr)))


(set! *warn-on-reflection* true)


(def min-score -100000000)
(def max-leafs 50)
(def max-resid 1000000)


(def sim-stats* (atom {}))


(def ^:dynamic *log-steps* 1)
(def test-timer* (atom nil))


(def ^DecimalFormat score-format (DecimalFormat. "###.#####"))


(defn format-fn-str
  [fn-str]
  (str/replace (str/trim-newline (str fn-str)) #"\n|\r" ""))


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


(defn compute-residual
  [expected actual]
  (let [res (if (not-finite? actual)
              max-resid
              (- expected actual))]
    (if (not-finite? res)
      (do
        (println "warning, res not a number: " res
                 " exp: " expected " actual: " actual)
        max-resid)
      (min max-resid (abs res)))))


(defn compute-score-from-actuals-and-expecteds
  [pheno f-of-xs input-ys-vec leafs]
  (try
    (let [abs-resids       (map compute-residual input-ys-vec f-of-xs)
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
    (catch Exception e
      (println "Err in computing score from residuals: "
               (.getMessage e) ", fn: " (str (:expr pheno)) ", from: " (:expr pheno))
      (tally-min-score min-score))))


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
            (compute-score-from-actuals-and-expecteds pheno f-of-xs input-ys-vec leafs)
            (tally-min-score min-score)))))
    (catch Exception e
      (println "Err in score fn: " (.getMessage e) ", fn: " (str (:expr pheno)) ", from: " (:expr pheno))
      (tally-min-score min-score))))


(defn mutation-fn
  [initial-muts p-winner p-discard]
  (try
    (let [start   (Date.)
          [new-pheno iters mods] (ops-modify/apply-modifications
                                   max-leafs (rand-nth ops-modify/mutations-sampler) initial-muts p-winner p-discard)
          diff-ms (ops-common/start-date->diff-ms start)]

      (when (> diff-ms 5000)
        (println "Warning, this modification sequence took a long time: "
                 diff-ms " ms for mods: " (count mods)
                 "\n for old expr: " (:expr p-winner)
                 "\n and new expr: " (:expr new-pheno)
                 "\n mods: " mods))

      (swap! sim-stats* update-in [:mutations :counts iters] #(inc (or % 0)))
      (swap! sim-stats* update-in [:mutations :size-in] #(into (or % []) [(.leafCount ^IExpr (:expr p-winner))]))
      (swap! sim-stats* update-in [:mutations :size-out] #(into (or % []) [(.leafCount ^IExpr (:expr new-pheno))]))

      (assoc new-pheno :mods-applied iters))
    (catch Exception e
      (println "Err in mutation: " e))))


(defn crossover-fn
  [initial-muts p p-discard]
  (let [crossover-result (ops-modify/crossover p p-discard)]
    (when crossover-result
      (swap! sim-stats* update-in [:crossovers :counts] #(inc (or % 0))))
    (or
      crossover-result
      (mutation-fn initial-muts p p-discard))))


(defn sort-population
  [pops]
  (->>
    (:pop pops)
    (remove #(nil? (:score %)))
    (sort-by :score)
    (reverse)))


(defn reportable-phen-str
  [{:keys [^IExpr expr ^double score last-op mods-applied] p-id :id :as p}]
  (str
    " id: " (str/join (take 3 (str p-id)))
    " last mod #: " (or mods-applied "-")
    " last op: " (format "%17s" (str last-op)) #_(str/join (take 8 (str last-op)))
    " score: " (.format score-format score)
    " leafs: " (.leafCount expr)
    ;; strip newlines from here also:
    " fn: " (format-fn-str expr)))


(defn summarize-sim-stats
  []
  (try
    (let [{{xcs :counts}                :crossovers
           {cs     :counts
            sz-in  :size-in
            sz-out :size-out}           :mutations
           {len-deductions :len-deductions
            min-scores     :min-scores} :scoring
           :as                          dat} @sim-stats*

          len-deductions-sorted
          (sort len-deductions)

          sz-in-sorted
          (sort sz-in)

          sz-out-sorted
          (sort sz-out)
          summary-data
          (-> dat
              (assoc :crossovers
                     {:crossovers-count xcs})
              (assoc :scoring
                     {:len-deductions (count len-deductions)
                      :len-ded-mean   (/ (sum len-deductions) (count len-deductions))
                      :len-ded-min    (first len-deductions-sorted)
                      :len-ded-max    (last len-deductions-sorted)
                      :len-ded-med    (nth len-deductions-sorted
                                           (/ (count len-deductions-sorted) 2))})
              (assoc :mutations
                     {:counts              (reverse (sort-by second cs))
                      :sz-in-mean-max-min  [(Math/round ^double (/ (sum sz-in) (count sz-in)))
                                            (last sz-in-sorted)
                                            (first sz-in-sorted)]

                      :sz-out-mean-max-min [(Math/round ^double (/ (sum sz-out) (count sz-out)))
                                            (last sz-out-sorted)
                                            (first sz-out-sorted)]}))]
      (str "muts:" (count sz-in) " min scores: " min-scores
           " "
           (:scoring summary-data)
           "\n  "
           (:mutations summary-data)
           "\n  "
           (:crossovers summary-data)))
    (catch Exception e
      (println "Error summarizing stats: " e)
      (str "Error: " (.getMessage e)))))


(defn report-iteration
  [i
   iters
   ga-result
   {:keys [input-xs-list input-xs-count input-ys-vec
           sim-stop-start-chan sim->gui-chan extended-domain-args]
    :as   run-args}
   {:keys [use-gui?] :as run-config}]
  (when (or (= 1 i) (zero? (mod i *log-steps*)))
    (let [bests      (sort-population ga-result)
          took-s     (/ (ops-common/start-date->diff-ms @test-timer*) 1000.0)
          pop-size   (count (:pop ga-result))
          best-v     (first bests)
          best-95p-v (nth bests (* 0.05 (count bests)))
          best-50p-v (nth bests (* 0.5 (count bests)))
          best-05p-v (nth bests (* 0.95 (count bests)))
          evaled     (ops-eval/eval-vec-pheno best-v run-args)
          {evaled-extended :ys xs-extended :xs} (ops-eval/eval-vec-pheno-oversample
                                                  best-v run-args extended-domain-args)]

      (reset! test-timer* (Date.))
      (println i "-step pop size: " pop-size
               " took secs: " took-s
               " phenos/s: " (Math/round ^double (/ (* pop-size *log-steps*) took-s))
               (str "\n top 20 best:\n"
                    (->> (take 20 bests)
                         (map reportable-phen-str)
                         (str/join "\n")))
               "\n"
               (summarize-sim-stats))

      (when use-gui?
        (put! sim->gui-chan {:iters                 iters
                             :i                     (inc (- iters i))
                             :best-eval             evaled
                             :input-xs-vec-extended xs-extended
                             :best-eval-extended    evaled-extended
                             :best-f-str            (str (:expr best-v))
                             :best-score            (:score best-v)
                             :best-95p-v            best-95p-v
                             :best-50p-v            best-50p-v
                             :best-05p-v            best-05p-v}))))
  (reset! sim-stats* {}))
