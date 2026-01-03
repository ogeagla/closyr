(ns closyr.ops
  (:refer-clojure :exclude [rand rand-int rand-nth shuffle])
  (:require
    [clojure.core.async :as async :refer [go go-loop timeout <!! >!! <! >! chan put! take! alts!! alts! close!]]
    [clojure.string :as str]
    [closyr.adaptive :as adaptive]
    [closyr.ops.common :as ops-common]
    [closyr.ops.eval :as ops-eval]
    [closyr.ops.modify :as ops-modify]
    [closyr.util.log :as log]
    [closyr.util.prng :refer [rand rand-int rand-nth shuffle]]
    [closyr.util.spec :as specs])
  (:import
    (java.text
      DecimalFormat)
    (java.util
      Date)
    (org.matheclipse.core.interfaces
      IExpr)))


(set! *warn-on-reflection* true)


(def min-score
  "The lowest score a function can have.  Used when errors occur."
  -100000000)


(def default-max-leafs
  "Default max number of AST tree leafs in candidate pheno function"
  40)


(def max-resid
  "Default residual value to use when otherwise it would be invalid or infinite"
  1000000)


(def ^:private sim-stats* (atom {}))


(def ^:dynamic *log-steps*
  "How many iterations between logging and sending info to GUI"
  1)


(def test-timer*
  "Timer to use during GA evolution"
  (atom nil))


(def ^:private ^DecimalFormat score-format (DecimalFormat. "###.#####"))


(defn format-fn-str
  "Remove newlines from fn expr string"
  [fn-str]
  (str/replace (str/trim-newline (str fn-str)) #"\n|\r" ""))


(defn- sum
  [coll]
  (reduce + 0.0 coll))


(defn- tally-min-score
  [min-score]
  (swap! sim-stats* update-in [:scoring :min-scores] #(inc (or % 0)))
  min-score)


(defn- not-finite?
  [n]
  (or (not (number? n))
      (and (number? n) (Double/isNaN n))))


(defn compute-residual
  "Compute the residual (difference) between 2 number (y-values)"
  {:malli/schema [:=> [:cat number? number?] number?]}
  [expected actual]
  (let [res (if (not-finite? actual)
              max-resid
              (- expected actual))]
    (if (not-finite? res)
      (do
        (log/warn "Warning, residual is not a number: " res
                  " exp: " expected " actual: " actual)
        max-resid)
      (min max-resid (abs res)))))


(defn- compute-residuals-fast
  "Compute sum and max of residuals using primitive arrays.
   Returns [sum-residuals max-residual] or nil if f-of-xs is invalid.
   ~50x faster than map/reduce approach."
  ^doubles [^doubles ys-arr f-of-xs]
  (when (and f-of-xs (seq f-of-xs))
    (let [n (count f-of-xs)
          max-r (double max-resid)]
      (loop [i (int 0), sum (double 0.0), mx (double 0.0)]
        (if (< i n)
          (let [expected (aget ys-arr i)
                actual (double (nth f-of-xs i))
                resid (if (or (Double/isNaN actual) (Double/isInfinite actual))
                        max-r
                        (Math/abs (- expected actual)))
                resid (Math/min max-r resid)]
            (recur (unchecked-inc-int i)
                   (+ sum resid)
                   (Math/max mx resid)))
          (double-array [sum mx (double n)]))))))


(defn- length-deduction
  "A tiny score deduction based on the number of leafs, proportional to score.  Intentionally tiny to just break ties
  for otherwise same scores to break the tie and favor smaller functions."
  [score leafs]
  (* (abs score) (min 0.1 (* 0.0000001 leafs leafs))))


(defn compute-score-from-actuals-and-expecteds
  "Compute overall score for fn given some actual and expected ys.
   Uses fast primitive array computation when ys-arr is provided."
  {:malli/schema [:function
                  [:=> [:cat #'specs/GAPhenotype #'specs/NumberVector #'specs/NumberVector number?] number?]
                  [:=> [:cat #'specs/GAPhenotype #'specs/NumberVector #'specs/NumberVector number? [:maybe some?]] number?]]}
  ([pheno f-of-xs input-ys-vec leafs]
   ;; Legacy path - convert to array
   (compute-score-from-actuals-and-expecteds pheno f-of-xs input-ys-vec leafs nil))
  ([pheno f-of-xs input-ys-vec leafs ^doubles input-ys-arr]
   (try
     (let [[resid-sum max-resid-val n]
           (if input-ys-arr
             ;; Fast path with primitive array
             (let [^doubles result (compute-residuals-fast input-ys-arr f-of-xs)]
               (when result
                 [(aget result 0) (aget result 1) (aget result 2)]))
             ;; Fallback to original implementation
             (let [abs-resids (map compute-residual input-ys-vec f-of-xs)]
               [(sum abs-resids) (reduce max abs-resids) (count abs-resids)]))]
       (if resid-sum
         (let [score            (* -1.0 (+ (* 2.0 (/ resid-sum n))
                                           max-resid-val))
               length-deduction (length-deduction score leafs)
               overall-score    (- score length-deduction)]
           (swap! sim-stats* update-in [:scoring :len-deductions] #(into (or % []) [length-deduction]))
           overall-score)
         (tally-min-score min-score)))
     (catch Exception e
       (log/error "Err in computing score from residuals: "
                  (.getMessage e) ", fn: " (str (:expr pheno)) ", from: " (:expr pheno))
       (tally-min-score min-score)))))


(defn score-fn
  "Symbolic regression scoring.
   Uses primitive array for fast residual computation when input-ys-arr is available."
  {:malli/schema [:=> [:cat #'specs/ScoreFnArgs [:map {:closed false} [:max-leafs number?]] #'specs/GAPhenotype] number?]}
  [{:keys [input-xs-list input-xs-count input-ys-vec input-ys-arr]
    :as   run-args}
   {:keys [max-leafs]}
   pheno]
  (try
    (let [expr-str (str (:expr pheno))]
      ;; Skip Hold() expressions - they can't be numerically evaluated
      (if (str/starts-with? expr-str "Hold(")
        (tally-min-score min-score)
        (let [leafs (.leafCount ^IExpr (:expr pheno))]
          (if (> leafs max-leafs)
            (tally-min-score min-score)
            (let [f-of-xs (ops-eval/eval-vec-pheno pheno run-args)]
              (if f-of-xs
                (compute-score-from-actuals-and-expecteds pheno f-of-xs input-ys-vec leafs input-ys-arr)
                (tally-min-score min-score)))))))
    (catch Exception e
      (log/debug "Err in score fn: " (.getMessage e) ", fn: " (str (:expr pheno)) ", from: " (:expr pheno))
      (tally-min-score min-score))))


(def ^:dynamic *long-running-mutation-thresh-ms*
  "If a mutation takes longer than this in ms, log info about it"
  5000)


(defn mutation-fn
  "Symbolic regression mutation.
   Uses adaptive mutation count when adaptive mode is enabled."
  {:malli/schema
   [:=>

    [:cat [:map {:closed false} [:max-leafs number?]] [:sequential #'specs/GAMutation] #'specs/GAPhenotype #'specs/GAPhenotype]

    #'specs/GAPhenotype]}

  [{:keys [max-leafs]}
   initial-muts
   p-winner
   p-discard]
  (try
    (let [start   (Date.)
          {:keys [new-pheno iters mods]} (ops-modify/apply-modifications
                                           max-leafs (ops-modify/sample-mutation-count) initial-muts p-winner p-discard)
          diff-ms (ops-common/start-date->diff-ms start)]

      (when (> diff-ms *long-running-mutation-thresh-ms*)
        (log/warn "Warning, this modification sequence took a long time: "
                  diff-ms " ms for mods: " (count mods)
                  "\n for old expr: " (:expr p-winner)
                  "\n and new expr: " (:expr new-pheno)
                  "\n mods: " mods))

      (swap! sim-stats* update-in [:mutations :counts iters] #(inc (or % 0)))
      (swap! sim-stats* update-in [:mutations :size-in] #(into (or % []) [(.leafCount ^IExpr (:expr p-winner))]))
      (swap! sim-stats* update-in [:mutations :size-out] #(into (or % []) [(.leafCount ^IExpr (:expr new-pheno))]))

      (assoc new-pheno :mods-applied iters))
    (catch Exception e
      (log/error "Err in mutation: " (or (.getMessage e) e))
      (assoc p-winner :util (:util p-discard)))))


(defn crossover-fn
  "Symbolic regression crossover"
  [{:keys [max-leafs]
    :as   run-config}
   initial-muts
   p
   p-discard]
  (let [crossover-result (ops-modify/crossover max-leafs p p-discard)]
    (when crossover-result
      (swap! sim-stats* update-in [:crossovers :counts] #(inc (or % 0))))
    (or
      crossover-result
      (mutation-fn run-config initial-muts p p-discard))))


(defn- sort-population
  [pops]
  (->>
    (:pop pops)
    (remove #(nil? (:score %)))
    (sort-by :score)
    (reverse)))


(defn- reportable-phen-str
  [{:keys [^IExpr expr score last-op mods-applied] p-id :id :as p}]
  (if (and expr score)
    (str
      " id: " (str/join (take 3 (str p-id)))
      " last mod #: " (or mods-applied "-")
      " last op: " (format "%18s" (str last-op))
      " score: " (.format score-format (double score))
      " leafs: " (.leafCount expr)
      ;; strip newlines from here also:
      " fn: " (format-fn-str expr))
    " [invalid phenotype]"))


(defn- summarize-sim-stats
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
                      :len-ded-mean   (when (seq len-deductions)
                                        (/ (sum len-deductions) (count len-deductions)))
                      :len-ded-min    (first len-deductions-sorted)
                      :len-ded-max    (last len-deductions-sorted)
                      :len-ded-med    (when (seq len-deductions)
                                        (nth len-deductions-sorted
                                             (/ (count len-deductions-sorted) 2)))})
              (assoc :mutations
                     {:counts              (reverse (sort-by second cs))
                      :sz-in-mean-max-min  [(when (seq sz-in)
                                              (Math/round ^double (/ (sum sz-in) (count sz-in))))
                                            (last sz-in-sorted)
                                            (first sz-in-sorted)]

                      :sz-out-mean-max-min [(when (seq sz-out)
                                              (Math/round ^double (/ (sum sz-out) (count sz-out))))
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
      (log/error "Error summarizing stats: " e)
      (str "Error: " (.getMessage e)))))


(def ^:dynamic ^:private *print-top-n* 20)


(defn report-iteration
  "Print and maybe send to GUI a summary report of the population, including best fn/score/etc.
   Also calls progress-callback if provided in run-config.
   Updates adaptive mutation state based on population metrics."
  [iters-to-go
   iters
   ga-result
   {:keys [input-xs-list input-xs-count input-ys-vec
           sim-stop-start-chan sim->gui-chan extended-domain-args]
    :as   run-args}
   {:keys [use-gui? max-leafs progress-callback] :as run-config}]
  (when (or (= 1 iters-to-go) (zero? (mod iters-to-go *log-steps*)))
    (let [bests      (sort-population ga-result)
          ;; Update adaptive state with current population scores
          sorted-scores (mapv :score bests)
          _          (adaptive/update-adaptive-state! sorted-scores)
          timer      @test-timer*
          took-s     (if timer
                       (/ (ops-common/start-date->diff-ms timer) 1000.0)
                       0.0)
          pop-size   (count (:pop ga-result))
          best-v     (first bests)
          n-bests    (count bests)
          best-p99-v (when (pos? n-bests) (nth bests (min (dec n-bests) (int (* 0.01 n-bests)))))
          best-p95-v (when (pos? n-bests) (nth bests (min (dec n-bests) (int (* 0.05 n-bests)))))
          best-p90-v (when (pos? n-bests) (nth bests (min (dec n-bests) (int (* 0.1 n-bests)))))
          evaled     (ops-eval/eval-vec-pheno best-v run-args)
          {evaled-extended :ys xs-extended :xs} (ops-eval/eval-vec-pheno-oversample
                                                  best-v run-args extended-domain-args)
          current-iteration (inc (- iters iters-to-go))]

      (reset! test-timer* (Date.))

      ;; Log iteration details unless quiet-logs is set (e.g., when running from web API)
      (when-not (:quiet-logs run-config)
        (log/info current-iteration "-th-iter, "
                  " iters left: " (dec iters-to-go)
                  " pop size: " pop-size
                  " points: " (count input-ys-vec)
                  " max leafs: " max-leafs
                  " took secs: " took-s
                  " phenos/s: " (if (pos? took-s)
                                  (Math/round ^double (/ (* pop-size *log-steps*) took-s))
                                  0)
                  (str "\n top " *print-top-n* " best:\n"
                       (->> (take *print-top-n* bests)
                            (map reportable-phen-str)
                            (str/join "\n")))
                  "\n"
                  (summarize-sim-stats)
                  "\n  "
                  (adaptive/format-adaptive-status)))

      (when use-gui?
        (put! sim->gui-chan {:iters                 iters
                             :i                     current-iteration
                             :best-eval             evaled
                             :input-xs-vec-extended xs-extended
                             :best-eval-extended    evaled-extended
                             :best-f-str            (str (:expr best-v))
                             :best-score            (or (:score best-v) min-score)
                             :best-p99-score        (or (:score best-p99-v) min-score)
                             :best-p95-score        (or (:score best-p95-v) min-score)
                             :best-p90-score        (or (:score best-p90-v) min-score)}))

      ;; Call progress callback if provided (for HTTP API/SSE)
      (when (and progress-callback best-v)
        (try
          (progress-callback {:iteration        current-iteration
                              :total-iterations iters
                              :best-formula     (str (:expr best-v))
                              :best-score       (or (:score best-v) min-score)
                              :percentiles      {:p99 (or (:score best-p99-v) min-score)
                                                 :p95 (or (:score best-p95-v) min-score)
                                                 :p90 (or (:score best-p90-v) min-score)}})
          (catch Exception e
            ;; Re-throw stop exceptions so the solver actually stops
            (if (= :stopped (:type (ex-data e)))
              (throw e)
              (log/warn "Error in progress callback: " (.getMessage e))))))))
  (reset! sim-stats* {}))


(specs/instrument-all!)
