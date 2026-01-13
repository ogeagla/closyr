(ns closyr.adaptive
  "Adaptive mutation rate control based on population metrics.

  Tracks population diversity, stagnation, and mutation effectiveness
  to dynamically adjust:
  - Mutation vs crossover probability
  - Number of mutations per individual
  - (Future) Mutation type weights"
  (:require
    [closyr.util.log :as log]))


(set! *warn-on-reflection* true)


;; =============================================================================
;; Adaptive State
;; =============================================================================

(def ^:private default-state
  "Default adaptive state - starts with balanced exploration/exploitation"
  {:mutation-probability   0.8      ; probability of mutation vs crossover
   :mutation-count-boost   1.0      ; multiplier for mutation count (1.0 = normal)
   :best-score-history     []       ; recent best scores for stagnation detection
   :diversity-history      []       ; recent diversity values
   :stagnation-counter     0        ; iterations without improvement
   :last-best-score        nil})    ; previous best score for comparison


(def ^:private adaptive-state*
  "Atom holding current adaptive state"
  (atom default-state))


(defn reset-adaptive-state!
  "Reset adaptive state to defaults. Call at start of new run."
  []
  (reset! adaptive-state* default-state))


(defn get-adaptive-state
  "Get current adaptive state"
  []
  @adaptive-state*)


;; =============================================================================
;; Configuration
;; =============================================================================

(def ^:private config
  {:history-window         40       ; number of iterations to track for trends
   :stagnation-threshold   20       ; iterations without improvement before boosting
   :min-mutation-prob      0.5      ; minimum mutation probability
   :max-mutation-prob      0.95     ; maximum mutation probability
   :min-mutation-boost     0.5      ; minimum mutation count multiplier
   :max-mutation-boost     2.0      ; maximum mutation count multiplier
   :diversity-low-thresh   0.1      ; diversity below this triggers exploration
   :diversity-high-thresh  0.5      ; diversity above this triggers exploitation
   :improvement-threshold  0.001})  ; minimum improvement to count as progress


;; =============================================================================
;; Diversity Calculation
;; =============================================================================

(defn calculate-diversity
  "Calculate population diversity as normalized score spread.
  Returns value between 0 (no diversity) and 1 (high diversity).

  Uses the gap between best and median scores, normalized by score magnitude."
  [sorted-scores]
  (when (seq sorted-scores)
    (let [n           (count sorted-scores)
          best-score  (double (first sorted-scores))
          median-idx  (quot n 2)
          median-score (double (nth sorted-scores median-idx))
          p90-idx     (min (dec n) (int (* 0.1 n)))
          p90-score   (double (nth sorted-scores p90-idx))
          ;; Normalize by score magnitude to get relative diversity
          score-range (Math/abs (- p90-score best-score))
          normalizer  (max 1.0 (Math/abs best-score))]
      (min 1.0 (/ score-range normalizer)))))


;; =============================================================================
;; Stagnation Detection
;; =============================================================================

(defn- detect-stagnation
  "Check if we're making progress or stagnating.
  Returns updated stagnation counter."
  [current-best last-best threshold]
  (if (nil? last-best)
    0
    (let [improvement (- current-best last-best)]
      (if (> improvement threshold)
        0  ; reset counter on improvement
        1)))) ; increment will happen in update


(defn- update-stagnation-counter
  "Update stagnation counter based on improvement"
  [{:keys [stagnation-counter last-best-score]} current-best]
  (let [threshold (:improvement-threshold config)]
    (if (nil? last-best-score)
      0
      (if (> (- current-best last-best-score) threshold)
        0
        (inc stagnation-counter)))))


;; =============================================================================
;; Adaptive Rate Calculation
;; =============================================================================

(defn- calculate-mutation-probability
  "Calculate mutation probability based on diversity and stagnation.

  Low diversity OR stagnation → increase mutation (exploration)
  High diversity AND progress → decrease mutation (exploitation)"
  [diversity stagnation-counter]
  (let [{:keys [min-mutation-prob max-mutation-prob
                diversity-low-thresh diversity-high-thresh
                stagnation-threshold]} config
        ;; Stagnation boost: increase mutation when stuck
        stagnation-boost (if (>= stagnation-counter stagnation-threshold)
                           0.15
                           0.0)
        ;; Diversity-based adjustment
        diversity-adj (cond
                        (< diversity diversity-low-thresh)
                        0.1  ; low diversity → more mutation

                        (> diversity diversity-high-thresh)
                        -0.1 ; high diversity → less mutation

                        :else 0.0)
        ;; Base probability with adjustments
        base-prob 0.8
        new-prob  (+ base-prob diversity-adj stagnation-boost)]
    (max min-mutation-prob (min max-mutation-prob new-prob))))


(defn- calculate-mutation-count-boost
  "Calculate mutation count multiplier based on stagnation.

  When stuck, apply more mutations per individual to explore more aggressively."
  [stagnation-counter diversity]
  (let [{:keys [min-mutation-boost max-mutation-boost
                stagnation-threshold diversity-low-thresh]} config
        ;; Strong boost when stagnating
        stagnation-boost (if (>= stagnation-counter stagnation-threshold)
                           (min 0.5 (* 0.1 (- stagnation-counter stagnation-threshold)))
                           0.0)
        ;; Slight boost for low diversity
        diversity-boost (if (< diversity diversity-low-thresh)
                          0.2
                          0.0)
        new-boost (+ 1.0 stagnation-boost diversity-boost)]
    (max min-mutation-boost (min max-mutation-boost new-boost))))


;; =============================================================================
;; State Update
;; =============================================================================

(defn update-adaptive-state!
  "Update adaptive state based on current population metrics.

  Call this once per iteration with the sorted population scores.
  Returns the updated state."
  [sorted-scores]
  (let [current-best (when (seq sorted-scores) (double (first sorted-scores)))
        diversity    (or (calculate-diversity sorted-scores) 0.5)]
    (swap! adaptive-state*
           (fn [{:keys [best-score-history diversity-history] :as state}]
             (let [new-stagnation (update-stagnation-counter state current-best)
                   new-mutation-prob (calculate-mutation-probability diversity new-stagnation)
                   new-mutation-boost (calculate-mutation-count-boost new-stagnation diversity)
                   ;; Keep bounded history
                   window (:history-window config)
                   new-best-history (take window (cons current-best best-score-history))
                   new-div-history (take window (cons diversity diversity-history))]
               (-> state
                   (assoc :mutation-probability new-mutation-prob)
                   (assoc :mutation-count-boost new-mutation-boost)
                   (assoc :stagnation-counter new-stagnation)
                   (assoc :last-best-score current-best)
                   (assoc :best-score-history (vec new-best-history))
                   (assoc :diversity-history (vec new-div-history))))))))


;; =============================================================================
;; Public API for GA/Ops
;; =============================================================================

(defn should-mutate?
  "Returns true if mutation should be used, false for crossover.
  Uses current adaptive mutation probability."
  []
  (< (rand) (:mutation-probability @adaptive-state*)))


(defn get-mutation-count-boost
  "Returns current mutation count multiplier (1.0 = normal)."
  []
  (:mutation-count-boost @adaptive-state*))


(defn get-mutation-probability
  "Returns current mutation probability (0.0-1.0)."
  []
  (:mutation-probability @adaptive-state*))


(defn format-adaptive-status
  "Format current adaptive state for logging."
  []
  (let [{:keys [mutation-probability mutation-count-boost
                stagnation-counter diversity-history]} @adaptive-state*
        recent-diversity (first diversity-history)]
    (format "Adaptive: mut=%.0f%% boost=%.1fx stag=%d div=%.2f"
            (* 100 mutation-probability)
            mutation-count-boost
            stagnation-counter
            (or recent-diversity 0.0))))
