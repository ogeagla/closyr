(ns closyr.ga
  (:refer-clojure :exclude [rand rand-int rand-nth shuffle])
  (:require
    [closyr.adaptive :as adaptive]
    [closyr.util.log :as log]
    [closyr.util.prng :refer [rand rand-int rand-nth shuffle shuffle-arraylist!]])
  (:import
    (java.util ArrayList List)))


(set! *warn-on-reflection* true)


(def ^:dynamic *deterministic-mode*
  "When true, uses sequential map instead of pmap for deterministic results.
   Set to true when using a seeded PRNG for reproducibility."
  false)


(def ^:dynamic *adaptive-mode*
  "When true, uses adaptive mutation rates based on population diversity and stagnation.
   When false, uses fixed 80/20 mutation/crossover ratio."
  true)


(defn- maybe-pmap
  "Uses pmap for parallel execution unless *deterministic-mode* is true."
  [f coll]
  (if *deterministic-mode*
    (mapv f coll)
    (pmap f coll)))


(defn initialize
  "Initialize GA population and functions"
  [initial-pop score-fn mutation-fn crossover-fn]
  {:pop          initial-pop
   :score-fn     score-fn
   :mutation-fn  mutation-fn
   :crossover-fn crossover-fn})


(def ^:private new-phen-modifier-sampler
  ;; 4 / 5 chance of mutation instead of crossover (used when adaptive mode is off):
  [true true true true false])


(defn- should-use-mutation?
  "Decide whether to use mutation or crossover.
  Uses adaptive probability when *adaptive-mode* is true, otherwise fixed 80/20."
  []
  (if *adaptive-mode*
    (adaptive/should-mutate?)
    (rand-nth new-phen-modifier-sampler)))


(defn- with-score
  [the-score-fn p]
  (if (:score p)
    p
    (assoc p :score (the-score-fn p))))


(defn- compete
  [{:keys [pop score-fn mutation-fn crossover-fn]
    :as   config}
   [{^double e1-score :score :as e1} {^double e2-score :score :as e2}]]

  (if (nil? e2)
    [e1-score [e1]]

    (let [new-e-fn (if (should-use-mutation?)
                     mutation-fn
                     crossover-fn)
          next-e   (if (>= e1-score e2-score)
                     (with-score score-fn (new-e-fn e1 e2))
                     e2)]
      [(+ e1-score e2-score) [e1 next-e]])))


(defn- pop->chunks
  [all-pop]
  (cond
    (>= (count all-pop) 10000) 100
    (>= (count all-pop) 2000) 50
    (>= (count all-pop) 500) 20
    :else 10))


(defn- process-chunk
  "Process a chunk of population pairs, returning [scores new-individuals]."
  [config ^List chunk]
  (let [chunk-size (.size chunk)
        scores     (transient [])
        new-pop    (transient [])]
    (loop [i 0]
      (when (< i chunk-size)
        (let [e1                       (.get chunk i)
              e2                       (when (< (inc i) chunk-size) (.get chunk (inc i)))
              [pair-score pair-result] (compete config [e1 e2])]
          (conj! scores pair-score)
          (run! #(conj! new-pop %) pair-result)
          (recur (+ i 2)))))
    [(persistent! scores) (persistent! new-pop)]))


(defn- process-chunks-parallel
  "Process population chunks in parallel, collecting scores and new population."
  [config ^ArrayList shuffled-pop chunk-size]
  (let [pop-size    (.size shuffled-pop)
        num-chunks  (Math/ceil (/ pop-size (double chunk-size)))
        chunk-ranges (mapv (fn [i]
                            (let [start (* i chunk-size)
                                  end   (min (* (inc i) chunk-size) pop-size)]
                              [start end]))
                          (range (int num-chunks)))
        ;; Create sub-lists (views, no copy) for each chunk
        chunks      (mapv (fn [[start end]]
                           (.subList shuffled-pop start end))
                         chunk-ranges)
        ;; Process chunks in parallel
        results     (if *deterministic-mode*
                      (mapv (partial process-chunk config) chunks)
                      (pmap (partial process-chunk config) chunks))]
    ;; Combine results using transducers
    (let [all-scores (into [] (mapcat first) results)
          all-pop    (into [] (mapcat second) results)]
      [all-scores all-pop])))


(defn evolve
  "Evolve a population using random competition.
  Optimized to reduce intermediate allocations using transducers and in-place operations."
  [{:keys [pop score-fn mutation-fn crossover-fn]
    :as   config}]
  (try
    (let [;; Score population (parallel if not deterministic)
          scored-pop  (if *deterministic-mode*
                        (mapv (partial with-score score-fn) pop)
                        (into [] (pmap (partial with-score score-fn) pop)))
          ;; Shuffle in-place, returning ArrayList for efficient indexed access
          shuffled    (shuffle-arraylist! scored-pop)
          ;; Process in chunks
          chunk-size  (pop->chunks pop)
          [pop-scores new-pop] (process-chunks-parallel config shuffled chunk-size)]

      (assoc config
        :pop new-pop
        :pop-scores pop-scores))
    (catch Exception e
      (log/error "Err in evolve: " e)
      (throw e))))
