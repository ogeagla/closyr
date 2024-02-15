(ns closyr.ga
  (:refer-clojure :exclude [rand rand-int rand-nth shuffle])
  (:require
    [closyr.log :as log]
    [closyr.dataset.prng :refer :all]))


(set! *warn-on-reflection* true)


(defn initialize
  "Initialize GA population and functions"
  [initial-pop score-fn mutation-fn crossover-fn]
  {:pop          initial-pop
   :score-fn     score-fn
   :mutation-fn  mutation-fn
   :crossover-fn crossover-fn})


(def ^:private new-phen-modifier-sampler
  ;; 4 / 5 chance of mutation instead of crossover:
  [true true true true false])


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

    (let [new-e-fn (if (rand-nth new-phen-modifier-sampler)
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


(defn evolve
  "Evolve a population using random competition"
  [{:keys [pop score-fn mutation-fn crossover-fn]
    :as   config}]
  (try
    (let [pop-shuff    (->>
                         pop
                         (pmap (partial with-score score-fn))
                         (shuffle))

          new-pop-data (->>
                         (partition-all (pop->chunks pop) pop-shuff)
                         (pmap (fn [pop-chunk]
                                 (mapv (partial compete config)
                                       (partition-all 2 pop-chunk))))
                         (mapcat identity))

          pop-scores   (pmap first new-pop-data)
          new-pop      (->> (pmap second new-pop-data)
                            (mapcat identity)
                            (vec))]

      (merge config
             {:pop          new-pop
              :score-fn     score-fn
              :pop-scores   pop-scores
              :mutation-fn  mutation-fn
              :crossover-fn crossover-fn}))
    (catch Exception e
      (log/error "Err in evolve: " e))))
