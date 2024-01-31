(ns clj-symbolic-regression.dataset.prng
  (:refer-clojure :exclude [rand rand-int rand-nth shuffle])
  (:import
    (clojure.lang
      RT)
    (java.util
      ArrayList
      Collection
      Collections
      Random)))


;; from https://github.com/trystan/random-seed
(set! *warn-on-reflection* true)

(defonce ^Random rng (new Random))


(defn set-random-seed!
  "Sets the seed of the global random number generator."
  [seed]
  (.setSeed rng seed))


(defn rand
  "Returns a random floating point number between 0 (inclusive) and
  n (default 1) (exclusive). Works like clojure.core/rand except it
  uses the seed specified in set-random-seed!."
  ([] (.nextFloat rng))
  ([n] (* n (rand))))


(defn rand-int
  "Returns a random integer between 0 (inclusive) and n (exclusive).
  Works like clojure.core/rand except it uses the seed specified in
  set-random-seed!."
  [n]
  (int (rand n)))


(defn rand-nth
  "Return a random element of the (sequential) collection. Will have
  the same performance characteristics as nth for the given
  collection. Works like clojure.core/rand except it uses the seed
  specified in set-random-seed!."
  [coll]
  (nth coll (rand-int (count coll))))


(defn shuffle
  "Return a random permutation of coll. Works like clojure.core/shuffle
  except it uses the seed specified in set-random-seed!."
  [^Collection coll]
  (let [al (ArrayList. coll)]
    (Collections/shuffle al rng)
    (RT/vector (.toArray al))))


(set-random-seed! 888)
(rand-int 100)


(defn test-rand-int-gen
  [seed n]
  (set-random-seed! seed)
  (println "Seed: " seed " n: " n
           ;; https://github.com/trystan/random-seed/issues/3
           ;; odd that the first value is so similar for different seeds:
           " rand-int: " (rand-int n)
           " rand-int: " (rand-int n)))


(comment (mapv #(test-rand-int-gen % 50) [1 5 10 20 50 75 100 1000 10000 100000]))
