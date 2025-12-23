(ns closyr.util.prng
  (:refer-clojure :exclude [rand rand-int rand-nth shuffle random-uuid])
  (:import
    (clojure.lang
      RT)
    (java.util
      ArrayList
      Collection
      Collections
      Random)))


(set! *warn-on-reflection* true)

;; from https://github.com/trystan/random-seed
(def ^:private ^Random rng (new Random))


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


(defn random-uuid
  "Generate a random UUID using the seeded PRNG.
  This produces deterministic UUIDs when the seed is set."
  []
  (let [bytes (byte-array 16)]
    (.nextBytes rng bytes)
    ;; Set version to 4 (random) and variant to IETF
    ;; Use unchecked-byte to handle values > 127
    (aset bytes 6 (unchecked-byte (bit-or (bit-and (aget bytes 6) 0x0f) 0x40)))
    (aset bytes 8 (unchecked-byte (bit-or (bit-and (aget bytes 8) 0x3f) 0x80)))
    ;; Convert bytes to UUID
    (let [msb (reduce (fn [acc i]
                        (bit-or (bit-shift-left acc 8)
                                (bit-and (aget bytes i) 0xff)))
                      0 (range 8))
          lsb (reduce (fn [acc i]
                        (bit-or (bit-shift-left acc 8)
                                (bit-and (aget bytes i) 0xff)))
                      0 (range 8 16))]
      (java.util.UUID. msb lsb))))


(comment
  (set-random-seed! 888)
  (rand-int 100))
