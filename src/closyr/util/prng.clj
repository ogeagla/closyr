(ns closyr.util.prng
  (:refer-clojure :exclude [rand rand-int rand-nth shuffle random-uuid])
  (:import
    (clojure.lang
      RT)
    (java.util
      ArrayList
      Collection
      Collections
      Random
      UUID)))


(set! *warn-on-reflection* true)

;; from https://github.com/trystan/random-seed
(def ^:private rng* (atom nil))

(defn- get-random []
  (let [^Random rng @rng*]
    rng))

(defn init []
  (when-not @rng*
    (reset! rng* (new Random))))


(defn set-random-seed!
  "Sets the seed of the global random number generator."
  [seed]
  (init)
  (.setSeed ^Random (get-random) seed))


(defn rand
  "Returns a random floating point number between 0 (inclusive) and
  n (default 1) (exclusive). Works like clojure.core/rand except it
  uses the seed specified in set-random-seed!."
  ([] (init) (.nextFloat ^Random (get-random)))
  ([n] (init) (* n (rand))))


(defn rand-int
  "Returns a random integer between 0 (inclusive) and n (exclusive).
  Works like clojure.core/rand except it uses the seed specified in
  set-random-seed!."
  [n]
  (init)
  (int (rand n)))


(defn rand-nth
  "Return a random element of the (sequential) collection. Will have
  the same performance characteristics as nth for the given
  collection. Works like clojure.core/rand except it uses the seed
  specified in set-random-seed!."
  [coll]
  (init)
  (nth coll (rand-int (count coll))))


(defn shuffle
  "Return a random permutation of coll. Works like clojure.core/shuffle
  except it uses the seed specified in set-random-seed!."
  [^Collection coll]
  (init)
  (let [al (ArrayList. coll)]
    (Collections/shuffle al ^Random (get-random))
    (RT/vector (.toArray al))))


(defn shuffle-arraylist!
  "Shuffle a collection in-place and return as ArrayList for efficient iteration.
  Avoids vector conversion overhead when the result will be iterated sequentially."
  ^ArrayList [^Collection coll]
  (init)
  (let [^ArrayList al (if (instance? ArrayList coll) coll (ArrayList. coll))]
    (Collections/shuffle al ^Random (get-random))
    al))


(defn random-uuid
  "Generate a random UUID using the seeded PRNG.
  This produces deterministic UUIDs when the seed is set."
  []
  (init)
  (let [bytes (byte-array 16)]
    (.nextBytes ^Random (get-random) bytes)
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
      (UUID. msb lsb))))


(comment
  (set-random-seed! 888)
  (rand-int 100))
