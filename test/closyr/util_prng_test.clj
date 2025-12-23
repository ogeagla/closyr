(ns closyr.util-prng-test
  (:require
    [clojure.test :refer :all]
    [closyr.util.prng :as prng]))


(defn- test-rand-int-gen
  [seed n]
  (prng/set-random-seed! seed)
  (let [r1 (prng/rand-int n) r2 (prng/rand-int n)]
    (println "Seed: " seed " n: " n
             ;; https://github.com/trystan/random-seed/issues/3
             ;; odd that the first value is so similar for different seeds:
             " rand-int: " r1
             " rand-int: " r2)
    [r1 r2]))


(comment (mapv #(test-rand-int-gen % 50) [1 5 10 20 50 75 100 1000 10000 100000]))


(deftest test-basic-randomness
  (testing "can sample rand ints"
    (is (= (mapv #(test-rand-int-gen % 50) [1 5 10 20 50 75 100 1000 10000 100000])
           [[36 5] [36 8] [36 22] [36 30] [36 29] [36 46] [36 36] [35 12] [44 40] [26 29]]))))


(deftest test-random-uuid-deterministic
  (testing "random-uuid is deterministic with seed"
    (prng/set-random-seed! 12345)
    (let [uuid1 (prng/random-uuid)]
      (prng/set-random-seed! 12345)
      (let [uuid2 (prng/random-uuid)]
        (is (= uuid1 uuid2)
            "Same seed should produce same UUID")))))


(deftest test-random-uuid-different-seeds
  (testing "random-uuid produces different results with different seeds"
    (prng/set-random-seed! 111)
    (let [uuid1 (prng/random-uuid)]
      (prng/set-random-seed! 222)
      (let [uuid2 (prng/random-uuid)]
        (is (not= uuid1 uuid2)
            "Different seeds should produce different UUIDs")))))


(deftest test-shuffle-deterministic
  (testing "shuffle is deterministic with seed"
    (prng/set-random-seed! 42)
    (let [result1 (prng/shuffle [1 2 3 4 5 6 7 8 9 10])]
      (prng/set-random-seed! 42)
      (let [result2 (prng/shuffle [1 2 3 4 5 6 7 8 9 10])]
        (is (= result1 result2)
            "Same seed should produce same shuffle order")))))


(deftest test-rand-nth-deterministic
  (testing "rand-nth is deterministic with seed"
    (let [coll [:a :b :c :d :e :f :g :h :i :j]]
      (prng/set-random-seed! 999)
      (let [picks1 (vec (repeatedly 5 #(prng/rand-nth coll)))]
        (prng/set-random-seed! 999)
        (let [picks2 (vec (repeatedly 5 #(prng/rand-nth coll)))]
          (is (= picks1 picks2)
              "Same seed should produce same rand-nth sequence"))))))
