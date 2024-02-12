(ns closyr.dataset-prng-test
  (:require
    [clojure.test :refer :all]
    [closyr.dataset.prng :as prng]))


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
