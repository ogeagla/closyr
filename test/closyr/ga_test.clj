(ns closyr.ga-test
  (:require
    [clojure.test :refer :all]
    [closyr.ga :as ga]))


(def initial-pop
  [{:v 990}
   {:v 91}
   {:v 92}
   {:v 920}
   {:v -91}])


(defn score-fn
  [v]
  (* -1 (abs (- 10 (:v v)))))


(defn mutation-fn
  [v _]
  {:v (if (rand-nth [true false])
        (+ 1 (:v v))
        (+ -1 (:v v)))})


(defn crossover-fn
  [v _]
  v)


(deftest can-run-ga-iterations
  (testing "basic setup"
    (let [count* (atom 0)
          pop1   (ga/initialize initial-pop score-fn mutation-fn crossover-fn)]
      (is (= (count (loop [population pop1
                           i   100]
                      (swap! count* inc)
                      (if (zero? i)
                        population
                        (let [new-pop (ga/evolve population)
                              s       (reduce + 0.0 (:pop-scores new-pop))]
                          (when (zero? (rem i 20))
                            (println i " pop score: " s))
                          (recur new-pop
                                 (if (zero? s)
                                   (do
                                     (println "Perfect score!")
                                     0)
                                   (dec i)))))))
             (count initial-pop)))
      (is (= @count*
             101)))))
