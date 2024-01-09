(ns clj-symbolic-regression.ga)


(defn initialize
  [initial-pop score-fn mutation-fn crossover-fn]
  {:pop          initial-pop
   :score-fn     score-fn
   :mutation-fn  mutation-fn
   :crossover-fn crossover-fn})

(defn evolve
  [{:keys [pop score-fn mutation-fn crossover-fn]}]

  (let [pop-shuff (->>
                    pop
                    (map (fn [p] (assoc p :score (score-fn p))))
                    (shuffle))
        score*    (atom 0.0)
        new-pop   (loop [pop     pop-shuff
                         new-pop []]
                    (if (empty? pop)
                      new-pop
                      (recur
                        (drop 2 pop)
                        (let [[e1 e2] [(first pop) (second pop)]
                              newest-pop (if (nil? e2)
                                           (do
                                             (swap! score* + (:score e1))
                                             (concat new-pop [e1]))
                                           (let [[s1 s2] [(:score e1) (:score e2)]
                                                 better-e (if (>= s1 s2) e1 e2)
                                                 new-e    (if (rand-nth [true false])
                                                            (mutation-fn better-e)
                                                            (crossover-fn better-e))]
                                             (swap! score* + s1 s2)
                                             (concat new-pop [better-e new-e])))]
                          newest-pop))))]
    {:pop           new-pop
     :pop-old       pop
     :score-fn      score-fn
     :pop-old-score @score*
     :mutation-fn   mutation-fn
     :crossover-fn  crossover-fn}))


(def initial-pop [0 1 2 20 -1])


(defn score-fn
  [v]
  (* -1 (abs (- 10 v))))


(defn mutation-fn
  [v]
  (if (rand-nth [true false])
    (+ 1 v)
    (+ -1 v)))


(defn crossover-fn
  [v]
  v)


(defn run-test
  []
  (let [pop1 (initialize initial-pop score-fn mutation-fn crossover-fn)]
    (loop [pop pop1
           i   100]
      (if (zero? i)
        pop
        (let [new-pop (evolve pop)
              s       (:pop-old-score new-pop)]
          (println i " pop score: " s)
          (recur new-pop
                 (if (zero? s)
                   (do
                     (println "Perfect score!")
                     0)
                   (dec i))))))))


(comment (run-test))
