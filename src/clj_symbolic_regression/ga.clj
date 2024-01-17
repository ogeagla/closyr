(ns clj-symbolic-regression.ga)

(set! *warn-on-reflection* true)


(defn initialize
  [initial-pop score-fn mutation-fn crossover-fn]
  {:pop          initial-pop
   :score-fn     score-fn
   :mutation-fn  mutation-fn
   :crossover-fn crossover-fn})


(def new-phen-modifier-sampler
  ;; 3 / 4 chance of mutation instead of crossover:
  [true true true false])


(defn with-score
  [the-score-fn p]
  (if (:score p)
    p
    (assoc p :score (the-score-fn p))))


(defn compete
  [{:keys [pop score-fn mutation-fn crossover-fn]
    :as   config}
   [{^double e1-score :score :as e1} {^double e2-score :score :as e2}]]

  (if (nil? e2)
    [e1-score [e1]]

    (let [new-e-fn (if (rand-nth new-phen-modifier-sampler)
                     mutation-fn
                     crossover-fn)
          next-e   (if (>= e1-score e2-score)
                     (with-score score-fn (new-e-fn e1 e2 pop))
                     e2)]
      [(+ e1-score e2-score) [e1 next-e]])))


(defn evolve
  [{:keys [pop score-fn mutation-fn crossover-fn]
    :as   config}]
  (try
    (let [pop-shuff    (->>
                         pop
                         (pmap (partial with-score score-fn))
                         (shuffle))

          new-pop-data (->>
                         (partition-all 40 pop-shuff)
                         (pmap (fn [pop-chunk]
                                 (mapv (partial compete config)
                                       (partition-all 2 pop-chunk))))
                         (mapcat identity))

          pop-scores   (pmap first new-pop-data)
          pop-score    (reduce + 0.0 pop-scores)
          new-pop      (->> (pmap second new-pop-data)
                            (mapcat identity)
                            (vec))]

      (merge config
             {:pop            new-pop
              :pop-old        pop
              :score-fn       score-fn
              :pop-old-score  pop-score
              :pop-old-scores pop-scores
              :mutation-fn    mutation-fn
              :crossover-fn   crossover-fn}))
    (catch Exception e
      (println "Err in evolve: " e))))


(def initial-pop
  [{:v 0}
   {:v 1}
   {:v 2}
   {:v 20}
   {:v -1}])


(defn score-fn
  [v]
  (* -1 (abs (- 10 (:v v)))))


(defn mutation-fn
  [v _ _]
  {:v (if (rand-nth [true false])
        (+ 1 (:v v))
        (+ -1 (:v v)))})


(defn crossover-fn
  [v _ _]
  v)


(defn run-test
  []
  (try
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
                     (dec i)))))))
    (catch Exception e
      (println "ERR doing test GA: " e))))


(comment (run-test))
