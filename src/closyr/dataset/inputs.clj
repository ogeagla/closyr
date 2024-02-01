(ns closyr.dataset.inputs
  (:require
    [closyr.dataset.prime-10000 :as data-primes]
    [closyr.dataset.prime-counting :as data-prime-counting]))


(defn y->gui-coord-y
  [sketchpad-size* y]
  (+ (/ (or (:h @sketchpad-size*)
            170)
        2)
     (* -1 y)))

(def initial-fn "sin+cos 1")

(defn input-y-fns-data
  [sketchpad-size* sketch-input-x-count*]
  {initial-fn
   {:idx 0
    :fn  (fn [i]
           (y->gui-coord-y
             sketchpad-size*
             (+ (* 50 (Math/sin (/ i 4.0)))
                (* 30 (Math/cos (/ i 3.0))))))}

   "sin+cos 2"
   {:idx 5
    :fn  (fn [i]
           (y->gui-coord-y
             sketchpad-size*
             (+ (* 10 (Math/sin (/ i 1.125)))
                (* 50 (Math/cos (/ i 5.0))))))}
   "cos"
   {:idx 10
    :fn  (fn [i]
           (y->gui-coord-y
             sketchpad-size*
             (* 80 (Math/cos (/ i 3.0)))))}
   "sin"
   {:idx 20
    :fn  (fn [i]
           (y->gui-coord-y
             sketchpad-size*
             (* 80 (Math/sin (/ i 3.0)))))}
   "log"
   {:idx 30
    :fn  (fn [i]
           (y->gui-coord-y
             sketchpad-size*
             (* 50 (Math/log (+ 0.01 (/ i 4.0))))))}
   "hline"
   {:idx 40
    :fn  (fn [i] (y->gui-coord-y sketchpad-size* 0.0))}

   "prime count"
   {:idx 50
    :fn  (fn [i]
           (let [xys (data-prime-counting/get-data @sketch-input-x-count*)]
             (y->gui-coord-y sketchpad-size* (second (nth xys i)))))}
   "primes"
   {:idx 60
    :fn  (fn [i]
           (let [xys (data-primes/get-data @sketch-input-x-count*)]
             (y->gui-coord-y sketchpad-size* (second (nth xys i)))))}
   "gaussian"
   {:idx 70
    :fn  (fn [i]
           (y->gui-coord-y
             sketchpad-size*
             (* 40
                (Math/sqrt (* 2.0 Math/PI))
                (Math/exp (- (* (/ (/ (- i (/ @sketch-input-x-count* 2)) 5.0) 2.0)
                                (/ (- i (/ @sketch-input-x-count* 2)) 5.0)))))))}
   "random"
   {:idx 80
    :fn  (fn [i]
           (y->gui-coord-y
             sketchpad-size*
             (* 60
                (rand))))}})

