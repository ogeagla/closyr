(ns closyr.dataset.inputs
  (:refer-clojure :exclude [rand rand-int rand-nth shuffle])
  (:require
    [closyr.dataset.prime-10000 :as data-primes]
    [closyr.dataset.prime-counting :as data-prime-counting]
    [closyr.util.prng :refer [rand rand-int rand-nth shuffle]]))

(set! *warn-on-reflection* true)

(defn y->gui-coord-y
  "Translate actual Y to a GUI Y coordinate (scale, translate, flip upside down)"
  [sketchpad-size* y]
  (+ (/ (or (:h @sketchpad-size*)
            170)
        2)
     (* -1 y)))


(def initial-fn
  "Initial function to use on screen in sketchpad"
  "sin+cos 1")


(defn input-y-fns-data
  "Functions to display in GUI which can be used as input data"
  [sketchpad-size* sketch-input-x-count*]
  {initial-fn
   {:idx     0
    :formula "objective_y(x) = 50·sin(13x) + 30·cos(19x)"
    :fn      (fn [i]
               (y->gui-coord-y
                 sketchpad-size*
                 (+ (* 50 (Math/sin (* 13.0 (/ i @sketch-input-x-count*))))
                    (* 30 (Math/cos (* 19.0 (/ i @sketch-input-x-count*)))))))}

   "sin+cos 2"
   {:idx     5
    :formula "objective_y(x) = 10·sin(17x) + 50·cos(5x)"
    :fn      (fn [i]
               (y->gui-coord-y
                 sketchpad-size*
                 (+ (* 10 (Math/sin (* 17.0 (/ i @sketch-input-x-count*))))
                    (* 50 (Math/cos (* 5.0 (/ i @sketch-input-x-count*)))))))}
   "cos"
   {:idx     10
    :formula "objective_y(x) = 80·cos(12x)"
    :fn      (fn [i]
               (y->gui-coord-y
                 sketchpad-size*
                 (* 80 (Math/cos (* 12.0 (/ i @sketch-input-x-count*))))))}
   "sin"
   {:idx     20
    :formula "objective_y(x) = 80·sin(12x)"
    :fn      (fn [i]
               (y->gui-coord-y
                 sketchpad-size*
                 (* 80 (Math/sin (* 12.0 (/ i @sketch-input-x-count*))))))}
   "log"
   {:idx     30
    :formula "objective_y(x) = 10·ln(0.01 + x)"
    :fn      (fn [i]
               (y->gui-coord-y
                 sketchpad-size*
                 (* 10 (Math/log (+ 0.01 (/ i @sketch-input-x-count*))))))}
   "hline"
   {:idx     40
    :formula "objective_y(x) = 0"
    :fn      (fn [i] (y->gui-coord-y sketchpad-size* 0.0))}

   "prime count"
   {:idx     50
    :formula "objective_y(x) = π(x) (prime counting function)"
    :fn      (fn [i]
               (let [xys (data-prime-counting/get-data @sketch-input-x-count*)]
                 (y->gui-coord-y sketchpad-size* (second (nth xys i)))))}
   "primes"
   {:idx     60
    :formula "objective_y(x) = pₙ (nth prime number)"
    :fn      (fn [i]
               (let [xys (data-primes/get-data @sketch-input-x-count*)]
                 (y->gui-coord-y sketchpad-size* (second (nth xys i)))))}
   "gaussian"
   {:idx     70
    :formula "objective_y(x) = √(2π)·exp(-((x-μ)/σ)²/2)"
    :fn      (fn [i]
               (y->gui-coord-y
                 sketchpad-size*
                 (* 40
                    (Math/sqrt (* 2.0 Math/PI))
                    (Math/exp (- (* (/ (/ (- i (/ @sketch-input-x-count* 2)) 5.0) 2.0)
                                    (/ (- i (/ @sketch-input-x-count* 2)) 5.0)))))))}
   "random"
   {:idx     80
    :formula "objective_y(x) = random()"
    :fn      (fn [i]
               (y->gui-coord-y
                 sketchpad-size*
                 (* 60
                    (rand))))}

   ;; Nguyen-4: x⁶ + x⁵ + x⁴ + x³ + x² + x , x ∈ [-1, 1]
   "Nguyen-4"
   {:idx     90
    :formula "objective_y(x) = x⁶ + x⁵ + x⁴ + x³ + x² + x"
    :fn      (fn [i]
               (let [x (- (* 2.0 (/ i (double @sketch-input-x-count*))) 1.0)] ; map to [-1, 1]
                 (y->gui-coord-y
                   sketchpad-size*
                   (* 15 (+ (Math/pow x 6)
                            (Math/pow x 5)
                            (Math/pow x 4)
                            (Math/pow x 3)
                            (Math/pow x 2)
                            x)))))}

   ;; Nguyen-5: sin(x²)·cos(x) - 1 , x ∈ [-1, 1]
   "Nguyen-5"
   {:idx     100
    :formula "objective_y(x) = sin(x²)·cos(x) - 1"
    :fn      (fn [i]
               (let [x (- (* 2.0 (/ i (double @sketch-input-x-count*))) 1.0)] ; map to [-1, 1]
                 (y->gui-coord-y
                   sketchpad-size*
                   (* 40 (- (* (Math/sin (* x x))
                               (Math/cos x))
                            1)))))}

   ;; Lorentz factor: γ = 1/√(1 - v²/c²), v/c ∈ [0, 0.95]
   "Feynman Lorentz"
   {:idx     110
    :formula "objective_y(x) = 1/√(1 - v²/c²)"
    :fn      (fn [i]
               (let [v-over-c (* 0.95 (/ i (double @sketch-input-x-count*)))] ; map to [0, 0.95]
                 (y->gui-coord-y
                   sketchpad-size*
                   (* 25 (/ 1.0
                            (Math/sqrt (- 1.0 (* v-over-c v-over-c))))))))}

   ;; Wave equation: y = A·sin(kx - ωt)
   "Feynman Wave"
   {:idx     120
    :formula "objective_y(x) = A·sin(kx - ωt)"
    :fn      (fn [i]
               (let [x (* 12.0 (/ i (double @sketch-input-x-count*)))  ; spatial coordinate
                     k 1.0        ; wave number
                     omega 0.5    ; angular frequency
                     t 2.0]       ; fixed time
                 (y->gui-coord-y
                   sketchpad-size*
                   (* 60 (Math/sin (- (* k x) (* omega t)))))))}})
