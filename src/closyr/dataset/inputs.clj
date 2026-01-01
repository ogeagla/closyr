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
    :formula "50*Sin(13*x) + 30*Cos(19*x)"
    :fn      (fn [i]
               (y->gui-coord-y
                 sketchpad-size*
                 (+ (* 50 (Math/sin (* 13.0 (/ i @sketch-input-x-count*))))
                    (* 30 (Math/cos (* 19.0 (/ i @sketch-input-x-count*)))))))}

   "sin+cos 2"
   {:idx     5
    :formula "10*Sin(17*x) + 50*Cos(5*x)"
    :fn      (fn [i]
               (y->gui-coord-y
                 sketchpad-size*
                 (+ (* 10 (Math/sin (* 17.0 (/ i @sketch-input-x-count*))))
                    (* 50 (Math/cos (* 5.0 (/ i @sketch-input-x-count*)))))))}
   "cos"
   {:idx     10
    :formula "80*Cos(12*x)"
    :fn      (fn [i]
               (y->gui-coord-y
                 sketchpad-size*
                 (* 80 (Math/cos (* 12.0 (/ i @sketch-input-x-count*))))))}
   "sin"
   {:idx     20
    :formula "80*Sin(12*x)"
    :fn      (fn [i]
               (y->gui-coord-y
                 sketchpad-size*
                 (* 80 (Math/sin (* 12.0 (/ i @sketch-input-x-count*))))))}
   "log"
   {:idx     30
    :formula "10*Log(0.01 + x)"
    :fn      (fn [i]
               (y->gui-coord-y
                 sketchpad-size*
                 (* 10 (Math/log (+ 0.01 (/ i @sketch-input-x-count*))))))}
   "hline"
   {:idx     40
    :formula "0"
    :fn      (fn [i] (y->gui-coord-y sketchpad-size* 0.0))}

   "prime count"
   {:idx     50
    :formula "PrimePi(x)"
    :fn      (fn [i]
               (let [xys (data-prime-counting/get-data @sketch-input-x-count*)]
                 (y->gui-coord-y sketchpad-size* (second (nth xys i)))))}
   "primes"
   {:idx     60
    :formula "Prime(x)"
    :fn      (fn [i]
               (let [xys (data-primes/get-data @sketch-input-x-count*)]
                 (y->gui-coord-y sketchpad-size* (second (nth xys i)))))}
   "gaussian"
   {:idx     70
    :formula "Sqrt(2*Pi)*Exp(-((x-mu)/sigma)^2/2)"
    :fn      (fn [i]
               (y->gui-coord-y
                 sketchpad-size*
                 (* 40
                    (Math/sqrt (* 2.0 Math/PI))
                    (Math/exp (- (* (/ (/ (- i (/ @sketch-input-x-count* 2)) 5.0) 2.0)
                                    (/ (- i (/ @sketch-input-x-count* 2)) 5.0)))))))}
   "random"
   {:idx     80
    :formula "Random()"
    :fn      (fn [i]
               (y->gui-coord-y
                 sketchpad-size*
                 (* 60
                    (rand))))}

   ;; Nguyen-4: x^6 + x^5 + x^4 + x^3 + x^2 + x , x in [-1, 1]
   "Nguyen-4"
   {:idx     90
    :formula "x^6 + x^5 + x^4 + x^3 + x^2 + x"
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

   ;; Nguyen-5: Sin(x^2)*Cos(x) - 1 , x in [-1, 1]
   "Nguyen-5"
   {:idx     100
    :formula "Sin(x^2)*Cos(x) - 1"
    :fn      (fn [i]
               (let [x (- (* 2.0 (/ i (double @sketch-input-x-count*))) 1.0)] ; map to [-1, 1]
                 (y->gui-coord-y
                   sketchpad-size*
                   (* 40 (- (* (Math/sin (* x x))
                               (Math/cos x))
                            1)))))}

   ;; Lorentz factor: 1/Sqrt(1 - v^2/c^2), v/c in [0, 0.95]
   "Feynman Lorentz"
   {:idx     110
    :formula "1/Sqrt(1 - x^2)"
    :fn      (fn [i]
               (let [v-over-c (* 0.95 (/ i (double @sketch-input-x-count*)))] ; map to [0, 0.95]
                 (y->gui-coord-y
                   sketchpad-size*
                   (* 25 (/ 1.0
                            (Math/sqrt (- 1.0 (* v-over-c v-over-c))))))))}

   ;; Wave equation: A*Sin(k*x - omega*t)
   "Feynman Wave"
   {:idx     120
    :formula "A*Sin(k*x - omega*t)"
    :fn      (fn [i]
               (let [x (* 12.0 (/ i (double @sketch-input-x-count*)))  ; spatial coordinate
                     k 1.0        ; wave number
                     omega 0.5    ; angular frequency
                     t 2.0]       ; fixed time
                 (y->gui-coord-y
                   sketchpad-size*
                   (* 60 (Math/sin (- (* k x) (* omega t)))))))}

   ;; Diffraction grating: I = I0 * sin²(nθ/2) / sin²(θ/2), n=5
   "Feynman Diffraction"
   {:idx     130
    :formula "Sin(n*x/2)^2 / Sin(x/2)^2"
    :fn      (fn [i]
               (let [theta (+ 0.1 (* 6.0 (/ i (double @sketch-input-x-count*))))  ; θ in [0.1, 6.1]
                     n 5.0
                     half-theta (/ theta 2.0)
                     sin-half (Math/sin half-theta)
                     sin-n-half (Math/sin (* n half-theta))
                     intensity (if (< (Math/abs sin-half) 1e-10)
                                 (* n n)
                                 (/ (* sin-n-half sin-n-half)
                                    (* sin-half sin-half)))]
                 (y->gui-coord-y
                   sketchpad-size*
                   (* 3 intensity))))}

   ;; Planck radiation spectrum: x³ / (exp(x) - 1)
   "Feynman Planck"
   {:idx     140
    :formula "x^3 / (Exp(x) - 1)"
    :fn      (fn [i]
               (let [x (+ 0.1 (* 5.0 (/ i (double @sketch-input-x-count*))))  ; x in [0.1, 5.1]
                     planck (/ (* x x x)
                               (- (Math/exp x) 1.0))]
                 (y->gui-coord-y
                   sketchpad-size*
                   (* 50 planck))))}

   ;; Rutherford scattering: 1 / sin⁴(θ/2)
   "Feynman Rutherford"
   {:idx     150
    :formula "1 / Sin(x/2)^4"
    :fn      (fn [i]
               (let [theta (+ 0.3 (* 2.8 (/ i (double @sketch-input-x-count*))))  ; θ in [0.3, 3.1]
                     sin-half (Math/sin (/ theta 2.0))
                     rutherford (/ 1.0
                                   (* sin-half sin-half sin-half sin-half))]
                 (y->gui-coord-y
                   sketchpad-size*
                   (* 1 (Math/log rutherford)))))}  ; use log scale for display

   ;; Elliptical orbit: r = a(1-e²) / (1 + e*cos(θ)), e=0.6
   "Feynman Ellipse"
   {:idx     160
    :formula "a*(1-e^2) / (1 + e*Cos(x))"
    :fn      (fn [i]
               (let [theta (* 2.0 Math/PI (/ i (double @sketch-input-x-count*)))  ; θ in [0, 2π]
                     e 0.6
                     a 1.0
                     radius (/ (* a (- 1.0 (* e e)))
                               (+ 1.0 (* e (Math/cos theta))))]
                 (y->gui-coord-y
                   sketchpad-size*
                   (* 60 radius))))}})