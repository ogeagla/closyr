(ns closyr.benchmark-functions-test
  "Benchmark tests for standard symbolic regression test functions (Nguyen, Feynman)"
  (:require
    [clojure.test :refer :all]
    [closyr.ops :as ops]
    [closyr.ops.common :as ops-common]
    [closyr.ops.initialize :as ops-init]
    [closyr.symbolic-regression :as symreg]
    [closyr.test-utils :as test-utils])
  (:import
    (java.text DecimalFormat)))


(use-fixtures :once test-utils/quiet-logging-fixture)

(alter-var-root #'symreg/*is-testing* (constantly true))


;; =============================================================================
;; Timing Utilities
;; =============================================================================

(def ^:private decimal-fmt (DecimalFormat. "0.00"))


(defn- get-best-fn-str
  "Extract the best formula string from the final population"
  [final-population]
  (let [{:keys [pop pop-scores]} final-population
        best-idx (->> pop-scores
                      (map-indexed vector)
                      (apply max-key second)
                      first)
        best-pheno (nth pop best-idx)]
    (ops/format-fn-str (:expr best-pheno))))


(defmacro with-timing
  "Execute body and return [result elapsed-ms]"
  [& body]
  `(let [start# (System/nanoTime)
         result# (do ~@body)
         elapsed# (/ (- (System/nanoTime) start#) 1e6)]
     [result# elapsed#]))


(defn format-score [score]
  (.format decimal-fmt score))


(defn format-time [ms]
  (if (>= ms 1000)
    (str (.format decimal-fmt (/ ms 1000.0)) "s")
    (str (.format decimal-fmt ms) "ms")))


;; =============================================================================
;; Benchmark Function Definitions
;; =============================================================================

(defn nguyen-4
  "Nguyen-4: x^6 + x^5 + x^4 + x^3 + x^2 + x, x in [-1, 1]"
  [x]
  (+ (Math/pow x 6)
     (Math/pow x 5)
     (Math/pow x 4)
     (Math/pow x 3)
     (Math/pow x 2)
     x))


(defn nguyen-5
  "Nguyen-5: sin(x^2) * cos(x) - 1, x in [-1, 1]"
  [x]
  (- (* (Math/sin (* x x))
        (Math/cos x))
     1.0))


(defn feynman-lorentz
  "Lorentz factor: 1/sqrt(1 - v^2/c^2), v/c in [0, 0.95]"
  [v-over-c]
  (/ 1.0
     (Math/sqrt (- 1.0 (* v-over-c v-over-c)))))


(defn feynman-wave
  "Wave equation: A * sin(kx - wt), with k=1, w=0.5, t=2, A=1"
  [x]
  (Math/sin (- x 1.0)))  ; simplified: k=1, omega*t=1


(defn feynman-diffraction
  "Diffraction grating intensity: I = I0 * sin²(nθ/2) / sin²(θ/2), n=5, I0=1
   From Feynman I.30.3"
  [theta]
  (let [n 5.0
        half-theta (/ theta 2.0)
        sin-half (Math/sin half-theta)
        sin-n-half (Math/sin (* n half-theta))]
    (if (< (Math/abs sin-half) 1e-10)
      (* n n)  ; limit as theta->0 is n²
      (/ (* sin-n-half sin-n-half)
         (* sin-half sin-half)))))


(defn feynman-planck
  "Planck radiation spectrum (simplified): x³ / (exp(x) - 1)
   Core shape of black-body radiation. From Feynman I.41.16"
  [x]
  (if (< x 0.01)
    (* x x)  ; Taylor expansion near 0
    (/ (* x x x)
       (- (Math/exp x) 1.0))))


(defn feynman-rutherford
  "Rutherford scattering cross-section: 1 / sin⁴(θ/2)
   Simplified from Feynman B1. θ in (0.1, π)"
  [theta]
  (let [sin-half (Math/sin (/ theta 2.0))]
    (/ 1.0
       (* sin-half sin-half sin-half sin-half))))


(defn feynman-elliptical-orbit
  "Elliptical orbit radius: r = a(1-e²) / (1 + e*cos(θ))
   Kepler's first law. e=0.6 (eccentricity), a=1. From Feynman B3"
  [theta]
  (let [e 0.6
        a 1.0]
    (/ (* a (- 1.0 (* e e)))
       (+ 1.0 (* e (Math/cos theta))))))


(defn feynman-transition
  "Quantum transition probability (sinc² function): sin²(x) / x²
   From Feynman III.9.52: PI→II = (2πμEt/h)² × sin²((ω-ω₀)t/2) / ((ω-ω₀)t/2)²
   Core shape where x = (ω-ω₀)t/2"
  [x]
  (if (< (Math/abs x) 1e-10)
    1.0  ; limit as x->0 is 1
    (/ (* (Math/sin x) (Math/sin x))
       (* x x))))


;; =============================================================================
;; Data Generation
;; =============================================================================

(defn generate-benchmark-data
  "Generate x,y pairs for a benchmark function"
  [f x-min x-max n-points]
  (let [xs (mapv (fn [i]
                   (+ x-min
                      (* (/ i (dec (double n-points)))
                         (- x-max x-min))))
                 (range n-points))
        ys (mapv f xs)]
    {:xs xs :ys ys}))


;; =============================================================================
;; Benchmark Tests
;; =============================================================================

(deftest ^:benchmark nguyen-4-benchmark
  (testing "Nguyen-4: polynomial x^6 + x^5 + x^4 + x^3 + x^2 + x"
    (let [{:keys [xs ys]} (generate-benchmark-data nguyen-4 -1.0 1.0 30)
          [{:keys [final-population iters-done]} elapsed-ms]
          (with-timing
            (binding [ops/*print-top-n* 1]
              (with-redefs-fn {#'symreg/config->log-steps (fn [_ _] 50)}
                (fn []
                  (symreg/run-find-formula
                    {:input-phenos-count 200
                     :initial-muts       (ops-init/initial-mutations)
                     :iters              100
                     :use-gui?           false
                     :use-flamechart     false
                     :random-seed        42
                     :input-xs-exprs     (ops-common/doubles->exprs xs)
                     :input-ys-exprs     (ops-common/doubles->exprs ys)})))))
          best-score (apply max (:pop-scores final-population))
          best-fn-str (get-best-fn-str final-population)]

      (is (= 200 (count (:pop final-population))))
      (is (= 100 iters-done))
      ;; Best score should be negative (error) and improving
      (is (neg? best-score))
      ;; With random-seed 42, the resulting formula should be deterministic
      (is (= "-1/100+Sin(x)+x*(x+1/50*x*Csc(x)*(49/100+E^(2*(-1/10+E)^x)-11/10*Sin(1.6*x)))"
             best-fn-str)
          "Expected formula for Nguyen-4 with seed 42")
      (println (str "| Nguyen-4         | " (format-score best-score)
                    " | " (format-time elapsed-ms)
                    " | fn: " best-fn-str " |")))))


(deftest ^:benchmark nguyen-5-benchmark
  (testing "Nguyen-5: sin(x^2)*cos(x) - 1"
    (let [{:keys [xs ys]} (generate-benchmark-data nguyen-5 -1.0 1.0 30)
          [{:keys [final-population iters-done]} elapsed-ms]
          (with-timing
            (binding [ops/*print-top-n* 1]
              (with-redefs-fn {#'symreg/config->log-steps (fn [_ _] 50)}
                (fn []
                  (symreg/run-find-formula
                    {:input-phenos-count 200
                     :initial-muts       (ops-init/initial-mutations)
                     :iters              100
                     :use-gui?           false
                     :use-flamechart     false
                     :random-seed        42
                     :input-xs-exprs     (ops-common/doubles->exprs xs)
                     :input-ys-exprs     (ops-common/doubles->exprs ys)})))))
          best-score (apply max (:pop-scores final-population))
          best-fn-str (get-best-fn-str final-population)]

      (is (= 200 (count (:pop final-population))))
      (is (= 100 iters-done))
      (is (neg? best-score))
      ;; With random-seed 42, the resulting formula should be deterministic
      (is (= "Cos(x)+0.9/(-Cos(x)+x*(Cos(1/2-1.1*(11/100-99/100*Cos(0.9*x)))-Sin(x)))"
             best-fn-str)
          "Expected formula for Nguyen-5 with seed 42")
      (println (str "| Nguyen-5         | " (format-score best-score)
                    " | " (format-time elapsed-ms)
                    " | fn: " best-fn-str " |")))))


(deftest ^:benchmark feynman-lorentz-benchmark
  (testing "Feynman Lorentz factor: 1/sqrt(1 - v^2/c^2)"
    (let [{:keys [xs ys]} (generate-benchmark-data feynman-lorentz 0.0 0.9 30)
          [{:keys [final-population iters-done]} elapsed-ms]
          (with-timing
            (binding [ops/*print-top-n* 1]
              (with-redefs-fn {#'symreg/config->log-steps (fn [_ _] 50)}
                (fn []
                  (symreg/run-find-formula
                    {:input-phenos-count 200
                     :initial-muts       (ops-init/initial-mutations)
                     :iters              100
                     :use-gui?           false
                     :use-flamechart     false
                     :random-seed        42
                     :input-xs-exprs     (ops-common/doubles->exprs xs)
                     :input-ys-exprs     (ops-common/doubles->exprs ys)})))))
          best-score (apply max (:pop-scores final-population))
          best-fn-str (get-best-fn-str final-population)]

      (is (= 200 (count (:pop final-population))))
      (is (= 100 iters-done))
      (is (neg? best-score))
      ;; With random-seed 42, the resulting formula should be deterministic
      (is (= "1/2+x-Cos(x)+Cos(1/2-x)*(-x^2+0.9*Log(-1/100+3.05997*x^4+Cos(x)))"
             best-fn-str)
          "Expected formula for Feynman Lorentz with seed 42")
      (println (str "| Feynman Lorentz  | " (format-score best-score)
                    " | " (format-time elapsed-ms)
                    " | fn: " best-fn-str " |")))))


(deftest ^:benchmark feynman-wave-benchmark
  (testing "Feynman Wave equation: sin(kx - wt)"
    (let [{:keys [xs ys]} (generate-benchmark-data feynman-wave 0.0 (* 4 Math/PI) 30)
          [{:keys [final-population iters-done]} elapsed-ms]
          (with-timing
            (binding [ops/*print-top-n* 1]
              (with-redefs-fn {#'symreg/config->log-steps (fn [_ _] 50)}
                (fn []
                  (symreg/run-find-formula
                    {:input-phenos-count 200
                     :initial-muts       (ops-init/initial-mutations)
                     :iters              100
                     :use-gui?           false
                     :use-flamechart     false
                     :random-seed        42
                     :input-xs-exprs     (ops-common/doubles->exprs xs)
                     :input-ys-exprs     (ops-common/doubles->exprs ys)})))))
          best-score (apply max (:pop-scores final-population))
          best-fn-str (get-best-fn-str final-population)]

      (is (= 200 (count (:pop final-population))))
      (is (= 100 iters-done))
      (is (neg? best-score))
      ;; With random-seed 42, the resulting formula should be deterministic
      (is (= "-Cos(3/5+x)"
             best-fn-str)
          "Expected formula for Feynman Wave with seed 42")
      (println (str "| Feynman Wave     | " (format-score best-score)
                    " | " (format-time elapsed-ms)
                    " | fn: " best-fn-str " |")))))


(deftest ^:benchmark feynman-diffraction-benchmark
  (testing "Feynman Diffraction: sin²(nθ/2) / sin²(θ/2)"
    (let [{:keys [xs ys]} (generate-benchmark-data feynman-diffraction 0.1 (* 2 Math/PI) 30)
          [{:keys [final-population iters-done]} elapsed-ms]
          (with-timing
            (binding [ops/*print-top-n* 1]
              (with-redefs-fn {#'symreg/config->log-steps (fn [_ _] 50)}
                (fn []
                  (symreg/run-find-formula
                    {:input-phenos-count 200
                     :initial-muts       (ops-init/initial-mutations)
                     :iters              100
                     :use-gui?           false
                     :use-flamechart     false
                     :random-seed        42
                     :input-xs-exprs     (ops-common/doubles->exprs xs)
                     :input-ys-exprs     (ops-common/doubles->exprs ys)})))))
          best-score (apply max (:pop-scores final-population))
          best-fn-str (get-best-fn-str final-population)]

      (is (= 200 (count (:pop final-population))))
      (is (= 100 iters-done))
      (is (neg? best-score))
      (println (str "| Feynman Diffraction | " (format-score best-score)
                    " | " (format-time elapsed-ms)
                    " | fn: " best-fn-str " |")))))


(deftest ^:benchmark feynman-planck-benchmark
  (testing "Feynman Planck radiation: x³ / (exp(x) - 1)"
    (let [{:keys [xs ys]} (generate-benchmark-data feynman-planck 0.1 5.0 30)
          [{:keys [final-population iters-done]} elapsed-ms]
          (with-timing
            (binding [ops/*print-top-n* 1]
              (with-redefs-fn {#'symreg/config->log-steps (fn [_ _] 50)}
                (fn []
                  (symreg/run-find-formula
                    {:input-phenos-count 200
                     :initial-muts       (ops-init/initial-mutations)
                     :iters              100
                     :use-gui?           false
                     :use-flamechart     false
                     :random-seed        42
                     :input-xs-exprs     (ops-common/doubles->exprs xs)
                     :input-ys-exprs     (ops-common/doubles->exprs ys)})))))
          best-score (apply max (:pop-scores final-population))
          best-fn-str (get-best-fn-str final-population)]

      (is (= 200 (count (:pop final-population))))
      (is (= 100 iters-done))
      (is (neg? best-score))
      (println (str "| Feynman Planck   | " (format-score best-score)
                    " | " (format-time elapsed-ms)
                    " | fn: " best-fn-str " |")))))


(deftest ^:benchmark feynman-rutherford-benchmark
  (testing "Feynman Rutherford scattering: 1 / sin⁴(θ/2)"
    (let [{:keys [xs ys]} (generate-benchmark-data feynman-rutherford 0.3 Math/PI 30)
          [{:keys [final-population iters-done]} elapsed-ms]
          (with-timing
            (binding [ops/*print-top-n* 1]
              (with-redefs-fn {#'symreg/config->log-steps (fn [_ _] 50)}
                (fn []
                  (symreg/run-find-formula
                    {:input-phenos-count 200
                     :initial-muts       (ops-init/initial-mutations)
                     :iters              100
                     :use-gui?           false
                     :use-flamechart     false
                     :random-seed        42
                     :input-xs-exprs     (ops-common/doubles->exprs xs)
                     :input-ys-exprs     (ops-common/doubles->exprs ys)})))))
          best-score (apply max (:pop-scores final-population))
          best-fn-str (get-best-fn-str final-population)]

      (is (= 200 (count (:pop final-population))))
      (is (= 100 iters-done))
      (is (neg? best-score))
      (println (str "| Feynman Rutherford | " (format-score best-score)
                    " | " (format-time elapsed-ms)
                    " | fn: " best-fn-str " |")))))


(deftest ^:benchmark feynman-elliptical-orbit-benchmark
  (testing "Feynman Elliptical orbit: a(1-e²) / (1 + e*cos(θ))"
    (let [{:keys [xs ys]} (generate-benchmark-data feynman-elliptical-orbit 0.0 (* 2 Math/PI) 30)
          [{:keys [final-population iters-done]} elapsed-ms]
          (with-timing
            (binding [ops/*print-top-n* 1]
              (with-redefs-fn {#'symreg/config->log-steps (fn [_ _] 50)}
                (fn []
                  (symreg/run-find-formula
                    {:input-phenos-count 200
                     :initial-muts       (ops-init/initial-mutations)
                     :iters              100
                     :use-gui?           false
                     :use-flamechart     false
                     :random-seed        42
                     :input-xs-exprs     (ops-common/doubles->exprs xs)
                     :input-ys-exprs     (ops-common/doubles->exprs ys)})))))
          best-score (apply max (:pop-scores final-population))
          best-fn-str (get-best-fn-str final-population)]

      (is (= 200 (count (:pop final-population))))
      (is (= 100 iters-done))
      (is (neg? best-score))
      (println (str "| Feynman Elliptical | " (format-score best-score)
                    " | " (format-time elapsed-ms)
                    " | fn: " best-fn-str " |")))))


(deftest ^:benchmark feynman-transition-benchmark
  (testing "Feynman Transition (III.9.52): sin²(x) / x²"
    (let [{:keys [xs ys]} (generate-benchmark-data feynman-transition (- (* 3 Math/PI)) (* 3 Math/PI) 30)
          [{:keys [final-population iters-done]} elapsed-ms]
          (with-timing
            (binding [ops/*print-top-n* 1]
              (with-redefs-fn {#'symreg/config->log-steps (fn [_ _] 50)}
                (fn []
                  (symreg/run-find-formula
                    {:input-phenos-count 200
                     :initial-muts       (ops-init/initial-mutations)
                     :iters              100
                     :use-gui?           false
                     :use-flamechart     false
                     :random-seed        42
                     :input-xs-exprs     (ops-common/doubles->exprs xs)
                     :input-ys-exprs     (ops-common/doubles->exprs ys)})))))
          best-score (apply max (:pop-scores final-population))
          best-fn-str (get-best-fn-str final-population)]

      (is (= 200 (count (:pop final-population))))
      (is (= 100 iters-done))
      (is (neg? best-score))
      (println (str "| Feynman Transition | " (format-score best-score)
                    " | " (format-time elapsed-ms)
                    " | fn: " best-fn-str " |")))))


(deftest benchmark-data-generation
  (testing "Data generation produces correct ranges"
    (let [{:keys [xs ys]} (generate-benchmark-data nguyen-4 -1.0 1.0 10)]
      (is (= 10 (count xs)))
      (is (= 10 (count ys)))
      (is (= -1.0 (first xs)))
      (is (= 1.0 (last xs)))
      ;; nguyen-4 at x=-1: (-1)^6 + (-1)^5 + (-1)^4 + (-1)^3 + (-1)^2 + (-1) = 1 - 1 + 1 - 1 + 1 - 1 = 0
      (is (< (Math/abs (- (first ys) 0.0)) 1e-10))
      ;; nguyen-4 at x=1: 1 + 1 + 1 + 1 + 1 + 1 = 6
      (is (< (Math/abs (- (last ys) 6.0)) 1e-10)))))
