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
      (is (= "-1/100+Sin(x)+x*(x+1/50*x*Csc(x)*(-1/100+E^(2*(-1/10+E)^x)-11/10*Sin(121.0*x)))"
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
      (is (= "-9601/10000"
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
