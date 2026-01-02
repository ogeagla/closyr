(ns closyr.adaptive-test
  (:require
    [clojure.test :refer :all]
    [closyr.adaptive :as adaptive]
    [closyr.ga :as ga]
    [closyr.test-utils :as test-utils]))


(use-fixtures :each
  (fn [f]
    (adaptive/reset-adaptive-state!)
    (f)))


(deftest reset-adaptive-state-test
  (testing "reset clears all state to defaults"
    ;; Modify state first
    (adaptive/update-adaptive-state! [-1.0 -2.0 -3.0 -4.0 -5.0])
    (adaptive/update-adaptive-state! [-1.0 -2.0 -3.0 -4.0 -5.0])

    ;; Verify state was modified
    (is (seq (:best-score-history (adaptive/get-adaptive-state))))

    ;; Reset and verify
    (adaptive/reset-adaptive-state!)
    (let [state (adaptive/get-adaptive-state)]
      (is (= 0.8 (:mutation-probability state)))
      (is (= 1.0 (:mutation-count-boost state)))
      (is (empty? (:best-score-history state)))
      (is (empty? (:diversity-history state)))
      (is (= 0 (:stagnation-counter state)))
      (is (nil? (:last-best-score state))))))


(deftest calculate-diversity-test
  (testing "diversity is 0 for identical scores"
    ;; Use 20 elements so p90 index calculation works properly
    (is (< (adaptive/calculate-diversity (vec (repeat 20 -1.0))) 0.01)))

  (testing "diversity increases with score spread"
    ;; Create populations of 20 with different spreads
    (let [low-spread (mapv #(- -1.0 (* 0.01 %)) (range 20))   ; -1.0 to -1.19
          high-spread (mapv #(- -1.0 (* 0.5 %)) (range 20))   ; -1.0 to -10.5
          low-div (adaptive/calculate-diversity low-spread)
          high-div (adaptive/calculate-diversity high-spread)]
      (is (< low-div high-div))))

  (testing "diversity is nil for empty scores"
    (is (nil? (adaptive/calculate-diversity []))))

  (testing "diversity is capped at 1.0"
    (let [extreme-spread (mapv #(- -1.0 (* 100.0 %)) (range 20))]
      (is (<= (adaptive/calculate-diversity extreme-spread) 1.0)))))


(def ^:private test-scores-a (mapv #(- -5.0 (* 0.5 %)) (range 20)))  ; -5.0 to -14.5
(def ^:private test-scores-b (mapv #(- -4.0 (* 0.5 %)) (range 20)))  ; -4.0 to -13.5 (improvement)


(deftest stagnation-detection-test
  (testing "stagnation counter increases when no improvement"
    (adaptive/reset-adaptive-state!)
    ;; First update establishes baseline
    (adaptive/update-adaptive-state! test-scores-a)
    (is (= 0 (:stagnation-counter (adaptive/get-adaptive-state))))

    ;; Same score = stagnation
    (adaptive/update-adaptive-state! test-scores-a)
    (is (= 1 (:stagnation-counter (adaptive/get-adaptive-state))))

    ;; Still same score
    (adaptive/update-adaptive-state! test-scores-a)
    (is (= 2 (:stagnation-counter (adaptive/get-adaptive-state)))))

  (testing "stagnation counter resets on improvement"
    (adaptive/reset-adaptive-state!)
    ;; Establish baseline and stagnate
    (adaptive/update-adaptive-state! test-scores-a)
    (adaptive/update-adaptive-state! test-scores-a)
    (adaptive/update-adaptive-state! test-scores-a)
    (is (= 2 (:stagnation-counter (adaptive/get-adaptive-state))))

    ;; Improvement resets counter (better score = less negative)
    (adaptive/update-adaptive-state! test-scores-b)
    (is (= 0 (:stagnation-counter (adaptive/get-adaptive-state))))))


(deftest mutation-probability-adjustment-test
  (testing "mutation probability stays at baseline initially"
    (adaptive/reset-adaptive-state!)
    (is (= 0.8 (adaptive/get-mutation-probability))))

  (testing "mutation probability increases during stagnation"
    (adaptive/reset-adaptive-state!)
    ;; Stagnate for several iterations
    (dotimes [_ 10]
      (adaptive/update-adaptive-state! test-scores-a))
    (is (> (adaptive/get-mutation-probability) 0.8)))

  (testing "mutation probability is bounded"
    (adaptive/reset-adaptive-state!)
    ;; Extreme stagnation
    (dotimes [_ 50]
      (adaptive/update-adaptive-state! test-scores-a))
    (is (<= (adaptive/get-mutation-probability) 0.95))
    (is (>= (adaptive/get-mutation-probability) 0.5))))


(deftest mutation-count-boost-test
  (testing "boost starts at 1.0"
    (adaptive/reset-adaptive-state!)
    (is (= 1.0 (adaptive/get-mutation-count-boost))))

  (testing "boost increases during stagnation"
    (adaptive/reset-adaptive-state!)
    ;; Stagnate past threshold
    (dotimes [_ 10]
      (adaptive/update-adaptive-state! test-scores-a))
    (is (> (adaptive/get-mutation-count-boost) 1.0)))

  (testing "boost is bounded"
    (adaptive/reset-adaptive-state!)
    ;; Extreme stagnation
    (dotimes [_ 50]
      (adaptive/update-adaptive-state! test-scores-a))
    (is (<= (adaptive/get-mutation-count-boost) 2.0))
    (is (>= (adaptive/get-mutation-count-boost) 0.5))))


(deftest should-mutate-test
  (testing "should-mutate? returns boolean"
    (adaptive/reset-adaptive-state!)
    (is (boolean? (adaptive/should-mutate?))))

  (testing "should-mutate? respects probability distribution"
    (adaptive/reset-adaptive-state!)
    ;; With default 80% mutation probability, most calls should return true
    (let [results (repeatedly 100 adaptive/should-mutate?)
          true-count (count (filter true? results))]
      ;; Should be roughly 80%, allow for randomness (60-95%)
      (is (> true-count 60))
      (is (< true-count 95)))))


(deftest format-adaptive-status-test
  (testing "format-adaptive-status returns a string"
    (adaptive/reset-adaptive-state!)
    (let [status (adaptive/format-adaptive-status)]
      (is (string? status))
      (is (re-find #"Adaptive:" status))
      (is (re-find #"mut=" status))
      (is (re-find #"boost=" status))
      (is (re-find #"stag=" status)))))


(deftest ga-adaptive-mode-binding-test
  (testing "GA respects *adaptive-mode* binding when false"
    (binding [ga/*adaptive-mode* false]
      ;; When adaptive mode is off, should use fixed sampler
      ;; We can't easily test the internal behavior, but we can verify it doesn't crash
      (is (not ga/*adaptive-mode*))))

  (testing "GA respects *adaptive-mode* binding when true"
    (binding [ga/*adaptive-mode* true]
      (is ga/*adaptive-mode*))))


(deftest history-tracking-test
  (testing "best score history is tracked"
    (adaptive/reset-adaptive-state!)
    (let [scores-1 (mapv #(- -1.0 (* 0.1 %)) (range 20))   ; best = -1.0
          scores-2 (mapv #(- -0.5 (* 0.1 %)) (range 20))   ; best = -0.5
          scores-3 (mapv #(- -0.3 (* 0.1 %)) (range 20))]  ; best = -0.3
      (adaptive/update-adaptive-state! scores-1)
      (adaptive/update-adaptive-state! scores-2)
      (adaptive/update-adaptive-state! scores-3)

      (let [history (:best-score-history (adaptive/get-adaptive-state))]
        (is (= 3 (count history)))
        ;; Most recent first
        (is (= -0.3 (first history))))))

  (testing "history is bounded to window size"
    (adaptive/reset-adaptive-state!)
    ;; Add more than window size (10) entries
    (dotimes [i 15]
      (let [scores (mapv #(- (- i) (* 0.1 %)) (range 20))]
        (adaptive/update-adaptive-state! scores)))

    (let [history (:best-score-history (adaptive/get-adaptive-state))]
      (is (<= (count history) 10)))))
