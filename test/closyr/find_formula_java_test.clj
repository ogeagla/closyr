(ns closyr.find-formula-java-test
  "Tests for the Java FindFormula API that calls into Clojure symbolic regression."
  (:require
    [clojure.test :refer :all]
    [closyr.test-utils :as test-utils])
  (:import
    (org.closyr.core
      FindFormula
      FindFormula$Config
      FindFormula$Result
      FindFormula$Solution)))


(use-fixtures :once test-utils/quiet-logging-fixture)


(deftest test-find-formula-with-linear-data
  (testing "Simple linear relationship: y = 2x"
    (let [xs (double-array [1.0 2.0 3.0 4.0 5.0])
          ys (double-array [2.0 4.0 6.0 8.0 10.0])
          config (-> (FindFormula$Config.)
                     (.iterations 5)
                     (.populationSize 30))
          result (FindFormula/findFormula xs ys config)]
      (is (instance? FindFormula$Result result))
      (is (string? (.getFormulaString result)))
      (is (not (empty? (.getFormulaString result))))
      (is (number? (.getScore result)))
      (is (pos? (.getIterationsDone result)))
      (is (seq (.getAllSolutions result))))))


(deftest test-find-formula-with-default-config
  (testing "Using default configuration"
    (let [xs (double-array [0.0 1.0 2.0 3.0])
          ys (double-array [1.0 2.0 5.0 10.0])
          result (FindFormula/findFormula xs ys)]
      (is (instance? FindFormula$Result result))
      (is (= 20 (.getIterationsDone result))) ; default iterations
      (is (string? (.getFormulaString result))))))


(deftest test-result-contains-symja-expr
  (testing "Result contains a Symja IExpr for symbolic computation"
    (let [xs (double-array [1.0 2.0 3.0])
          ys (double-array [3.0 6.0 9.0])
          config (-> (FindFormula$Config.)
                     (.iterations 3)
                     (.populationSize 20))
          result (FindFormula/findFormula xs ys config)
          formula-expr (.getFormulaExpr result)]
      (is (some? formula-expr) "Formula IExpr should not be null")
      (is (string? (.toString formula-expr)))
      (is (not (empty? (.toString formula-expr)))))))


(deftest test-all-solutions-sorted-by-score
  (testing "Solutions are sorted by score (best first)"
    (let [xs (double-array [1.0 2.0 3.0 4.0])
          ys (double-array [2.0 4.0 8.0 16.0])
          config (-> (FindFormula$Config.)
                     (.iterations 3)
                     (.populationSize 30))
          result (FindFormula/findFormula xs ys config)
          solutions (.getAllSolutions result)]
      (is (seq solutions))
      ;; Verify sorted by score descending (higher/closer to 0 is better)
      (doseq [[prev curr] (partition 2 1 solutions)]
        (is (>= (.getScore prev) (.getScore curr))
            "Solutions should be sorted by score descending"))
      ;; Best solution should match result's formula
      (is (= (.getFormulaString result)
             (.getFormulaString (first solutions))))
      (is (= (.getScore result)
             (.getScore (first solutions)))))))


(deftest test-null-xs-throws-exception
  (testing "Null xs array throws IllegalArgumentException"
    (let [ys (double-array [1.0 2.0 3.0])]
      (is (thrown-with-msg? IllegalArgumentException #"null"
                            (FindFormula/findFormula nil ys))))))


(deftest test-null-ys-throws-exception
  (testing "Null ys array throws IllegalArgumentException"
    (let [xs (double-array [1.0 2.0 3.0])]
      (is (thrown-with-msg? IllegalArgumentException #"null"
                            (FindFormula/findFormula xs nil))))))


(deftest test-mismatched-array-lengths-throws-exception
  (testing "Mismatched array lengths throws IllegalArgumentException"
    (let [xs (double-array [1.0 2.0 3.0])
          ys (double-array [1.0 2.0])]
      (is (thrown-with-msg? IllegalArgumentException #"same length"
                            (FindFormula/findFormula xs ys))))))


(deftest test-too-few-data-points-throws-exception
  (testing "Less than 2 data points throws IllegalArgumentException"
    (let [xs (double-array [1.0])
          ys (double-array [2.0])]
      (is (thrown-with-msg? IllegalArgumentException #"At least 2"
                            (FindFormula/findFormula xs ys))))))


(deftest test-config-builder
  (testing "Config builder works correctly"
    (let [config (-> (FindFormula$Config.)
                     (.iterations 50)
                     (.populationSize 200)
                     (.maxLeafs 30))]
      (is (= 50 (.getIterations config)))
      (is (= 200 (.getPopulationSize config)))
      (is (= 30 (.getMaxLeafs config))))))


(deftest test-result-to-string
  (testing "Result toString includes expected fields"
    (let [xs (double-array [1.0 2.0 3.0])
          ys (double-array [2.0 4.0 6.0])
          config (-> (FindFormula$Config.)
                     (.iterations 2)
                     (.populationSize 10))
          result (FindFormula/findFormula xs ys config)
          str-repr (.toString result)]
      (is (string? str-repr))
      (is (.contains str-repr "Result{"))
      (is (.contains str-repr "formula="))
      (is (.contains str-repr "score="))
      (is (.contains str-repr "iterations=")))))


(deftest test-solution-to-string
  (testing "Solution toString includes expected fields"
    (let [xs (double-array [1.0 2.0 3.0])
          ys (double-array [1.0 4.0 9.0])
          config (-> (FindFormula$Config.)
                     (.iterations 2)
                     (.populationSize 10))
          result (FindFormula/findFormula xs ys config)
          solution (first (.getAllSolutions result))
          str-repr (.toString solution)]
      (is (string? str-repr))
      (is (.contains str-repr "Solution{"))
      (is (.contains str-repr "formula="))
      (is (.contains str-repr "score=")))))


(deftest test-multiple-calls-work
  (testing "Multiple calls to findFormula work correctly"
    ;; Verify the API works for multiple sequential calls
    (let [xs (double-array [1.0 2.0])
          ys (double-array [1.0 2.0])
          config (-> (FindFormula$Config.)
                     (.iterations 1)
                     (.populationSize 5))
          result1 (FindFormula/findFormula xs ys config)
          result2 (FindFormula/findFormula xs ys config)]
      (is (some? result1))
      (is (some? result2)))))
