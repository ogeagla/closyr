(ns closyr.api-test
  "Tests for the Java-friendly API classes."
  (:require
    [clojure.test :refer :all]
    [closyr.api.types :as types]
    [closyr.api.finder :as finder])
  (:import
    (org.closyr.api
      FormulaConfigBuilder
      FormulaFinder
      IFormulaConfig
      IFormulaFinder
      IFormulaResult
      IFormulaSolution)))


;; ============================================================================
;; FormulaConfigBuilder tests
;; ============================================================================

(deftest test-config-builder-defaults
  (testing "Builder creates config with default values"
    (let [config (.build (FormulaConfigBuilder/builder))]
      (is (instance? IFormulaConfig config))
      (is (= 20 (.getIterations config)))
      (is (= 100 (.getPopulationSize config)))
      (is (= 40 (.getMaxLeafs config))))))


(deftest test-config-builder-custom-values
  (testing "Builder accepts custom values"
    (let [config (-> (FormulaConfigBuilder/builder)
                     (.iterations 50)
                     (.populationSize 200)
                     (.maxLeafs 30)
                     (.build))]
      (is (= 50 (.getIterations config)))
      (is (= 200 (.getPopulationSize config)))
      (is (= 30 (.getMaxLeafs config))))))


;; ============================================================================
;; Clojure types tests
;; ============================================================================

(deftest test-clojure-config
  (testing "Clojure config function creates valid IFormulaConfig"
    (let [config (types/config {:iterations 10 :population-size 50 :max-leafs 25})]
      (is (instance? IFormulaConfig config))
      (is (= 10 (.getIterations config)))
      (is (= 50 (.getPopulationSize config)))
      (is (= 25 (.getMaxLeafs config))))))


(deftest test-clojure-config-defaults
  (testing "Clojure config uses defaults when not specified"
    (let [config (types/config)]
      (is (= 20 (.getIterations config)))
      (is (= 100 (.getPopulationSize config)))
      (is (= 40 (.getMaxLeafs config))))))


;; ============================================================================
;; FormulaFinder static methods
;; ============================================================================

(deftest test-formula-finder-static-find
  (testing "FormulaFinder.find() static method works"
    (let [xs (double-array [1.0 2.0 3.0 4.0 5.0])
          ys (double-array [2.0 4.0 6.0 8.0 10.0])
          config (-> (FormulaConfigBuilder/builder)
                     (.iterations 3)
                     (.populationSize 20)
                     (.build))
          result (FormulaFinder/find xs ys config)]
      (is (instance? IFormulaResult result))
      (is (pos? (.getIterationsDone result)))
      (is (some? (.getBestSolution result)))
      (is (string? (.getBestFormula result)))
      (is (number? (.getBestScore result))))))


(deftest test-formula-finder-static-find-default-config
  (testing "FormulaFinder.find() with default config"
    (let [xs (double-array [1.0 2.0 3.0])
          ys (double-array [1.0 4.0 9.0])
          result (FormulaFinder/find xs ys)]
      (is (instance? IFormulaResult result))
      (is (= 20 (.getIterationsDone result))))))


;; ============================================================================
;; FormulaFinder instance methods
;; ============================================================================

(deftest test-formula-finder-instance
  (testing "FormulaFinder instance implements IFormulaFinder"
    (let [finder (FormulaFinder/create)]
      (is (instance? IFormulaFinder finder)))))


(deftest test-formula-finder-instance-find
  (testing "FormulaFinder instance findFormula method"
    (let [finder (FormulaFinder/create)
          xs (double-array [1.0 2.0 3.0])
          ys (double-array [2.0 4.0 6.0])
          config (-> (FormulaConfigBuilder/builder)
                     (.iterations 2)
                     (.populationSize 10)
                     (.build))
          result (.findFormula finder xs ys config)]
      (is (instance? IFormulaResult result))
      (is (some? (.getBestSolution result))))))


;; ============================================================================
;; IFormulaSolution tests
;; ============================================================================

(deftest test-formula-solution-properties
  (testing "IFormulaSolution has expected properties"
    (let [xs (double-array [1.0 2.0 3.0])
          ys (double-array [3.0 6.0 9.0])
          config (-> (FormulaConfigBuilder/builder)
                     (.iterations 2)
                     (.populationSize 10)
                     (.build))
          result (FormulaFinder/find xs ys config)
          best (.getBestSolution result)]
      (is (instance? IFormulaSolution best))
      (is (string? (.getFormula best)))
      (is (not (empty? (.getFormula best))))
      (is (number? (.getScore best)))
      (is (>= (.getLeafCount best) 0)))))


(deftest test-all-solutions-sorted
  (testing "getAllSolutions returns solutions sorted by score"
    (let [xs (double-array [1.0 2.0 3.0 4.0])
          ys (double-array [1.0 4.0 9.0 16.0])
          config (-> (FormulaConfigBuilder/builder)
                     (.iterations 3)
                     (.populationSize 30)
                     (.build))
          result (FormulaFinder/find xs ys config)
          solutions (.getAllSolutions result)]
      (is (seq solutions))
      ;; Check sorted descending by score
      (doseq [[prev curr] (partition 2 1 solutions)]
        (is (>= (.getScore prev) (.getScore curr))
            "Solutions should be sorted by score descending")))))


;; ============================================================================
;; Validation tests
;; ============================================================================

(deftest test-null-xs-throws
  (testing "Null xs throws IllegalArgumentException"
    (let [ys (double-array [1.0 2.0 3.0])]
      (is (thrown-with-msg? IllegalArgumentException #"null"
                            (FormulaFinder/find nil ys))))))


(deftest test-null-ys-throws
  (testing "Null ys throws IllegalArgumentException"
    (let [xs (double-array [1.0 2.0 3.0])]
      (is (thrown-with-msg? IllegalArgumentException #"null"
                            (FormulaFinder/find xs nil))))))


(deftest test-mismatched-lengths-throws
  (testing "Mismatched array lengths throws IllegalArgumentException"
    (let [xs (double-array [1.0 2.0 3.0])
          ys (double-array [1.0 2.0])]
      (is (thrown-with-msg? IllegalArgumentException #"same length"
                            (FormulaFinder/find xs ys))))))


(deftest test-too-few-points-throws
  (testing "Less than 2 data points throws IllegalArgumentException"
    (let [xs (double-array [1.0])
          ys (double-array [2.0])]
      (is (thrown-with-msg? IllegalArgumentException #"At least 2"
                            (FormulaFinder/find xs ys))))))


;; ============================================================================
;; toString tests
;; ============================================================================

(deftest test-result-to-string
  (testing "Result toString contains expected info"
    (let [xs (double-array [1.0 2.0 3.0])
          ys (double-array [2.0 4.0 6.0])
          config (-> (FormulaConfigBuilder/builder)
                     (.iterations 1)
                     (.populationSize 5)
                     (.build))
          result (FormulaFinder/find xs ys config)
          str-repr (.toString result)]
      (is (.contains str-repr "FormulaResult"))
      (is (.contains str-repr "bestFormula")))))


(deftest test-solution-to-string
  (testing "Solution toString contains expected info"
    (let [xs (double-array [1.0 2.0 3.0])
          ys (double-array [1.0 4.0 9.0])
          config (-> (FormulaConfigBuilder/builder)
                     (.iterations 1)
                     (.populationSize 5)
                     (.build))
          result (FormulaFinder/find xs ys config)
          solution (.getBestSolution result)
          str-repr (.toString solution)]
      (is (.contains str-repr "FormulaSolution"))
      (is (.contains str-repr "formula")))))


;; ============================================================================
;; Deterministic seed tests
;; ============================================================================

(deftest test-random-seed-produces-deterministic-results
  (testing "Running solver with same seed produces identical results"
    (let [xs (double-array [1.0 2.0 3.0 4.0 5.0])
          ys (double-array [2.0 4.0 6.0 8.0 10.0])
          seed 42
          config (-> (FormulaConfigBuilder/builder)
                     (.iterations 3)
                     (.populationSize 20)
                     (.randomSeed seed)
                     (.build))
          ;; Run solver twice with same seed
          result1 (FormulaFinder/find xs ys config)
          result2 (FormulaFinder/find xs ys config)
          best1 (.getBestSolution result1)
          best2 (.getBestSolution result2)]
      ;; Results should be identical
      (is (= (.getFormula best1) (.getFormula best2))
          "Same seed should produce identical formulas")
      (is (= (.getScore best1) (.getScore best2))
          "Same seed should produce identical scores")
      (is (= (.getLeafCount best1) (.getLeafCount best2))
          "Same seed should produce identical leaf counts"))))


(deftest test-different-seeds-produce-different-results
  (testing "Running solver with different seeds produces different results"
    (let [xs (double-array [1.0 2.0 3.0 4.0 5.0])
          ys (double-array [1.0 4.0 9.0 16.0 25.0])
          config1 (-> (FormulaConfigBuilder/builder)
                      (.iterations 3)
                      (.populationSize 20)
                      (.randomSeed 123)
                      (.build))
          config2 (-> (FormulaConfigBuilder/builder)
                      (.iterations 3)
                      (.populationSize 20)
                      (.randomSeed 456)
                      (.build))
          result1 (FormulaFinder/find xs ys config1)
          result2 (FormulaFinder/find xs ys config2)
          best1 (.getBestSolution result1)
          best2 (.getBestSolution result2)
          ;; Get all solution formulas to compare
          all-formulas1 (set (map #(.getFormula %) (.getAllSolutions result1)))
          all-formulas2 (set (map #(.getFormula %) (.getAllSolutions result2)))]
      ;; The full population should differ between runs with different seeds
      (is (not= all-formulas1 all-formulas2)
          "Different seeds should produce different populations"))))


(deftest test-config-builder-with-seed
  (testing "FormulaConfigBuilder accepts random seed"
    (let [config (-> (FormulaConfigBuilder/builder)
                     (.randomSeed 12345)
                     (.build))]
      (is (= 12345 (.getRandomSeed config))))))


(deftest test-clojure-config-with-seed
  (testing "Clojure config accepts random seed"
    (let [config (types/config {:random-seed 98765})]
      (is (= 98765 (.getRandomSeed config))))))
