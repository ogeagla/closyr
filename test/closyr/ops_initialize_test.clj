(ns closyr.ops-initialize-test
  "Tests for ops/initialize namespace, including mutation filtering"
  (:require
    [clojure.test :refer :all]
    [closyr.ops.initialize :as ops-init]
    [closyr.test-utils :as test-utils]))


(use-fixtures :once test-utils/quiet-logging-fixture)


(deftest test-mutation-labels
  (testing "mutation-labels returns a vector of strings"
    (let [labels (ops-init/mutation-labels)]
      (is (vector? labels))
      (is (every? string? labels))
      (is (pos? (count labels)))
      ;; Check some known mutation labels exist
      (is (some #(= "+Sin" %) labels))
      (is (some #(= "*x" %) labels))
      (is (some #(= "Derivative" %) labels)))))


(deftest test-filter-mutations-whitelist
  (testing "whitelist filters to only specified mutations"
    (let [whitelist ["+Sin" "-Sin" "+Cos"]
          filtered (ops-init/filter-mutations {:whitelist whitelist})]
      (is (= 3 (count filtered)))
      (is (= (set whitelist) (set (map :label filtered))))))

  (testing "whitelist with single mutation"
    (let [filtered (ops-init/filter-mutations {:whitelist ["Derivative"]})]
      (is (= 1 (count filtered)))
      (is (= "Derivative" (:label (first filtered)))))))


(deftest test-filter-mutations-blacklist
  (testing "blacklist excludes specified mutations"
    (let [all-count (count (ops-init/initial-mutations))
          blacklist ["Derivative" "+Sin" "-Sin"]
          filtered (ops-init/filter-mutations {:blacklist blacklist})]
      (is (= (- all-count 3) (count filtered)))
      (is (not-any? #(contains? (set blacklist) (:label %)) filtered))))

  (testing "blacklist with single mutation"
    (let [all-count (count (ops-init/initial-mutations))
          filtered (ops-init/filter-mutations {:blacklist ["Derivative"]})]
      (is (= (dec all-count) (count filtered)))
      (is (not-any? #(= "Derivative" (:label %)) filtered)))))


(deftest test-filter-mutations-whitelist-and-blacklist
  (testing "whitelist applied first, then blacklist"
    (let [whitelist ["+Sin" "-Sin" "+Cos" "-Cos"]
          blacklist ["+Sin" "+Cos"]
          filtered (ops-init/filter-mutations {:whitelist whitelist
                                               :blacklist blacklist})]
      ;; Should have only -Sin and -Cos (whitelist minus blacklist)
      (is (= 2 (count filtered)))
      (is (= #{"-Sin" "-Cos"} (set (map :label filtered)))))))


(deftest test-filter-mutations-empty-result-throws
  (testing "throws exception when filtering leaves no mutations"
    (is (thrown-with-msg? IllegalArgumentException
                          #"No mutations remaining"
                          (ops-init/filter-mutations {:whitelist ["nonexistent"]})))

    (is (thrown-with-msg? IllegalArgumentException
                          #"No mutations remaining"
                          (ops-init/filter-mutations {:whitelist ["+Sin"]
                                                      :blacklist ["+Sin"]})))))


(deftest test-filter-mutations-no-filters
  (testing "no filters returns all mutations"
    (let [all-muts (ops-init/initial-mutations)
          filtered (ops-init/filter-mutations {})]
      (is (= (count all-muts) (count filtered))))))
