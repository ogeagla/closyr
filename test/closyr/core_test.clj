(ns closyr.core-test
  (:require
    [clojure.test :refer :all]
    [closyr.core :refer :all]))


(deftest cli-options-test
  (testing "handles invalid inputs"
    (is (=
          (let [test-input '("-vvvp80000" "foo" "--help" "--invalid-opt" "-i50000" "-y1,1,1" "-x1,1,1,1,1,1,1,1,1")]
            (validate-symreg-opts (parse-main-opts test-input)))

          nil)))
  (testing "handles valid short options w data inline"
    (is (=
          (let [test-input '("-t" "-p1000" "foo" "-i" "200" "-y" "1,2,30,4,5,6,10" "-x" "0,1,2,3,4,5,6")]
            (validate-symreg-opts (parse-main-opts test-input)))

          {:headless   true
           :iterations 200
           :population 1000
           :xs         [0.0 1.0 2.0 3.0 4.0 5.0 6.0]
           :ys         [1.0 2.0 30.0 4.0 5.0 6.0 10.0]})))


  (testing "handles valid short options w csv data"
    (is (=
          (let [test-input '("-t" "-p1000" "foo" "-i" "200" "-f" "resources/csvs/test_inputs_1.csv")]
            (validate-symreg-opts (parse-main-opts test-input)))

          {:headless   true
           :iterations 200
           :population 1000
           :xs         [0.0 1.0 2.0 3.0 4.0 6.0 15.0 20.0]
           :ys         [1.0 1.0 1.0 2.0 3.0 0.0 -1.0 -12.0]})))

  (testing "handles valid long options w data inline"
    (is (=
          (let [test-input '("--headless" "--population" "1000" "--iterations" "200" "--ys" "1,2,30,4,5,6,10" "--xs" "0,1,2,3,4,5,6")]
            (validate-symreg-opts (parse-main-opts test-input)))

          {:headless   true
           :iterations 200
           :population 1000
           :xs         [0.0 1.0 2.0 3.0 4.0 5.0 6.0]
           :ys         [1.0 2.0 30.0 4.0 5.0 6.0 10.0]})))


  (testing "handles valid long options w csv data"
    (is (=
          (let [test-input '("--headless" "--population" "1000" "--iterations" "200" "-f" "resources/csvs/test_inputs_1.csv")]
            (validate-symreg-opts (parse-main-opts test-input)))

          {:headless   true
           :iterations 200
           :population 1000
           :xs         [0.0 1.0 2.0 3.0 4.0 6.0 15.0 20.0]
           :ys         [1.0 1.0 1.0 2.0 3.0 0.0 -1.0 -12.0]}))))
