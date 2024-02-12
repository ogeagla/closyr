(ns closyr.core-test
  (:require
    [clojure.test :refer :all]
    [closyr.core :as core]
    [closyr.symbolic-regression :as symreg]))


(deftest cli-options-test
  (testing "handles invalid inputs"
    (is (=
          (let [test-input '("-vvvp80000" "foo" "--help" "--invalid-opt" "-i50000" "-y1,1,1" "-x1,1,1,1,1,1,1,1,1")]
            (#'core/validate-symreg-opts (#'core/parse-main-opts test-input)))

          nil)))
  (testing "handles valid short options w data inline"
    (is (=
          (let [test-input
                '("-t" "-p1000" "foo" "-i" "200" "-y" "1,2,30,4,5,6,10" "-x" "0,1,2,3,4,5,6" "-l" "20" "-c"
                       "-g" "debug")]
            (#'core/validate-symreg-opts (#'core/parse-main-opts test-input)))

          {:headless       true
           :log-level      :debug
           :max-leafs      20
           :use-flamechart true
           :iterations     200
           :population     1000
           :xs             [0.0 1.0 2.0 3.0 4.0 5.0 6.0]
           :ys             [1.0 2.0 30.0 4.0 5.0 6.0 10.0]})))


  (testing "if xs, also needs ys"
    (is (=
          (let [test-input '("-t" "-p1000" "-i" "200" "-x" "0,1,2,3,4,5,6")]
            (#'core/validate-symreg-opts (#'core/parse-main-opts test-input)))

          nil)))

  (testing "if xs and ys, need to be same count"
    (is (=
          (let [test-input '("-t" "-p1000" "-i" "200" "-x" "0,1,2,3,4,5,6" "-y" "0,1,2,3,4,5,6,7,8")]
            (#'core/validate-symreg-opts (#'core/parse-main-opts test-input)))

          nil)))


  (testing "if ys, also needs xs"
    (is (=
          (let [test-input '("-t" "-p1000" "-i" "200" "-y" "0,1,2,3,4,5,6")]
            (#'core/validate-symreg-opts (#'core/parse-main-opts test-input)))

          nil)))

  (testing "unrecognized log levels"
    (is (=
          (let [test-input '("-t" "-p1000" "-i" "200" "-g" "wearn")]
            (#'core/validate-symreg-opts (#'core/parse-main-opts test-input)))

          nil)))


  (testing "handles valid short options w csv data"
    (is (=
          (let [test-input '("-t" "-p1000" "foo" "-i" "200" "-f" "resources/csvs/test_inputs_1.csv" "-g" "warn")]
            (#'core/validate-symreg-opts (#'core/parse-main-opts test-input)))

          {:headless       true
           :log-level      :warn
           :max-leafs      40
           :use-flamechart false
           :iterations     200
           :population     1000
           :xs             [0.0 1.0 2.0 3.0 4.0 6.0 15.0 20.0]
           :ys             [1.0 1.0 1.0 2.0 3.0 0.0 -1.0 -12.0]})))

  (testing "handles valid long options w data inline"
    (is (=
          (let [test-input '("--headless" "--population" "1000" "--iterations" "200" "--ys" "1,2,30,4,5,6,10" "--xs" "0,1,2,3,4,5,6" "--use-flamechart" "--max-leafs" "60" "--log-level" "error")]
            (#'core/validate-symreg-opts (#'core/parse-main-opts test-input)))

          {:headless       true
           :log-level      :error
           :max-leafs      60
           :use-flamechart true
           :iterations     200
           :population     1000
           :xs             [0.0 1.0 2.0 3.0 4.0 5.0 6.0]
           :ys             [1.0 2.0 30.0 4.0 5.0 6.0 10.0]})))


  (testing "handles valid long options w csv data with columns"
    (is (=
          (let [test-input '("--headless" "--population" "1000" "--iterations" "200" "-f" "resources/csvs/test_inputs_1.csv")]
            (#'core/validate-symreg-opts (#'core/parse-main-opts test-input)))

          {:headless       true
           :log-level      :info
           :max-leafs      40
           :use-flamechart false
           :iterations     200
           :population     1000
           :xs             [0.0 1.0 2.0 3.0 4.0 6.0 15.0 20.0]
           :ys             [1.0 1.0 1.0 2.0 3.0 0.0 -1.0 -12.0]})))


  (testing "handles valid long options w csv data with columns with in order y,x"
    (is (=
          (let [test-input '("--headless" "--population" "1000" "--iterations" "200" "-f" "resources/csvs/test_inputs_3.csv")]
            (#'core/validate-symreg-opts (#'core/parse-main-opts test-input)))

          {:headless       true
           :log-level      :info
           :max-leafs      40
           :use-flamechart false
           :iterations     200
           :population     1000
           :xs             [0.0 1.0 2.0 3.0 4.0 6.0 15.0 20.0 30.0 45.0 55.0 60.0]
           :ys             [1.0 1.0 1.0 2.0 3.0 0.0 -1.0 -12.0 -22.0 -25.0 -10.0 10.0]})))

  (testing "handles valid long options w csv data without columns"
    (is (=
          (let [test-input '("--headless" "--population" "1000" "--iterations" "200" "-f" "resources/csvs/test_inputs_2.csv")]
            (#'core/validate-symreg-opts (#'core/parse-main-opts test-input)))

          {:headless       true
           :log-level      :info
           :max-leafs      40
           :use-flamechart false
           :iterations     200
           :population     1000
           :xs             [0.0 1.0 2.0 3.0 4.0 6.0 15.0 20.0]
           :ys             [1.0 1.0 1.0 2.0 3.0 0.0 -1.0 -12.0]}))))


(deftest main-test
  (testing "with valid args"
    (let [exited* (atom nil)]
      (is (=
            (with-redefs-fn {#'symreg/config->log-steps (fn [_ _] 100)
                             #'symreg/exit              (fn [] (reset! exited* true))}
              (fn []
                (some->
                  (core/-main "-t" "-p" "20" "-i" "10" "-y" "1,2,30,4,5,6,10" "-x" "0,1,2,3,4,5,6")
                  :final-population
                  :pop
                  count)))
            20))
      (is (true? @exited*)))))
