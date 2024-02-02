(ns closyr.symreg-test
  (:require
    [clojure.test :refer :all]
    [closyr.ops.common :as ops-common]
    [closyr.ops.initialize :as ops-init]
    [closyr.symreg :as symreg]))


(deftest can-run-experiment

  (testing "with built-in sample data"
    (is (= nil
           (binding [symreg/*log-steps* 10]
             (symreg/run-app-without-gui)))))

  (testing "with provided data"
    (is (= nil
           (binding [symreg/*log-steps* 5]
             (symreg/run-with-monitoring
               (fn []
                 (symreg/run-experiment
                   {:initial-phenos (ops-init/initial-phenotypes 10)
                    :initial-muts   (ops-init/initial-mutations)
                    :iters          5
                    :use-gui?       false
                    :input-exprs    (->> (range 50)
                                         (map (fn [i] (* Math/PI (/ i 15.0))))
                                         ops-common/doubles->exprs)
                    :output-exprs   (->> (range 50)
                                         (map (fn [i]
                                                (+ 2.0
                                                   (/ i 10.0)
                                                   (Math/cos (* Math/PI (/ i 15.0))))))
                                         ops-common/doubles->exprs)}))))))))


;; running this hangs the test with the GUI open:
#_(deftest can-run-experiment-gui
    (testing "gui can start"
      (is (= nil
             (symreg/run-app-with-gui)))))
