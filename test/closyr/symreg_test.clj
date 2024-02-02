(ns closyr.symreg-test
  (:require
    [clojure.core.async :as async :refer [go go-loop timeout <!! >!! <! >! chan put! take! alts!!]]
    [clojure.test :refer :all]
    [closyr.ops.common :as ops-common]
    [closyr.ops.initialize :as ops-init]
    [closyr.symreg :as symreg]))


(deftest can-run-experiment

  (testing "with built-in sample data"
    (is (= (binding [symreg/*log-steps* 10]
             (symreg/run-app-without-gui))
           nil)))

  (testing "with provided data"
    (is (= (binding [symreg/*log-steps* 5]
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
                                         ops-common/doubles->exprs)}))))
           nil))))


(deftest can-run-experiment-gui
  (testing "gui can start and run experiments; NOTE: do not run this while in headless mode, eg on CI"
    (is (=
          (binding [symreg/*log-steps* 5]
            (let [control-process (go
                                    (<! (timeout 1000))
                                    (println "Start sim")
                                    (put! symreg/*sim-stop-start-chan*
                                          {:new-state          true
                                           :reset              false
                                           :input-data-x       [0 1 2 3 4]
                                           :input-data-y       [1 3 6 8 8]
                                           :input-iters        5
                                           :input-phenos-count 10})
                                    (<! (timeout 1500))
                                    (is true)
                                    (put! symreg/*gui-close-chan* :close-please)
                                    (put! symreg/*sim->gui-chan* :next))]

              (symreg/run-with-monitoring
                (fn []
                  (symreg/run-experiment
                    {:initial-phenos (ops-init/initial-phenotypes 20)
                     :initial-muts   (ops-init/initial-mutations)
                     :input-exprs    symreg/input-exprs
                     :output-exprs   symreg/output-exprs
                     :iters          20
                     :use-gui?       true})))

              (<!! control-process)))

          true))))
