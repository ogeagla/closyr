(ns closyr.symbolic-regression-test
  (:require
    [clojure.core.async :as async :refer [go go-loop timeout <!! >!! <! >! chan put! take! alts!!]]
    [clojure.test :refer :all]
    [closyr.ops :as ops]
    [closyr.ops.common :as ops-common]
    [closyr.ops.initialize :as ops-init]
    [closyr.symbolic-regression :as symreg]))


(deftest can-run-experiment

  (binding [ops/*print-top-n* 3]
    (testing "with built-in sample data"
      (is (= (count (:pop
                      (:final-population
                        (with-redefs-fn {#'symreg/config->log-steps (fn [_ _] 10)}
                          (fn []
                            (symreg/run-app-without-gui))))))
             100)))

    (testing "with provided data"
      (with-redefs-fn {#'symreg/config->log-steps (fn [_ _] 10)}
        (fn []
          (let [{:keys [final-population next-step iters-done]}
                (symreg/run-experiment
                  {:input-phenos-count 100
                   :initial-muts       (ops-init/initial-mutations)
                   :iters              5
                   :use-gui?           false
                   :input-xs-exprs     (->> (range 50)
                                            (map (fn [i] (* Math/PI (/ i 15.0))))
                                            ops-common/doubles->exprs)
                   :input-ys-exprs     (->> (range 50)
                                            (map (fn [i]
                                                   (+ 2.0
                                                      (/ i 10.0)
                                                      (Math/cos (* Math/PI (/ i 15.0))))))
                                            ops-common/doubles->exprs)})]
            (is (= (count (:pop final-population))
                   100))

            (is (= iters-done
                   5))))))))


#_(deftest can-run-experiment-gui:start-stop
  (binding [ops/*print-top-n* 3]
    (testing "gui can start and run experiments; NOTE: do not run this while in headless mode, eg on CI"
      (with-redefs-fn {#'symreg/config->log-steps (fn [_ _] 10)}
        (fn []
          (let [control-process (go
                                  (<! (timeout 1000))
                                  (is (put! symreg/*sim-stop-start-chan*
                                            {:new-state          :start
                                             :input-data-x       [0 1 2 3 4]
                                             :input-data-y       [11 3 6 8 8]
                                             :input-iters        5
                                             :input-phenos-count 10}))
                                  (<! (timeout 1000))
                                  (is (put! symreg/*gui-close-chan* :close-please))
                                  (is (put! symreg/*sim->gui-chan* :next))
                                  true)]

            (symreg/run-experiment
              {:initial-phenos (ops-init/initial-phenotypes 20)
               :initial-muts   (ops-init/initial-mutations)
               :input-xs-exprs symreg/input-xs-exprs
               :input-ys-exprs symreg/input-ys-exprs
               :iters          20
               :use-gui?       true})


            (is (= (<!! control-process) true))))))))


(deftest can-run-experiment-gui:start-restart-stop
  (binding [ops/*print-top-n* 3]
    (testing "gui can start and restart experiments; NOTE: do not run this while in headless mode, eg on CI"
      (with-redefs-fn {#'symreg/config->log-steps (fn [_ _] 50)}
        (fn []
          (let [control-process (go
                                  (<! (timeout 1000))

                                  (is (put! symreg/*sim-stop-start-chan*
                                            {:new-state          :start
                                             :input-data-x       [0 1 2 3 4]
                                             :input-data-y       [1 3 6 18 8]
                                             :input-iters        100
                                             :input-phenos-count 2000}))

                                  (<! (timeout 1000))

                                  (is (put! symreg/*sim-stop-start-chan*
                                            {:new-state          :restart
                                             :input-data-x       [0 1 2 3 4]
                                             :input-data-y       [1 13 16 8 8]
                                             :input-iters        500
                                             :input-phenos-count 2000}))

                                  (<! (timeout 1000))

                                  (is (put! symreg/*sim-stop-start-chan*
                                            {:new-state          :stop}))

                                  (<! (timeout 1000))

                                  (is (put! symreg/*gui-close-chan* :close-please))
                                  (is (put! symreg/*sim->gui-chan* :next))
                                  true)]

            (symreg/run-experiment
              {:initial-phenos (ops-init/initial-phenotypes 20)
               :initial-muts   (ops-init/initial-mutations)
               :input-xs-exprs symreg/input-xs-exprs
               :input-ys-exprs symreg/input-ys-exprs
               :iters          20
               :use-gui?       true})


            (is (= (<!! control-process) true))))))))
