(ns closyr.symbolic-regression-test
  (:require
    [clojure.core.async :as async :refer [go go-loop timeout <!! >!! <! >! chan put! take! alts!!]]
    [clojure.test :refer :all]
    [closyr.ops :as ops]
    [closyr.ops.common :as ops-common]
    [closyr.ops.initialize :as ops-init]
    [closyr.symbolic-regression :as symreg]))


(deftest can-run-from-cli-args
  (testing "headless"
    (let [args* (atom nil)]
      (binding [ops/*print-top-n* 3]
        (is (=
              (with-redefs-fn {#'symreg/run-ga-iterations (fn [run-config run-args]
                                                            (reset! args*
                                                                    [(dissoc run-config :initial-muts :initial-phenos :input-xs-exprs :input-ys-exprs)
                                                                     (dissoc run-args :extended-domain-args :initial-phenos :input-xs-list)])
                                                            {:next-step :stop})
                               #'symreg/config->log-steps (fn [_ _] 20)}
                (fn []
                  (symreg/run-app-from-cli-args
                    {:iterations     20
                     :population     20
                     :headless       true
                     :xs             [0 1 2]
                     :ys             [1 4 19]
                     :use-flamechart false
                     :max-leafs      20})))
              {:next-step :stop}))

        (is (= @args*
               [{:iters     20
                 :max-leafs 20
                 :use-gui?  false}
                {:input-iters        20
                 :input-phenos-count nil
                 :input-xs-count     3
                 :input-xs-vec       [0.0 1.0 2.0]
                 :input-ys-vec       [1.0 4.0 19.0]
                 :max-leafs          20}]))))))


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
                                             :input-iters        200
                                             :input-phenos-count 500}))

                                  (<! (timeout 1000))
                                  (is (put! symreg/*sim-stop-start-chan*
                                            {:new-state :pause}))
                                  (<! (timeout 1000))
                                  (is (put! symreg/*sim-stop-start-chan*
                                            {:new-state :start}))
                                  (<! (timeout 1000))
                                  (is (put! symreg/*sim-stop-start-chan*
                                            {:new-state :pause}))
                                  (<! (timeout 1000))

                                  (is (put! symreg/*sim-stop-start-chan*
                                            {:new-state          :restart
                                             :input-data-x       [0 1 2 3 4]
                                             :input-data-y       [1 13 16 8 8]
                                             :input-iters        500
                                             :input-phenos-count 500}))

                                  (<! (timeout 1000))

                                  (is (put! symreg/*sim-stop-start-chan*
                                            {:new-state :stop}))

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


(deftest derive-log-steps
  (testing "with basic input"
    (is (=
          (symreg/config->log-steps {:iters 100000 :initial-phenos (vec (repeat 0 10))}
                                    {:input-xs-count 10})
          25))

    (is (=
          (symreg/config->log-steps {:iters 10 :initial-phenos (vec (repeat 0 10))}
                                    {:input-xs-count 10})
          1))))
