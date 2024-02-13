(ns closyr.symbolic-regression-test
  (:require
    [clojure.core.async :as async :refer [go go-loop timeout <!! >!! <! >! chan put! take! alts!!]]
    [clojure.test :refer :all]
    [closyr.ops :as ops]
    [closyr.ops.common :as ops-common]
    [closyr.ops.initialize :as ops-init]
    [closyr.symbolic-regression :as symreg]))


(deftest end-iters-if-solution-found-test
  (testing "not solved"
    (is (=
          (#'symreg/next-iters 10 [-10.0])
          9)))
  (testing "an exact solution"
    (is (=
          (#'symreg/next-iters 10 [0.0])
          0))))


(deftest can-run-from-cli-args
  (testing "args from CLI are passed along correctly to implementation"
    (let [args* (atom nil)]
      (binding [ops/*print-top-n* 1]
        (is (=
              (with-redefs-fn {#'symreg/run-ga-iterations (fn [run-config run-args]
                                                            (reset! args*
                                                                    [(dissoc run-config :initial-muts :initial-phenos :input-xs-exprs :input-ys-exprs)
                                                                     (dissoc run-args :extended-domain-args :initial-phenos :input-xs-list)])
                                                            {:next-step :stop})
                               #'symreg/config->log-steps (fn [_ _] 200)}
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
                 :max-leafs          20}])))))


  (testing "args from CLI are passed along correctly to implementation, and when none are passed we use defaults"
    (let [args* (atom nil)]
      (binding [ops/*print-top-n* 1]
        (is (=
              (with-redefs-fn {#'symreg/run-ga-iterations (fn [run-config run-args]
                                                            (reset! args*
                                                                    [(dissoc run-config :initial-muts :initial-phenos :input-xs-exprs :input-ys-exprs)
                                                                     (dissoc run-args :extended-domain-args :initial-phenos :input-xs-list)])
                                                            {:next-step :stop})
                               #'symreg/config->log-steps (fn [_ _] 200)}
                (fn []
                  (symreg/run-app-from-cli-args
                    {:population 30
                     :iterations 20
                     :headless   true})))
              {:next-step :stop}))

        (is (= @args*
               [{:iters     20
                 :max-leafs 40
                 :use-gui?  false}
                {:input-iters        20
                 :input-phenos-count nil
                 :input-xs-count     50
                 :input-xs-vec       [0.0 0.20943951023931953 0.41887902047863906 0.6283185307179586 0.8377580409572781 1.0471975511965976 1.2566370614359172 1.4660765716752369 1.6755160819145563 1.8849555921538759 2.0943951023931953 2.3038346126325147 2.5132741228718345 2.7227136331111543 2.9321531433504737 3.141592653589793 3.3510321638291125 3.560471674068432 3.7699111843077517 3.979350694547071 4.1887902047863905 4.39822971502571 4.607669225265029 4.81710873550435 5.026548245743669 5.235987755982989 5.445427266222309 5.654866776461628 5.8643062867009474 6.073745796940266 6.283185307179586 6.492624817418906 6.702064327658225 6.911503837897546 7.120943348136864 7.3303828583761845 7.5398223686155035 7.749261878854823 7.958701389094142 8.168140899333462 8.377580409572781 8.587019919812102 8.79645943005142 9.00589894029074 9.215338450530059 9.42477796076938 9.6342174710087 9.843656981248019 10.053096491487338 10.262536001726657]
                 :input-ys-vec       [0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0]
                 :max-leafs          nil}]))))))


(deftest can-run-experiment

  (binding [ops/*print-top-n* 1]
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
                   5))))))

    (testing "with provided data using record"
      (with-redefs-fn {#'symreg/config->log-steps (fn [_ _] 10)}
        (fn []
          (let [{:keys [final-population next-step iters-done]}
                (symreg/solve
                  (symreg/map->SymbolicRegressionSolver
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
                                              ops-common/doubles->exprs)}))]
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
                 :input-xs-exprs symreg/example-input-xs-exprs
                 :input-ys-exprs symreg/example-input-ys-exprs
                 :iters          20
                 :use-gui?       true})


              (is (= (<!! control-process) true))))))))


(deftest can-run-experiment-gui:start-restart-stop
  (binding [ops/*print-top-n* 1]
    (testing "gui can start and restart experiments; NOTE: do not run this while in headless mode, eg on CI"
      (with-redefs-fn {#'symreg/config->log-steps (fn [_ _] 500)}
        (fn []
          (let [control-process (go
                                  (<! (timeout 200))

                                  (is (put! symreg/*sim-stop-start-chan*
                                            {:new-state          :start
                                             :input-data-x       [0 1 2 3 4]
                                             :input-data-y       [1 3 6 18 8]
                                             :input-iters        200
                                             :input-phenos-count 500}))

                                  (<! (timeout 100))
                                  (is (put! symreg/*sim-stop-start-chan*
                                            {:new-state :pause}))
                                  (<! (timeout 200))
                                  (is (put! symreg/*sim-stop-start-chan*
                                            {:new-state :start}))
                                  (<! (timeout 100))
                                  (is (put! symreg/*sim-stop-start-chan*
                                            {:new-state :pause}))
                                  (<! (timeout 200))

                                  (is (put! symreg/*sim-stop-start-chan*
                                            {:new-state          :restart
                                             :input-data-x       [0 1 2 3 4]
                                             :input-data-y       [1 13 16 8 8]
                                             :input-iters        500
                                             :input-phenos-count 500}))

                                  (<! (timeout 200))

                                  (is (put! symreg/*sim-stop-start-chan*
                                            {:new-state          :restart
                                             :input-data-x       [0 1 2 3 4]
                                             :input-data-y       [11 3 6 18 8]
                                             :input-iters        300
                                             :input-phenos-count 400}))

                                  (<! (timeout 200))

                                  (is (put! symreg/*sim-stop-start-chan*
                                            {:new-state :stop}))

                                  (<! (timeout 100))

                                  (is (put! symreg/*gui-close-chan* :close-please))
                                  (is (put! symreg/*sim->gui-chan* :next))
                                  true)]

            (symreg/run-experiment
              {:initial-phenos (ops-init/initial-phenotypes 20)
               :initial-muts   (ops-init/initial-mutations)
               :input-xs-exprs symreg/example-input-xs-exprs
               :input-ys-exprs symreg/example-input-ys-exprs
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


(deftest check-if-done-test
  (testing "not done"
    (is (=
          (#'symreg/check-if-done 1 10 nil nil)
          nil))))
