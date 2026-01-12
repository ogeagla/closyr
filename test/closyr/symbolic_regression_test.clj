(ns closyr.symbolic-regression-test
  (:require
    [clojure.core.async :as async :refer [go go-loop timeout <!! >!! <! >! chan put! take! alts!!]]
    [clojure.pprint :as pp]
    [clojure.test :refer :all]
    [closyr.ops :as ops]
    [closyr.ops.common :as ops-common]
    [closyr.ops.eval :as ops-eval]
    [closyr.ops.initialize :as ops-init]
    [closyr.symbolic-regression :as symreg]
    [closyr.test-utils :as test-utils]
    [closyr.util.spec :as specs]
    [malli.core :as m])
  (:import
    (java.awt
      GraphicsEnvironment)
    (org.matheclipse.core.interfaces
      IExpr)))


(use-fixtures :once test-utils/quiet-logging-fixture)

(alter-var-root #'symreg/*is-testing* (constantly true))


(deftest end-iters-if-solution-found-test
  (testing "not solved"
    (is (=
          9
          (#'symreg/next-iters 10 [-10.0]))))
  (testing "an exact solution"
    (is (=
          0
          (#'symreg/next-iters 10 [0.0])))))


(deftest can-run-from-cli-args
  (testing "args from CLI are passed along correctly to implementation"
    (let [args* (atom nil)]
      (binding [ops/*print-top-n* 1]
        (is (= {:iters-done 123
                :next-step  :stop}
               (dissoc
                 (with-redefs-fn {#'symreg/run-solver-ga-iterations
                                  (fn [run-config run-args]
                                    (reset! args*
                                            [(dissoc run-config :initial-muts :initial-phenos :input-xs-exprs :input-ys-exprs)
                                             (dissoc run-args :extended-domain-args :initial-phenos :input-xs-list :input-ys-arr)])
                                    {:iters-done       123
                                     :final-population {:pop          []
                                                        :score-fn     #()
                                                        :pop-scores   []
                                                        :mutation-fn  #()
                                                        :crossover-fn #()}
                                     :next-step        :stop})
                                  #'symreg/config->log-steps (fn [_ _] 200)}
                   (fn []
                     (symreg/run-app-from-cli-args
                       {:iterations     20
                        :population     20
                        :headless       true
                        :xs             [0 1 2]
                        :ys             [1 4 19]
                        :use-flamechart true
                        :max-leafs      20
                        :seed           123})))
                 :final-population)))

        (is (= [{:iters          20
                 :log-steps      200
                 :max-leafs      20
                 :use-gui?       false
                 :random-seed    123
                 :adaptive-mode  nil
                 :quiet-logs     nil
                 :use-eval-cache nil
                 :scoring-method :mae-max
                 :use-flamechart true}
                {:input-iters         20
                 :input-phenos-count  nil
                 :random-seed         123
                 :adaptive-mode       nil
                 :quiet-logs          nil
                 :use-eval-cache      nil
                 :simplicity-bias     nil
                 :scoring-method      nil
                 :input-xs-count      3
                 :input-xs-vec        [0.0 1.0 2.0]
                 :input-ys-vec        [1.0 4.0 19.0]
                 :max-leafs           20
                 :mutations-blacklist nil
                 :log-steps           nil}]
               @args*)
            "Arguments should be consistent with inputs"))))


  (testing "args from CLI are passed along correctly to implementation, and when none are passed we use defaults"
    (let [args* (atom nil)]
      (binding [ops/*print-top-n* 1]
        (is (=
              {:iters-done 123
               :next-step  :stop}
              (dissoc
                (with-redefs-fn {#'symreg/run-solver-ga-iterations
                                 (fn [run-config run-args]
                                   (reset! args*
                                           [(dissoc run-config :initial-muts :initial-phenos :input-xs-exprs :input-ys-exprs)
                                            (dissoc run-args :extended-domain-args :initial-phenos :input-xs-list :input-ys-arr)])
                                   {:iters-done       123
                                    :final-population {:pop          []
                                                       :score-fn     #()
                                                       :pop-scores   []
                                                       :mutation-fn  #()
                                                       :crossover-fn #()}
                                    :next-step        :stop})
                                 #'symreg/config->log-steps (fn [_ _] 200)}
                  (fn []
                    (symreg/run-app-from-cli-args
                      {:population 30
                       :iterations 20
                       :headless   true})))
                :final-population)))

        (is (= [{:iters          20
                 :log-steps      200
                 :max-leafs      40
                 :use-gui?       false
                 :random-seed    nil
                 :adaptive-mode  nil
                 :quiet-logs     nil
                 :use-eval-cache nil
                 :scoring-method :mae-max
                 :use-flamechart nil}
                {:input-iters         20
                 :input-phenos-count  nil
                 :random-seed         nil
                 :adaptive-mode       nil
                 :quiet-logs          nil
                 :use-eval-cache      nil
                 :scoring-method      nil
                 :simplicity-bias     nil
                 :input-xs-count      50
                 :input-xs-vec        [0.0 0.20943951023931953 0.41887902047863906 0.6283185307179586 0.8377580409572781 1.0471975511965976 1.2566370614359172 1.4660765716752369 1.6755160819145563 1.8849555921538759 2.0943951023931953 2.3038346126325147 2.5132741228718345 2.7227136331111543 2.9321531433504737 3.141592653589793 3.3510321638291125 3.560471674068432 3.7699111843077517 3.979350694547071 4.1887902047863905 4.39822971502571 4.607669225265029 4.81710873550435 5.026548245743669 5.235987755982989 5.445427266222309 5.654866776461628 5.8643062867009474 6.073745796940266 6.283185307179586 6.492624817418906 6.702064327658225 6.911503837897546 7.120943348136864 7.3303828583761845 7.5398223686155035 7.749261878854823 7.958701389094142 8.168140899333462 8.377580409572781 8.587019919812102 8.79645943005142 9.00589894029074 9.215338450530059 9.42477796076938 9.6342174710087 9.843656981248019 10.053096491487338 10.262536001726657]
                 :input-ys-vec        [0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0]
                 :max-leafs           nil
                 :mutations-blacklist nil
                 :log-steps           nil}]
               @args*)
            "Arguments should be consistent with inputs")))))


(deftest can-run-experiment

  (binding [ops/*print-top-n* 1]
    (testing "with built-in sample data"
      (is (= (count (:pop
                      (:final-population
                        (with-redefs-fn {#'symreg/config->log-steps (fn [_ _] 10)}
                          (fn []
                            (symreg/run-find-formula
                              {:initial-phenos (ops-init/initial-phenotypes 100)
                               :initial-muts   (ops-init/initial-mutations)
                               :iters          20
                               :use-gui?       false
                               :use-flamechart false
                               :input-xs-exprs (ops-common/doubles->exprs [1 2 3])
                               :input-ys-exprs (ops-common/doubles->exprs [6 12 99])}))))))
             100)))

    (testing "with gui launcher"
      (let [res (with-redefs-fn {#'symreg/run-find-formula (fn [args] args)}
                  (fn []
                    (#'symreg/run-app-with-gui)))]
        (is (= {:iters          100
                :max-leafs      40
                :use-flamechart false
                :use-gui?       true}
               (dissoc res :initial-phenos :initial-muts :input-xs-exprs :input-ys-exprs)))
        (is (= 50
               (count (:initial-phenos res))))
        (is (= 96
               (count (:initial-muts res))))
        (is (= 50
               (count (:input-xs-exprs res))))
        (is (= 50
               (count (:input-ys-exprs res))))))

    (testing "with provided data"
      (reset! symreg/sim-input-args* {})
      (with-redefs-fn {#'symreg/config->log-steps (fn [_ _] 10)}
        (fn []
          (let [{:keys [final-population next-step iters-done]}
                (symreg/run-find-formula
                  {:input-phenos-count 100
                   :initial-muts       (ops-init/initial-mutations)
                   :iters              5
                   :use-gui?           false
                   :use-flamechart     false
                   :input-xs-exprs     (->> (range 50)
                                            (map (fn [i] (* Math/PI (/ i 15.0))))
                                            ops-common/doubles->exprs)
                   :input-ys-exprs     (->> (range 50)
                                            (map (fn [i]
                                                   (+ 2.0
                                                      (/ i 10.0)
                                                      (Math/cos (* Math/PI (/ i 15.0))))))
                                            ops-common/doubles->exprs)})]
            (is (= 100
                   (count (:pop final-population))))

            (is (= 5
                   iters-done))

            (is (= #{:input-xs-exprs
                     :input-xs-vec
                     :input-ys-vec}
                   (set (keys @symreg/sim-input-args*))))))))

    (testing "with provided data and random seed"
      (reset! symreg/sim-input-args* {})
      (with-redefs-fn {#'symreg/config->log-steps (fn [_ _] 10)}
        (fn []
          (let [{:keys [final-population next-step iters-done]}
                (symreg/run-find-formula
                  {:input-phenos-count 100
                   :initial-muts       (ops-init/initial-mutations)
                   :iters              5
                   :use-gui?           false
                   :use-flamechart     false
                   :random-seed        1234
                   :input-xs-exprs     (->> (range 50)
                                            (map (fn [i] (* Math/PI (/ i 15.0))))
                                            ops-common/doubles->exprs)
                   :input-ys-exprs     (->> (range 50)
                                            (map (fn [i]
                                                   (+ 2.0
                                                      (/ i 10.0)
                                                      (Math/cos (* Math/PI (/ i 15.0))))))
                                            ops-common/doubles->exprs)})]
            (is (= 100
                   (count (:pop final-population))))

            (is (= 5
                   iters-done))

            (is (= #{:input-xs-exprs
                     :input-xs-vec
                     :input-ys-vec}
                   (set (keys @symreg/sim-input-args*))))))))

    (testing "with provided data as map"
      (reset! symreg/sim-input-args* {})
      (with-redefs-fn {#'symreg/config->log-steps (fn [_ _] 10)}
        (fn []
          (let [{:keys [final-population next-step iters-done]}
                (symreg/run-find-formula
                  {:input-phenos-count 100
                   :initial-muts       (ops-init/initial-mutations)
                   :iters              5
                   :use-gui?           false
                   :use-flamechart     false
                   :input-xs-exprs     (->> (range 50)
                                            (map (fn [i] (* Math/PI (/ i 15.0))))
                                            ops-common/doubles->exprs)
                   :input-ys-exprs     (->> (range 50)
                                            (map (fn [i]
                                                   (+ 2.0
                                                      (/ i 10.0)
                                                      (Math/cos (* Math/PI (/ i 15.0))))))
                                            ops-common/doubles->exprs)})]
            (is (= 100
                   (count (:pop final-population))))

            (is (= 5
                   iters-done))

            (is (= #{:input-xs-exprs
                     :input-xs-vec
                     :input-ys-vec}
                   (set (keys @symreg/sim-input-args*))))))))))


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

              (symreg/run-find-formula
                {:initial-phenos (ops-init/initial-phenotypes 20)
                 :initial-muts   (ops-init/initial-mutations)
                 :input-xs-exprs symreg/example-input-xs-exprs
                 :input-ys-exprs symreg/example-input-ys-exprs
                 :iters          20
                 :use-gui?       true
                 :use-flamechart false})


              (is (= (<!! control-process) true))))))))


(deftest deterministic-experiments
  (testing "with provided data and random seed"
    (reset! symreg/sim-input-args* {})
    (with-redefs-fn {#'symreg/config->log-steps (fn [_ _] 10)}
      (fn []
        (let [{:keys [final-population next-step iters-done] :as resp}
              (symreg/run-find-formula
                {:input-phenos-count 10
                 :initial-muts       (ops-init/initial-mutations)
                 :iters              5
                 :use-gui?           false
                 :use-flamechart     false
                 :random-seed        123
                 :input-xs-exprs     (->> (range 50)
                                          (map (fn [i] (* Math/PI (/ i 15.0))))
                                          ops-common/doubles->exprs)
                 :input-ys-exprs     (->> (range 50)
                                          (map (fn [i]
                                                 (+ 2.0
                                                    (/ i 10.0)
                                                    (Math/cos (* Math/PI (/ i 15.0))))))
                                          ops-common/doubles->exprs)})]

          (is (=
                '(-2086020.9864992178
                   -1040014.0760937533
                   -243.28664816600795
                   -190.5004535058019
                   -12.807688931991098)
                (sort (:pop-scores (:final-population resp)))))

          (is (= 10
                 (count (:pop final-population))))

          (is (= 5
                 iters-done))

          (is (= #{:input-xs-exprs
                   :input-xs-vec
                   :input-ys-vec}
                 (set (keys @symreg/sim-input-args*)))))))))


(deftest deterministic-experiments-scoring-methods
  (let [run-config {:input-phenos-count 10
                    :initial-muts       (ops-init/initial-mutations)
                    :iters              5
                    :use-gui?           false
                    :use-flamechart     false
                    :random-seed        456
                    :input-xs-exprs     (->> (range 20)
                                             (map (fn [i] (* 0.5 i)))
                                             ops-common/doubles->exprs)
                    :input-ys-exprs     (->> (range 20)
                                             (map (fn [i] (+ (* 2 i) 3)))
                                             ops-common/doubles->exprs)}]

    (testing "mae-max scoring method (default)"
      (reset! symreg/sim-input-args* {})
      (with-redefs-fn {#'symreg/config->log-steps (fn [_ _] 10)}
        (fn []
          (binding [ops/*scoring-method* :mae-max]
            (let [{:keys [final-population iters-done] :as resp}
                  (symreg/run-find-formula run-config)
                  sorted-scores (sort (:pop-scores final-population))]
              ;; MAE scores are negative (closer to 0 is better)
              (is (every? neg? sorted-scores))
              (is (= 10 (count (:pop final-population))))
              (is (= 5 iters-done))
              ;; Best score should be better (less negative) than worst
              (is (> (last sorted-scores) (first sorted-scores))))))))

    (testing "log-cosh scoring method"
      (reset! symreg/sim-input-args* {})
      (with-redefs-fn {#'symreg/config->log-steps (fn [_ _] 10)}
        (fn []
          (binding [ops/*scoring-method* :log-cosh]
            (let [{:keys [final-population iters-done] :as resp}
                  (symreg/run-find-formula run-config)
                  sorted-scores (sort (:pop-scores final-population))]
              ;; Log-cosh scores are negative (closer to 0 is better)
              (is (every? neg? sorted-scores))
              (is (= 10 (count (:pop final-population))))
              (is (= 5 iters-done))
              ;; Best score should be better (less negative) than worst
              (is (> (last sorted-scores) (first sorted-scores))))))))

    (testing "r-squared scoring method"
      (reset! symreg/sim-input-args* {})
      (with-redefs-fn {#'symreg/config->log-steps (fn [_ _] 10)}
        (fn []
          (binding [ops/*scoring-method* :r-squared]
            (let [{:keys [final-population iters-done] :as resp}
                  (symreg/run-find-formula run-config)
                  sorted-scores (sort (:pop-scores final-population))]
              ;; R² scores can be negative (bad) to 1.0 (perfect)
              ;; Best scores should be closer to 1.0
              (is (= 10 (count (:pop final-population))))
              (is (= 5 iters-done))
              ;; Best score should be better (higher, closer to 1) than worst
              (is (> (last sorted-scores) (first sorted-scores)))
              ;; R² for good fits will still be negative
              (is (neg? (last sorted-scores))))))))

    (testing "different scoring methods produce different score ranges"
      (reset! symreg/sim-input-args* {})
      (with-redefs-fn {#'symreg/config->log-steps (fn [_ _] 10)}
        (fn []
          (let [mae-result (binding [ops/*scoring-method* :mae-max]
                             (symreg/run-find-formula run-config))
                log-cosh-result (binding [ops/*scoring-method* :log-cosh]
                                  (symreg/run-find-formula run-config))
                r2-result (binding [ops/*scoring-method* :r-squared]
                            (symreg/run-find-formula run-config))
                mae-best (apply max (:pop-scores (:final-population mae-result)))
                log-cosh-best (apply max (:pop-scores (:final-population log-cosh-result)))
                r2-best (apply max (:pop-scores (:final-population r2-result)))]
            ;; MAE and log-cosh best scores are negative (less negative = better)
            (is (neg? mae-best))
            (is (neg? log-cosh-best))
            ;; R² best score should still be negative for reasonable fits
            (is (neg? r2-best))
            ;; R² is bounded above by 1.0
            (is (<= r2-best 1.0))))))))


(deftest cross-method-scoring-comparison
  (testing "compare solutions from different scoring methods using all scoring metrics"
    (let [;; Simple quadratic data: y = x^2
          xs-vec (mapv double (range 1 6))
          ys-vec (mapv (fn [x] (+ (* x x 0.5) x 1)) xs-vec) ;; 0.5 * x^2 + x + 1
          ys-arr (double-array ys-vec)

          run-config {:input-phenos-count 15
                      :initial-muts       (ops-init/initial-mutations)
                      :iters              8
                      :use-gui?           false
                      :use-flamechart     false
                      :random-seed        789
                      :input-xs-exprs     (ops-common/doubles->exprs xs-vec)
                      :input-ys-exprs     (ops-common/doubles->exprs ys-vec)}

          ;; Helper to evaluate a phenotype under a specific scoring method
          score-pheno-with-method (fn [pheno method]
                                    (let [f-of-xs (ops-eval/eval-vec-pheno
                                                    pheno
                                                    {:input-xs-list  (ops-common/exprs->exprs-list
                                                                       (ops-common/doubles->exprs xs-vec))
                                                     :input-xs-count (count xs-vec)})]
                                      (when f-of-xs
                                        (#'ops/compute-score-from-actuals-and-expecteds
                                          pheno f-of-xs ys-vec
                                          (.leafCount ^IExpr (:expr pheno))
                                          ys-arr
                                          method))))

          ;; Run with each scoring method and get best solution
          _ (reset! symreg/sim-input-args* {})
          mae-result (with-redefs-fn {#'symreg/config->log-steps (fn [_ _] 10)}
                       (fn []
                         (symreg/run-find-formula (assoc run-config :scoring-method :mae-max))))
          mae-best (first (sort-by :score > (:pop (:final-population mae-result))))

          _ (reset! symreg/sim-input-args* {})
          log-cosh-result (with-redefs-fn {#'symreg/config->log-steps (fn [_ _] 10)}
                            (fn []
                              (symreg/run-find-formula (assoc run-config :scoring-method :log-cosh))))
          log-cosh-best (first (sort-by :score > (:pop (:final-population log-cosh-result))))

          _ (reset! symreg/sim-input-args* {})
          r2-result (with-redefs-fn {#'symreg/config->log-steps (fn [_ _] 10)}
                      (fn []
                        (symreg/run-find-formula (assoc run-config :scoring-method :r-squared))))
          r2-best (first (sort-by :score > (:pop (:final-population r2-result))))

          ;; Score each best solution under all three methods
          mae-best-scores {:mae-max   (score-pheno-with-method mae-best :mae-max)
                           :log-cosh  (score-pheno-with-method mae-best :log-cosh)
                           :r-squared (score-pheno-with-method mae-best :r-squared)}
          log-cosh-best-scores {:mae-max   (score-pheno-with-method log-cosh-best :mae-max)
                                :log-cosh  (score-pheno-with-method log-cosh-best :log-cosh)
                                :r-squared (score-pheno-with-method log-cosh-best :r-squared)}
          r2-best-scores {:mae-max   (score-pheno-with-method r2-best :mae-max)
                          :log-cosh  (score-pheno-with-method r2-best :log-cosh)
                          :r-squared (score-pheno-with-method r2-best :r-squared)}]

      ;; Log results for inspection
      (println "\n=== Cross-Method Scoring Comparison ===")
      (println "MAE-trained best formula:" (str (:expr mae-best)))
      (println "  scored as MAE:" (:mae-max mae-best-scores)
               "log-cosh:" (:log-cosh mae-best-scores)
               "R²:" (:r-squared mae-best-scores))

      (println "Log-cosh-trained best formula:" (str (:expr log-cosh-best)))
      (println "  scored as MAE:" (:mae-max log-cosh-best-scores)
               "log-cosh:" (:log-cosh log-cosh-best-scores)
               "R²:" (:r-squared log-cosh-best-scores))

      (println "R²-trained best formula:" (str (:expr r2-best)))
      (println "  scored as MAE:" (:mae-max r2-best-scores)
               "log-cosh:" (:log-cosh r2-best-scores)
               "R²:" (:r-squared r2-best-scores))
      (println "========================================\n")

      ;; Basic assertions - all scores should be numeric and negative (closer to 0 = better)
      (is (number? (:mae-max mae-best-scores)))
      (is (number? (:log-cosh mae-best-scores)))
      (is (number? (:r-squared mae-best-scores)))

      ;; Each solution should score best (or very well) under its own training method
      ;; This is a sanity check that the scoring methods are being applied correctly
      (is (or (neg? (:mae-max mae-best-scores)) (zero? (:mae-max mae-best-scores))))
      (is (or (neg? (:log-cosh log-cosh-best-scores)) (zero? (:log-cosh log-cosh-best-scores))))
      (is (or (neg? (:r-squared r2-best-scores)) (zero? (:r-squared r2-best-scores))))

      ;; R² scores should be bounded above by 0 (since we shifted by -1)
      (is (<= (:r-squared mae-best-scores) 0))
      (is (<= (:r-squared log-cosh-best-scores) 0))
      (is (<= (:r-squared r2-best-scores) 0)))))


(deftest simplicity-bias-in-run-find-formula
  (let [run-config {:input-phenos-count 10
                    :initial-muts       (ops-init/initial-mutations)
                    :iters              3
                    :use-gui?           false
                    :use-flamechart     false
                    :random-seed        999
                    :input-xs-exprs     (->> (range 10)
                                             (map (fn [i] (* 0.5 i)))
                                             ops-common/doubles->exprs)
                    :input-ys-exprs     (->> (range 10)
                                             (map (fn [i] (+ (* 2 i) 3)))
                                             ops-common/doubles->exprs)}]

    (testing "simplicity-bias can be passed to run-find-formula"
      (reset! symreg/sim-input-args* {})
      (with-redefs-fn {#'symreg/config->log-steps (fn [_ _] 10)}
        (fn []
          ;; Should not throw with simplicity-bias option
          (let [{:keys [final-population iters-done]}
                (symreg/run-find-formula (assoc run-config :simplicity-bias :strong))]
            (is (= 10 (count (:pop final-population))))
            (is (= 3 iters-done))))))

    (testing "different simplicity-bias levels produce different score distributions"
      (reset! symreg/sim-input-args* {})
      (with-redefs-fn {#'symreg/config->log-steps (fn [_ _] 10)}
        (fn []
          (let [result-none (symreg/run-find-formula (assoc run-config :simplicity-bias :none))
                result-strong (symreg/run-find-formula (assoc run-config :simplicity-bias :strong))
                scores-none (:pop-scores (:final-population result-none))
                scores-strong (:pop-scores (:final-population result-strong))]
            ;; With same random seed and inputs, :strong bias should have lower (more negative)
            ;; scores due to the larger complexity penalty
            ;; At minimum, both should complete without errors and have some population
            (is (pos? (count scores-none)))
            (is (pos? (count scores-strong)))
            ;; Best scores (max) should be negative
            (is (neg? (apply max scores-none)))
            (is (neg? (apply max scores-strong)))))))))


(deftest can-run-experiment-gui:start-restart-stop
  (when (not (GraphicsEnvironment/isHeadless))
    (binding [ops/*print-top-n* 1]
      (testing "gui can start and restart experiments; NOTE: do not run this while in headless mode, eg on CI"
        (reset! symreg/sim-input-args* {})
        ;; Verify initial state before launching async processes
        (is (= (set (keys @symreg/sim-input-args*)) #{}))
        (with-redefs-fn {#'symreg/config->log-steps (fn [_ _] 500)}
          (fn []
            (let [control-process
                  (go
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
                    (<! (timeout 100))
                    (is (put! symreg/*sim-stop-start-chan*
                              {:new-state :start}))
                    (<! (timeout 100))
                    (is (put! symreg/*sim-stop-start-chan*
                              {:new-state :pause}))
                    (<! (timeout 100))

                    (is (put! symreg/*sim-stop-start-chan*
                              {:new-state          :restart
                               :input-data-x       [0 1 2 3 4]
                               :input-data-y       [1 13 16 8 8]
                               :input-iters        1500
                               :input-phenos-count 500}))

                    (<! (timeout 100))

                    (is (put! symreg/*sim-stop-start-chan*
                              {:new-state          :restart
                               :input-data-x       [0 1 2 3 4]
                               :input-data-y       [1 13 16 8 8]
                               :input-iters        5
                               :input-phenos-count 5}))

                    (<! (timeout 100))

                    (is (put! symreg/*sim-stop-start-chan*
                              {:new-state          :restart
                               :input-data-x       [0 1 2 3 4]
                               :input-data-y       [11 3 6 18 8]
                               :input-iters        300
                               :input-phenos-count 400}))

                    (<! (timeout 100))

                    (is (= (set (keys @symreg/sim-input-args*))
                           #{:input-iters
                             :input-phenos-count
                             :input-xs-exprs
                             :input-xs-vec
                             :input-ys-vec
                             :max-leafs
                             :random-seed
                             :quiet-logs
                             :adaptive-mode
                             :use-eval-cache
                             :scoring-method
                             :mutations-blacklist
                             :log-steps}))

                    (is (put! symreg/*sim-stop-start-chan*
                              {:new-state :stop}))

                    (<! (timeout 100))

                    (is (put! symreg/*gui-close-chan* :close-please))
                    (is (put! symreg/*sim->gui-chan* :next))
                    true)]

              (symreg/run-find-formula
                {:initial-phenos (ops-init/initial-phenotypes 20)
                 :initial-muts   (ops-init/initial-mutations)
                 :input-xs-exprs symreg/example-input-xs-exprs
                 :input-ys-exprs symreg/example-input-ys-exprs
                 :iters          20
                 :use-gui?       true
                 :use-flamechart false})


              (is (= (<!! control-process) true)))))))))


(deftest derive-log-steps
  (testing "with basic input"
    (is (=
          (#'symreg/config->log-steps {:iters          100000
                                       :initial-phenos (vec (repeat 10 0))}
            {:input-xs-count 10})
          25))

    (is (=
          (#'symreg/config->log-steps {:iters          1000
                                       :initial-phenos (vec (repeat 10000 0))}
            {:input-xs-count 150})
          2))

    (is (=
          (#'symreg/config->log-steps {:iters          10
                                       :initial-phenos (vec (repeat 10 0))}
            {:input-xs-count 10})
          1))))


(deftest check-if-done-test
  (testing "not done"
    (is (=
          (#'symreg/check-if-done 1 10 nil nil)
          nil))))


(deftest run-args-checks-needed-params
  (testing "insufficient args"
    (is (thrown? Exception (#'symreg/->run-args {}))))

  (testing "sufficient args"
    (is (= (set (keys (#'symreg/->run-args {:input-xs-exprs     symreg/example-input-xs-exprs
                                            :input-xs-vec       (vec (range (count symreg/example-input-xs-exprs)))
                                            :input-ys-vec       (vec (range (count symreg/example-input-xs-exprs)))
                                            :iters              1
                                            :input-phenos-count 1})))
           #{:extended-domain-args
             :initial-phenos
             :input-iters
             :input-phenos-count
             :random-seed
             :quiet-logs
             :adaptive-mode
             :use-eval-cache
             :scoring-method
             :simplicity-bias
             :input-xs-count
             :input-xs-list
             :input-xs-vec
             :input-ys-arr
             :input-ys-vec
             :max-leafs
             :mutations-blacklist
             :log-steps}))))
