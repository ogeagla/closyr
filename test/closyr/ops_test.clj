(ns closyr.ops-test
  (:require
    [clojure.test :refer :all]
    [closyr.ops :as ops]
    [closyr.ops.common :as ops-common]
    [closyr.ops.eval :as ops-eval]
    [closyr.ops.modify :as ops-modify]
    [closyr.util.prng :as prng])
  (:import
    (org.matheclipse.core.expression
      F)
    (org.matheclipse.core.interfaces
      IAST
      IExpr
      ISymbol)))


(deftest score-fn-test
  (testing "simple eval score"
    (is (=
          (ops/score-fn {:input-ys-vec   [0 1 2]
                         :input-xs-list  (ops-common/exprs->exprs-list
                                           (ops-common/doubles->exprs [0.5 1.0 2.0]))
                         :input-xs-count 3}
                        {:max-leafs ops/default-max-leafs}
                        (let [x (F/Dummy "x")]
                          (ops-common/->phenotype x (F/Subtract (F/Times x x) F/C1D2) nil)))
          -3.0000147)))

  (testing "eval score on Hold-wrapped expr returns min-score"
    (is (=
          (ops/score-fn {:input-ys-vec   [0 1 2]
                         :input-xs-list  (ops-common/exprs->exprs-list
                                           (ops-common/doubles->exprs [0.5 1.0 2.0]))
                         :input-xs-count 3}
                        {:max-leafs ops/default-max-leafs}
                        (let [x (F/Dummy "x")]
                          ;; Manually create a phenotype with Hold-wrapped expr
                          {:sym  x
                           :id   (random-uuid)
                           :expr (F/Hold (F/Sin x))}))
          ops/min-score)))

  (testing "eval score on too big fn"
    (is (=
          (ops/score-fn {:input-ys-vec   [0 1 2]
                         :input-xs-list  (ops-common/exprs->exprs-list
                                           (ops-common/doubles->exprs [0.5 1.0 2.0]))
                         :input-xs-count 3}
                        {:max-leafs 0}
                        (let [x (F/Dummy "x")]
                          (ops-common/->phenotype x (F/Subtract (F/Times x x) F/C1D2) nil)))
          ops/min-score)))

  (testing "eval score on failing function"
    (is (=
          (with-redefs-fn {#'ops-eval/eval-vec-pheno (fn [_ _] nil)}
            (fn []
              (ops/score-fn {:input-ys-vec   [0 1 2]
                             :input-xs-list  (ops-common/exprs->exprs-list
                                               (ops-common/doubles->exprs [0.5 1.0 2.0]))
                             :input-xs-count 3}
                            {:max-leafs ops/default-max-leafs}
                            (let [x (F/Dummy "x")]
                              (ops-common/->phenotype x (F/Subtract (F/Times x x) F/C1D2) nil)))))
          ops/min-score)))

  (testing "eval score on throwing function"
    (is (=
          (with-redefs-fn {#'ops-eval/eval-vec-pheno (fn [_ _] (throw (Exception. "Test exception")))}
            (fn []
              (ops/score-fn {:input-ys-vec   [0 1 2]
                             :input-xs-list  (ops-common/exprs->exprs-list
                                               (ops-common/doubles->exprs [0.5 1.0 2.0]))
                             :input-xs-count 3}
                            {:max-leafs ops/default-max-leafs}
                            (let [x (F/Dummy "x")]
                              (ops-common/->phenotype x (F/Subtract (F/Times x x) F/C1D2) nil)))))
          ops/min-score))))


(deftest compute-score-from-actuals-and-expecteds-test
  (testing "simple inputs"
    (let [x (F/Dummy "x")]
      (is (= (#'ops/compute-score-from-actuals-and-expecteds
              (ops-common/->phenotype x (F/Plus (F/Sin x) F/C1D2) nil)
              [0.5]
              [1.0]
              10)
             -1.500015))))

  (testing "throws exception"
    (let [x (F/Dummy "x")]
      (is (= (with-redefs-fn
               {#'ops/compute-residual (fn [_ _] (throw (Exception. "Test Exception")))}

               (fn []
                 (#'ops/compute-score-from-actuals-and-expecteds
                  (ops-common/->phenotype x (F/Plus (F/Sin x) F/C1D2) nil)
                  [0.5]
                  [1.0]
                  10)))

             ops/min-score))))

  (testing "without length deduction"
    (with-redefs-fn {#'ops/length-deduction (fn [score leafs] score)}
      (fn []
        (let [x (F/Dummy "x")]
          (is (= (#'ops/compute-score-from-actuals-and-expecteds
                  (ops-common/->phenotype x (F/Plus (F/Sin x) F/C1D2) nil)
                  [0.5]
                  [1.0]
                  10)
                 0.0))))))

  (testing "without length deduction 2"
    (with-redefs-fn {#'ops/length-deduction (fn [score leafs] 0)}
      (fn []
        (let [x (F/Dummy "x")]
          (is (= (#'ops/compute-score-from-actuals-and-expecteds
                  (ops-common/->phenotype x (F/Plus (F/Sin x) F/C1D2) nil)
                  [0.5]
                  [1.0]
                  10)
                 -1.5))))))

  (testing "log-cosh scoring method"
    (with-redefs-fn {#'ops/length-deduction (fn [score leafs] 0)}
      (fn []
        (let [x (F/Dummy "x")
              ys-arr (double-array [1.0 2.0 3.0])
              ;; Perfect predictions - log-cosh of 0 residuals = 0
              perfect-score (#'ops/compute-score-from-actuals-and-expecteds
                              (ops-common/->phenotype x x nil)
                              [1.0 2.0 3.0]
                              [1.0 2.0 3.0]
                              5
                              ys-arr
                              :log-cosh)]
          ;; Perfect fit should give score of 0 (or very close)
          (is (< (abs perfect-score) 0.0001)))
        (let [x (F/Dummy "x")
              ys-arr (double-array [1.0 2.0 3.0])
              ;; Predictions off by 1 each
              imperfect-score (#'ops/compute-score-from-actuals-and-expecteds
                                (ops-common/->phenotype x x nil)
                                [2.0 3.0 4.0]
                                [1.0 2.0 3.0]
                                5
                                ys-arr
                                :log-cosh)]
          ;; Score should be negative (log-cosh(1) ≈ 0.433)
          (is (< imperfect-score 0))
          (is (> imperfect-score -1.0))))))

  (testing "r-squared scoring method"
    (with-redefs-fn {#'ops/length-deduction (fn [score leafs] 0)}
      (fn []
        (let [x (F/Dummy "x")
              ys-arr (double-array [1.0 2.0 3.0])
              ;; Perfect predictions - (R² - 1) = 0
              perfect-score (#'ops/compute-score-from-actuals-and-expecteds
                              (ops-common/->phenotype x x nil)
                              [1.0 2.0 3.0]
                              [1.0 2.0 3.0]
                              5
                              ys-arr
                              :r-squared)]
          (is (= perfect-score 0.0)))
        (let [x (F/Dummy "x")
              ys-arr (double-array [1.0 2.0 3.0])
              ;; Predictions = mean (2.0) - R² = 0, so (R² - 1) = -1
              mean-score (#'ops/compute-score-from-actuals-and-expecteds
                           (ops-common/->phenotype x x nil)
                           [2.0 2.0 2.0]
                           [1.0 2.0 3.0]
                           5
                           ys-arr
                           :r-squared)]
          (is (< (abs (- mean-score -1.0)) 0.0001)))
        (let [x (F/Dummy "x")
              ys-arr (double-array [1.0 2.0 3.0])
              ;; Predictions worse than mean - R² < 0, so (R² - 1) < -1
              bad-score (#'ops/compute-score-from-actuals-and-expecteds
                          (ops-common/->phenotype x x nil)
                          [10.0 10.0 10.0]
                          [1.0 2.0 3.0]
                          5
                          ys-arr
                          :r-squared)]
          (is (< bad-score -1.0))))))

  (testing "scoring method via dynamic var"
    (with-redefs-fn {#'ops/length-deduction (fn [score leafs] 0)}
      (fn []
        (let [x (F/Dummy "x")
              ys-arr (double-array [1.0 2.0 3.0])
              ;; Default MAE score
              mae-score (#'ops/compute-score-from-actuals-and-expecteds
                          (ops-common/->phenotype x x nil)
                          [1.0 2.0 3.0]
                          [1.0 2.0 3.0]
                          5
                          ys-arr
                          :mae-max)
              ;; R² score via binding
              r2-score (binding [ops/*scoring-method* :r-squared]
                         (#'ops/compute-score-from-actuals-and-expecteds
                           (ops-common/->phenotype x x nil)
                           [1.0 2.0 3.0]
                           [1.0 2.0 3.0]
                           5
                           ys-arr
                           ops/*scoring-method*))]
          ;; Perfect fit: MAE gives 0, R² gives 0 (both use 0 as perfect score)
          (is (= mae-score 0.0))
          (is (= r2-score 0.0)))))))


(deftest mutation-fn-test
  (testing "exception in modify"
    (let [x (F/Dummy "x")]
      (with-redefs-fn {#'ops-modify/apply-modifications
                       (fn [_ _ _ _ _] (throw (Exception. "Testing failed apply modifications")))}
        (fn []
          (is (=
                (str (:expr (ops/mutation-fn
                              {:max-leafs 100}
                              [{:op               :modify-leafs
                                :leaf-modifier-fn (fn ^IExpr [leaf-count
                                                              {^IAST expr :expr ^ISymbol x-sym :sym :as pheno}
                                                              ^IExpr ie]
                                                    (if (= (.toString ie) "x")
                                                      (F/Sin ie)
                                                      ie))}]
                              (ops-common/->phenotype x (F/Subtract x F/C1) nil)
                              (ops-common/->phenotype x (F/Plus x F/C1D2) nil))))
                "-1+x"))))))

  (testing "long running mod"
    (let [x (F/Dummy "x")]
      (binding [ops/*long-running-mutation-thresh-ms* 100]
        (with-redefs-fn {#'ops-modify/apply-modifications
                         (fn [max-leafs mods-count initial-muts p-winner p-discard]
                           (Thread/sleep 200)
                           {:new-pheno p-winner :iters 1 :mods []})}
          (fn []
            (is (=
                  (str (:expr (ops/mutation-fn
                                {:max-leafs 100}
                                [{:op               :modify-leafs
                                  :leaf-modifier-fn (fn ^IExpr [leaf-count
                                                                {^IAST expr :expr ^ISymbol x-sym :sym :as pheno}
                                                                ^IExpr ie]
                                                      (if (= (.toString ie) "x")
                                                        (F/Sin ie)
                                                        ie))}]
                                (ops-common/->phenotype x (F/Subtract (F/Times x x) F/C1) nil)
                                (ops-common/->phenotype x (F/Plus (F/Sin x) F/C1D2) nil))))
                  "-1+x^2"))))))))


(deftest crossover-fn-test
  (with-redefs-fn {#'prng/rand-int (fn [maxv] (dec maxv))
                   #'prng/rand-nth (fn [coll] (first coll))}
    (fn []
      (with-redefs [ops-modify/crossover-sampler [:plus]]

        (testing "simple crossover"
          (let [x (F/Dummy "x")]
            (is (=
                  (str (:expr (ops/crossover-fn
                                {:max-leafs ops/default-max-leafs}
                                []
                                (ops-common/->phenotype x (F/Subtract (F/Times x x) F/C1) nil)
                                (ops-common/->phenotype x (F/Plus (F/Sin x) F/C1D2) nil))))
                  "x^2+Sin(x)")))))))


  (with-redefs-fn {#'prng/rand-int        (fn [maxv] (dec maxv))
                   #'prng/rand-nth        (fn [coll] (first coll))
                   #'ops-modify/crossover (fn [_ _ _] nil)}
    (fn []
      (with-redefs [ops-modify/crossover-sampler [:plus]]

        (testing "simple crossover failure"
          (let [x (F/Dummy "x")]
            (is (=
                  (str (:expr (ops/crossover-fn
                                {:max-leafs ops/default-max-leafs}
                                []
                                (ops-common/->phenotype x (F/Subtract (F/Times x x) F/C1) nil)
                                (ops-common/->phenotype x (F/Plus (F/Sin x) F/C1D2) nil))))
                  "-1+x^2"))))))))


(deftest compute-residuals
  (testing "valid input"
    (is (= (#'ops/compute-residual 10.0 5.0)
           5.0)))

  (testing "invalid input 1"
    (is (= (#'ops/compute-residual ##Inf 5.0)
           ops/max-resid)))

  (testing "invalid input 2"
    (is (= (#'ops/compute-residual ##Inf ##Inf)
           ops/max-resid))))


(deftest eval-cache-test
  (testing "cache starts empty"
    (ops/clear-eval-cache!)
    (let [stats (ops/eval-cache-stats)]
      (is (= 0 (:size stats)))))

  (testing "cache is populated when enabled"
    (ops/clear-eval-cache!)
    (binding [ops/*use-eval-cache* true]
      (let [run-args {:input-ys-vec   [0 1 2]
                      :input-xs-list  (ops-common/exprs->exprs-list
                                        (ops-common/doubles->exprs [0.5 1.0 2.0]))
                      :input-xs-count 3}
            run-config {:max-leafs ops/default-max-leafs}
            x (F/Dummy "x")
            pheno (ops-common/->phenotype x (F/Subtract (F/Times x x) F/C1D2) nil)]
        ;; First call should miss
        (ops/score-fn run-args run-config pheno)
        (let [stats (ops/eval-cache-stats)]
          (is (= 1 (:size stats)))
          (is (= 1 (:misses stats)))
          (is (= 0 (:hits stats))))
        ;; Second call with same expr should hit
        (ops/score-fn run-args run-config pheno)
        (let [stats (ops/eval-cache-stats)]
          (is (= 1 (:size stats)))
          (is (= 1 (:misses stats)))
          (is (= 1 (:hits stats)))))))

  (testing "cache is not populated when disabled"
    (ops/clear-eval-cache!)
    (binding [ops/*use-eval-cache* false]
      (let [run-args {:input-ys-vec   [0 1 2]
                      :input-xs-list  (ops-common/exprs->exprs-list
                                        (ops-common/doubles->exprs [0.5 1.0 2.0]))
                      :input-xs-count 3}
            run-config {:max-leafs ops/default-max-leafs}
            x (F/Dummy "x")
            pheno (ops-common/->phenotype x (F/Subtract (F/Times x x) F/C1D2) nil)]
        (ops/score-fn run-args run-config pheno)
        (ops/score-fn run-args run-config pheno)
        (let [stats (ops/eval-cache-stats)]
          (is (= 0 (:size stats)))))))

  (testing "cache returns same score for same expression"
    (ops/clear-eval-cache!)
    (binding [ops/*use-eval-cache* true]
      (let [run-args {:input-ys-vec   [0 1 2]
                      :input-xs-list  (ops-common/exprs->exprs-list
                                        (ops-common/doubles->exprs [0.5 1.0 2.0]))
                      :input-xs-count 3}
            run-config {:max-leafs ops/default-max-leafs}
            x (F/Dummy "x")
            pheno (ops-common/->phenotype x (F/Subtract (F/Times x x) F/C1D2) nil)
            score1 (ops/score-fn run-args run-config pheno)
            score2 (ops/score-fn run-args run-config pheno)]
        (is (= score1 score2))
        ;; Cache key is [expr-str scoring-method] to prevent cross-contamination
        (is (= {["-1/2+x^2" :mae-max] -3.0000147}
               @ops/eval-cache*)))))

  (testing "different scoring methods have separate cache entries"
    (ops/clear-eval-cache!)
    (binding [ops/*use-eval-cache* true]
      ;; Use data where x^2 is NOT a perfect fit so different scoring methods produce different scores
      (let [run-args {:input-ys-vec   [0.0 2.0 5.0]  ; Not a perfect fit for x^2
                      :input-ys-arr   (double-array [0.0 2.0 5.0])
                      :input-xs-list  (ops-common/exprs->exprs-list
                                        (ops-common/doubles->exprs [0.0 1.0 2.0]))
                      :input-xs-count 3}
            run-config-mae {:max-leafs ops/default-max-leafs :scoring-method :mae-max}
            run-config-r2 {:max-leafs ops/default-max-leafs :scoring-method :r-squared}
            x (F/Dummy "x")
            pheno (ops-common/->phenotype x (F/Times x x) nil)  ; x^2 gives [0, 1, 4], not [0, 2, 5]
            ;; Score with MAE method
            score-mae (ops/score-fn run-args run-config-mae pheno)
            ;; Score with R-squared method - should NOT hit cache
            score-r2 (ops/score-fn run-args run-config-r2 pheno)]
        ;; Different scoring methods should produce different scores for imperfect fit
        (is (not= score-mae score-r2))
        ;; Cache should have 2 entries (one per scoring method)
        (is (= 2 (:size (ops/eval-cache-stats))))
        (is (= 2 (:misses (ops/eval-cache-stats))))
        (is (= 0 (:hits (ops/eval-cache-stats))))
        ;; Now call again with same methods - should hit cache
        (ops/score-fn run-args run-config-mae pheno)
        (ops/score-fn run-args run-config-r2 pheno)
        (is (= 2 (:hits (ops/eval-cache-stats)))))))

  (testing "clear-eval-cache! resets cache"
    (ops/clear-eval-cache!)
    (binding [ops/*use-eval-cache* true]
      (let [run-args {:input-ys-vec   [0 1 2]
                      :input-xs-list  (ops-common/exprs->exprs-list
                                        (ops-common/doubles->exprs [0.5 1.0 2.0]))
                      :input-xs-count 3}
            run-config {:max-leafs ops/default-max-leafs}
            x (F/Dummy "x")
            pheno (ops-common/->phenotype x (F/Subtract (F/Times x x) F/C1D2) nil)]
        (ops/score-fn run-args run-config pheno)
        (is (= 1 (:size (ops/eval-cache-stats))))
        (ops/clear-eval-cache!)
        (is (= 0 (:size (ops/eval-cache-stats))))))))


(deftest simplicity-bias-test
  (testing "simplicity-bias :none returns zero deduction on perfect fit"
    (binding [ops/*simplicity-bias* :none]
      (let [x (F/Dummy "x")
            ys-arr (double-array [1.0 2.0 3.0])
            ;; Perfect fit with identity function
            score (#'ops/compute-score-from-actuals-and-expecteds
                    (ops-common/->phenotype x x nil)
                    [1.0 2.0 3.0]
                    [1.0 2.0 3.0]
                    5
                    ys-arr
                    :mae-max)]
        ;; With :none bias and perfect fit, score should be exactly 0
        (is (= score 0.0)))))

  (testing "simplicity-bias :tiebreaker applies tiny deduction on imperfect fit"
    (binding [ops/*simplicity-bias* :tiebreaker]
      (let [x (F/Dummy "x")
            ;; Use a complex expression that does NOT perfectly fit the data
            ;; x^2 + sin(x) evaluated at [1, 2, 3] gives roughly [1.84, 4.91, 9.14]
            ;; We'll use ys = [2, 5, 10] for a slight mismatch
            complex-expr (F/Plus (F/Times x x) (F/Sin x))
            ys-arr (double-array [2.0 5.0 10.0])
            score (#'ops/compute-score-from-actuals-and-expecteds
                    (ops-common/->phenotype x complex-expr nil)
                    [1.8414709848078965 4.909297426825682 9.141120008059867]  ; actual f(x) values
                    [2.0 5.0 10.0]
                    5
                    ys-arr
                    :mae-max)]
        ;; Score should be negative (error + deduction)
        (is (< score 0))
        ;; The deduction component should be tiny relative to the error
        (is (> score -5.0)))))

  (testing "simplicity-bias levels have increasing deductions"
    (let [x (F/Dummy "x")
          ;; Use a complex expression with imperfect fit to get non-zero base score
          ;; x^2 evaluated at [0, 1, 2] gives [0, 1, 4], we use ys = [0.5, 1.5, 4.5] for error
          complex-expr (F/Times x x)
          actuals [0.0 1.0 4.0]
          expected [0.5 1.5 4.5]
          ys-arr (double-array expected)
          ;; Calculate scores with different bias levels
          score-none (binding [ops/*simplicity-bias* :none]
                       (#'ops/compute-score-from-actuals-and-expecteds
                         (ops-common/->phenotype x complex-expr nil)
                         actuals
                         expected
                         5
                         ys-arr
                         :mae-max))
          score-tiebreaker (binding [ops/*simplicity-bias* :tiebreaker]
                             (#'ops/compute-score-from-actuals-and-expecteds
                               (ops-common/->phenotype x complex-expr nil)
                               actuals
                               expected
                               5
                               ys-arr
                               :mae-max))
          score-light (binding [ops/*simplicity-bias* :light]
                        (#'ops/compute-score-from-actuals-and-expecteds
                          (ops-common/->phenotype x complex-expr nil)
                          actuals
                          expected
                          5
                          ys-arr
                          :mae-max))
          score-strong (binding [ops/*simplicity-bias* :strong]
                         (#'ops/compute-score-from-actuals-and-expecteds
                           (ops-common/->phenotype x complex-expr nil)
                           actuals
                           expected
                           5
                           ys-arr
                           :mae-max))]
      ;; All scores should be negative (there's error)
      (is (< score-none 0))
      ;; Each level should have lower (more negative) score due to larger deduction
      ;; The deduction is proportional to abs(score), so with non-zero error we'll see differences
      (is (>= score-none score-tiebreaker))
      (is (>= score-tiebreaker score-light))
      (is (>= score-light score-strong))
      ;; At least some differences should exist (strong should be noticeably lower)
      (is (> score-none score-strong))))

  (testing "simplicity-bias default is :tiebreaker"
    (is (= ops/*simplicity-bias* :tiebreaker)))

  (testing "simplicity-bias-multipliers has expected keys"
    (is (contains? @#'ops/simplicity-bias-multipliers :none))
    (is (contains? @#'ops/simplicity-bias-multipliers :tiebreaker))
    (is (contains? @#'ops/simplicity-bias-multipliers :light))
    (is (contains? @#'ops/simplicity-bias-multipliers :strong))
    ;; :none should have 0 multiplier
    (is (= 0.0 (:none @#'ops/simplicity-bias-multipliers)))
    ;; Others should have increasing values
    (is (< (:tiebreaker @#'ops/simplicity-bias-multipliers)
           (:light @#'ops/simplicity-bias-multipliers)))
    (is (< (:light @#'ops/simplicity-bias-multipliers)
           (:strong @#'ops/simplicity-bias-multipliers)))))
