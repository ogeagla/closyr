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
                 -1.5)))))))


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
        (is (= {"-1/2+x^2" -3.0000147}
               @ops/eval-cache*)))))

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
