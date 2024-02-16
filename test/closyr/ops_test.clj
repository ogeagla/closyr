(ns closyr.ops-test
  (:require
    [clojure.test :refer :all]
    [closyr.util.prng :as prng]
    [closyr.ops :as ops]
    [closyr.ops.common :as ops-common]
    [closyr.ops.eval :as ops-eval]
    [closyr.ops.modify :as ops-modify])
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
                  "x^2+Sin(x)"))))))))


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
