(ns closyr.ops-test
  (:require
    [clojure.test :refer :all]
    [closyr.dataset.prng :as prng]
    [closyr.ops :as ops]
    [closyr.ops.common :as ops-common]
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
                        {:max-leafs      ops/default-max-leafs}
                        (let [x (F/Dummy "x")]
                          (ops-common/->phenotype x (F/Subtract (F/Times x x) F/C1D2) nil)))
          -3.0000147))))


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
