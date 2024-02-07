(ns closyr.ops-test
  (:require
    [clojure.test :refer :all]
    [closyr.ops :as ops]
    [closyr.ops.common :as ops-common])
  (:import
    (org.matheclipse.core.expression
      F)))


(deftest score-fn-test

  (testing "simple eval score"
    (is (=
          (ops/score-fn {:input-ys-vec   [0 1 2]
                         :input-xs-list  (ops-common/exprs->exprs-list (ops-common/doubles->exprs [0.5 1.0 2.0]))
                         :input-xs-count 3}
                        (let [x (F/Dummy "x")]
                          (ops-common/->phenotype x (F/Subtract (F/Times x x) F/C1D2) nil)))
          -3.0000147))))
