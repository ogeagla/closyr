(ns closyr.ops-eval-test
  (:require
    [clojure.core.async :as async :refer [go go-loop timeout <!! >!! <! >! chan put! take! alts!! alt!! close!]]
    [clojure.test :refer :all]
    [closyr.ops.common :as ops-common]
    [closyr.ops.eval :as ops-eval])
  (:import
    (org.matheclipse.core.expression
      F)
    (org.matheclipse.core.interfaces
      IAST
      IExpr
      ISymbol)))


(set! *warn-on-reflection* true)


(deftest eval-f-test
  (let [x (F/Dummy "x")]
    (testing "can eval various fns for simple inputs"
      (is (= (mapv
               ops-common/expr->double
               (ops-eval/eval-phenotype-on-expr-args
                 (ops-common/->phenotype x (F/Subtract x F/C1D2) nil)
                 (ops-common/exprs->exprs-list (ops-common/doubles->exprs [0.5]))))
             [0.0]))

      (is (instance? IExpr (F/Subtract F/E F/C1D2)))
      (is (instance? IAST (F/Subtract F/E F/C1D2)))

      (is (= (str (F/Subtract F/E F/C1D2))
             "-1/2+E"))

      (is (= (str (.eval (F/Subtract F/E F/C1D2)))
             "-1/2+E"))

      (is (= (str (.eval (F/Sqrt (F/Subtract x F/C1))))
             "Sqrt(-1+x)"))

      (is (= (str (F/Sqrt (F/Subtract x F/C1)))
             "Sqrt(-1+x)"))

      (is (= (.toNumber (F/Subtract F/E F/C1D2))
             2.218281828459045))

      (is (= (try (.toNumber (F/Subtract x F/C1D2))
                  (catch Exception e nil))
             nil))

      (is (= (ops-eval/eval-vec-pheno
               (ops-common/->phenotype x (F/Subtract F/E F/C1D2) nil)
               {:input-xs-list  (ops-common/exprs->exprs-list (ops-common/doubles->exprs [0.5]))
                :input-xs-count 1})
             [2.218281828459045]))

      (is (= (ops-eval/eval-vec-pheno
               (ops-common/->phenotype x (F/Subtract F/C1 F/C1D2) nil)
               {:input-xs-list  (ops-common/exprs->exprs-list (ops-common/doubles->exprs [0.5]))
                :input-xs-count 1})
             [0.5]))


      (is (= (ops-eval/eval-vec-pheno
               (ops-common/->phenotype x (F/Subtract x F/C1D2) nil)
               {:input-xs-list  (ops-common/exprs->exprs-list (ops-common/doubles->exprs [0.5]))
                :input-xs-count 1})
             [0.0]))

      (is (= (ops-eval/eval-vec-pheno
               (ops-common/->phenotype x (F/Subtract (F/Times x (F/Sin x)) F/C1D2) nil)
               {:input-xs-list  (ops-common/exprs->exprs-list (ops-common/doubles->exprs [0.0 0.5 1.0]))
                :input-xs-count 3})
             [-0.5 -0.2602872306978985 0.3414709848078965]))

      ;; complex results are considered to have infinite values:
      (is (= (ops-eval/eval-vec-pheno
               (ops-common/->phenotype x (F/Sqrt (F/Subtract x F/C1)) nil)
               {:input-xs-list  (ops-common/exprs->exprs-list (ops-common/doubles->exprs [0.0 0.5 1.0]))
                :input-xs-count 3})
             [##Inf ##Inf 0.0])))))


(deftest eval-f-benchmark-test
  (let [x (F/Dummy "x")]
    (println "size 20:")
    (time
      (testing "vector size 20"
        (is (= (ops-eval/eval-vec-pheno
                 (ops-common/->phenotype x (F/Subtract (F/Times x x) F/C1D2) nil)
                 {:input-xs-list  (ops-common/exprs->exprs-list (ops-common/doubles->exprs (vec (range 20))))
                  :input-xs-count 20})
               (mapv
                 #(- (* % %) 0.5)
                 (range 20))))))
    (println "size 40:")
    (time
      (testing "vector size 40"
        (is (= (ops-eval/eval-vec-pheno
                 (ops-common/->phenotype x (F/Subtract (F/Times x x) F/C1D2) nil)
                 {:input-xs-list  (ops-common/exprs->exprs-list (ops-common/doubles->exprs (vec (range 40))))
                  :input-xs-count 40})
               (mapv
                 #(- (* % %) 0.5)
                 (range 40))))))

    (println "size 80:")
    (time
      (testing "vector size 80"
        (is (= (ops-eval/eval-vec-pheno
                 (ops-common/->phenotype x (F/Subtract (F/Times x x) F/C1D2) nil)
                 {:input-xs-list  (ops-common/exprs->exprs-list (ops-common/doubles->exprs (vec (range 80))))
                  :input-xs-count 80})
               (mapv
                 #(- (* % %) 0.5)
                 (range 80))))))


    (println "size 160:")
    (time
      (testing "vector size 160"
        (is (= (ops-eval/eval-vec-pheno
                 (ops-common/->phenotype x (F/Subtract (F/Times x x) F/C1D2) nil)
                 {:input-xs-list  (ops-common/exprs->exprs-list (ops-common/doubles->exprs (vec (range 160))))
                  :input-xs-count 160})
               (mapv
                 #(- (* % %) 0.5)
                 (range 160))))))


    (println "size 320:")
    (time
      (testing "vector size 320"
        (is (= (ops-eval/eval-vec-pheno
                 (ops-common/->phenotype x (F/Subtract (F/Times x x) F/C1D2) nil)
                 {:input-xs-list  (ops-common/exprs->exprs-list (ops-common/doubles->exprs (vec (range 320))))
                  :input-xs-count 320})
               (mapv
                 #(- (* % %) 0.5)
                 (range 320))))))))


(comment (run-tests 'closyr.ops-eval-test))
