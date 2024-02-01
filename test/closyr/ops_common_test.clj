(ns closyr.ops-common-test
  (:require
    [clojure.core.async :as async :refer [go go-loop timeout <!! >!! <! >! chan put! take! alts!! alt!! close!]]
    [clojure.test :refer :all]
    [closyr.dataset.prng :as prng]
    [closyr.ops.common :as ops-common])
  (:import
    (org.matheclipse.core.eval
      EvalEngine)
    (org.matheclipse.core.expression
      F)
    (org.matheclipse.core.interfaces
      IAST
      IExpr
      ISymbol)))


(set! *warn-on-reflection* true)

(alter-var-root #'ops-common/*simplify-probability-sampler* (constantly 2.0))
(alter-var-root #'ops-common/*simplify-max-leafs* (constantly 100))


(deftest parse-expr-str-test
  (testing "can parse simple fn str"
    (let [x      (F/Dummy "x")
          parsed (.parse (ops-common/new-eval-engine) "Cos(x)")]
      (is (instance? IAST parsed))
      (is (=
            (str parsed)
            (str (F/Cos x))))))
  (testing "can parse simple fn str 2"
    (let [x      (F/Dummy "x")
          parsed (.parse (ops-common/new-eval-engine) "x+Cos(x^2)")]
      (is (instance? IAST parsed))
      (is (=
            (str parsed)
            "x+Cos(x^2)")))))


(deftest simplify-test
  (testing "simplify trig compose"
    (let [x (F/Dummy "x")]
      (is (=
            (update (ops-common/maybe-simplify {:expr (F/Sin (F/ArcSin x))})
                    :expr str)
            {:expr    "x"
             :simple? true}))))

  (testing "simplify sum"
    (let [x (F/Dummy "x")]
      (is (=
            (binding [ops-common/*simplify-max-leafs* 100]
              (update (ops-common/maybe-simplify {:expr (F/Plus (F/Cos x) (F/Cos x))})
                      :expr str))
            {:expr    "2*Cos(x)"
             :simple? true}))))

  (testing "simplify deep 1"
    (let [x (F/Dummy "x")]
      (is (=
            (binding [ops-common/*simplify-max-leafs* 100]
              (update (ops-common/maybe-simplify
                        {:expr (F/Plus
                                 (F/Plus
                                   (F/Times
                                     x
                                     (F/Sin x))

                                   (F/Times
                                     x
                                     (F/Cos x)))
                                 (F/Plus
                                   (F/Times
                                     x
                                     x)

                                   (F/Times
                                     x
                                     (F/Times
                                       (F/Cos (F/ArcCos x))
                                       (F/Sin (F/ArcSin x))))))})
                      :expr str))
            {:expr    "x*(x+x^2+Cos(x)+Sin(x))"
             :simple? true}))))

  (testing "simplify deep 2"
    (let [x (F/Dummy "x")]
      (is (=
            (binding [ops-common/*simplify-max-leafs* 100]
              (update (ops-common/maybe-simplify
                        {:expr (F/Sin
                                 (F/Times
                                   (F/Plus (F/Cos x) (F/Cos x))
                                   (F/Sqrt
                                     (F/Times
                                       (F/Sin (F/Plus (F/Exp x) (F/Cos x)))
                                       (F/Times
                                         (F/Sqrt (F/ArcCos (F/Exp x)))
                                         (F/Times
                                           (F/Cos (F/ArcCos x))
                                           (F/Sin (F/ArcSin x))))))))})
                      :expr str))
            {:expr    "Sin(2*Cos(x)*Sqrt(x^2*Sqrt(ArcCos(E^x))*Sin(E^x+Cos(x))))"
             :simple? true}))))

  (testing "simplify deep 3"
    (let [x (F/Dummy "x")]
      (is (=
            (binding [ops-common/*simplify-max-leafs* 100]
              (update (ops-common/maybe-simplify
                        {:expr
                         (F/Times
                           (F/Subtract (F/Power (F/Times F/CN1 F/E) x)
                                       (F/Sqrt x))
                           (F/Csc x))})
                      :expr str))
            {:expr    "((-E)^x-Sqrt(x))*Csc(x)"
             :simple? true}))))

  (testing "simplify deep 4"
    (let [x (F/Dummy "x")]
      (is (=
            (binding [ops-common/*simplify-max-leafs* 100]
              (update (ops-common/maybe-simplify
                        {:expr
                         (F/Times
                           (F/Times F/CN1 F/C1D5)
                           (F/Times (F/Cos (F/Sqrt x))
                                    (F/Sin (F/Plus F/C1D2 (F/Sin x)))))})
                      :expr str))
            {:expr    "-1/5*Cos(Sqrt(x))*Sin(1/2+Sin(x))"
             :simple? true}))))

  #_(testing "simplify deep 5 goes on for almost 2 minutes"
      (let [x (F/Dummy "x")]
        (is (=
              (binding [ops-common/*simplify-max-leafs* 100]
                (update (ops-common/maybe-simplify
                          {:expr
                           (F/Divide
                             (F/Times (F/Cos x) (F/num 0.605))
                             (F/Plus (F/Divide F/C1 F/C100)
                                     (F/Power (F/Cos x)
                                              (F/Times F/C100 (F/Cos x)))))})
                        :expr str))
              {:expr    "(0.605*Cos(x))/(1/100+Cos(x)^(100*Cos(x)))"
               :simple? true}))))

  (testing "simplify no-op"
    (let [x (F/Dummy "x")]
      (is (=
            (update (ops-common/maybe-simplify {:simple? true
                                                :expr    (F/Plus (F/Cos x) (F/Cos x))})
                    :expr str)
            {:expr    "Cos(x)+Cos(x)"
             :simple? true}))))

  #_(testing "simplify timeout"
      (let [x (F/Dummy "x")]
        (is (=
              (binding [ops-common/*simplify-timeout* 1]
                (update (ops-common/maybe-simplify {:expr (F/Plus (F/Cos x) (F/Cos x))})
                        :expr str))
              {:expr    "Cos(x)+Cos(x)"
               :simple? true})))))










(comment (run-tests 'closyr.ops-common-test))