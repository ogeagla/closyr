(ns closyr.ops-common-test
  (:require
    [clojure.test :refer :all]
    [closyr.ops.common :as ops-common])
  (:import
    (org.matheclipse.core.eval.exception
      ArgumentTypeException)
    (org.matheclipse.core.expression
      F)
    (org.matheclipse.core.interfaces
      IAST
      IExpr
      ISymbol)))


(set! *warn-on-reflection* true)

(alter-var-root #'ops-common/*simplify-probability-sampler* (constantly 2.0))
(alter-var-root #'ops-common/*simplify-max-leafs* (constantly 100))


(deftest expr->numbers-test
  (testing "simple case"
    (is (= (ops-common/exprs->doubles [F/C1 (F/num 0.1234)])
           [1.0 0.1234])))

  (testing "simple case with infinity"
    (is (= (ops-common/exprs->doubles [F/C1 (F/num 0.1234) F/Infinity])
           [1.0 0.1234 ##Inf])))

  (testing "simple case with Complex Number"
    (is (thrown? ArgumentTypeException (ops-common/exprs->doubles [F/C1 (F/num 0.1234) F/I])))))


(deftest create-pheno-test
  (testing "with valid expr"
    (is (= (let [x (F/Dummy "x")]
             (str (:expr (ops-common/->phenotype x (F/Sin x) nil))))
           "Sin(x)")))

  (testing "with expr throwing exception"
    (is (=
          (let [x (F/Dummy "x")]
            (with-redefs-fn {#'ops-common/valid-expr-or-default (fn [variable iexpr]
                                                                  (throw (Exception. "Test exception")))}
              (fn []
                (ops-common/->phenotype x (F/Sin x) nil))))
          nil)))

  (testing "with valid expr 2"
    (is (= (let [x (F/Dummy "x")]
             (str (:expr (ops-common/->phenotype x x nil))))
           "x")))

  (testing "with invalid expr, defaults to y=x"
    (is (= (let [x (F/Dummy "x")]
             (str (:expr (ops-common/->phenotype x F/Times nil))))
           "x")))

  (testing "with invalid expr 2, defaults to y=x"
    (is (= (let [x (F/Dummy "x")]
             (str (:expr (ops-common/->phenotype x F/Sin nil))))
           "x")))

  (testing "with nil expr defaults to y=x"
    (is (= (str (:expr (ops-common/->phenotype (F/Dummy "x") F/NIL nil)))
           "x"))))


(deftest parse-expr-str-test
  (testing "can parse simple fn str"
    (let [x      (F/Dummy "x")
          parsed (.parse (ops-common/new-eval-engine) "Cos(x)")]
      (is (instance? IAST parsed))
      (is (= (str parsed)
             (str (F/Cos x))))))
  (testing "can parse simple fn str 2"
    (let [x      (F/Dummy "x")
          parsed (.parse (ops-common/new-eval-engine) "x+Cos(x^2)")]
      (is (instance? IAST parsed))
      (is (= (str parsed)
             "x+Cos(x^2)")))))


(deftest simplify-with-slow-functions
  (testing "simplify takes a long time logs the slow fn"
    (binding [ops-common/*long-simplify-thresh-ms* 500]
      (with-redefs-fn {#'ops-common/simplify (fn [^IAST expr]
                                               (Thread/sleep 600)
                                               (F/Simplify expr))}
        (fn []
          (let [x (F/Dummy "x")]
            (is (= (update (ops-common/maybe-simplify {:expr (F/Plus F/C1D5 (F/Sin (F/ArcSin x)))})
                           :expr str)
                   {:expr    "1/5+x"
                    :simple? true}))
            (is (= @ops-common/do-not-simplify-fns*
                   {"1/5+Sin(ArcSin(x))" 1}))))))))


(deftest simplify-test
  (testing "simplify trig compose"
    (let [x (F/Dummy "x")]
      (is (= (dissoc (update (ops-common/maybe-simplify {:expr (F/Sin (F/ArcSin x)) :util (ops-common/new-util)})
                             :expr str)
                     :util)
             {:expr    "x"
              :simple? true}))))

  (testing "simplify function too long"
    (let [x (F/Dummy "x")]
      (is (= (binding [ops-common/*simplify-max-leafs* 10]
               (update (ops-common/maybe-simplify {:expr
                                                   (-> (F/Cos (F/ArcSin x))
                                                       (F/Plus (F/Tan (F/ArcSin x)))
                                                       (F/Times F/C2)
                                                       (F/Plus F/C1)
                                                       (F/Plus (F/Sin x))
                                                       (F/Plus (F/Cos x))
                                                       (F/Plus (F/Sqrt x)))})
                       :expr str))
             {:expr "1+Sqrt(x)+Cos(x)+Sin(x)+2*(Cos(ArcSin(x))+Tan(ArcSin(x)))"}))))

  (testing "simplify ignore"
    (reset! ops-common/do-not-simplify-fns* {"x" 1})
    (let [x (F/Dummy "x")]
      (is (= (update (ops-common/maybe-simplify {:expr x})
                     :expr str)
             {:expr    "x"
              :simple? true}))
      (is (= @ops-common/do-not-simplify-fns*
             {"x" 2})))
    (reset! ops-common/do-not-simplify-fns* {}))

  (testing "simplify sum"
    (let [x (F/Dummy "x")]
      (is (= (binding [ops-common/*simplify-max-leafs* 100]
               (update (ops-common/maybe-simplify {:expr (F/Plus (F/Cos x) (F/Cos x))})
                       :expr str))
             {:expr    "2*Cos(x)"
              :simple? true}))))

  (testing "simplify deep 1"
    (let [x (F/Dummy "x")]
      (is (= (binding [ops-common/*simplify-max-leafs* 100]
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
      (is (= (binding [ops-common/*simplify-max-leafs* 100]
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
      (is (= (binding [ops-common/*simplify-max-leafs* 100]
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
      (is (= (binding [ops-common/*simplify-max-leafs* 100]
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
        (is (= (binding [ops-common/*simplify-max-leafs* 100]
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
      (is (= (update (ops-common/maybe-simplify {:simple? true
                                                 :expr    (F/Plus (F/Cos x) (F/Cos x))})
                     :expr str)
             {:expr    "Cos(x)+Cos(x)"
              :simple? true}))))

  #_(testing "simplify timeout"
      (let [x (F/Dummy "x")]
        (is (= (binding [ops-common/*simplify-timeout* 1]
                 (update (ops-common/maybe-simplify {:expr (F/Plus (F/Cos x) (F/Cos x))})
                         :expr str))
               {:expr    "Cos(x)+Cos(x)"
                :simple? true})))))


(comment (run-tests 'closyr.ops-common-test))
