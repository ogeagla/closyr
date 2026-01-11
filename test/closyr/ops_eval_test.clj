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

    (testing "basic assumptions"

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
             nil)))

    (testing "when eval throws exception"
      (is (= (with-redefs-fn {#'ops-common/expr->fn (fn [_] (throw (Exception. "Test exception")))}
               (fn []
                 (ops-eval/eval-phenotype-on-expr-args
                   (ops-common/->phenotype x (F/Subtract x F/C1D2) (ops-common/new-util))
                   (ops-common/exprs->exprs-list (ops-common/doubles->exprs [0.5 1.0])))))
             nil)))

    (testing "eval on nil expr defaults to y=x"
      (is (= (str (ops-eval/eval-phenotype-on-expr-args
                    (ops-common/->phenotype x F/NIL nil)
                    (ops-common/exprs->exprs-list (ops-common/doubles->exprs [0.0 1.0]))))
             "{0.0,1.0}")))

    (testing "can eval various fns for simple inputs"
      (is (= (mapv
               ops-common/expr->double
               (ops-eval/eval-phenotype-on-expr-args
                 (ops-common/->phenotype x (F/Subtract x F/C1D2) (ops-common/new-util))
                 (ops-common/exprs->exprs-list (ops-common/doubles->exprs [0.5 1.0]))))
             [0.0 0.5])))

    (testing "can eval various fns for simple inputs without provided util in pheno"
      (is (= (mapv
               ops-common/expr->double
               (ops-eval/eval-phenotype-on-expr-args
                 (dissoc (ops-common/->phenotype x (F/Subtract x F/C1D2) (ops-common/new-util)) :util)
                 (ops-common/exprs->exprs-list (ops-common/doubles->exprs [0.5 1.0]))))
             [0.0 0.5])))

    (testing "can eval various fns for simple inputs without provided expr in pheno"
      (is (= (ops-eval/eval-phenotype-on-expr-args
               (dissoc (ops-common/->phenotype x (F/Subtract x F/C1D2) (ops-common/new-util)) :expr)
               (ops-common/exprs->exprs-list (ops-common/doubles->exprs [0.5 1.0])))
             nil)))

    (testing "can eval various fns for simple inputs and y=x"
      (is (= (mapv
               ops-common/expr->double
               (ops-eval/eval-phenotype-on-expr-args
                 (ops-common/->phenotype x (F/Times x F/C1) nil)
                 (ops-common/exprs->exprs-list (ops-common/doubles->exprs [0.5 1.0]))))
             [0.5 1.0])))

    (testing "indeterminate eval results, defaults to using y=x"

      (is (= (ops-eval/eval-vec-pheno
               (ops-common/->phenotype x F/Indeterminate nil)
               {:input-xs-list  (ops-common/exprs->exprs-list (ops-common/doubles->exprs [0.5]))
                :input-xs-count 1})
             [0.5]))

      (is (= (with-redefs-fn {#'ops-eval/eval-phenotype-on-expr-args (fn [_ _] F/Indeterminate)}
               (fn []
                 (ops-eval/eval-vec-pheno
                   (ops-common/->phenotype x (F/Subtract F/C1 F/C1D2) nil)
                   {:input-xs-list  (ops-common/exprs->exprs-list (ops-common/doubles->exprs [0.5]))
                    :input-xs-count 1})))
             nil))

      (is (= (with-redefs-fn {#'ops-eval/eval-phenotype-on-expr-args (fn [_ _] nil)}
               (fn []
                 (ops-eval/eval-vec-pheno
                   (ops-common/->phenotype x (F/Subtract F/C1 F/C1D2) nil)
                   {:input-xs-list  (ops-common/exprs->exprs-list (ops-common/doubles->exprs [0.5]))
                    :input-xs-count 1})))
             nil)))


    (testing "with failing get-arg handles error"
      (is (=
            (with-redefs-fn {#'ops-eval/get-arg (fn [_ _ _] (throw (Exception. "Test exception")))}
              (fn []
                (ops-eval/eval-vec-pheno
                  (ops-common/->phenotype x (F/Subtract x F/C1D2) nil)
                  {:input-xs-list  (ops-common/exprs->exprs-list (ops-common/doubles->exprs [0.5]))
                   :input-xs-count 1})))
            [##Inf])))

    (testing "with failing conversion returns infinity"
      ;; Exceptions during conversion are caught and return infinity
      (is (=
            (with-redefs-fn {#'ops-common/expr->double (fn [_] (throw (Exception. "Test exception")))}
              (fn []
                (ops-eval/eval-vec-pheno
                  (ops-common/->phenotype x (F/Subtract F/C1 F/C1D2) nil)
                  {:input-xs-list  (ops-common/exprs->exprs-list (ops-common/doubles->exprs [0.5]))
                   :input-xs-count 1})))
            [##Inf])))

    (testing "with failing constant input conversion returns infinity"
      ;; Exceptions in result-args->constant-input are caught and return infinity
      (is (=
            (with-redefs-fn {#'ops-eval/result-args->constant-input (fn [_ _ _] (throw (Exception. "Test exception")))}
              (fn []
                (ops-eval/eval-vec-pheno
                  (ops-common/->phenotype x (F/Subtract x F/C1D2) nil)
                  {:input-xs-list  (ops-common/exprs->exprs-list (ops-common/doubles->exprs [0.5 1.0]))
                   :input-xs-count 1})))
            [##Inf])))

    (testing "with failing conversion handles error"
      (is (=
            (with-redefs-fn {#'ops-eval/get-arg (fn [_ _ _] (F/num 0.1))}
              (fn []
                (ops-eval/eval-vec-pheno
                  (ops-common/->phenotype x (F/Subtract x F/C1D2) nil)
                  {:input-xs-list  (ops-common/exprs->exprs-list (ops-common/doubles->exprs [0.5 1.0]))
                   :input-xs-count 1})))
            [0.1])))

    (testing "can eval various fns for simple inputs 2"
      (is (= (mapv
               ops-common/expr->double
               (ops-eval/eval-phenotype-on-expr-args
                 (ops-common/->phenotype x (F/Subtract x F/C1D2) nil)
                 (ops-common/exprs->exprs-list (ops-common/doubles->exprs [0.5 1.0]))))
             [0.0 0.5]))

      (is (= (mapv
               ops-common/expr->double
               (ops-eval/eval-phenotype-on-expr-args
                 (ops-common/->phenotype x (F/Subtract x F/C1D2) nil)
                 (ops-common/exprs->exprs-list (ops-common/doubles->exprs [##Inf]))))
             [##Inf]))

      (is (= (mapv
               ops-common/expr->double
               (ops-eval/eval-phenotype-on-expr-args
                 (ops-common/->phenotype x (F/Subtract x F/Infinity) nil)
                 (ops-common/exprs->exprs-list (ops-common/doubles->exprs [0.0]))))
             [##-Inf]))


      (is (= (ops-eval/eval-vec-pheno
               (ops-common/->phenotype x (F/Subtract F/E F/C1D2) (ops-common/new-util))
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
             [##Inf ##Inf 0.0]))

      (is (= (ops-eval/eval-vec-pheno
               (ops-common/->phenotype x (F/C1) nil)
               {:input-xs-list  (ops-common/exprs->exprs-list (ops-common/doubles->exprs [0.0 0.5 1.0]))
                :input-xs-count 3})
             [1.0 1.0 1.0]))

      (is (= (ops-eval/eval-vec-pheno
               (ops-common/->phenotype x F/Infinity nil)
               {:input-xs-list  (ops-common/exprs->exprs-list (ops-common/doubles->exprs [0.0 0.5 1.0]))
                :input-xs-count 3})
             [0.0 0.5 1.0]))

      (is (= (ops-eval/eval-vec-pheno
               (ops-common/->phenotype x F/CN1 nil)
               {:input-xs-list  (ops-common/exprs->exprs-list (ops-common/doubles->exprs [0.0 0.5 1.0]))
                :input-xs-count 3})
             [-1.0 -1.0 -1.0]))

      (is (= (ops-eval/eval-vec-pheno
               (ops-common/->phenotype x (F/num 0.123) nil)
               {:input-xs-list  (ops-common/exprs->exprs-list (ops-common/doubles->exprs [0.0 0.5 1.0]))
                :input-xs-count 3})
             [0.123 0.123 0.123]))

      (is (= (ops-eval/eval-vec-pheno
               (ops-common/->phenotype x (F/Plus F/CN1 F/C1) nil)
               {:input-xs-list  (ops-common/exprs->exprs-list (ops-common/doubles->exprs [0.0 0.5 1.0]))
                :input-xs-count 3})
             [0.0 0.0 0.0]))

      (is (= (ops-eval/eval-vec-pheno
               (ops-common/->phenotype x x nil)
               {:input-xs-list  (ops-common/exprs->exprs-list (ops-common/doubles->exprs [0.0 0.5 1.0]))
                :input-xs-count 3})
             [0.0 0.5 1.0])))))


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


(deftest eval-extended-test
  (let [x (F/Dummy "x")]
    (testing "eval-extended returns nil when middle-section is nil"
      (is (= (with-redefs-fn {#'ops-eval/eval-vec-pheno (fn [_ _] nil)}
               (fn []
                 (ops-eval/eval-extended
                   (ops-common/->phenotype x (F/Sin x) nil)
                   {:input-xs-list  (ops-common/exprs->exprs-list (ops-common/doubles->exprs [1.0 2.0]))
                    :input-xs-count 2}
                   {:x-head      [0.5]
                    :x-head-list (ops-common/exprs->exprs-list (ops-common/doubles->exprs [0.5]))
                    :x-tail      [3.0]
                    :x-tail-list (ops-common/exprs->exprs-list (ops-common/doubles->exprs [3.0]))})))
             nil)))

    (testing "eval-extended returns nil when middle-section is empty"
      (is (= (with-redefs-fn {#'ops-eval/eval-vec-pheno (fn [_ _] [])}
               (fn []
                 (ops-eval/eval-extended
                   (ops-common/->phenotype x (F/Sin x) nil)
                   {:input-xs-list  (ops-common/exprs->exprs-list (ops-common/doubles->exprs [1.0 2.0]))
                    :input-xs-count 2}
                   {:x-head      [0.5]
                    :x-head-list (ops-common/exprs->exprs-list (ops-common/doubles->exprs [0.5]))
                    :x-tail      [3.0]
                    :x-tail-list (ops-common/exprs->exprs-list (ops-common/doubles->exprs [3.0]))})))
             nil)))))

(deftest long-running-fns-test

  ;; 2.55+x+(-1.33+1.31736/x^(4351/200000)+x*(9/10+0.89*Log(-3/5+x))-133/100*Log(x))^(101/100)-Log(x)
  (testing "a known long-running fn"
    (let [x (F/Dummy "x")
          p (ops-common/->phenotype
              x
              (F/Plus (F/num 2.55)
                      (F/Plus x
                              (F/Subtract
                                (F/Power
                                  (F/Plus
                                    (F/num -1.33)
                                    (F/Subtract
                                      (F/Plus
                                        (F/Divide
                                          (F/num 1.31726)
                                          (F/Power x (F/num (float (/ 4351 200000)))))
                                        (F/Times x
                                                 (F/Plus
                                                   (F/num 0.9)
                                                   (F/Times (F/num 0.89)
                                                            (F/Log (F/Plus (F/num -0.6) x))))))
                                      (F/Times (F/num 1.33)
                                               (F/Log x))))
                                  (F/num 1.01))
                                (F/Log x))))
              (ops-common/new-util))]

      (is (= [3.6198954469921762
              5.30675637508254
              8.028748008549133
              11.334902089392536
              15.031610064000752
              19.017953836452868
              23.233095713241145
              27.63672344397211
              32.200313077602296
              36.90268798682677
              41.72753958418945
              46.66194111908503
              51.695405636638846
              56.81926140603571
              62.02622223443477
              67.3100825662299
              72.66549536889605
              78.0878066313582
              83.57292960227778
              89.11724756826268
              94.71753754430598
              100.37090956267996
              106.07475778318434
              111.82672069109606
              117.62464837198287
              123.46657536234513
              129.3506979405347
              135.2753549883469
              141.23901174985158
              147.24024596055037
              153.27773593065334
              159.35025025080688
              165.45663885380483
              171.5958252165628
              177.76679952647245
              183.9686126677778
              190.20037090875272
              196.4612311906346
              202.75039693558395
              209.06711430420148
              215.41066884398796
              221.7803824790561
              228.17561079878726
              234.59574060926056
              241.04018771640287
              247.50839491410636
              253.99983015418005
              260.5139848780605
              267.05037249280963
              273.60852697614
              280.188001597104
              286.78836774070965
              293.4092138261243
              300.0501443093376
              306.71077876220386
              313.39075102068966
              320.08970839594997
              326.8073109425465
              333.5432307787265
              340.2971514542159
              347.068767361446
              353.85778318654803
              360.66391339681456
              367.4868817616474
              374.3264209043056
              381.1822718820126
              388.0541837922185
              394.94191340300966
              401.8452248058449
              408.76388908895416
              415.69768402988905
              422.64639380583674
              429.60980872043854
              436.58772494594757
              443.57994427966935
              450.58627391370453
              457.6065262171033
              464.6405185296001
              471.6880729661739
              478.74901623172957
              485.82317944525664
              492.91039797286317
              500.01051126913455
              507.12336272630375
              514.2487995307574
              521.3866725264379
              528.536836084731
              535.6991479804582
              542.8734692736228
              550.0596641965745
              557.2576000462905
              564.4671470814827
              571.6881784242667
              578.9205699661395
              586.1642002780333
              593.4189505242285
              600.6847043799195
              607.9613479522391
              615.2487697045685]
             (mapv
               ops-common/expr->double
               (ops-eval/eval-phenotype-on-expr-args
                 p
                 (ops-common/exprs->exprs-list (ops-common/doubles->exprs (vec (range 1.0 100.0 1.0)))))))
          "Can eval long running fn"))))


(comment (run-tests 'closyr.ops-eval-test))
