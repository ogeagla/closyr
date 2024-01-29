(ns clj-symbolic-regression.ops-test
  (:require
    [clj-symbolic-regression.ops :as ops]
    [clojure.test :refer :all])
  (:import
    (org.matheclipse.core.expression
      F)
    (org.matheclipse.core.interfaces
      IAST
      IExpr
      ISymbol)))


(set! *warn-on-reflection* true)


(deftest modify-test
  (testing "substitute"
    (let [x (F/Dummy "x")]
      (is (= (str (F/Sin x))
             (str (:expr (ops/modify
                           {:op           :modify-substitute
                            :find-expr    F/Cos
                            :replace-expr F/Sin}
                           {:sym  x
                            :expr (F/Cos x)})))))))

  (testing "fn"
    (let [x (F/Dummy "x")]
      (is (= (str (.plus x (F/Sin x)))
             (str (:expr (ops/modify
                           {:op          :modify-fn
                            :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                                           (F/Plus expr (F/Sin expr)))}
                           {:sym  x
                            :expr (.plus F/C0 x)})))))))

  (testing "modify-leafs"
    (let [x (F/Dummy "x")]
      (is (= (str (.plus (F/num 1.0) ^IExpr (F/Sin x)))
             (str
               (:expr
                 (ops/modify
                   {:op               :modify-leafs
                    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                                        (if (and (= (.toString ie) "x"))
                                          (F/Sin ie)
                                          ie))}
                   {:sym  x
                    :expr (.plus (F/num 1.0) x)})))))))

  (testing "modify-branches"
    (let [x (F/Dummy "x")]
      (is (= (str (F/Cos (.plus (F/num 1.0) x)))
             (str
               (:expr
                 (ops/modify
                   {:op               :modify-branches
                    :label            "branch cos"
                    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                                        (F/Cos ie))}
                   {:sym  x
                    :expr (.plus (F/num 1.0) x)})))))))

  (testing "modify-ast-head"
    (let [x (F/Dummy "x")]
      (is (= (str (.plus (F/num 1.0) ^IExpr (F/Cos x)))
             (str
               (:expr
                 (ops/modify
                   {:op               :modify-ast-head
                    :label            "sin->cos"
                    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                                        (if (= F/Sin ie)
                                          F/Cos
                                          ie))}
                   {:sym  x
                    :expr (.plus (F/num 1.0) ^IExpr (F/Sin x))}))))))))


(deftest mutations-test

  (testing "all mutations results on a function"
    (with-redefs-fn {#'rand (fn [] 0.0)}

      (fn []
        (let [x (F/Dummy "x")]
          (is (= [[:modify-fn
                   "Derivative"
                   "1+x*Cos(1/2-x)-Sin(1/2-x)-Sin(x)"]
                  [:modify-fn
                   "+1/2"
                   "-1/2+x+Cos(x)-x*Sin(1/2-x)"]
                  [:modify-fn
                   "-1/2"
                   "-3/2+x+Cos(x)-x*Sin(1/2-x)"]
                  [:modify-fn
                   "+1/10"
                   "-9/10+x+Cos(x)-x*Sin(1/2-x)"]
                  [:modify-fn
                   "-1/10"
                   "-11/10+x+Cos(x)-x*Sin(1/2-x)"]
                  [:modify-fn
                   "+1/100"
                   "-99/100+x+Cos(x)-x*Sin(1/2-x)"]
                  [:modify-fn
                   "-1/100"
                   "-101/100+x+Cos(x)-x*Sin(1/2-x)"]
                  [:modify-fn
                   "+Sin"
                   "-1+x+Cos(x)-x*Sin(1/2-x)+Sin(x)"]
                  [:modify-fn
                   "-Sin"
                   "-1+x+Cos(x)-x*Sin(1/2-x)-Sin(x)"]
                  [:modify-fn
                   "+Log"
                   "-1+x+Cos(x)+Log(x)-x*Sin(1/2-x)"]
                  [:modify-fn
                   "-Log"
                   "-1+x+Cos(x)-Log(x)-x*Sin(1/2-x)"]
                  [:modify-fn
                   "+Exp"
                   "-1+E^x+x+Cos(x)-x*Sin(1/2-x)"]
                  [:modify-fn
                   "-Exp"
                   "-1-E^x+x+Cos(x)-x*Sin(1/2-x)"]
                  [:modify-fn
                   "+Cos"
                   "-1+x+2*Cos(x)-x*Sin(1/2-x)"]
                  [:modify-fn
                   "-Cos"
                   "-1+x-x*Sin(1/2-x)"]
                  [:modify-fn
                   "*Sin"
                   "(-1+x+Cos(x)-x*Sin(1/2-x))*Sin(x)"]
                  [:modify-fn
                   "/Sin"
                   "Csc(x)*(-1+x+Cos(x)-x*Sin(1/2-x))"]
                  [:modify-fn
                   "*Cos"
                   "Cos(x)*(-1+x+Cos(x)-x*Sin(1/2-x))"]
                  [:modify-fn
                   "/Cos"
                   "Sec(x)*(-1+x+Cos(x)-x*Sin(1/2-x))"]
                  [:modify-fn
                   "+x"
                   "-1+2*x+Cos(x)-x*Sin(1/2-x)"]
                  [:modify-fn
                   "-x"
                   "-1+Cos(x)-x*Sin(1/2-x)"]
                  [:modify-fn
                   "+x^2"
                   "-1+x+x^2+Cos(x)-x*Sin(1/2-x)"]
                  [:modify-fn
                   "-x^2"
                   "-1+x-x^2+Cos(x)-x*Sin(1/2-x)"]
                  [:modify-fn
                   "+x^1/2"
                   "-1+Sqrt(x)+x+Cos(x)-x*Sin(1/2-x)"]
                  [:modify-fn
                   "-x^1/2"
                   "-1-Sqrt(x)+x+Cos(x)-x*Sin(1/2-x)"]
                  [:modify-fn
                   "*x"
                   "x*(-1+x+Cos(x)-x*Sin(1/2-x))"]
                  [:modify-fn
                   "/x"
                   "(-1+x+Cos(x)-x*Sin(1/2-x))/x"]
                  [:modify-fn
                   "1/f"
                   "1/(-1+x+Cos(x)-x*Sin(1/2-x))"]
                  [:modify-fn
                   "*-1"
                   "1-x-Cos(x)+x*Sin(1/2-x)"]
                  [:modify-fn
                   "/2"
                   "1/2*(-1+x+Cos(x)-x*Sin(1/2-x))"]
                  [:modify-fn
                   "*2"
                   "2*(-1+x+Cos(x)-x*Sin(1/2-x))"]
                  [:modify-fn
                   "/10"
                   "1/10*(-1+x+Cos(x)-x*Sin(1/2-x))"]
                  [:modify-fn
                   "*10"
                   "10*(-1+x+Cos(x)-x*Sin(1/2-x))"]
                  [:modify-fn
                   "/100"
                   "1/100*(-1+x+Cos(x)-x*Sin(1/2-x))"]
                  [:modify-fn
                   "*100"
                   "100*(-1+x+Cos(x)-x*Sin(1/2-x))"]
                  [:modify-fn
                   "*1.1"
                   "1.1*(-1+x+Cos(x)-x*Sin(1/2-x))"]
                  [:modify-fn
                   "*0.9"
                   "0.9*(-1+x+Cos(x)-x*Sin(1/2-x))"]
                  [:modify-leafs
                   "x+1/2"
                   "-1/2+x+Cos(1/2+x)+(1/2+x)*Sin(x)"]
                  [:modify-leafs
                   "x-1/2"
                   "-3/2+x+Cos(1/2-x)+(1/2-x)*Sin(1-x)"]
                  [:modify-leafs
                   "x/10"
                   "-1+x/10+Cos(x/10)-1/10*x*Sin(1/2-x/10)"]
                  [:modify-leafs
                   "10*x"
                   "-1+10*x+Cos(10*x)-10*x*Sin(1/2-10*x)"]
                  [:modify-leafs
                   "1/x"
                   "-1+1/x+Cos(1/x)-Sin(1/2-1/x)/x"]
                  [:modify-leafs
                   "x/100"
                   "-1+x/100+Cos(x/100)-1/100*x*Sin(1/2-x/100)"]
                  [:modify-leafs
                   "100*x"
                   "-1+100*x+Cos(100*x)-100*x*Sin(1/2-100*x)"]
                  [:modify-leafs
                   "-1*x"
                   "-1-x+Cos(x)+x*Sin(1/2+x)"]
                  [:modify-leafs
                   "1.1*x"
                   "-1+1.1*x+Cos(1.1*x)-1.1*x*Sin(1/2-1.1*x)"]
                  [:modify-leafs
                   "0.9*x"
                   "-1+0.9*x+Cos(0.9*x)-0.9*x*Sin(1/2-0.9*x)"]
                  [:modify-leafs
                   "sin(x)"
                   "-1+Cos(Sin(x))+Sin(x)-Sin(x)*Sin(1/2-Sin(x))"]
                  [:modify-leafs
                   "cos(x)"
                   "-1+Cos(x)+Cos(Cos(x))-Cos(x)*Sin(1/2-Cos(x))"]
                  [:modify-leafs
                   "log(x)"
                   "-1+Cos(Log(x))+Log(x)-Log(x)*Sin(1/2-Log(x))"]
                  [:modify-leafs
                   "exp(x)"
                   "-1+E^x+Cos(E^x)-E^x*Sin(1/2-E^x)"]
                  [:modify-leafs
                   "x^1/2"
                   "-1+Sqrt(x)+Cos(Sqrt(x))-Sqrt(x)*Sin(1/2-Sqrt(x))"]
                  [:modify-leafs
                   "x^2"
                   "-1+x^2+Cos(x^2)-x^2*Sin(1/2-x^2)"]
                  [:modify-leafs
                   "x+1/10"
                   "-9/10+x+Cos(1/10+x)-(1/10+x)*Sin(2/5-x)"]
                  [:modify-leafs
                   "x-1/10"
                   "-11/10+x+Cos(1/10-x)+(1/10-x)*Sin(3/5-x)"]
                  [:modify-leafs
                   "x+1/100"
                   "-99/100+x+Cos(1/100+x)-(1/100+x)*Sin(49/100-x)"]
                  [:modify-leafs
                   "x-1/100"
                   "-101/100+x+Cos(1/100-x)+(1/100-x)*Sin(51/100-x)"]
                  [:modify-leafs
                   "c/2"
                   "-1/2+x+Cos(x)-1/2*x*Sin(1/4-x/2)"]
                  [:modify-leafs
                   "c*2"
                   "-2+x+Cos(x)-2*x*Sin(1-2*x)"]
                  [:modify-leafs
                   "c*-1"
                   "1+x+Cos(x)-x*Sin(1/2-x)"]
                  [:modify-leafs
                   "c/10"
                   "-1/10+x+Cos(x)-1/10*x*Sin(1/20-x/10)"]
                  [:modify-leafs
                   "c*10"
                   "-10+x+Cos(x)-10*x*Sin(5-10*x)"]
                  [:modify-leafs
                   "c+1/10"
                   "-9/10+x+Cos(x)-9/10*x*Sin(3/5-9/10*x)"]
                  [:modify-leafs
                   "c-1/10"
                   "-11/10+x+Cos(x)-11/10*x*Sin(2/5-11/10*x)"]
                  [:modify-leafs
                   "c+1/100"
                   "-99/100+x+Cos(x)-99/100*x*Sin(51/100-99/100*x)"]
                  [:modify-leafs
                   "c-1/100"
                   "-101/100+x+Cos(x)-101/100*x*Sin(49/100-101/100*x)"]
                  [:modify-leafs
                   "c+1/2"
                   "-1/2+x+Cos(x)-1/2*x*Sin(1-x/2)"]
                  [:modify-leafs
                   "c-1/2"
                   "-3/2+x+Cos(x)+3/2*x*Sin(3/2*x)"]
                  [:modify-substitute
                   "Sin->Cos"
                   "-1+x-x*Cos(1/2-x)+Cos(x)"]
                  [:modify-substitute
                   "Cos->Sin"
                   "-1+x-x*Sin(1/2-x)+Sin(x)"]
                  [:modify-ast-head
                   "sin->cos"
                   "-1+x-x*Cos(1/2-x)+Cos(x)"]
                  [:modify-ast-head
                   "cos->sin"
                   "-1+x-x*Sin(1/2-x)+Sin(x)"]
                  [:modify-ast-head
                   "+->*"
                   "-x^2*Cos(x)*Sin(x/2)"]
                  [:modify-ast-head
                   "*->+"
                   "-2+2*x+Cos(x)-Sin(1/2-x)"]
                  [:modify-ast-head
                   "^->*"
                   "-1+x+Cos(x)-x*Sin(1/2-x)"]
                  [:modify-ast-head
                   "/->*"
                   "-1+x+Cos(x)-x*Sin(1/2-x)"]
                  [:modify-ast-head
                   "/->+"
                   "-1+x+Cos(x)-x*Sin(1/2-x)"]
                  [:modify-branches
                   "b derivative"
                   "1-Cos(x)"]
                  [:modify-branches
                   "b sin"
                   "-Sin(1-x-Sin(Cos(x))+Sin(x*Sin(Sin(Sin(1/2-Sin(x))))))"]
                  [:modify-branches
                   "b cos"
                   "Cos(1-x-Cos(Cos(x))-Cos(x*Cos(Sin(Cos(1/2+Cos(x))))))"]
                  [:modify-branches
                   "b exp"
                   "E^(-1+E^(-E^Sin(E^(1/2+E^(-x)))*x)+E^Cos(x)+x)"]
                  [:modify-branches
                   "b log"
                   "Log(-1+x+Log(Cos(x))+Log(-x*Log(Sin(Log(1/2+Log(-x))))))"]
                  [:modify-branches
                   "b*b"
                   "(1-x-Cos(x)^2-x^2*Sin((1/2+x^2)^2)^4)^2"]
                  [:modify-branches
                   "b^1/2"
                   "Sqrt(-1+x+Sqrt(Cos(x))+Sqrt(-x*Sqrt(Sin(Sqrt(1/2+Sqrt(-x))))))"]
                  [:modify-branches
                   "b^-2"
                   "1/(1-x-Sec(x)^2-Sin(1/(1/2+1/x^2)^2)^4/x^2)^2"]
                  [:modify-branches
                   "b^-1"
                   "1/(-1+x+Sec(x)-Sin(1/(1/2-1/x))/x)"]
                  [:modify-branches
                   "b*-1"
                   "1-x+Cos(x)-x*Sin(1/2+x)"]
                  [:modify-branches
                   "b*1.1"
                   "1.1*(-1+x+1.1*Cos(x)-1.2100000000000002*x*Sin(1.1*(1/2-1.1*x)))"]
                  [:modify-branches
                   "b*0.9"
                   "0.9*(-1+x+0.9*Cos(x)-0.81*x*Sin(0.9*(1/2-0.9*x)))"]
                  [:modify-branches
                   "b+0.1"
                   "-0.7000000000000001+x+Cos(x)-x*(0.1+Sin(0.7-x))"]
                  [:modify-branches
                   "b-0.1"
                   "-1.3000000000000003+x+Cos(x)+x*(0.1-Sin(0.3-x))"]]
                 (vec
                   (map
                     (fn [m]
                       [(:op m)
                        (:label m)
                        (str
                          (:expr
                            (ops/modify
                              m
                              {:sym  x
                               ;; x + cos(x) + x*sin(x-0.5) - 1
                               :expr (.minus
                                       (.plus
                                         x
                                         (.plus
                                           (F/Cos x)
                                           (.times
                                             x
                                             (F/Sin (.minus x F/C1D2)))))
                                       F/C1)})))])
                     (ops/initial-mutations))))))))))


(deftest crossover-test
  (with-redefs-fn {#'rand-int (fn [maxv] (dec maxv))
                   #'rand-nth (fn [coll] (first coll))}
    (fn []
      (with-redefs [ops/crossover-sampler [:plus]]
        (let [x (F/Dummy "x")]
          (testing "Can crossover mix of IExpr and IAST"
            (is (= (str (F/Plus x (F/Times x (F/Cos (F/Subtract F/C1D2 x)))))
                   (str (:expr
                          (ops/crossover
                            {:sym  x
                             :expr (F/Cos x)}
                            {:sym  x
                             :expr (F/Plus x (F/Times x (F/Cos (F/Subtract x F/C1D2))))})))))
            (is (= (str (F/Plus F/C1 (F/Times x (F/Cos (F/Subtract F/C1D2 x)))))
                   (str (:expr
                          (ops/crossover
                            {:sym  x
                             :expr (F/Plus F/C1 F/C1D3)}
                            {:sym  x
                             :expr (F/Plus x (F/Times x (F/Cos (F/Subtract x F/C1D2))))})))))
            (is (= (str (F/Plus F/E (F/Times x (F/Cos (F/Subtract F/C1D2 x)))))
                   (str (:expr
                          (ops/crossover
                            {:sym  x
                             :expr (F/Plus x (F/Times x (F/Cos (F/Subtract x F/C1D2))))}
                            {:sym  x
                             :expr F/E})))))
            (is (= (str (F/Plus F/C1D2 (F/Times x (F/Cos (F/Subtract F/C2 x)))))
                   (str (:expr
                          (ops/crossover
                            {:sym  x
                             :expr F/C1D2}
                            {:sym  x
                             :expr (F/Plus x (F/Times x (F/Cos (F/Subtract x F/C2))))})))))
            (is (= (str (F/Plus F/C1D2 F/E))
                   (str (:expr
                          (ops/crossover
                            {:sym  x
                             :expr F/C1D2}
                            {:sym  x
                             :expr F/E}))))))))))
  (with-redefs-fn {#'rand-int (fn [maxv] (dec maxv))
                   #'rand-nth (fn [coll] (last coll))}
    (fn []
      (with-redefs [ops/crossover-sampler [:times]]
        (let [x (F/Dummy "x")]
          (testing "Can crossover mix of IExpr and IAST with Times"
            (is (= (str (F/Times F/C4 (F/Times x (F/Cos (F/Subtract F/C1D2 x)))))
                   (str (:expr
                          (ops/crossover
                            {:sym  x
                             :expr F/C4}
                            {:sym  x
                             :expr (F/Plus x (F/Times x (F/Cos (F/Subtract x F/C1D2))))}))))))))))

  (with-redefs-fn {#'rand-int (fn [maxv] (dec maxv))
                   #'rand-nth (fn [coll] (last coll))}
    (fn []
      (with-redefs [ops/crossover-sampler [:divide12]]
        (let [x (F/Dummy "x")]
          (testing "Can crossover mix of IExpr and IAST with Divide12"
            (is (= (str (F/Divide (F/Times F/C4 (F/Sec (F/Subtract F/C1D2 x))) x))
                   (str (:expr
                          (ops/crossover
                            {:sym  x
                             :expr F/C4}
                            {:sym  x
                             :expr (F/Plus x (F/Times x (F/Cos (F/Subtract x F/C1D2))))})))))))))))


(deftest eval-f-test
  (let [x (F/Dummy "x")]
    (testing "can eval various fns for simple inputs"
      (is (= [0.0]
             (mapv
               ops/expr->double
               (ops/eval-phenotype-on-expr-args
                 (ops/->phenotype x (F/Subtract x F/C1D2) nil)
                 (ops/exprs->input-exprs-list (ops/doubles->exprs [0.5]))))))

      (is (instance? IExpr (F/Subtract F/E F/C1D2)))
      (is (instance? IAST (F/Subtract F/E F/C1D2)))

      (is (= "-1/2+E"
             (str (F/Subtract F/E F/C1D2))))

      (is (= "-1/2+E"
             (str (.eval (F/Subtract F/E F/C1D2)))))

      (is (= 2.218281828459045
             (.toNumber (F/Subtract F/E F/C1D2))))

      (is (= nil
             (try (.toNumber (F/Subtract x F/C1D2))
                  (catch Exception e nil))))

      (is (= [2.218281828459045]
             (ops/eval-vec-pheno
               (ops/->phenotype x (F/Subtract F/E F/C1D2) nil)
               {:input-exprs-list  (ops/exprs->input-exprs-list (ops/doubles->exprs [0.5]))
                :input-exprs-count 1})))

      (is (= [0.5]
             (ops/eval-vec-pheno
               (ops/->phenotype x (F/Subtract F/C1 F/C1D2) nil)
               {:input-exprs-list  (ops/exprs->input-exprs-list (ops/doubles->exprs [0.5]))
                :input-exprs-count 1}))))))


(comment (run-tests 'clj-symbolic-regression.ops-test))
