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


(deftest modify-test
  (testing "substitute"
    (let [x (F/Dummy "x")]
      (is (= (str (F/Sin x))
             (str (:expr (ops/modify
                           {:op           :substitute
                            :find-expr    F/Cos
                            :replace-expr F/Sin}
                           {:sym  x
                            :expr (F/Cos x)})))))))

  (testing "fn"
    (let [x (F/Dummy "x")]
      (is (= (str (.plus x (F/Sin x)))
             (str (:expr (ops/modify
                           {:op          :fn
                            :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                                           (F/Plus expr (F/Sin expr)))}
                           {:sym  x
                            :expr (.plus F/C0 x)})))))))

  (testing "modify-leafs"
    (let [x (F/Dummy "x")]
      (is (= (str (.plus (F/num 1.0) ^IExpr (F/Sin x)))
             (str (:expr (ops/modify
                           {:op               :modify-leafs
                            :leaf-modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                                                (if (and (= (.toString ie) "x"))
                                                  (F/Sin ie)
                                                  ie))}
                           {:sym  x
                            :expr (.plus (F/num 1.0) x)}))))))))


(deftest mutations-test

  (testing "all mutations results on y=x"
    (with-redefs [ops/modify-leafs-sampler [true]]
      (let [x (F/Dummy "x")]
        (is (= ["1+x*Cos(x)"
                "1/2+x+Cos(x)+x*Sin(x)"
                "-1/2+x+Cos(x)+x*Sin(x)"
                "1/10+x+Cos(x)+x*Sin(x)"
                "-1/10+x+Cos(x)+x*Sin(x)"
                "x+Cos(x)+Sin(x)+x*Sin(x)"
                "x+Cos(x)-Sin(x)+x*Sin(x)"
                "x+Cos(x)+Log(x)+x*Sin(x)"
                "x+Cos(x)-Log(x)+x*Sin(x)"
                "E^x+x+Cos(x)+x*Sin(x)"
                "-E^x+x+Cos(x)+x*Sin(x)"
                "x+2*Cos(x)+x*Sin(x)"
                "x+x*Sin(x)"
                "Sin(x)*(x+Cos(x)+x*Sin(x))"
                "Csc(x)*(x+Cos(x)+x*Sin(x))"
                "Cos(x)*(x+Cos(x)+x*Sin(x))"
                "Sec(x)*(x+Cos(x)+x*Sin(x))"
                "2*x+Cos(x)+x*Sin(x)"
                "Cos(x)+x*Sin(x)"
                "x+x^2+Cos(x)+x*Sin(x)"
                "x-x^2+Cos(x)+x*Sin(x)"
                "Sqrt(x)+x+Cos(x)+x*Sin(x)"
                "-Sqrt(x)+x+Cos(x)+x*Sin(x)"
                "x*(x+Cos(x)+x*Sin(x))"
                "(x+Cos(x)+x*Sin(x))/x"
                "-x-Cos(x)-x*Sin(x)"
                "1/2*(x+Cos(x)+x*Sin(x))"
                "2*(x+Cos(x)+x*Sin(x))"
                "1/10*(x+Cos(x)+x*Sin(x))"
                "10*(x+Cos(x)+x*Sin(x))"
                "1/2+x+Cos(1/2+x)+(1/2+x)*Sin(1/2+x)"
                "-1/2+x+Cos(1/2-x)+(1/2-x)*Sin(1/2-x)"
                "x/10+Cos(x/10)+1/10*x*Sin(x/10)"
                "10*x+Cos(10*x)+10*x*Sin(10*x)"
                "10*x+Cos(10*x)+10*x*Sin(10*x)"
                "Cos(Sin(x))+Sin(x)+Sin(x)*Sin(Sin(x))"
                "Cos(x)+Cos(Cos(x))+Cos(x)*Sin(Cos(x))"
                "Cos(Log(x))+Log(x)+Log(x)*Sin(Log(x))"
                "E^x+Cos(E^x)+E^x*Sin(E^x)"
                "Sqrt(x)+Cos(Sqrt(x))+Sqrt(x)*Sin(Sqrt(x))"
                "x^2+Cos(x^2)+x^2*Sin(x^2)"
                "1/10+x+Cos(1/10+x)+(1/10+x)*Sin(1/10+x)"
                "-1/10+x+Cos(1/10-x)+(1/10-x)*Sin(1/10-x)"
                "x+Cos(x)+x*Sin(x)"
                "x+Cos(x)+x*Sin(x)"
                "x+Cos(x)+x*Sin(x)"
                "x+Cos(x)+x*Sin(x)"
                "x+Cos(x)+x*Sin(x)"
                "x+Cos(x)+x*Sin(x)"
                "x+Cos(x)+x*Sin(x)"
                "x+Cos(x)+x*Sin(x)"
                "x+Cos(x)+x*Sin(x)"
                "x+Cos(x)+x*Sin(x)"
                "x+Cos(x)+x*Sin(x)"
                "x+Cos(x)+x*Cos(x)"
                "x+Sin(x)+x*Sin(x)"]
               (vec
                 (map
                   (fn [m]
                     (str (:expr (ops/modify m {:sym  x
                                                ;; x + cos(x) + x*sin(x)
                                                :expr (.plus x (.plus (F/Cos x) (.times x (F/Sin x))))}))))
                   (ops/initial-mutations)))))))))
