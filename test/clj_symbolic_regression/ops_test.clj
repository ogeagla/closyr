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
             (str (:expr (ops/modify {:op           :substitute
                                      :find-expr    F/Cos
                                      :replace-expr F/Sin}
                                     {:x-sym x
                                      :expr  (F/Cos x)})))))))

  (testing "fn"
    (let [x (F/Dummy "x")]
      (is (= (str (.plus x (F/Sin x)))
             (str (:expr (ops/modify {:op          :fn
                                      :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                                                     (F/Plus expr (F/Sin expr)))}
                                     {:x-sym x
                                      :expr  (.plus F/C0 x)})))))))

  (testing "modify-leafs"
    (let [x (F/Dummy "x")]
      (is (= (str (.plus F/C1 ^IExpr (F/Sin x)) )
             (str (:expr (ops/modify {:op               :modify-leafs
                                      :leaf-modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                                                          (if (and (= (.toString ie) "x"))
                                                            (F/Sin ie)
                                                            ie))}
                                     {:x-sym x
                                      :expr  (.plus F/C1 x)}))))))))
