(ns closyr.ops-modify-test
  (:require
    [clojure.core.async :as async :refer [go go-loop timeout <!! >!! <! >! chan put! take! alts!! alt!! close!]]
    [clojure.test :refer :all]
    [closyr.dataset.prng :as prng]
    [closyr.ops.common :as ops-common]
    [closyr.ops.initialize :as ops-init]
    [closyr.ops.modify :as ops-modify])
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
      (is (= (str (:expr (ops-modify/modify
                           {:op           :modify-substitute
                            :find-expr    F/Cos
                            :replace-expr F/Sin}
                           {:sym  x
                            :expr (F/Cos x)})))
             (str (F/Sin x))))))

  (testing "fn"
    (let [x (F/Dummy "x")]
      (is (= (str (:expr (ops-modify/modify
                           {:op          :modify-fn
                            :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                                           (F/Plus expr (F/Sin expr)))}
                           {:sym  x
                            :expr (.plus F/C0 x)})))
             (str (.plus x (F/Sin x)))))))

  (testing "modify-leafs"
    (let [x (F/Dummy "x")]
      (is (= (str
               (:expr
                 (ops-modify/modify
                   {:op               :modify-leafs
                    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                                        (if (= (.toString ie) "x")
                                          (F/Sin ie)
                                          ie))}
                   {:sym  x
                    :expr (.plus (F/num 1.0) x)})))
             (str (.plus (F/num 1.0) ^IExpr (F/Sin x)))))))

  (testing "modify-branches"
    (let [x           (F/Dummy "x")
          mods-count* (atom 0)]
      (is (= (str
               (:expr
                 (ops-modify/modify
                   {:op               :modify-branches
                    :label            "branch cos"
                    :leaf-modifier-fn (fn ^IExpr [leaf-count
                                                  {^IAST expr :expr ^ISymbol x-sym :sym
                                                   :as        pheno}
                                                  ^IExpr ie]
                                        (swap! mods-count* inc)
                                        (F/Cos ie))}
                   {:sym  x
                    :expr (.plus (F/num 1.0) x)})))
             (str (F/Cos (.plus (F/num 1.0) x)))))
      (is (=
            @mods-count*
            1))))

  (testing "modify-branches 2"
    (let [x           (F/Dummy "x")
          mods-count* (atom 0)]
      (is (= (str
               (:expr
                 (ops-modify/modify
                   {:op               :modify-branches
                    :label            "branch cos"
                    :leaf-modifier-fn (fn ^IExpr [leaf-count
                                                  {^IAST expr :expr ^ISymbol x-sym :sym
                                                   :as        pheno}
                                                  ^IExpr ie]
                                        (swap! mods-count* inc)
                                        (F/Cos ie))}
                   {:sym  x
                    :expr (.plus (F/Times (F/num 1.0) (F/Sin (F/Plus x F/C1))) x)})))
             "Cos(x+Cos(Sin(Cos(1+x))))"))
      (is (=
            @mods-count*
            3))))

  (testing "modify-ast-head"
    (let [x           (F/Dummy "x")
          mods-count* (atom 0)]
      (is
        (=
          (str
            (:expr
              (ops-modify/modify
                {:op               :modify-ast-head
                 :label            "sin->cos"
                 :leaf-modifier-fn (fn ^IExpr [leaf-count
                                               {^IAST expr :expr ^ISymbol x-sym :sym
                                                :as        pheno}
                                               ^IExpr ie]
                                     (swap! mods-count* inc)
                                     (if (= F/Sin ie)
                                       F/Cos
                                       ie))}
                {:sym  x
                 :expr (.plus (F/num 1.0) ^IExpr (F/Sin x))})))
          (str (.plus (F/num 1.0) ^IExpr (F/Cos x)))))
      (is (=
            @mods-count*
            2)))))


(deftest apply-modifications-test
  (testing "single mod: modify-branches"
    (let [x (F/Dummy "x")
          [pheno iters mods] (ops-modify/apply-modifications
                               100
                               1
                               [{:op               :modify-branches
                                 :label            "branch cos"
                                 :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                                                     (F/Cos ie))}]
                               {:sym  x
                                :expr (.plus (F/num 1.0) x)}
                               {:sym  x
                                :expr (.plus (F/num 1.0) x)})]
      (is (= iters 1))
      (is (= (str (:expr pheno))
             (str (F/Cos (.plus (F/num 1.0) x)))))))

  (testing "multiple mods 1"
    (let [x (F/Dummy "x")
          [pheno iters mods] (ops-modify/apply-modifications
                               100
                               2
                               [{:op               :modify-branches
                                 :label            "branch cos"
                                 :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                                                     (F/Cos ie))}]
                               {:sym  x
                                :expr (.plus (F/num 1.0) x)}
                               {:sym  x
                                :expr (.plus (F/num 1.0) x)})]
      (is (= iters 2))
      (is (= (str (:expr pheno))
             "Cos(Cos(Cos(1.0+x)))"))))


  (testing "multiple mods 2"
    (let [rand* (atom -1)]
      (with-redefs-fn {#'prng/rand-int (fn [maxv] (dec maxv))
                       #'prng/rand-nth (fn [coll] (nth coll (swap! rand* inc)))}
        (fn []
          (let [x (F/Dummy "x")
                [pheno iters mods] (ops-modify/apply-modifications
                                     100
                                     2
                                     [{:op               :modify-branches
                                       :label            "branch cos"
                                       :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                                                           (F/Cos ie))}
                                      {:op               :modify-branches
                                       :label            "branch sin"
                                       :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                                                           (F/Sin ie))}]
                                     {:sym  x
                                      :expr (.plus (F/num 1.0) x)}
                                     {:sym  x
                                      :expr (.plus (F/num 1.0) x)})]
            (is (= iters 2))
            (is (= (str (:expr pheno))
                   "Sin(Cos(Sin(1.0+x)))")))))))


  (testing "multiple mods 3"
    (let [rand* (atom -1)]
      (with-redefs-fn {#'prng/rand-int (fn [maxv] (dec maxv))
                       #'prng/rand-nth (fn [coll] (nth coll (swap! rand* inc)))}
        (fn []
          (let [x (F/Dummy "x")
                [pheno iters mods] (ops-modify/apply-modifications
                                     100
                                     5
                                     [{:op               :modify-branches
                                       :label            "branch cos"
                                       :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                                                           (F/Cos ie))}
                                      {:op               :modify-branches
                                       :label            "b*1.1"
                                       :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                                                           (F/Times ie (F/num 1.1)))}
                                      {:op               :modify-ast-head
                                       :label            "cos->acos"
                                       :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                                                           (if (= F/Cos ie)
                                                             F/ArcCos
                                                             ie))}
                                      {:op               :modify-leafs
                                       :label            "c+1/2"
                                       :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                                                           (if (and (.isNumber ie) (ops-common/should-modify-leaf leaf-count pheno))
                                                             (F/Plus ie (F/Divide 1 F/C2))
                                                             ie))}
                                      {:op          :modify-fn
                                       :label       "-x^1/2"
                                       :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                                                      (F/Subtract expr (F/Sqrt x-sym)))}]
                                     {:sym  x
                                      :expr (.plus (F/num 1.0) x)}
                                     {:sym  x
                                      :expr (.plus (F/num 1.0) x)})]
            (is (= iters 5))
            (is (= (str (:expr pheno))
                   "-Sqrt(x)+1.1*ArcCos(1.1*(1.0+x))"))))))))


(deftest crossover-test
  (with-redefs-fn {#'prng/rand-int (fn [maxv] (dec maxv))
                   #'prng/rand-nth (fn [coll] (first coll))}
    (fn []
      (with-redefs [ops-modify/crossover-sampler [:plus]]
        (let [x (F/Dummy "x")]
          (testing "Can crossover mix of IExpr and IAST"
            (is (= (str (:expr
                          (ops-modify/crossover
                            {:sym  x
                             :expr (F/Cos x)}
                            {:sym  x
                             :expr (F/Plus x (F/Times x (F/Cos (F/Subtract x F/C1D2))))})))
                   (str (F/Plus x (F/Times x (F/Cos (F/Subtract F/C1D2 x)))))))
            (is (= (str (:expr
                          (ops-modify/crossover
                            {:sym  x
                             :expr (F/Plus F/C1 F/C1D3)}
                            {:sym  x
                             :expr (F/Plus x (F/Times x (F/Cos (F/Subtract x F/C1D2))))})))
                   (str (F/Plus F/C1 (F/Times x (F/Cos (F/Subtract F/C1D2 x)))))))
            (is (= (str (:expr
                          (ops-modify/crossover
                            {:sym  x
                             :expr (F/Plus x (F/Times x (F/Cos (F/Subtract x F/C1D2))))}
                            {:sym  x
                             :expr F/E})))
                   (str (F/Plus F/E (F/Times x (F/Cos (F/Subtract F/C1D2 x)))))))
            (is (= (str (:expr
                          (ops-modify/crossover
                            {:sym  x
                             :expr F/C1D2}
                            {:sym  x
                             :expr (F/Plus x (F/Times x (F/Cos (F/Subtract x F/C2))))})))
                   (str (F/Plus F/C1D2 (F/Times x (F/Cos (F/Subtract F/C2 x)))))))
            (is (= (str (:expr
                          (ops-modify/crossover
                            {:sym  x
                             :expr F/C1D2}
                            {:sym  x
                             :expr F/E})))
                   (str (F/Plus F/C1D2 F/E)))))))))
  (with-redefs-fn {#'prng/rand-int (fn [maxv] (dec maxv))
                   #'prng/rand-nth (fn [coll] (last coll))}
    (fn []
      (with-redefs [ops-modify/crossover-sampler [:times]]
        (let [x (F/Dummy "x")]
          (testing "Can crossover mix of IExpr and IAST with Times"
            (is (= (str (:expr
                          (ops-modify/crossover
                            {:sym  x
                             :expr F/C4}
                            {:sym  x
                             :expr (F/Plus x (F/Times x (F/Cos (F/Subtract x F/C1D2))))})))
                   (str (F/Times F/C4 (F/Times x (F/Cos (F/Subtract F/C1D2 x))))))))))))

  (with-redefs-fn {#'prng/rand-int (fn [maxv] (dec maxv))
                   #'prng/rand-nth (fn [coll] (last coll))}
    (fn []
      (with-redefs [ops-modify/crossover-sampler [:divide12]]
        (let [x (F/Dummy "x")]
          (testing "Can crossover mix of IExpr and IAST with Divide12"
            (is (= (str (:expr
                          (ops-modify/crossover
                            {:sym  x
                             :expr F/C4}
                            {:sym  x
                             :expr (F/Plus x (F/Times x (F/Cos (F/Subtract x F/C1D2))))})))
                   (str (F/Divide (F/Times F/C4 (F/Sec (F/Subtract F/C1D2 x))) x)))))))))

  (with-redefs-fn {#'prng/rand-int (fn [maxv] (dec maxv))
                   #'prng/rand-nth (fn [coll] (last coll))}
    (fn []
      (with-redefs [ops-modify/crossover-sampler [:minus12]]
        (let [x (F/Dummy "x")]
          (testing "Can crossover mix of IExpr and IAST with Minus12"
            (is (= (str (:expr
                          (ops-modify/crossover
                            {:sym  x
                             :expr F/C4}
                            {:sym  x
                             :expr (F/Plus x (F/Times x (F/Cos (F/Subtract x F/C1D2))))})))
                   (str (F/Subtract F/C4 (F/Times x (F/Cos (F/Subtract F/C1D2 x))))))))))))

  (with-redefs-fn {#'prng/rand-int (fn [maxv] (dec maxv))
                   #'prng/rand-nth (fn [coll] (last coll))}
    (fn []
      (with-redefs [ops-modify/crossover-sampler [:exp12]]
        (let [x (F/Dummy "x")]
          (testing "Can crossover mix of IExpr and IAST with Exp12"
            (is (= (str (:expr
                          (ops-modify/crossover
                            {:sym  x
                             :expr F/C4}
                            {:sym  x
                             :expr (F/Plus x (F/Times x (F/Cos (F/Subtract x F/C1D2))))})))
                   (str (F/Power F/C4 (F/Times x (F/Cos (F/Subtract F/C1D2 x)))))))))))))


(def all-mods-applied-on-fn-expected
  [[:modify-fn
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
   ;; [:modify-fn
   ;; "+1/100"
   ;; "-99/100+x+Cos(x)-x*Sin(1/2-x)"]
   ;; [:modify-fn
   ;; "-1/100"
   ;; "-101/100+x+Cos(x)-x*Sin(1/2-x)"]
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
   ;; [:modify-fn
   ;; "/100"
   ;; "1/100*(-1+x+Cos(x)-x*Sin(1/2-x))"]
   ;; [:modify-fn
   ;; "*100"
   ;; "100*(-1+x+Cos(x)-x*Sin(1/2-x))"]
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
   ;; [:modify-leafs
   ;; "x/100"
   ;; "-1+x/100+Cos(x/100)-1/100*x*Sin(1/2-x/100)"]
   ;; [:modify-leafs
   ;; "100*x"
   ;; "-1+100*x+Cos(100*x)-100*x*Sin(1/2-100*x)"]
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
    "asin(x)"
    "-1+Sqrt(1-x^2)+ArcSin(x)-ArcSin(x)*Sin(1/2-ArcSin(x))"]
   [:modify-leafs
    "acos(x)"
    "-1+x+ArcCos(x)-ArcCos(x)*Sin(1/2-ArcCos(x))"]
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
   ;; [:modify-leafs
   ;; "x+1/100"
   ;; "-99/100+x+Cos(1/100+x)-(1/100+x)*Sin(49/100-x)"]
   ;; [:modify-leafs
   ;; "x-1/100"
   ;; "-101/100+x+Cos(1/100-x)+(1/100-x)*Sin(51/100-x)"]
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
   ;; [:modify-leafs
   ;; "c+1/100"
   ;; "-99/100+x+Cos(x)-99/100*x*Sin(51/100-99/100*x)"]
   ;; [:modify-leafs
   ;; "c-1/100"
   ;; "-101/100+x+Cos(x)-101/100*x*Sin(49/100-101/100*x)"]
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
    "sin->asin"
    "-1+x-x*ArcSin(1/2-x)+Cos(x)"]
   [:modify-ast-head
    "cos->acos"
    "-1+x+ArcCos(x)-x*Sin(1/2-x)"]
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
    "b simplify"
    "-1+x+Cos(x)-x*Sin(1/2-x)"]
   [:modify-branches
    "b sin"
    "-Sin(1-x-Sin(Cos(x))+Sin(x*Sin(Sin(Sin(1/2-Sin(x))))))"]
   [:modify-branches
    "b cos"
    "Cos(1-x-Cos(Cos(x))-Cos(x*Cos(Sin(Cos(1/2+Cos(x))))))"]
   [:modify-branches
    "b asin"
    "-ArcSin(1-x+ArcSin(x*ArcSin(1/2-ArcSin(x)))-ArcSin(Cos(x)))"]
   [:modify-branches
    "b acos"
    "ArcCos(-1+x+ArcCos(-x*ArcCos(Sqrt(1-(1/2+ArcCos(-x))^2)))+ArcCos(Cos(x)))"]
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
    "-1.3000000000000003+x+Cos(x)+x*(0.1-Sin(0.3-x))"]])


(deftest mutations-test
  (with-redefs-fn {#'prng/rand (fn [] 0.0)}
    (fn []
      (let [x                      (F/Dummy "x")
            ;; x + cos(x) + x*sin(x-0.5) - 1
            test-expr              (.minus
                                     (.plus x (.plus
                                                (F/Cos x)
                                                (.times x (F/Sin (.minus x F/C1D2)))))
                                     F/C1)
            all-mods-applied-on-fn (vec (map (fn [m]
                                               [(:op m) (:label m) (str (:expr (ops-modify/modify
                                                                                 m
                                                                                 {:sym  x
                                                                                  :expr test-expr})))])
                                             (ops-init/initial-mutations)))]
        (is (= (count all-mods-applied-on-fn)
               (count all-mods-applied-on-fn-expected)))
        (when (= (count all-mods-applied-on-fn)
                 (count all-mods-applied-on-fn-expected))
          (doall
            (map
              (fn [[actual-op actual-label actual-fn-str] [expected-op expected-label expected-fn-str]]
                (testing (str "result of the modification: " (name expected-op) " / " expected-label)
                  (is (= actual-op expected-op))
                  (is (= actual-label expected-label))
                  (is (= actual-fn-str expected-fn-str))))
              all-mods-applied-on-fn
              all-mods-applied-on-fn-expected)))))))


(comment (run-tests 'closyr.ops-modify-test))
