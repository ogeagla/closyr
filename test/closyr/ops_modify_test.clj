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


  (testing "modify-leafs 2"
    (let [x           (F/Dummy "x")
          mods-count* (atom 0)]
      (is (= (str
               (:expr
                 (ops-modify/modify
                   {:op               :modify-leafs
                    :leaf-modifier-fn (fn ^IExpr [leaf-count
                                                  {^IAST expr :expr ^ISymbol x-sym :sym
                                                   :as        pheno}
                                                  ^IExpr ie]
                                        (swap! mods-count* inc)
                                        (if (= (.toString ie) "x")
                                          (F/Sin ie)
                                          ie))}
                   {:sym  x
                    :expr (.plus (F/Divide (F/Times (F/num 1.0) (F/Sin (F/Plus x F/C1)))
                                           (F/Plus x (F/Cos x)))
                                 x)})))
             "Sin(x)+Sin(1+Sin(x))/(Cos(Sin(x))+Sin(x))"))
      (is (=
            @mods-count*
            6))))

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
                    :expr (F/Plus (F/Sin x) (.plus (F/num 1.0) x))})))
             "Cos(1.0+x+Cos(Sin(x)))"))
      (is (=
            @mods-count*
            2))))

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

  (testing "single mod: divide by zero"
    (let [x            (F/Dummy "x")
          div-by-zero* (atom 0)
          [pheno iters mods] (with-redefs-fn {#'ops-modify/divided-by-zero (fn [] (swap! div-by-zero* inc))}
                               (fn []
                                 (ops-modify/apply-modifications
                                   100
                                   2
                                   [{:op          :modify-fn
                                     :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                                                    (.divide expr F/C0))}]
                                   {:sym  x
                                    :expr x}
                                   {:sym  x
                                    :expr x})))]
      (is (= iters 2))
      (is (= @div-by-zero* 2))
      (is (= (str (:expr pheno))
             (str x)))))

  (testing "single mod: divide by nonsense"
    (let [x            (F/Dummy "x")
          div-by-zero* (atom 0)
          [pheno iters mods] (with-redefs-fn {#'ops-modify/divided-by-zero (fn [] (swap! div-by-zero* inc))}
                               (fn []
                                 (ops-modify/apply-modifications
                                   100
                                   2
                                   [{:op          :modify-fn
                                     :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                                                    (.divide expr F/NIL))}]
                                   {:sym  x
                                    :expr x}
                                   {:sym  x
                                    :expr x})))]
      (is (= iters 2))
      (is (= @div-by-zero* 0))
      (is (= (str (:expr pheno))
             (str x)))))

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
                                                           (if (.isNumber ie)
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
                   "-Sqrt(x)+1.6*ArcCos(1.6*(1.5+x))"))))))))


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
                   (str (F/Power F/C4 (F/Times x (F/Cos (F/Subtract F/C1D2 x))))))))))))

  (with-redefs-fn {#'prng/rand-int (fn [maxv] (dec maxv))
                   #'prng/rand-nth (fn [coll] (last coll))}
    (fn []
      (with-redefs [ops-modify/crossover-sampler [:exp21]]
        (let [x (F/Dummy "x")]
          (testing "Can crossover mix of IExpr and IAST with Exp21"
            (is (= (str (:expr
                          (ops-modify/crossover
                            {:sym  x
                             :expr F/C4}
                            {:sym  x
                             :expr (F/Plus x (F/Times x (F/Cos (F/Subtract x F/C1D2))))})))
                   "x^4*Cos(1/2-x)^4"))))))))


(def all-mods-applied-on-fn-expected
  [[:modify-fn
    "Derivative"
    "1+x*Cos(1/2-x)-Cos(x)/(2*x^(3/2))-Sin(1/2-x)-Sin(x)/Sqrt(x)"]
   [:modify-fn
    "+1/2"
    "-1/2+x+Cos(x)/Sqrt(x)-x*Sin(1/2-x)"]
   [:modify-fn
    "-1/2"
    "-3/2+x+Cos(x)/Sqrt(x)-x*Sin(1/2-x)"]
   [:modify-fn
    "+1/10"
    "-9/10+x+Cos(x)/Sqrt(x)-x*Sin(1/2-x)"]
   [:modify-fn
    "-1/10"
    "-11/10+x+Cos(x)/Sqrt(x)-x*Sin(1/2-x)"]
   [:modify-fn
    "+Sin"
    "-1+x+Cos(x)/Sqrt(x)-x*Sin(1/2-x)+Sin(x)"]
   [:modify-fn
    "-Sin"
    "-1+x+Cos(x)/Sqrt(x)-x*Sin(1/2-x)-Sin(x)"]
   [:modify-fn
    "+Log"
    "-1+x+Cos(x)/Sqrt(x)+Log(x)-x*Sin(1/2-x)"]
   [:modify-fn
    "-Log"
    "-1+x+Cos(x)/Sqrt(x)-Log(x)-x*Sin(1/2-x)"]
   [:modify-fn
    "+Exp"
    "-1+E^x+x+Cos(x)/Sqrt(x)-x*Sin(1/2-x)"]
   [:modify-fn
    "-Exp"
    "-1-E^x+x+Cos(x)/Sqrt(x)-x*Sin(1/2-x)"]
   [:modify-fn
    "+Cos"
    "-1+x+Cos(x)+Cos(x)/Sqrt(x)-x*Sin(1/2-x)"]
   [:modify-fn
    "-Cos"
    "-1+x-Cos(x)+Cos(x)/Sqrt(x)-x*Sin(1/2-x)"]
   [:modify-fn
    "*Sin"
    "(-1+x+Cos(x)/Sqrt(x)-x*Sin(1/2-x))*Sin(x)"]
   [:modify-fn
    "/Sin"
    "Csc(x)*(-1+x+Cos(x)/Sqrt(x)-x*Sin(1/2-x))"]
   [:modify-fn
    "*Cos"
    "Cos(x)*(-1+x+Cos(x)/Sqrt(x)-x*Sin(1/2-x))"]
   [:modify-fn
    "/Cos"
    "Sec(x)*(-1+x+Cos(x)/Sqrt(x)-x*Sin(1/2-x))"]
   [:modify-fn
    "+x"
    "-1+2*x+Cos(x)/Sqrt(x)-x*Sin(1/2-x)"]
   [:modify-fn
    "-x"
    "-1+Cos(x)/Sqrt(x)-x*Sin(1/2-x)"]
   [:modify-fn
    "+x^2"
    "-1+x+x^2+Cos(x)/Sqrt(x)-x*Sin(1/2-x)"]
   [:modify-fn
    "-x^2"
    "-1+x-x^2+Cos(x)/Sqrt(x)-x*Sin(1/2-x)"]
   [:modify-fn
    "+x^1/2"
    "-1+Sqrt(x)+x+Cos(x)/Sqrt(x)-x*Sin(1/2-x)"]
   [:modify-fn
    "-x^1/2"
    "-1-Sqrt(x)+x+Cos(x)/Sqrt(x)-x*Sin(1/2-x)"]
   [:modify-fn
    "*x"
    "x*(-1+x+Cos(x)/Sqrt(x)-x*Sin(1/2-x))"]
   [:modify-fn
    "/x"
    "(-1+x+Cos(x)/Sqrt(x)-x*Sin(1/2-x))/x"]
   [:modify-fn
    "1/f"
    "1/(-1+x+Cos(x)/Sqrt(x)-x*Sin(1/2-x))"]
   [:modify-fn
    "*-1"
    "1-x-Cos(x)/Sqrt(x)+x*Sin(1/2-x)"]
   [:modify-fn
    "/2"
    "1/2*(-1+x+Cos(x)/Sqrt(x)-x*Sin(1/2-x))"]
   [:modify-fn
    "*2"
    "2*(-1+x+Cos(x)/Sqrt(x)-x*Sin(1/2-x))"]
   [:modify-fn
    "/10"
    "1/10*(-1+x+Cos(x)/Sqrt(x)-x*Sin(1/2-x))"]
   [:modify-fn
    "*10"
    "10*(-1+x+Cos(x)/Sqrt(x)-x*Sin(1/2-x))"]
   [:modify-fn
    "*1.1"
    "1.1*(-1+x+Cos(x)/Sqrt(x)-x*Sin(1/2-x))"]
   [:modify-fn
    "*0.9"
    "0.9*(-1+x+Cos(x)/Sqrt(x)-x*Sin(1/2-x))"]
   [:modify-leafs
    "x+1/2"
    "-1/2+x+Cos(1/2+x)/Sqrt(1/2+x)+(1/2+x)*Sin(x)"]
   [:modify-leafs
    "x-1/2"
    "-3/2+x+Cos(1/2-x)/Sqrt(-1/2+x)+(1/2-x)*Sin(1-x)"]
   [:modify-leafs
    "x/10"
    "-1+x/10+(Sqrt(10)*Cos(x/10))/Sqrt(x)-1/10*x*Sin(1/2-x/10)"]
   [:modify-leafs
    "10*x"
    "-1+10*x+Cos(10*x)/(Sqrt(10)*Sqrt(x))-10*x*Sin(1/2-10*x)"]
   [:modify-leafs
    "1/x"
    "-1+1/x+Cos(1/x)/Sqrt(1/x)-Sin(1/2-1/x)/x"]
   [:modify-leafs
    "-1*x"
    "-1-x+Cos(x)/Sqrt(-x)+x*Sin(1/2+x)"]
   [:modify-leafs
    "1.1*x"
    "-1+1.1*x+(0.9534625892455922*Cos(1.1*x))/Sqrt(x)-1.1*x*Sin(1/2-1.1*x)"]
   [:modify-leafs
    "0.9*x"
    "-1+0.9*x+(1.0540925533894598*Cos(0.9*x))/Sqrt(x)-0.9*x*Sin(1/2-0.9*x)"]
   [:modify-leafs
    "sin(x)"
    "-1+Cos(Sin(x))/Sqrt(Sin(x))+Sin(x)-Sin(x)*Sin(1/2-Sin(x))"]
   [:modify-leafs
    "cos(x)"
    "-1+Cos(x)+Cos(Cos(x))/Sqrt(Cos(x))-Cos(x)*Sin(1/2-Cos(x))"]
   [:modify-leafs
    "asin(x)"
    "-1+Sqrt(1-x^2)/Sqrt(ArcSin(x))+ArcSin(x)-ArcSin(x)*Sin(1/2-ArcSin(x))"]
   [:modify-leafs
    "acos(x)"
    "-1+x/Sqrt(ArcCos(x))+ArcCos(x)-ArcCos(x)*Sin(1/2-ArcCos(x))"]
   [:modify-leafs
    "log(x)"
    "-1+Cos(Log(x))/Sqrt(Log(x))+Log(x)-Log(x)*Sin(1/2-Log(x))"]
   [:modify-leafs
    "exp(x)"
    "-1+E^x+Cos(E^x)/Sqrt(E^x)-E^x*Sin(1/2-E^x)"]
   [:modify-leafs
    "x^1/2"
    "-1+Sqrt(x)+Cos(Sqrt(x))/x^(1/4)-Sqrt(x)*Sin(1/2-Sqrt(x))"]
   [:modify-leafs
    "x^2"
    "-1+x^2+Cos(x^2)/Sqrt(x^2)-x^2*Sin(1/2-x^2)"]
   [:modify-leafs
    "x+1/10"
    "-9/10+x+Cos(1/10+x)/Sqrt(1/10+x)-(1/10+x)*Sin(2/5-x)"]
   [:modify-leafs
    "x-1/10"
    "-11/10+x+Cos(1/10-x)/Sqrt(-1/10+x)+(1/10-x)*Sin(3/5-x)"]
   [:modify-leafs
    "c/2"
    "-1/2+x+Cos(x)/x^(1/4)-1/2*x*Sin(1/4-x/2)"]
   [:modify-leafs
    "c*2"
    "-2+x+Cos(x)/x-2*x*Sin(1-2*x)"]
   [:modify-leafs
    "c*-1"
    "1+x+Sqrt(x)*Cos(x)-x*Sin(1/2-x)"]
   [:modify-leafs
    "c/10"
    "-1/10+x+Cos(x)/x^(1/20)-1/10*x*Sin(1/20-x/10)"]
   [:modify-leafs
    "c*10"
    "-10+x+Cos(x)/x^5-10*x*Sin(5-10*x)"]
   [:modify-leafs
    "c+1/10"
    "-9/10+x+Cos(x)/x^(2/5)-9/10*x*Sin(3/5-9/10*x)"]
   [:modify-leafs
    "c-1/10"
    "-11/10+x+Cos(x)/x^(3/5)-11/10*x*Sin(2/5-11/10*x)"]
   [:modify-leafs
    "1/c"
    "-1+x+Cos(x)/x^2-x*Sin(2-x)"]
   [:modify-leafs
    "c+1/2"
    "-1/2+x+Cos(x)-1/2*x*Sin(1-x/2)"]
   [:modify-leafs
    "c-1/2"
    "-3/2+x+Cos(x)/x+3/2*x*Sin(3/2*x)"]
   [:modify-substitute
    "Sin->Cos"
    "-1+x-x*Cos(1/2-x)+Cos(x)/Sqrt(x)"]
   [:modify-substitute
    "Cos->Sin"
    "-1+x-x*Sin(1/2-x)+Sin(x)/Sqrt(x)"]
   [:modify-ast-head
    "sin->cos"
    "-1+x-x*Cos(1/2-x)+Cos(x)/Sqrt(x)"]
   [:modify-ast-head
    "cos->sin"
    "-1+x-x*Sin(1/2-x)+Sin(x)/Sqrt(x)"]
   [:modify-ast-head
    "sin->asin"
    "-1+x-x*ArcSin(1/2-x)+Cos(x)/Sqrt(x)"]
   [:modify-ast-head
    "cos->acos"
    "-1+x+ArcCos(x)/Sqrt(x)-x*Sin(1/2-x)"]
   [:modify-ast-head
    "+->*"
    "-x^(3/2)*Cos(x)*Sin(x/2)"]
   [:modify-ast-head
    "*->+"
    "-2+1/Sqrt(x)+2*x+Cos(x)-Sin(1/2-x)"]
   [:modify-ast-head
    "^->*"
    "-1+x-1/2*x*Cos(x)-x*Sin(1/2-x)"]
   #_[:modify-ast-head
      "/->*"
      "-1+x+Cos(x)/Sqrt(x)-x*Sin(1/2-x)"]
   #_[:modify-ast-head
      "/->+"
      "-1+x+Cos(x)/Sqrt(x)-x*Sin(1/2-x)"]
   [:modify-branches
    "b derivative"
    "1-3/2*Cos(x)/x^(5/2)+15/8*Sin(x)/x^(7/2)-Sin(x)/(2*x^(3/2))"]
   #_[:modify-branches
      "b simplify"
      "-1+x+Cos(x)/Sqrt(x)-x*Sin(1/2-x)"]
   [:modify-branches
    "b sin"
    "-Sin(1-x-Sin(Sin(1/Sqrt(x))*Sin(Cos(x)))+Sin(x*Sin(Sin(Sin(1/2-Sin(x))))))"]
   [:modify-branches
    "b cos"
    "Cos(1-x-Cos(Cos(1/Sqrt(x))*Cos(Cos(x)))-Cos(x*Cos(Sin(Cos(1/2+Cos(x))))))"]
   [:modify-branches
    "b asin"
    "-ArcSin(1-x+ArcSin(x*ArcSin(1/2-ArcSin(x)))-ArcSin(ArcSin(1/Sqrt(x))*ArcSin(Cos(x))))"]
   [:modify-branches
    "b acos"
    "ArcCos(-1+x+ArcCos(-x*ArcCos(Sqrt(1-(1/2+ArcCos(-x))^2)))+ArcCos(ArcCos(1/Sqrt(x))*ArcCos(Cos(x))))"]
   [:modify-branches
    "b exp"
    "E^(-1+E^E^(1/Sqrt(x)+Cos(x))+E^(-E^Sin(E^(1/2+E^(-x)))*x)+x)"]
   [:modify-branches
    "b log"
    "Log(-1+x+Log(Log(1/Sqrt(x))*Log(Cos(x)))+Log(-x*Log(Sin(Log(1/2+Log(-x))))))"]
   [:modify-branches
    "b*b"
    "(1-x-Cos(x)^4/x^2-x^2*Sin((1/2+x^2)^2)^4)^2"]
   [:modify-branches
    "b^1/2"
    "Sqrt(-1+x+Sqrt(Sqrt(Cos(x))/x^(1/4))+Sqrt(-x*Sqrt(Sin(Sqrt(1/2+Sqrt(-x))))))"]
   [:modify-branches
    "b^-2"
    "1/(1-x-Cos(x)^4/x^2-Sin(1/(1/2+1/x^2)^2)^4/x^2)^2"]
   [:modify-branches
    "b^-1"
    "1/(-1+x+Cos(x)/Sqrt(x)-Sin(1/(1/2-1/x))/x)"]
   [:modify-branches
    "b*-1"
    "1-x+Cos(x)/Sqrt(x)-x*Sin(1/2+x)"]
   [:modify-branches
    "b*1.1"
    "1.1*(-1+x+(1.3310000000000004*Cos(x))/Sqrt(x)-1.2100000000000002*x*Sin(1.1*(1/2-1.1*x)))"]
   [:modify-branches
    "b*0.9"
    "0.9*(-1+x+(0.7290000000000001*Cos(x))/Sqrt(x)-0.81*x*Sin(0.9*(1/2-0.9*x)))"]
   [:modify-branches
    "b+0.1"
    "-0.7000000000000001+x+(0.1+1/Sqrt(x))*(0.1+Cos(x))-x*(0.1+Sin(0.7-x))"]
   [:modify-branches
    "b-0.1"
    "-1.3000000000000003+x+(-0.1+1/Sqrt(x))*(-0.1+Cos(x))+x*(0.1-Sin(0.30000000000000004-x))"]])


(deftest mutations-test
  (with-redefs-fn {#'ops-common/should-modify-ast-head (fn [_ _] true)
                   #'prng/rand                         (fn [] 0.0)}
    (fn []
      (let [x                      (F/Dummy "x")
            ;; x + cos(x) + x*sin(x-0.5) - 1
            test-expr              (.minus
                                     (.plus x (.plus
                                                (.divide (F/Cos x) (F/Sqrt x))
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
              all-mods-applied-on-fn-expected)))
        (is (= all-mods-applied-on-fn
               all-mods-applied-on-fn-expected))))))


(comment (run-tests 'closyr.ops-modify-test))
