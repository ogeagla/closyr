(ns closyr.ops.initialize
  (:require
    [clojure.tools.logging :as log]
    [closyr.ops.common :as ops-common])
  (:import
    (org.matheclipse.core.expression
      AST
      F)
    (org.matheclipse.core.interfaces
      IAST
      IExpr
      ISymbol)))


(def initial-exprs
  (let [^ISymbol x ops-common/sym-x]
    [x]
    #_[
     ;F/C0
     F/C1
     x
     x
     x
     ;x
     ;(F/Times -1 (ops-common/->iexprs [x]))
     (F/Times -1 (ops-common/->iexprs [x]))
     (F/Times -1 (ops-common/->iexprs [x]))
     (F/Times -1 (ops-common/->iexprs [x]))
     ;; (F/Log x)
     (F/Exp x)
     (F/Sin x)
     (F/Cos x)
     ;; (F/Sqr x)
     ;; (F/Times -1 (->iexprs [(F/Sqr x)]))
     ]))


(defn initial-phenotypes
  [reps]
  (let [^ISymbol x ops-common/sym-x]
    (->>
      (fn []
        initial-exprs)
      (repeatedly reps)
      (mapcat identity)
      (mapv (fn [^IExpr expr] (ops-common/->phenotype x expr nil))))))


(defn initial-mutations
  []
  [{:op          :modify-fn
    :label       "Derivative"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (F/D expr x-sym))}

   {:op          :modify-fn
    :label       "+1/2"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.plus expr F/C1D2))}

   {:op          :modify-fn
    :label       "-1/2"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.minus expr F/C1D2))}
   {:op          :modify-fn
    :label       "+1/10"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.plus expr (F/Divide 1 F/C10)))}

   {:op          :modify-fn
    :label       "-1/10"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.minus expr (F/Divide 1 F/C10)))}

   ;; {:op          :modify-fn
   ;; :label       "+1/100"
   ;; :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
   ;;                (.plus expr (F/Divide 1 F/C100)))}
   ;;
   ;; {:op          :modify-fn
   ;; :label       "-1/100"
   ;; :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
   ;;                (.minus expr (F/Divide 1 F/C100)))}

   {:op          :modify-fn
    :label       "+Sin"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.plus expr (F/Sin x-sym)))}

   {:op          :modify-fn
    :label       "-Sin"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.minus expr (F/Sin x-sym)))}

   {:op          :modify-fn
    :label       "+Log"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.plus expr (F/Log x-sym)))}

   {:op          :modify-fn
    :label       "-Log"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.minus expr (F/Log x-sym)))}

   {:op          :modify-fn
    :label       "+Exp"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.plus expr (F/Exp x-sym)))}

   {:op          :modify-fn
    :label       "-Exp"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.minus expr (F/Exp x-sym)))}

   {:op          :modify-fn
    :label       "+Cos"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.plus expr (F/Cos x-sym)))}

   {:op          :modify-fn
    :label       "-Cos"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.minus expr (F/Cos x-sym)))}

   {:op          :modify-fn
    :label       "*Sin"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.times expr (F/Sin x-sym)))}

   {:op          :modify-fn
    :label       "/Sin"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.times expr (F/Divide 1 (F/Sin x-sym))))}

   {:op          :modify-fn
    :label       "*Cos"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.times expr (F/Cos x-sym)))}

   {:op          :modify-fn
    :label       "/Cos"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.times expr (F/Divide 1 (F/Cos x-sym))))}

   {:op          :modify-fn
    :label       "+x"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.plus expr x-sym))}

   {:op          :modify-fn
    :label       "-x"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.minus expr x-sym))}

   {:op          :modify-fn
    :label       "+x^2"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.plus expr (F/Power x-sym 2)))}

   {:op          :modify-fn
    :label       "-x^2"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.minus expr (F/Power x-sym 2)))}

   {:op          :modify-fn
    :label       "+x^1/2"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.plus expr (F/Sqrt x-sym)))}

   {:op          :modify-fn
    :label       "-x^1/2"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.minus expr (F/Sqrt x-sym)))}

   {:op          :modify-fn
    :label       "*x"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.times expr x-sym))}

   {:op          :modify-fn
    :label       "/x"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.divide expr x-sym))}

   {:op          :modify-fn
    :label       "1/f"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.divide F/C1 expr))}

   {:op          :modify-fn
    :label       "*-1"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.times expr F/CN1))}

   {:op          :modify-fn
    :label       "/2"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.times expr F/C1D2))}

   {:op          :modify-fn
    :label       "*2"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.times expr F/C2))}

   {:op          :modify-fn
    :label       "/10"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.divide expr F/C10))}

   {:op          :modify-fn
    :label       "*10"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.times expr F/C10))}

   ;; {:op          :modify-fn
   ;; :label       "/100"
   ;; :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
   ;;                (.times expr (.divide 1 F/C100)))}
   ;;
   ;; {:op          :modify-fn
   ;; :label       "*100"
   ;; :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
   ;;                (.times expr F/C100))}

   {:op          :modify-fn
    :label       "*1.1"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.times expr (F/num 1.1)))}

   {:op          :modify-fn
    :label       "*0.9"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.times expr (F/num 0.9)))}

   {:op               :modify-leafs
    :label            "x+1/2"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (.isSymbol ie) (ops-common/should-modify-leaf leaf-count pheno))
                          (.plus ie (F/C1D2))
                          ie))}

   {:op               :modify-leafs
    :label            "x-1/2"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (.isSymbol ie) (ops-common/should-modify-leaf leaf-count pheno))
                          (.minus ie (F/C1D2))
                          ie))}

   {:op               :modify-leafs
    :label            "x/10"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (.isSymbol ie) (ops-common/should-modify-leaf leaf-count pheno))
                          (.divide ie (F/C10))
                          ie))}

   {:op               :modify-leafs
    :label            "10*x"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (.isSymbol ie) (ops-common/should-modify-leaf leaf-count pheno))
                          (.times ie (F/C10))
                          ie))}

   {:op               :modify-leafs
    :label            "1/x"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (.isSymbol ie) (ops-common/should-modify-leaf leaf-count pheno))
                          (.divide (F/C1) ie)
                          ie))}

   ;; {:op               :modify-leafs
   ;; :label            "x/100"
   ;; :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
   ;;                     (if (and (.isSymbol ie) (ops-common/should-modify-leaf leaf-count pheno))
   ;;                       (.divide ie (F/C100))
   ;;                       ie))}
   ;;
   ;;
   ;; {:op               :modify-leafs
   ;; :label            "100*x"
   ;; :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
   ;;                     (if (and (.isSymbol ie) (ops-common/should-modify-leaf leaf-count pheno))
   ;;                       (.times ie (F/C100))
   ;;                       ie))}

   {:op               :modify-leafs
    :label            "-1*x"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (.isSymbol ie) (ops-common/should-modify-leaf leaf-count pheno))
                          (.times ie (F/CN1))
                          ie))}

   {:op               :modify-leafs
    :label            "1.1*x"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (.isSymbol ie) (ops-common/should-modify-leaf leaf-count pheno))
                          (.times ie (F/num 1.1))
                          ie))}

   {:op               :modify-leafs
    :label            "0.9*x"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (.isSymbol ie) (ops-common/should-modify-leaf leaf-count pheno))
                          (.times ie (F/num 0.9))
                          ie))}

   {:op               :modify-leafs
    :label            "sin(x)"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (.isSymbol ie) (ops-common/should-modify-leaf leaf-count pheno))
                          (F/Sin x-sym)
                          ie))}

   {:op               :modify-leafs
    :label            "cos(x)"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (.isSymbol ie) (ops-common/should-modify-leaf leaf-count pheno))
                          (F/Cos x-sym)
                          ie))}

   {:op               :modify-leafs
    :label            "asin(x)"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (.isSymbol ie) (ops-common/should-modify-leaf leaf-count pheno))
                          (F/ArcSin x-sym)
                          ie))}

   {:op               :modify-leafs
    :label            "acos(x)"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (.isSymbol ie) (ops-common/should-modify-leaf leaf-count pheno))
                          (F/ArcCos x-sym)
                          ie))}

   {:op               :modify-leafs
    :label            "log(x)"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (.isSymbol ie) (ops-common/should-modify-leaf leaf-count pheno))
                          (F/Log x-sym)
                          ie))}

   {:op               :modify-leafs
    :label            "exp(x)"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (.isSymbol ie) (ops-common/should-modify-leaf leaf-count pheno))
                          (F/Exp x-sym)
                          ie))}

   {:op               :modify-leafs
    :label            "x^1/2"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (.isSymbol ie) (ops-common/should-modify-leaf leaf-count pheno))
                          (F/Power ie (F/C1D2))
                          ie))}

   {:op               :modify-leafs
    :label            "x^2"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (.isSymbol ie) (ops-common/should-modify-leaf leaf-count pheno))
                          (F/Power ie (F/C2))
                          ie))}

   {:op               :modify-leafs
    :label            "x+1/10"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (.isSymbol ie) (ops-common/should-modify-leaf leaf-count pheno))
                          (.plus ie (F/Divide 1 F/C10))
                          ie))}

   {:op               :modify-leafs
    :label            "x-1/10"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (.isSymbol ie) (ops-common/should-modify-leaf leaf-count pheno))
                          (.minus ie (F/Divide 1 F/C10))
                          ie))}

   ;; {:op               :modify-leafs
   ;; :label            "x+1/100"
   ;; :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
   ;;                     (if (and (.isSymbol ie) (ops-common/should-modify-leaf leaf-count pheno))
   ;;                       (.plus ie (F/Divide 1 F/C100))
   ;;                       ie))}
   ;;
   ;; {:op               :modify-leafs
   ;; :label            "x-1/100"
   ;; :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
   ;;                     (if (and (.isSymbol ie) (ops-common/should-modify-leaf leaf-count pheno))
   ;;                       (.minus ie (F/Divide 1 F/C100))
   ;;                       ie))}

   {:op               :modify-leafs
    :label            "c/2"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (.isNumber ie) (ops-common/should-modify-leaf leaf-count pheno))
                          (.times ie (F/C1D2))
                          ie))}

   {:op               :modify-leafs
    :label            "c*2"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (.isNumber ie) (ops-common/should-modify-leaf leaf-count pheno))
                          (.times ie (F/C2))
                          ie))}

   {:op               :modify-leafs
    :label            "c*-1"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (.isNumber ie) (ops-common/should-modify-leaf leaf-count pheno))
                          (.times ie (F/CN1))
                          ie))}

   {:op               :modify-leafs
    :label            "c/10"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (.isNumber ie) (ops-common/should-modify-leaf leaf-count pheno))
                          (.times ie (F/Divide 1 F/C10))
                          ie))}

   {:op               :modify-leafs
    :label            "c*10"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (.isNumber ie) (ops-common/should-modify-leaf leaf-count pheno))
                          (.times ie F/C10)
                          ie))}

   {:op               :modify-leafs
    :label            "c+1/10"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (.isNumber ie) (ops-common/should-modify-leaf leaf-count pheno))
                          (do
                            (.plus ie (F/Divide 1 F/C10)))
                          ie))}

   {:op               :modify-leafs
    :label            "c-1/10"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (.isNumber ie) (ops-common/should-modify-leaf leaf-count pheno))
                          (.minus ie (F/Divide 1 F/C10))
                          ie))}

   {:op               :modify-leafs
    :label            "1/c"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (.isNumber ie) (ops-common/should-modify-leaf leaf-count pheno))
                          (.divide F/C1 ie)
                          ie))}

   ;; {:op               :modify-leafs
   ;; :label            "c+1/100"
   ;; :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
   ;;                     (if (and (.isNumber ie) (ops-common/should-modify-leaf leaf-count pheno))
   ;;                       (do
   ;;                         (.plus ie (F/Divide 1 F/C100)))
   ;;                       ie))}
   ;;
   ;; {:op               :modify-leafs
   ;; :label            "c-1/100"
   ;; :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
   ;;                     (if (and (.isNumber ie) (ops-common/should-modify-leaf leaf-count pheno))
   ;;                       (.minus ie (F/Divide 1 F/C100))
   ;;                       ie))}

   {:op               :modify-leafs
    :label            "c+1/2"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (.isNumber ie) (ops-common/should-modify-leaf leaf-count pheno))
                          (.plus ie (F/Divide 1 F/C2))
                          ie))}

   {:op               :modify-leafs
    :label            "c-1/2"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (.isNumber ie) (ops-common/should-modify-leaf leaf-count pheno))
                          (.minus ie (F/Divide 1 F/C2))
                          ie))}

   {:op           :modify-substitute
    :label        "Sin->Cos"
    :find-expr    F/Sin
    :replace-expr F/Cos}

   {:op           :modify-substitute
    :label        "Cos->Sin"
    :find-expr    F/Cos
    :replace-expr F/Sin}

   {:op               :modify-ast-head
    :label            "sin->cos"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (ops-common/should-modify-ast-head leaf-count pheno)
                                 (= F/Sin ie))
                          F/Cos
                          ie))}

   {:op               :modify-ast-head
    :label            "cos->sin"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (ops-common/should-modify-ast-head leaf-count pheno)
                                 (= F/Cos ie))
                          F/Sin
                          ie))}

   {:op               :modify-ast-head
    :label            "sin->asin"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (ops-common/should-modify-ast-head leaf-count pheno)
                                 (= F/Sin ie))
                          F/ArcSin
                          ie))}

   {:op               :modify-ast-head
    :label            "cos->acos"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (ops-common/should-modify-ast-head leaf-count pheno)
                                 (= F/Cos ie))
                          F/ArcCos
                          ie))}

   {:op               :modify-ast-head
    :label            "+->*"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (ops-common/should-modify-ast-head leaf-count pheno)
                                 (= F/Plus ie))
                          F/Times
                          ie))}

   {:op               :modify-ast-head
    :label            "*->+"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (ops-common/should-modify-ast-head leaf-count pheno)
                                 (= F/Times ie))
                          F/Plus
                          ie))}

   {:op               :modify-ast-head
    :label            "^->*"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (ops-common/should-modify-ast-head leaf-count pheno)
                                 (= F/Power ie))
                          F/Times
                          ie))}

   {:op               :modify-ast-head
    :label            "/->*"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (ops-common/should-modify-ast-head leaf-count pheno)
                                 (= F/Divide ie))
                          F/Times
                          ie))}

   {:op               :modify-ast-head
    :label            "/->+"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (ops-common/should-modify-ast-head leaf-count pheno)
                                 (= F/Divide ie))
                          F/Plus
                          ie))}

   {:op               :modify-branches
    :label            "b derivative"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (ops-common/should-modify-branch leaf-count pheno)
                          (F/D ie x-sym)
                          ie))}

   {:op               :modify-branches
    :label            "b simplify"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (> 9 (.leafCount ie) 4)
                          (binding [ops-common/*simplify-max-leafs* 8]
                            #_(println "b simplify: " (.leafCount ie) " : " (str ie))
                            (try (:expr (ops-common/maybe-simplify (assoc pheno :expr ie)))
                                 (catch Exception e
                                   (log/error "Error in simplify branch: " e))))
                          ie))}

   {:op               :modify-branches
    :label            "b sin"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (ops-common/should-modify-branch leaf-count pheno)
                          (F/Sin ie)
                          ie))}

   {:op               :modify-branches
    :label            "b cos"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (ops-common/should-modify-branch leaf-count pheno)
                          (F/Cos ie)
                          ie))}

   {:op               :modify-branches
    :label            "b asin"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (ops-common/should-modify-branch leaf-count pheno)
                          (F/ArcSin ie)
                          ie))}

   {:op               :modify-branches
    :label            "b acos"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (ops-common/should-modify-branch leaf-count pheno)
                          (F/ArcCos ie)
                          ie))}

   {:op               :modify-branches
    :label            "b exp"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (ops-common/should-modify-branch leaf-count pheno)
                          (F/Exp ie)
                          ie))}

   {:op               :modify-branches
    :label            "b log"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (ops-common/should-modify-branch leaf-count pheno)
                          (F/Log ie)
                          ie))}

   {:op               :modify-branches
    :label            "b*b"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (ops-common/should-modify-branch leaf-count pheno)
                          (.times ie ie)
                          ie))}

   {:op               :modify-branches
    :label            "b^1/2"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (ops-common/should-modify-branch leaf-count pheno)
                          (F/Power ie F/C1D2)
                          ie))}

   {:op               :modify-branches
    :label            "b^-2"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (ops-common/should-modify-branch leaf-count pheno)
                          (F/Power ie F/CN2)
                          ie))}

   {:op               :modify-branches
    :label            "b^-1"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (ops-common/should-modify-branch leaf-count pheno)
                          (F/Power ie F/CN1)
                          ie))}

   {:op               :modify-branches
    :label            "b*-1"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (ops-common/should-modify-branch leaf-count pheno)
                          (.times ie F/CN1)
                          ie))}

   {:op               :modify-branches
    :label            "b*1.1"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (ops-common/should-modify-branch leaf-count pheno)
                          (.times ie (F/num 1.1))
                          ie))}

   {:op               :modify-branches
    :label            "b*0.9"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (ops-common/should-modify-branch leaf-count pheno)
                          (.times ie (F/num 0.9))
                          ie))}
   {:op               :modify-branches
    :label            "b+0.1"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (ops-common/should-modify-branch leaf-count pheno)
                          (.plus ie (F/num 0.1))
                          ie))}

   {:op               :modify-branches
    :label            "b-0.1"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (ops-common/should-modify-branch leaf-count pheno)
                          (.minus ie (F/num 0.1))
                          ie))}])
