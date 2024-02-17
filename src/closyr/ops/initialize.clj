(ns closyr.ops.initialize
  (:require
    [closyr.ops.common :as ops-common]
    [closyr.util.log :as log]
    [closyr.util.spec :as specs])
  (:import
    (org.matheclipse.core.expression
      AST
      F)
    (org.matheclipse.core.interfaces
      IAST
      IExpr
      ISymbol)))

(set! *warn-on-reflection* true)

(defn initial-exprs
  "Initial exprs to use in GA evolution"
  []
  (let [^ISymbol x ops-common/sym-x]
    [(.times x F/C1)]))


(defn initial-phenotypes
  "Initial exprs scaled up in quantity to use in GA evolution"
  {:malli/schema [:=> [:cat pos-int?] #'specs/GAPopulationPhenotypes]}
  [p-count]
  (let [^ISymbol x ops-common/sym-x]
    (->>
      (repeatedly (/ p-count (count (initial-exprs))) initial-exprs)
      (mapcat identity)
      (mapv (fn [^IExpr expr] (ops-common/->phenotype x expr nil))))))


(defn initial-mutations
  "All mutations to use in GA evolutions"
  {:malli/schema [:=> [:cat] [:vector #'specs/GAMutation]]}
  []
  [{:op          :modify-fn
    :label       "Derivative"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (F/D expr x-sym))}

   {:op          :modify-fn
    :label       "+1/2"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (F/Plus expr F/C1D2))}

   {:op          :modify-fn
    :label       "-1/2"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (F/Subtract expr F/C1D2))}
   {:op          :modify-fn
    :label       "+1/10"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (F/Plus expr (F/Divide 1 F/C10)))}

   {:op          :modify-fn
    :label       "-1/10"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (F/Subtract expr (F/Divide 1 F/C10)))}

   {:op          :modify-fn
    :label       "+1/100"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (F/Plus expr (F/Divide 1 F/C100)))}

   {:op          :modify-fn
    :label       "-1/100"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (F/Subtract expr (F/Divide 1 F/C100)))}

   {:op          :modify-fn
    :label       "+Sin"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (F/Plus expr (F/Sin x-sym)))}

   {:op          :modify-fn
    :label       "-Sin"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (F/Subtract expr (F/Sin x-sym)))}

   {:op          :modify-fn
    :label       "+Log"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (F/Plus expr (F/Log x-sym)))}

   {:op          :modify-fn
    :label       "-Log"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (F/Subtract expr (F/Log x-sym)))}

   {:op          :modify-fn
    :label       "+Exp"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (F/Plus expr (F/Exp x-sym)))}

   {:op          :modify-fn
    :label       "-Exp"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (F/Subtract expr (F/Exp x-sym)))}

   {:op          :modify-fn
    :label       "+Cos"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (F/Plus expr (F/Cos x-sym)))}

   {:op          :modify-fn
    :label       "-Cos"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (F/Subtract expr (F/Cos x-sym)))}

   {:op          :modify-fn
    :label       "*Sin"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (F/Times expr (F/Sin x-sym)))}

   {:op          :modify-fn
    :label       "/Sin"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (F/Times expr (F/Divide 1 (F/Sin x-sym))))}

   {:op          :modify-fn
    :label       "*Cos"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (F/Times expr (F/Cos x-sym)))}

   {:op          :modify-fn
    :label       "/Cos"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (F/Times expr (F/Divide 1 (F/Cos x-sym))))}

   {:op          :modify-fn
    :label       "+x"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (F/Plus expr x-sym))}

   {:op          :modify-fn
    :label       "-x"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (F/Subtract expr x-sym))}

   {:op          :modify-fn
    :label       "+x^2"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (F/Plus expr (F/Power x-sym 2)))}

   {:op          :modify-fn
    :label       "-x^2"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (F/Subtract expr (F/Power x-sym 2)))}

   {:op          :modify-fn
    :label       "+x^1/2"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (F/Plus expr (F/Sqrt x-sym)))}

   {:op          :modify-fn
    :label       "-x^1/2"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (F/Subtract expr (F/Sqrt x-sym)))}

   {:op          :modify-fn
    :label       "*x"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (F/Times expr x-sym))}

   {:op          :modify-fn
    :label       "/x"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (F/Divide expr x-sym))}

   {:op          :modify-fn
    :label       "1/f"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (F/Divide F/C1 expr))}

   {:op          :modify-fn
    :label       "*-1"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (F/Times expr F/CN1))}

   {:op          :modify-fn
    :label       "/2"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (F/Times expr F/C1D2))}

   {:op          :modify-fn
    :label       "*2"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (F/Times expr F/C2))}

   {:op          :modify-fn
    :label       "/10"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (F/Divide expr F/C10))}

   {:op          :modify-fn
    :label       "*10"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (F/Times expr F/C10))}

   {:op          :modify-fn
    :label       "/100"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (F/Times expr (F/Divide 1 F/C100)))}

   {:op          :modify-fn
    :label       "*100"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (F/Times expr F/C100))}

   {:op          :modify-fn
    :label       "*1.1"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (F/Times expr (F/num 1.1)))}

   {:op          :modify-fn
    :label       "*0.9"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (F/Times expr (F/num 0.9)))}

   {:op               :modify-leafs
    :label            "x+1/2"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (.isSymbol ie) (ops-common/should-modify-leaf leaf-count pheno))
                          (F/Plus ie (F/C1D2))
                          ie))}

   {:op               :modify-leafs
    :label            "x-1/2"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (.isSymbol ie) (ops-common/should-modify-leaf leaf-count pheno))
                          (F/Subtract ie (F/C1D2))
                          ie))}

   {:op               :modify-leafs
    :label            "x/10"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (.isSymbol ie) (ops-common/should-modify-leaf leaf-count pheno))
                          (F/Divide ie (F/C10))
                          ie))}

   {:op               :modify-leafs
    :label            "10*x"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (.isSymbol ie) (ops-common/should-modify-leaf leaf-count pheno))
                          (F/Times ie (F/C10))
                          ie))}

   {:op               :modify-leafs
    :label            "1/x"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (.isSymbol ie) (ops-common/should-modify-leaf leaf-count pheno))
                          (F/Divide (F/C1) ie)
                          ie))}

   {:op               :modify-leafs
    :label            "x/100"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (.isSymbol ie) (ops-common/should-modify-leaf leaf-count pheno))
                          (F/Divide ie (F/C100))
                          ie))}


   {:op               :modify-leafs
    :label            "100*x"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (.isSymbol ie) (ops-common/should-modify-leaf leaf-count pheno))
                          (F/Times ie (F/C100))
                          ie))}

   {:op               :modify-leafs
    :label            "-1*x"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (.isSymbol ie) (ops-common/should-modify-leaf leaf-count pheno))
                          (F/Times ie (F/CN1))
                          ie))}

   {:op               :modify-leafs
    :label            "1.1*x"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (.isSymbol ie) (ops-common/should-modify-leaf leaf-count pheno))
                          (F/Times ie (F/num 1.1))
                          ie))}

   {:op               :modify-leafs
    :label            "0.9*x"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (.isSymbol ie) (ops-common/should-modify-leaf leaf-count pheno))
                          (F/Times ie (F/num 0.9))
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
                          (F/Plus ie (F/Divide 1 F/C10))
                          ie))}

   {:op               :modify-leafs
    :label            "x-1/10"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (.isSymbol ie) (ops-common/should-modify-leaf leaf-count pheno))
                          (F/Subtract ie (F/Divide 1 F/C10))
                          ie))}

   {:op               :modify-leafs
    :label            "x+1/100"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (.isSymbol ie) (ops-common/should-modify-leaf leaf-count pheno))
                          (F/Plus ie (F/Divide 1 F/C100))
                          ie))}

   {:op               :modify-leafs
    :label            "x-1/100"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (.isSymbol ie) (ops-common/should-modify-leaf leaf-count pheno))
                          (F/Subtract ie (F/Divide 1 F/C100))
                          ie))}

   {:op               :modify-leafs
    :label            "c/2"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (.isNumber ie) (ops-common/should-modify-leaf leaf-count pheno))
                          (F/Times ie (F/C1D2))
                          ie))}

   {:op               :modify-leafs
    :label            "c*2"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (.isNumber ie) (ops-common/should-modify-leaf leaf-count pheno))
                          (F/Times ie (F/C2))
                          ie))}

   {:op               :modify-leafs
    :label            "c*-1"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (.isNumber ie) (ops-common/should-modify-leaf leaf-count pheno))
                          (F/Times ie (F/CN1))
                          ie))}

   {:op               :modify-leafs
    :label            "c/10"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (.isNumber ie) (ops-common/should-modify-leaf leaf-count pheno))
                          (F/Times ie (F/Divide 1 F/C10))
                          ie))}

   {:op               :modify-leafs
    :label            "c*10"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (.isNumber ie) (ops-common/should-modify-leaf leaf-count pheno))
                          (F/Times ie F/C10)
                          ie))}

   {:op               :modify-leafs
    :label            "c+1/10"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (.isNumber ie) (ops-common/should-modify-leaf leaf-count pheno))
                          (do
                            (F/Plus ie (F/Divide 1 F/C10)))
                          ie))}

   {:op               :modify-leafs
    :label            "c-1/10"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (.isNumber ie) (ops-common/should-modify-leaf leaf-count pheno))
                          (F/Subtract ie (F/Divide 1 F/C10))
                          ie))}

   {:op               :modify-leafs
    :label            "1/c"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (.isNumber ie) (ops-common/should-modify-leaf leaf-count pheno))
                          (F/Divide F/C1 ie)
                          ie))}

   {:op               :modify-leafs
    :label            "c+1/100"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (.isNumber ie) (ops-common/should-modify-leaf leaf-count pheno))
                          (do
                            (F/Plus ie (F/Divide 1 F/C100)))
                          ie))}

   {:op               :modify-leafs
    :label            "c-1/100"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (.isNumber ie) (ops-common/should-modify-leaf leaf-count pheno))
                          (F/Subtract ie (F/Divide 1 F/C100))
                          ie))}

   {:op               :modify-leafs
    :label            "c+1/2"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (.isNumber ie) (ops-common/should-modify-leaf leaf-count pheno))
                          (F/Plus ie (F/Divide 1 F/C2))
                          ie))}

   {:op               :modify-leafs
    :label            "c-1/2"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (and (.isNumber ie) (ops-common/should-modify-leaf leaf-count pheno))
                          (F/Subtract ie (F/Divide 1 F/C2))
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

   #_{:op               :modify-ast-head
      :label            "/->*"
      :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                          (if (and (ops-common/should-modify-ast-head leaf-count pheno)
                                   (= F/Divide ie))
                            F/Times
                            ie))}

   #_{:op               :modify-ast-head
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

   #_{:op               :modify-branches
      :label            "b simplify"
      :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                          (if (> 9 (.leafCount ie) 4)
                            (binding [ops-common/*simplify-max-leafs* 8]
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
                          (F/Times ie ie)
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
                          (F/Times ie F/CN1)
                          ie))}

   {:op               :modify-branches
    :label            "b*1.1"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (ops-common/should-modify-branch leaf-count pheno)
                          (F/Times ie (F/num 1.1))
                          ie))}

   {:op               :modify-branches
    :label            "b*0.9"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (ops-common/should-modify-branch leaf-count pheno)
                          (F/Times ie (F/num 0.9))
                          ie))}
   {:op               :modify-branches
    :label            "b+0.1"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (ops-common/should-modify-branch leaf-count pheno)
                          (F/Plus ie (F/num 0.1))
                          ie))}

   {:op               :modify-branches
    :label            "b-0.1"
    :leaf-modifier-fn (fn ^IExpr [leaf-count {^IAST expr :expr ^ISymbol x-sym :sym :as pheno} ^IExpr ie]
                        (if (ops-common/should-modify-branch leaf-count pheno)
                          (F/Subtract ie (F/num 0.1))
                          ie))}])


(specs/instrument-all!)
