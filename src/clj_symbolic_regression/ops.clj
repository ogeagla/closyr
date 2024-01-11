(ns clj-symbolic-regression.ops
  (:import
    (java.util
      Date
      UUID)
    org.matheclipse.core.eval.ExprEvaluator
    (org.matheclipse.core.expression
      AST
      F)
    (org.matheclipse.core.interfaces
      IAST
      IExpr
      ISymbol)))


;; https://github.com/axkr/symja_android_library?tab=readme-ov-file#examples


(defn ^IAST expr->fn
  [^ISymbol variable ^IAST expr]
  (F/Function (F/List (into-array ISymbol [variable])) expr))


(defn ->iexprs
  [coll]
  (into-array IExpr coll))


(defn ->strings
  [coll]
  (into-array String coll))


(defn replace-fn
  ^IExpr [^IExpr ie]
  (println "Add 5 to " ie)
  (.plus ie (F/C5)))


(defn ^ExprEvaluator new-util
  []
  ;; (println "new evaluator")
  (ExprEvaluator. true 0))


(defn ^java.util.function.Function as-function
  [f]
  (reify java.util.function.Function
    (apply [this arg] (f arg))))


(defn ->phenotype
  ([{v :sym e :expr u :util}]
   (->phenotype v e u))
  ([^ISymbol variable ^IAST expr ^ExprEvaluator util]
   (try
     (let [^ExprEvaluator util (or util (new-util))
           ^IAST expr          (.eval util expr)]
       {:sym  variable
        :util util
        :id   (UUID/randomUUID)
        :expr expr
        :fn   (expr->fn variable expr)})
     (catch Exception e
       (println "Err creating pheno: " expr " , " variable " , " e)))))


(defn eval-phenotype
  [{^IAST pfn :fn ^ExprEvaluator util :util} x]
  (.evalFunction util pfn (->strings [(str x)])))


(defn ^java.util.function.Function tree-leaf-modifier
  [modifier]
  (as-function (fn ^IExpr [^IExpr ie]
                 (if (instance? IAST ie)
                   (.map ^IAST ie (tree-leaf-modifier modifier))
                   (modifier ie)))))


(defmulti modify (fn [{:keys [op]} pheno] op))


(defmethod modify :substitute
  [{:keys [^IExpr find-expr ^IExpr replace-expr]}
   {^IAST expr :expr ^ISymbol x-sym :sym ^ExprEvaluator util :util first-modify :first-modify :as pheno}]
  (->phenotype x-sym (.subs expr find-expr replace-expr) (if first-modify nil util)))


(defmethod modify :modify-leafs
  [{:keys [leaf-modifier-fn]}
   {^IAST expr :expr ^ISymbol x-sym :sym ^ExprEvaluator util :util first-modify :first-modify :as pheno}]
  (->phenotype x-sym (.replaceAll expr (tree-leaf-modifier leaf-modifier-fn)) (if first-modify nil util)))


(defmethod modify :fn
  [{:keys [modifier-fn]}
   {^IAST expr :expr ^ISymbol x-sym :sym ^ExprEvaluator util :util first-modify :first-modify :as pheno}]
  (->phenotype x-sym (modifier-fn pheno) (if first-modify nil util)))


(defn initial-phenotypes
  [^ISymbol x reps]
  (->>
    [F/C0
     x
     (F/Times -1 (->iexprs [x]))]
    (repeat reps)
    (mapcat identity)
    (mapv (fn [^IExpr expr] (->phenotype x expr nil)))))


(def modify-leafs-sampler [true false])


(defn initial-mutations
  []
  [{:op          :fn
    :label       "Derivative"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (F/D expr x-sym))}

   {:op          :fn
    :label       "+1/2"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.plus expr F/C1D2))}

   {:op          :fn
    :label       "-1/2"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.minus expr F/C1D2))}
   {:op          :fn
    :label       "+1/10"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.plus expr (F/Divide 1 F/C10)))}

   {:op          :fn
    :label       "-1/10"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.minus expr (F/Divide 1 F/C10)))}

   {:op          :fn
    :label       "+Sin"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.plus expr (F/Sin x-sym)))}

   {:op          :fn
    :label       "-Sin"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.minus expr (F/Sin x-sym)))}

   {:op          :fn
    :label       "+Cos"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.plus expr (F/Cos x-sym)))}

   {:op          :fn
    :label       "-Cos"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.minus expr (F/Cos x-sym)))}


   ;; {:op          :fn
   ;; :label       "+Exp"
   ;; :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
   ;;                (.plus expr (F/Exp x-sym)))}
   ;;
   ;; {:op          :fn
   ;; :label       "-Exp"
   ;; :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
   ;;                (.minus expr (F/Exp x-sym)))}
   ;;
   ;;
   ;;
   ;; {:op          :fn
   ;; :label       "+ 1/Exp"
   ;; :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
   ;;                (.plus expr (F/Divide 1 (F/Exp x-sym))))}
   ;;
   ;; {:op          :fn
   ;; :label       "- 1/Exp"
   ;; :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
   ;;                (.minus expr (F/Divide 1 (F/Exp x-sym))))}
   ;;
   ;;
   ;;
   ;; {:op          :fn
   ;; :label       "*Exp"
   ;; :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
   ;;                (.times expr (F/Exp x-sym)))}
   ;;
   ;; {:op          :fn
   ;; :label       "/Exp"
   ;; :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
   ;;                (.times expr (F/Divide 1 (F/Exp x-sym))))}


   {:op          :fn
    :label       "*Sin"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.times expr (F/Sin x-sym)))}

   {:op          :fn
    :label       "/Sin"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.times expr (F/Divide 1 (F/Sin x-sym))))}

   {:op          :fn
    :label       "*Cos"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.times expr (F/Cos x-sym)))}

   {:op          :fn
    :label       "/Cos"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.times expr (F/Divide 1 (F/Cos x-sym))))}

   {:op          :fn
    :label       "+x"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.plus expr x-sym))}

   {:op          :fn
    :label       "-x"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.minus expr x-sym))}
   {:op          :fn
    :label       "*x"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.times expr x-sym))}

   {:op          :fn
    :label       "/x"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.divide expr x-sym))}
   {:op          :fn
    :label       "*-1"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.times expr F/CN1))}

   {:op          :fn
    :label       "/2"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.times expr F/C1D2))}

   {:op          :fn
    :label       "*2"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.times expr F/C2))}


   {:op          :fn
    :label       "/10"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.times expr (F/Divide 1 F/C10)))}

   {:op          :fn
    :label       "*10"
    :modifier-fn (fn ^IExpr [{^IAST expr :expr ^ISymbol x-sym :sym :as pheno}]
                   (.times expr F/C10))}



   {:op               :modify-leafs
    :label            "x+1/5"
    :leaf-modifier-fn (fn ^IExpr [^IExpr ie]
                        (if (and (= (.toString ie) "x") (rand-nth modify-leafs-sampler))
                          (.plus ie (F/C1D5))
                          ie))}


   {:op               :modify-leafs
    :label            "x-1/5"
    :leaf-modifier-fn (fn ^IExpr [^IExpr ie]
                        (if (and (= (.toString ie) "x") (rand-nth modify-leafs-sampler))
                          (.minus ie (F/C1D5))
                          ie))}


   {:op               :modify-leafs
    :label            "x+1/10"
    :leaf-modifier-fn (fn ^IExpr [^IExpr ie]
                        (if (and (= (.toString ie) "x") (rand-nth modify-leafs-sampler))
                          (.plus ie (F/Divide 1 F/C10))
                          ie))}


   {:op               :modify-leafs
    :label            "x-1/10"
    :leaf-modifier-fn (fn ^IExpr [^IExpr ie]
                        (if (and (= (.toString ie) "x") (rand-nth modify-leafs-sampler))
                          (.minus ie (F/Divide 1 F/C10))
                          ie))}


   {:op               :modify-leafs
    :label            "c/2"
    :leaf-modifier-fn (fn ^IExpr [^IExpr ie]
                        (if (and (= (.toString ie) "x") (rand-nth modify-leafs-sampler))
                          ie
                          (.times ie (F/C1D2))))}


   {:op               :modify-leafs
    :label            "c*2"
    :leaf-modifier-fn (fn ^IExpr [^IExpr ie]
                        (if (and (= (.toString ie) "x") (rand-nth modify-leafs-sampler))
                          ie
                          (.times ie (F/C2))))}

   {:op               :modify-leafs
    :label            "c/10"
    :leaf-modifier-fn (fn ^IExpr [^IExpr ie]
                        (if (and (= (.toString ie) "x") (rand-nth modify-leafs-sampler))
                          ie
                          (.times ie (F/Divide 1 F/C10))))}


   {:op               :modify-leafs
    :label            "c*10"
    :leaf-modifier-fn (fn ^IExpr [^IExpr ie]
                        (if (and (= (.toString ie) "x") (rand-nth modify-leafs-sampler))
                          ie
                          (.times ie F/C10)))}

   {:op           :substitute
    :label        "Divide->Times"
    :find-expr    F/Divide
    :replace-expr F/Times}

   {:op           :substitute
    :label        "Minus->Plus"
    :find-expr    F/Minus
    :replace-expr F/Plus}

   {:op           :substitute
    :label        "Sin->Cos"
    :find-expr    F/Sin
    :replace-expr F/Cos}

   {:op           :substitute
    :label        "Cos->Sin"
    :find-expr    F/Cos
    :replace-expr F/Sin}])


(defn demo-math-2
  []

  (let [start          (Date.)
        _              (println "start " start)
        ^ISymbol sym-x (F/Dummy "x")
        initial-phenos (initial-phenotypes sym-x 1)
        initial-muts   (initial-mutations)
        report         (->>
                         (for [p  initial-phenos
                               m1 initial-muts
                               m2 initial-muts
                               m3 initial-muts]
                           (let [new-p           (modify m3 (modify m2 (modify m1 p)))
                                 ^IExpr new-expr (:expr new-p)
                                 new-is-const    (.isNumber new-expr)
                                 input-exprs     [(.add F/C0 1.123456) F/C1D2 F/C1D5 F/C1D4 F/C1D3 F/C1]
                                 eval-p          (eval-phenotype new-p (F/List (into-array IExpr input-exprs)))
                                 vs              (vec (pmap
                                                        (fn [i]
                                                          (let [v (try
                                                                    (.doubleValue
                                                                      (.toNumber (.getArg eval-p (inc i) F/Infinity)))
                                                                    (catch Exception e
                                                                      Double/POSITIVE_INFINITY))]
                                                            #_(println "Got output vec item: " v)
                                                            v))
                                                        (range (dec (.size eval-p)))))
                                 vs              (if (seq vs)
                                                   vs
                                                   (vec (pmap
                                                          (fn [i]
                                                            (.doubleValue
                                                              (.toNumber (if new-is-const
                                                                           new-expr
                                                                           (.getArg eval-p 0 F/Infinity)))))
                                                          (range (count input-exprs)))))]


                             (str
                               "\n" (:expr p) " -> " (:label m1) "." (:label m2) "." (:label m3) " :: "
                               (:expr new-p)
                               " -->> type:" (type eval-p) " size:" (.size eval-p) " " #_eval-p
                               " head: " (.getArg eval-p 0 nil)
                               " output: " vs)))
                         (sort-by count)
                         (reverse))
        end            (Date.)
        diff           (- (.getTime end) (.getTime start))]

    (println "initial muts: " (count initial-muts))
    (println "initial fn x muts: "
             (take 20 report)
             "\n...\n"
             (take 20 (take-last (/ (count report) 2) report))
             (take-last 20 (take (/ (count report) 2) report))
             "\n...\n"
             (take-last 20 report))
    (println "Took " (/ diff 1000.0) " seconds")))


(comment
  (demo-math-2))
