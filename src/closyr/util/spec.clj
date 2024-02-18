(ns closyr.util.spec
  (:require
    [clojure.pprint :as pp]
    [clojure.test.check.generators :as gen]
    [closyr.util.log :as log]
    [malli.core :as m]
    [malli.error :as me]
    [malli.generator :as mg]
    [malli.instrument :as mi]
    [malli.transform :as mt])
  (:import
    (org.matheclipse.core.eval
      EvalEngine
      ExprEvaluator)
    (org.matheclipse.core.expression
      F)
    (org.matheclipse.core.interfaces
      IExpr
      ISymbol)))


(set! *warn-on-reflection* true)


(def ^:dynamic *check-schema*
  "Toggle checking schema and potentially throwing exceptions"
  true)


(defn validate!
  "Check that an object o is compliant with schema s with name n, or throw exception"
  [n s o]
  (when (and *check-schema* (not (m/validate s o)))
    (let [explained (me/humanize (m/explain s o))]
      (log/error "Error in input schema: " n)
      (pp/pprint [n explained])
      (throw (Exception. (str "Error, input failed schema: " [n explained])))))
  true)


(defn instrument-all!
  "Instrument all defn schema"
  []
  (when *check-schema*
    (mi/collect!)
    (mi/instrument!)))


#_(defn- disable-validate-instrumentation!
    "Turn off all schema checks"
    []
    (alter-var-root #'*check-schema* (constantly false))
    (mi/unstrument!
      {:filters [(apply mi/-filter-ns (concat (keys (m/function-schemas))
                                              ['closyr.util.spec-test
                                               'cursive.tests.runner
                                               'user]))]}))


(def iexpr-array-class
  "Class for IExpr[]"
  (Class/forName "[Lorg.matheclipse.core.interfaces.IExpr;"))


(def ^:private PrimitiveArrayOfIExpr
  (m/-simple-schema
    {:type            :user/iexpr-array
     :pred            #(instance? iexpr-array-class %)
     :type-properties {:error/fn      (fn [error _] (str "should be a IExpr[], got " (:value error)))
                       :error/message "should be IExpr[]"
                       :gen/gen       (gen/elements [(into-array IExpr [F/C1 F/CN1 (F/Sin (F/Dummy "x"))])])}}))


#_(def ^:private PrimitiveArrayOfIExpr
    some?)


(def ^:private SymbolicVariable
  (m/-simple-schema
    {:type            :user/symbolic-variable
     :pred            #(instance? ISymbol %)
     :type-properties {:error/fn      (fn [error _] (str "should be an ISymbol, got " (:value error)))
                       :error/message "should be ISymbol"
                       :gen/gen       (gen/elements [(F/Dummy "x")])}}))


(def ^:private SymbolicEvaluator
  (m/-simple-schema
    {:type            :user/symbolic-evaluator
     :pred            #(instance? ExprEvaluator %)
     :type-properties {:error/fn      (fn [error _] (str "should be an ExprEvaluator, got " (:value error)))
                       :error/message "should be ExprEvaluator"
                       :gen/gen       (gen/let [f1 (gen/large-integer* {:min 1 :max 100})]
                                        (gen/return
                                          (ExprEvaluator.
                                            (doto (EvalEngine. true)
                                              (.setQuietMode true))
                                            true
                                            0)))}}))


(def ^:private SymbolicExpr
  (m/-simple-schema
    {:type            :user/symbolic-expr
     :pred            #(instance? IExpr %)
     :type-properties {:error/fn      (fn [error _] (str "should be an IExpr, got " (:value error)))
                       :error/message "should be IExpr"
                       :gen/gen       (gen/let [f1 (gen/large-integer* {:min -1000 :max 1000})
                                                f2 (gen/double* {:min -1000.0 :max 1000.0})]
                                        (let [x (F/Dummy "x")]
                                          (gen/elements
                                            (mapv
                                              (fn [^IExpr expr]
                                                (F/Times (F/num (float f2))
                                                         (F/Plus expr (F/num (float f1)))))
                                              [x
                                               (F/Sin x)
                                               (F/Cos x)
                                               (F/Exp x)
                                               (F/Log x)
                                               (F/Sqrt x)
                                               (F/Times x F/C2)
                                               (F/Plus x F/C5)
                                               (F/Times x x)
                                               (F/Times x (F/Times x x))]))))}}))


(def ^:private NumberVector
  [:vector number?])


(def ^:private MaxLeafs
  [:int {:min 1 :max 500}])


(def ^:private Iterations
  [:int {:min 1 :max 1000000}])


(def ^:private PointsCount
  [:int {:min 1 :max 10000}])


(def ^:private PopulationCount
  [:int {:min 1 :max 10000}])


(def ^:private GAPhenotype
  [:map
   {:closed true}
   [:id {:optional true} :uuid]
   [:sym #'SymbolicVariable]
   [:expr {:optional true} #'SymbolicExpr]
   [:score {:optional true} number?]
   [:util {:optional true} [:maybe #'SymbolicEvaluator]]
   [:last-op {:optional true} :string]
   [:mods-applied {:optional true} :int]])


(def ^:private GAPopulationPhenotypes
  [:vector #'GAPhenotype])


(def ^:private GAMutationOps
  [:enum :modify-substitute :modify-fn :modify-leafs :modify-branches :modify-ast-head])


(def ^:private GAMutationLabel
  :string)


(def ^:private GAMutation
  [:map
   {:closed true}
   [:op #'GAMutationOps]
   [:label {:optional true} #'GAMutationLabel]
   [:leaf-modifier-fn {:optional true} fn?]
   [:modifier-fn {:optional true} fn?]
   [:find-expr {:optional true} #'SymbolicExpr]
   [:replace-expr {:optional true} #'SymbolicExpr]])


(def ^:private SolverRunConfig
  [:map
   {:closed true}
   [:iters #'Iterations]
   [:initial-phenos #'GAPopulationPhenotypes]
   [:initial-muts [:sequential #'GAMutation]]
   [:use-gui? :boolean]
   [:max-leafs #'MaxLeafs]
   [:input-phenos-count {:optional true} #'PopulationCount]
   [:log-steps pos-int?]
   [:use-flamechart [:maybe :boolean]]
   [:input-xs-exprs [:vector #'SymbolicExpr]]
   [:input-ys-exprs [:vector #'SymbolicExpr]]])


(def ^:private ExtendedDomainArgs
  [:map
   {:closed true}
   [:xs #'NumberVector]
   [:x-head #'NumberVector]
   [:x-head-list #'PrimitiveArrayOfIExpr]
   [:x-tail #'NumberVector]
   [:x-tail-list #'PrimitiveArrayOfIExpr]])


(def ^:private SolverRunArgs
  [:map
   {:closed true}
   [:sim->gui-chan {:optional true} some?]
   [:sim-stop-start-chan {:optional true} some?]
   [:extended-domain-args #'ExtendedDomainArgs]
   [:input-xs-list #'PrimitiveArrayOfIExpr]
   [:input-xs-count #'PointsCount]
   [:input-xs-vec #'NumberVector]
   [:input-ys-vec #'NumberVector]
   [:input-iters #'Iterations]
   [:initial-phenos [:maybe #'GAPopulationPhenotypes]]
   [:input-phenos-count [:maybe #'PopulationCount]]
   [:max-leafs [:maybe #'MaxLeafs]]])


(def ^:private SolverEvalArgs
  [:map
   {:closed false}
   [:input-xs-list #'PrimitiveArrayOfIExpr]
   [:input-xs-count #'PointsCount]])


(def ^:private ScoreFnArgs
  [:map
   {:closed false}
   [:input-ys-vec #'NumberVector]
   [:input-xs-list #'PrimitiveArrayOfIExpr]
   [:input-xs-count #'PointsCount]])


(def ^:private GAPopulation
  [:map
   {:closed true}
   [:pop #'GAPopulationPhenotypes]
   [:score-fn fn?]
   [:pop-scores #'NumberVector]
   [:mutation-fn fn?]
   [:crossover-fn fn?]])


(def ^:private SolverRunResults
  [:map
   {:closed true}
   [:iters-done number?]
   [:final-population #'GAPopulation]
   [:next-step [:enum :wait :stop :restart]]])


(def ^:private SolverGUIInputArgs
  [:map
   {:closed true}
   [:input-xs-exprs [:vector #'SymbolicExpr]]
   [:input-xs-vec #'NumberVector]
   [:input-ys-vec #'NumberVector]
   [:input-iters #'Iterations]
   [:input-phenos-count #'PopulationCount]
   [:max-leafs [:maybe #'MaxLeafs]]])


(def ^:private SolverInputArgs
  [:map
   {:closed true}
   [:use-flamechart {:optional true} [:maybe :boolean]]
   [:use-gui? {:optional true} [:maybe :boolean]]
   [:initial-muts {:optional true} [:sequential #'GAMutation]]
   [:initial-phenos {:optional true} [:maybe #'GAPopulationPhenotypes]]
   [:input-xs-exprs {:optional true} [:vector #'SymbolicExpr]]
   [:input-ys-exprs {:optional true} [:vector #'SymbolicExpr]]
   [:input-xs-vec #'NumberVector]
   [:input-ys-vec #'NumberVector]
   [:input-iters {:optional true} #'Iterations]
   [:iters {:optional true} #'Iterations]
   [:input-phenos-count {:optional true} #'PopulationCount]
   [:max-leafs {:optional true} [:maybe #'MaxLeafs]]])


(def ^:private SolverGUIMessage
  [:map
   {:closed true}
   [:new-state [:enum :start :pause :stop :restart]]
   [:input-data-x #'NumberVector]
   [:input-data-y #'NumberVector]
   [:input-iters #'Iterations]
   [:input-phenos-count #'PopulationCount]
   [:max-leafs {:optional true} [:maybe #'MaxLeafs]]])


(def ^:private CLIArgs
  [:map
   {:closed true}
   [:log-level {:optional true} [:maybe [:enum :error :warn :info :debug]]]
   [:iterations #'Iterations]
   [:population #'PopulationCount]
   [:headless boolean?]
   [:xs {:optional true} [:maybe #'NumberVector]]
   [:ys {:optional true} [:maybe #'NumberVector]]
   [:use-flamechart {:optional true} boolean?]
   [:max-leafs {:optional true} #'MaxLeafs]])


(def ^:private ModificationsResult
  [:map {:closed true}
   [:new-pheno #'GAPhenotype]
   [:iters int?]
   [:mods [:sequential #'GAMutation]]])
