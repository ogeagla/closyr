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
      IExpr)))


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


(def ^:private PrimitiveArrayOfIExpr
  some?)


(def ^:private GAPhenotype
  [:map
   {:closed true}
   [:id {:optional true} :uuid]
   [:sym some?]
   [:expr {:optional true} #'SymbolicExpr]
   [:score {:optional true} number?]
   [:util {:optional true} [:maybe #'SymbolicEvaluator]]
   [:last-op {:optional true} :string]
   [:mods-applied {:optional true} :int]])


(def ^:private GAPopulationPhenotypes
  [:vector #'GAPhenotype])


(def ^:private GAMutation
  [:map
   {:closed true}
   [:op [:enum :modify-substitute :modify-fn :modify-leafs :modify-branches :modify-ast-head]]
   [:label {:optional true} :string]
   [:leaf-modifier-fn {:optional true} fn?]
   [:modifier-fn {:optional true} fn?]
   [:find-expr {:optional true} some?]
   [:replace-expr {:optional true} some?]])


(def ^:private SolverRunConfig
  [:map
   {:closed true}
   [:iters pos-int?]
   [:initial-phenos #'GAPopulationPhenotypes]
   [:initial-muts [:sequential #'GAMutation]]
   [:use-gui? :boolean]
   [:max-leafs pos-int?]
   [:input-phenos-count {:optional true} pos-int?]
   [:log-steps pos-int?]
   [:use-flamechart [:maybe :boolean]]
   [:input-xs-exprs [:sequential some?]]
   [:input-ys-exprs [:sequential some?]]])


(def ^:private ExtendedDomainArgs
  [:map
   {:closed true}
   [:xs #'NumberVector]
   [:x-head #'NumberVector]
   [:x-head-list some?]
   [:x-tail #'NumberVector]
   [:x-tail-list some?]])


(def ^:private SolverRunArgs
  [:map
   {:closed true}
   [:sim->gui-chan {:optional true} some?]
   [:sim-stop-start-chan {:optional true} some?]
   [:extended-domain-args #'ExtendedDomainArgs]
   [:input-xs-list some?]
   [:input-xs-count pos-int?]
   [:input-xs-vec #'NumberVector]
   [:input-ys-vec #'NumberVector]
   [:input-iters pos-int?]
   [:initial-phenos [:maybe #'GAPopulationPhenotypes]]
   [:input-phenos-count [:maybe pos-int?]]
   [:max-leafs [:maybe pos-int?]]])


(def ^:private SolverEvalArgs
  [:map
   {:closed false}
   [:input-xs-list some?]
   [:input-xs-count pos-int?]])


(def ^:private ScoreFnArgs
  [:map
   {:closed false}
   [:input-ys-vec #'NumberVector]
   [:input-xs-list some?]
   [:input-xs-count pos-int?]])


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
   [:input-xs-exprs some?]
   [:input-xs-vec #'NumberVector]
   [:input-ys-vec #'NumberVector]
   [:input-iters pos-int?]
   [:input-phenos-count pos-int?]
   [:max-leafs [:maybe pos-int?]]])


(def ^:private SolverGUIMessage
  [:map
   {:closed true}
   [:new-state [:enum :start :pause :stop :restart]]
   [:input-data-x #'NumberVector]
   [:input-data-y #'NumberVector]
   [:input-iters pos-int?]
   [:input-phenos-count pos-int?]
   [:max-leafs {:optional true} [:maybe pos-int?]]])


(def ^:private CLIArgs
  [:map
   {:closed true}
   [:log-level {:optional true} keyword?]
   [:iterations any?]
   [:population any?]
   [:headless any?]
   [:xs {:optional true} any?]
   [:ys {:optional true} any?]
   [:use-flamechart {:optional true} any?]
   [:max-leafs {:optional true} any?]])


(def ^:private ModificationsResult
  [:map {:closed true}
   [:new-pheno #'GAPhenotype]
   [:iters int?]
   [:mods [:sequential #'GAMutation]]])

