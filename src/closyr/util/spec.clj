(ns closyr.util.spec
  (:require
    [clojure.pprint :as pp]
    [closyr.util.log :as log]
    [malli.core :as m]
    [malli.error :as me]
    [malli.instrument :as mi]))


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


(def ^:private GAPhenotype
  [:map
   {:closed true}
   [:id {:optional true} :uuid]
   [:sym some?]
   [:expr {:optional true} some?]
   [:score {:optional true} number?]
   [:util {:optional true} any?]
   [:last-op {:optional true} :string]
   [:mods-applied {:optional true} :int]])


(def ^:private GAPopulationPhenotypes
  [:vector #'GAPhenotype])


(def ^:private GAMutation
  [:map
   {:closed true}
   [:op :keyword]
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


(def ^:private SolverRunArgs
  [:map
   {:closed true}
   [:sim->gui-chan {:optional true} some?]
   [:sim-stop-start-chan {:optional true} some?]
   [:extended-domain-args map?]
   [:input-xs-list some?]
   [:input-xs-count pos-int?]
   [:input-xs-vec [:vector number?]]
   [:input-ys-vec [:vector number?]]
   [:input-iters pos-int?]
   [:initial-phenos [:maybe [:sequential map?]]]
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
   [:input-ys-vec [:vector number?]]
   [:input-xs-list some?]
   [:input-xs-count pos-int?]])


(def ^:private SolverRunResults
  [:map
   {:closed true}
   [:iters-done number?]
   [:final-population map?]
   [:next-step :keyword]])


(def ^:private SolverGUIInputArgs
  [:map
   {:closed true}
   [:input-xs-exprs some?]
   [:input-xs-vec [:vector number?]]
   [:input-ys-vec [:vector number?]]
   [:input-iters pos-int?]
   [:input-phenos-count pos-int?]
   [:max-leafs [:maybe pos-int?]]])


(def ^:private SolverGUIMessage
  [:map
   {:closed true}
   [:new-state keyword?]
   [:input-data-x [:vector number?]]
   [:input-data-y [:vector number?]]
   [:input-iters pos-int?]
   [:input-phenos-count pos-int?]
   [:max-leafs {:optional true} [:maybe pos-int?]]])