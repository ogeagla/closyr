(ns closyr.spec
  (:require
    [clojure.pprint :as pp]
    [closyr.log :as log]
    [malli.core :as m]
    [malli.error :as me]
    [malli.instrument :as mi]))


(def ^:dynamic *check-schema*
  "Toggle checking schema and potentially throwing exceptions"
  true)


(defn check-schema!
  "Check that an object o is compliant with schema s with name n, or throw exception"
  [n s o]
  (when (and *check-schema* (not (m/validate s o)))
    (let [explained (me/humanize (m/explain s o))]
      (log/error "Error in input schema: " n)
      (pp/pprint [n explained])
      (throw (Exception. (str "Error, input failed schema: " [n explained]))))))


(defn instrument-all!
  "Instrument all defn schema"
  []
  (when *check-schema*
    (mi/collect!)
    (mi/instrument!)))

