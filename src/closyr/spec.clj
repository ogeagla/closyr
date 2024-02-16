(ns closyr.spec
  (:require
    [clojure.pprint :as pp]
    [closyr.log :as log]
    [malli.core :as m]))


(def ^:dynamic *check-schema*
  "Toggle checking schema and potentially throwing exceptions"
  true)


(defn check-schema!
  "Check that an object o is compliant with schema s with name n, or throw exception"
  [n s o]
  (when (and *check-schema* (not (m/validate s o)))
    (log/error "Error in input schema: " n)
    (pp/pprint (:errors (m/explain s o)))
    (throw (Exception. (str "Error, input failed schema: " n)))))


