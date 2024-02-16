(ns closyr.spec
  (:require [closyr.log :as log]
            [malli.core :as m]))


(defn check-schema!
  "Check that an object o is compliant with schema s with name n, or throw exception"
  [n s o]
  (when-not (m/validate s o)
    (log/error (str "Error in input schema: " n))
    (clojure.pprint/pprint (:errors (m/explain s o)))
    (throw (Exception. (str "Error, input failed schema: " n)))))


