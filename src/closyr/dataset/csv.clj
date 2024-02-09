(ns closyr.dataset.csv
  (:require
    [clojure.data.csv :as csv]
    [clojure.java.io :as io]
    [clojure.tools.logging :as log]))


(set! *warn-on-reflection* true)


(defn csv-data->maps
  [csv-data]
  (let [has-col-names (= "x" (ffirst csv-data))
        data-content  (map (fn [vs] (map #(Double/parseDouble %) vs))
                           (if has-col-names
                             (do
                               (log/warn "Got CSV with column names " (first csv-data))
                               (rest csv-data))
                             (do
                               (log/warn "Got CSV without column names " (first csv-data))
                               csv-data)))
        col-names     (if has-col-names
                        (->> (first csv-data)
                             (map keyword)
                             repeat)
                        (repeat [:x :y]))]
    (when-not (= #{:x :y} (set (first col-names))) (throw (Exception. "Need x/y columns")))
    (log/warn "Data content:" (count data-content) (first col-names) data-content)
    (map zipmap col-names data-content)))


(defn get-csv-data
  [csv-file]
  (with-open [reader (io/reader csv-file)]
    (doall
      (let [csv-data   (csv/read-csv reader)
            input-data (csv-data->maps csv-data)]
        input-data))))
