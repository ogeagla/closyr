(ns clj-symbolic-regression.plot
  (:require
    [clojure.string :as str])
  (:import
    (org.knowm.xchart
      QuickChart
      SwingWrapper
      XYChart
      XYSeries)
    (org.knowm.xchart.style.markers
      SeriesMarkers)))


(set! *warn-on-reflection* true)


;; https://github.com/knowm/XChart
;; https://knowm.org/open-source/xchart/xchart-example-code/



(defn test-plots-1
  []
  (let [x-data         (double-array [0.0 1.0 2.0])
        y-data         (double-array [2.0 1.0 0.0])
        ^XYChart chart (QuickChart/getChart "Sample" "X" "Y" "y(x)" x-data y-data)

        _              (-> (SwingWrapper. chart)
                           .displayChart)]))


(defn test-plots-2
  []
  (let [x-data           (double-array [0.0 1.0 2.0])
        y-data           (double-array [2.0 1.0 0.0])
        ^XYChart chart   (doto (XYChart. 600 400)
                           (.setTitle "Sample")
                           (.setXAxisTitle "X")
                           (.setYAxisTitle "Y"))
        ^XYSeries series (doto (.addSeries chart "y(x)" x-data y-data)
                           (.setMarker SeriesMarkers/CIRCLE))


        _                (-> (SwingWrapper. chart)
                             .displayChart)]))


(defn make-plot
  ^XYChart [^String series-1-label
            ^String series-2-label
            x-data y-data-1 y-data-2]
  (let [;; x-data             (double-array x-data)
        ;; y-data-1           (double-array y-data-1)
        ;; y-data-2           (double-array y-data-2)
        ^XYChart chart     (doto (XYChart. 900 600)
                             (.setTitle "Sample")
                             (.setXAxisTitle "X")
                             (.setYAxisTitle "Y"))
        ^XYSeries series-1 (doto (.addSeries chart (str/join (take 30 series-1-label)) x-data y-data-1)
                             (.setMarker SeriesMarkers/CIRCLE))

        ^XYSeries series-2 (doto (.addSeries chart (str/join (take 30 series-2-label)) x-data y-data-2)
                             (.setMarker SeriesMarkers/CROSS))]
    chart))


(defn show-plot
  [^String best-fn-label x-data y-data-1 y-data-2]
  (-> (SwingWrapper. (make-plot best-fn-label "objective(x)" x-data y-data-1 y-data-2))
      .displayChart))


(comment (test-plots-2))
