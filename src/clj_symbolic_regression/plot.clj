(ns clj-symbolic-regression.plot
  (:require
    [clojure.string :as str])
  (:import
    (java.util
      List)
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
        ^XYChart chart (QuickChart/getChart "Sample" "X" "Y" "y(x)" x-data y-data)]
    (-> (SwingWrapper. chart)
        .displayChart)))


(defn test-plots-2
  []
  (let [x-data           (double-array [0.0 1.0 2.0])
        y-data           (double-array [2.0 1.0 0.0])
        ^XYChart chart   (doto (XYChart. 600 400)
                           (.setTitle "Sample")
                           (.setXAxisTitle "X")
                           (.setYAxisTitle "Y"))
        ^XYSeries series (doto (.addSeries chart "y(x)" x-data y-data)
                           (.setMarker SeriesMarkers/CIRCLE))]
    (-> (SwingWrapper. chart)
        .displayChart)))


(defn make-plot:2-series
  ^XYChart [^String series-1-label
            ^String series-2-label
            ^List x-data-1
            ^List x-data-2
            ^List y-data-1
            ^List y-data-2]
  (let [^XYChart chart     (doto (XYChart. 900 600)
                             (.setTitle "")
                             (.setXAxisTitle "X")
                             (.setYAxisTitle "Y"))
        ^XYSeries series-1 (doto (.addSeries chart (str/join (take 30 series-1-label)) x-data-1 y-data-1)
                             (.setMarker SeriesMarkers/CIRCLE))

        ^XYSeries series-2 (doto (.addSeries chart (str/join (take 30 series-2-label)) x-data-2 y-data-2)
                             (.setMarker SeriesMarkers/CROSS))]
    chart))


(defn make-plot:1-series
  ^XYChart [^String series-1-label
            ^String x-axis-title
            ^String y-axis-title
            ^List x-data-1
            ^List y-data-1]
  (let [^XYChart chart     (doto (XYChart. 900 300)
                             (.setTitle "")
                             (.setXAxisTitle x-axis-title)
                             (.setYAxisTitle y-axis-title))
        ^XYSeries series-1 (doto (.addSeries chart (str/join (take 30 series-1-label)) x-data-1 y-data-1)
                             (.setMarker SeriesMarkers/CIRCLE))]
    chart))


(defn show-plot
  [^String best-fn-label x-data y-data-1 y-data-2]
  (-> (SwingWrapper. (make-plot:2-series best-fn-label "objective(x)" x-data x-data y-data-1 y-data-2))
      .displayChart))


(comment (test-plots-2))
