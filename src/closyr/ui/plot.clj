(ns closyr.ui.plot
  (:require
    [clojure.string :as str])
  (:import
    (java.awt
      Color)
    (java.util
      List)
    (org.knowm.xchart
      QuickChart
      SwingWrapper
      XYChart
      XYSeries)
    (org.knowm.xchart.style
      Styler$ChartTheme)
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
    (.displayChart (SwingWrapper. chart))))


(def color:plot-bg (Color. 60 60 60))


(defn apply-style
  [^XYChart chart]
  (doto (.getStyler chart)
    (.setChartFontColor Color/WHITE)
    (.setChartTitleBoxBorderColor color:plot-bg)
    (.setLegendBorderColor color:plot-bg)
    (.setLegendBackgroundColor color:plot-bg)
    (.setChartTitleBoxBackgroundColor color:plot-bg)
    (.setXAxisTitleColor Color/WHITE)
    (.setYAxisTitleColor Color/WHITE)
    (.setChartBackgroundColor color:plot-bg)
    (.setPlotBackgroundColor color:plot-bg)))


(defn test-plots-2
  []
  (let [x-data           (double-array [0.0 1.0 2.0])
        y-data           (double-array [2.0 1.0 0.0])
        ^XYChart chart   (doto (XYChart. 600 400 Styler$ChartTheme/GGPlot2)
                           (.setTitle "Sample")
                           (.setXAxisTitle "X")
                           (.setYAxisTitle "Y"))
        ^XYSeries series (doto (.addSeries chart "y(x)" x-data y-data)
                           (.setMarker SeriesMarkers/CIRCLE))]
    (apply-style chart)
    (.displayChart (SwingWrapper. chart))))


(defn make-plot:2-series
  ^XYChart [^String series-1-label
            ^String series-2-label
            ^List x-data-1
            ^List x-data-2
            ^List y-data-1
            ^List y-data-2]
  (let [^XYChart chart (doto (XYChart. 400 200 Styler$ChartTheme/GGPlot2)
                         (.setTitle "Start To See Data...")
                         (.setXAxisTitle "X")
                         (.setYAxisTitle "Y"))]
    (apply-style chart)

    (doto (.addSeries chart (str/join (take 30 series-2-label)) x-data-2 y-data-2)
      (.setMarker SeriesMarkers/CROSS))
    (doto (.addSeries chart (str/join (take 30 series-1-label)) x-data-1 y-data-1)
      (.setMarker SeriesMarkers/CIRCLE))
    chart))


(defn make-plot:1-series
  ^XYChart [^String series-1-label
            ^String x-axis-title
            ^String y-axis-title
            ^List x-data-1
            ^List y-data-1]
  (let [^XYChart chart     (doto (XYChart. 400 200 Styler$ChartTheme/GGPlot2)
                             (.setTitle "Start To See Data...")
                             (.setXAxisTitle x-axis-title)
                             (.setYAxisTitle y-axis-title))
        ^XYSeries series-1 (doto (.addSeries chart (str/join (take 30 series-1-label)) x-data-1 y-data-1)
                             (.setMarker SeriesMarkers/CIRCLE))]
    (apply-style chart)

    chart))


(defn show-plot
  [^String best-fn-label x-data y-data-1 y-data-2]
  (-> (make-plot:2-series best-fn-label "objective(x)" x-data x-data y-data-1 y-data-2)
      (SwingWrapper.)
      .displayChart))


(comment (test-plots-2))
