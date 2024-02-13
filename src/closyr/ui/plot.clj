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
      Marker
      SeriesMarkers)))


(set! *warn-on-reflection* true)


;; https://github.com/knowm/XChart
;; https://knowm.org/open-source/xchart/xchart-example-code/



(def color:plot-bg (Color. 57 57 57))


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


(defn make-plot:n-series
  ^XYChart [{:keys [x-axis-title y-axis-title chart-title
                    series
                    ^Integer width ^Integer height]
             :or   {width 400 height 200}}]
  (let [^XYChart chart (doto (XYChart. width height Styler$ChartTheme/GGPlot2)
                         (.setTitle chart-title)
                         (.setXAxisTitle x-axis-title)
                         (.setYAxisTitle y-axis-title))]
    (apply-style chart)

    (doseq [{:keys [^String label ^List xs ^List ys ^Marker marker]} series]
      (doto (.addSeries chart label xs ys)
        (.setMarker marker)))

    chart))

