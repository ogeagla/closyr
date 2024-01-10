(ns clj-symbolic-regression.plot
  (:import
    (org.knowm.xchart
      QuickChart
      SwingWrapper
      XYChart
      XYSeries)
    (org.knowm.xchart.style.markers
      SeriesMarkers)))


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


(comment (test-plots-2))
