(ns clj-symbolic-regression.plot
  (:import (org.knowm.xchart QuickChart SwingWrapper XYChart)))

;; https://github.com/knowm/XChart



(defn test-plots
  []
  (let [
        x-data (double-array  [0.0 1.0 2.0] )
        y-data (double-array  [2.0 1.0 0.0] )
        ^XYChart chart (QuickChart/getChart "Sample" "X" "Y" "y(x)" x-data y-data)

        _ (-> (SwingWrapper. chart)
              .displayChart)
        ])
  )


(comment (test-plots))
