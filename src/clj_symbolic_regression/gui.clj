(ns clj-symbolic-regression.gui
  (:require
    [clj-symbolic-regression.plot :as plot]
    [seesaw.core :as ss])
  (:import
    (javax.swing
      JFrame
      JLabel
      SwingUtilities)
    (org.knowm.xchart
      XChartPanel)))


(set! *warn-on-reflection* true)


(defn create-and-show-gui
  []

  (let [my-frame     (doto (JFrame. "My Frame")
                       (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
                       (.setSize 1600 1400))
        my-label     (JLabel. "Hello UI")
        content-pane (.getContentPane my-frame)
        chart-panel  (XChartPanel. (plot/make-plot "wip" [1] [1] [1]))]

    (.add content-pane my-label)
    (.add content-pane chart-panel)
    (.pack my-frame)
    (.setVisible my-frame true)))


(defn gui-1
  []
  (SwingUtilities/invokeLater create-and-show-gui))


(defn gui-2
  []
  (ss/invoke-later
    (-> (ss/frame :title "Hello",
                  :width 1600
                  :height 1400
                  :content "Hello, Seesaw",
                  :on-close :exit)
        ;; ss/pack!
        ss/show!)))


(comment (gui-1))
(comment (gui-2))
