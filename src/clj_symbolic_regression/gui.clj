(ns clj-symbolic-regression.gui
  (:require
    [clj-symbolic-regression.plot :as plot]
    [clojure.core.async :as async :refer [go go-loop timeout <!! >!! <! >! chan]]
    [seesaw.core :as ss])
  (:import
    (java.util
      List)
    (java.util.concurrent
      CopyOnWriteArrayList)
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
        xs           (doto (CopyOnWriteArrayList.) (.add 0.0) (.add 1.0))
        y1s          (doto (CopyOnWriteArrayList.) (.add 2.0) (.add 1.0))
        y2s          (doto (CopyOnWriteArrayList.) (.add 3.0) (.add 1.9))
        chart        (plot/make-plot "wip1" "wip2" xs y1s y2s)
        chart-panel  (XChartPanel. chart)]



    (.add content-pane my-label)
    (.add content-pane chart-panel)
    (.pack my-frame)
    (.setVisible my-frame true)

    (go-loop []
      (<! (timeout 1000))

      (println "Draw new points " (.size xs))
      (.add xs (.size xs))
      (.add y1s (.size xs))
      ;; (.remove y2s 0)
      (.add y2s (* 10.0 (Math/random)))

      (.updateXYSeries chart "wip1" xs y1s nil)
      (.updateXYSeries chart "wip2" xs y2s nil)

      (.revalidate chart-panel)
      (.repaint chart-panel)

      (recur))))


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
