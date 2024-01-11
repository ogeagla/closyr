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
      XChartPanel XYChart)))


(set! *warn-on-reflection* true)


(defn create-and-show-gui
  [
   {:keys [^List xs ^List y1s ^List y2s ^String s1l ^String s2l update-loop]
    :as   conf}
   ]
  (SwingUtilities/invokeLater
    (fn []
      (let [my-frame     (doto (JFrame. "My Frame")
                           (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
                           (.setSize 1600 1400))
            my-label     (JLabel. "Hello UI")
            content-pane (.getContentPane my-frame)
            ;xs           (doto (CopyOnWriteArrayList.) (.add 0.0) (.add 1.0))
            ;y1s          (doto (CopyOnWriteArrayList.) (.add 2.0) (.add 1.0))
            ;y2s          (doto (CopyOnWriteArrayList.) (.add 3.0) (.add 1.9))
            ;s1l     "wip1"
            ;s2l     "wip2"
            chart        (plot/make-plot s1l s2l xs y1s y2s)
            chart-panel  (XChartPanel. chart)]



        (.add content-pane my-label)
        (.add content-pane chart-panel)
        (.pack my-frame)
        (.setVisible my-frame true)

        (update-loop chart chart-panel conf)))))


(defn gui-1
  []
  (create-and-show-gui
    {
     :xs          (doto (CopyOnWriteArrayList.) (.add 0.0) (.add 1.0))
     :y1s         (doto (CopyOnWriteArrayList.) (.add 2.0) (.add 1.0))
     :y2s         (doto (CopyOnWriteArrayList.) (.add 3.0) (.add 1.9))
     :s1l         "series 1"
     :s2l         "series 2"
     :update-loop (fn [^XYChart chart
                       ^XChartPanel chart-panel
                       {:keys [^List xs ^List y1s ^List y2s ^String s1l ^String s2l update-loop]
                        :as   conf}]
                    (go-loop []
                      (<! (timeout 1000))

                      (println "Draw new points " (.size xs))
                      (.add xs (.size xs))
                      (.add y1s (.size xs))
                      ;; (.remove y2s 0)
                      (.add y2s (* 10.0 (Math/random)))

                      (.updateXYSeries chart s1l xs y1s nil)
                      (.updateXYSeries chart s2l xs y2s nil)

                      (.revalidate chart-panel)
                      (.repaint chart-panel)

                      (recur)))
     }
    ))


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
