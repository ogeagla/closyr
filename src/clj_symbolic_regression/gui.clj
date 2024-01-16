(ns clj-symbolic-regression.gui
  (:require
    [clj-symbolic-regression.plot :as plot]
    [clojure.core.async :as async :refer [go go-loop timeout <!! >!! <! >! chan put! alts!]]
    [seesaw.behave :as sb]
    [seesaw.border :as sbr]
    [seesaw.core :as ss]
    [seesaw.graphics :as sg])
  (:import
    (java.awt
      BorderLayout
      Color
      Container
      FlowLayout
      Graphics2D
      GridBagConstraints
      GridBagLayout
      GridLayout
      Point)
    (java.awt.event
      ActionEvent
      MouseEvent)
    (java.util
      List)
    (java.util.concurrent
      CopyOnWriteArrayList)
    (javax.swing
      BoxLayout
      ComboBoxModel
      JButton
      JComboBox
      JFrame
      JLabel
      JPanel
      SwingUtilities)
    (javax.swing.border
      Border)
    (javax.swing.text
      AbstractDocument$DefaultDocumentEvent)
    (org.knowm.xchart
      XChartPanel
      XYChart)))


(set! *warn-on-reflection* true)


(defn movable
  ([w] (movable w {:disable-x? false}))
  ([w {disable-x? :disable-x?}]
   (let [^Point start-point (Point.)]
     (sb/when-mouse-dragged
       w
       ;; When the mouse is pressed, move the widget to the front of the z order
       :start (fn [^MouseEvent e]
                (ss/move! e :to-front)
                (.setLocation start-point ^Point (.getPoint e)))
       ;; When the mouse is dragged move the widget
       ;; Unfortunately, the delta passed to this function doesn't work correctly
       ;; if the widget is moved during the drag. So, the move is calculated
       ;; manually.
       :drag (fn [^MouseEvent e _]
               (let [^Point p (.getPoint e)]
                 (ss/move! e :by [(if disable-x? 0 (- (.x p) (.x start-point)))
                                  (- (.y p) (.y start-point))]))))
     w)))


(defn make-label
  [location-fn text]
  (doto
    ;; Instead of a boring label, make the label rounded with
    ;; some custom drawing. Use the before paint hook to draw
    ;; under the label's text.
    (ss/label
      :border 5
      :text text
      :location (location-fn)
      :paint {:before (fn [c g]
                        (sg/draw g (sg/rounded-rect 3 3 (- (ss/width c) 6) (- (ss/height c) 6) 9)
                                 (sg/style :foreground "#FFFFaa"
                                           :background "#aaFFFF"
                                           :stroke 2)))})
    ;; Set the bounds to its preferred size. Note that this has to be
    ;; done after the label is fully constructed.
    (ss/config! :bounds :preferred)))


(defn draw-grid
  [c ^Graphics2D g]
  (let [w (ss/width c) h (ss/height c)]
    (doseq [x (range 0 w 10)]
      (.drawLine g x 0 x h))
    (doseq [y (range 0 h 10)]
      (.drawLine g 0 y w y))))


(defn sketchpad-on-click
  [items x-scale ^MouseEvent e]
  (doall
    (map-indexed
      (fn [i ^JLabel widget]
        (let [diff (/ (abs
                        (- (.getX (.getLocation widget))
                           (.getX (.getPoint e))))
                      500.0)]
          (.setLocation widget
                        (+ 50.0 (* i x-scale))
                        (+ (* (min 1 (+ 0.85 diff)) (.getY (.getLocation widget)))
                           (* (max 0 (- 0.15 diff)) (.getY (.getPoint e)))))))
      items))

  (ss/repaint! e))


(def x-count 50)
(def x-scale 15)


(defn input-data-items-widget
  [points-fn]
  (let [^JPanel bp          (doto (ss/border-panel
                                    :border (sbr/line-border :top 15 :color "#AAFFFF")
                                    :north (ss/label "I'm a draggable label with a text box!")
                                    :center (ss/text
                                              :text "Hey type some stuff here"
                                              :listen [:document
                                                       (fn [^AbstractDocument$DefaultDocumentEvent e]
                                                         (let [doc     (.getDocument e)
                                                               doc-txt (.getText doc 0 (.getLength doc))]
                                                           (println "New text: " doc-txt)))]))
                              (ss/config! :bounds :preferred)
                              (movable))


        pts                 (map
                              (fn [i]
                                [(+ 50.0 (* i x-scale)) (points-fn i)])
                              (range x-count))
        items               (map
                              (fn [pt]
                                (movable
                                  (make-label #(do pt)
                                              (str "x"))
                                  {:disable-x? true}))
                              pts)

        items-point-getters (map
                              (fn [^JLabel widget]
                                (fn [] (.getLocation widget)))
                              items)

        items-point-setters (map
                              (fn [^JLabel widget]
                                (fn [^double y] (.setLocation widget (.getX (.getLocation widget)) y)))
                              items)

        ^JPanel xyz-p       (ss/xyz-panel
                              :paint draw-grid
                              :id :xyz
                              :background "#222222"
                              :items items #_(conj items bp)
                              :listen [:mouse-clicked (partial sketchpad-on-click items x-scale)])]
    [xyz-p items-point-getters items-point-setters]))


(defn getters->input-data
  [items-point-getters]
  (mapv (fn [getter]
          (let [^Point pt (getter)]
            [(/ (- (.getX pt) 50.0) 15.0)
             (- 10.0 (/ (.getY pt) 15.0))]))
        items-point-getters))


(defn start-stop-on-click
  [sim-stop-start-chan items-point-getters ^MouseEvent e]
  (if-not sim-stop-start-chan
    (println "warning: no sim-stop-start-chan provided")
    (let [is-start   (= "Start" (ss/get-text* e))
          input-data (getters->input-data items-point-getters)
          input-x    (mapv first input-data)
          input-y    (mapv second input-data)]
      (println "clicked Start/Stop: " is-start)
      (put! sim-stop-start-chan {:new-state    is-start
                                 :input-data-x input-x
                                 :input-data-y input-y})
      (ss/set-text* e (if is-start
                        "Stop"
                        "Start")))))


(defn reset-on-click
  [^JButton start-top-label sim-stop-start-chan items-point-getters ^MouseEvent e]
  (if-not sim-stop-start-chan
    (println "warning: no sim-stop-start-chan provided")
    (let [input-data (getters->input-data items-point-getters)
          input-x    (mapv first input-data)
          input-y    (mapv second input-data)]
      (println "clicked Reset: ")
      (put! sim-stop-start-chan {:new-state    true
                                 :reset        true
                                 :input-data-x input-x
                                 :input-data-y input-y})
      (ss/set-text* start-top-label "Stop"))))


(def input-y-fns-data
  {"sin+cos"
   {:idx 0
    :fn  (fn [i]
           (+ 150
              (* -50 (Math/sin (/ i 4.0)))
              (* -30 (Math/cos (/ i 3.0)))))}
   "cos"
   {:idx 1
    :fn  (fn [i]
           (+ 150
              (* -30 (Math/cos (/ i 3.0)))))}
   "sin"
   {:idx 2
    :fn  (fn [i]
           (+ 150
              (* -50 (Math/sin (/ i 4.0)))))}

   "log"
   {:idx 3
    :fn  (fn [i]
           (+ 150
              (* -50 (Math/log (+ 0.01 (/ i 4.0))))))}})


(def input-y-fns
  (into {}
        (map
          (fn [[k v]] [k (:fn v)])
          input-y-fns-data)))


(def dataset-fns
  (->>
    input-y-fns-data
    (sort-by #(:idx (second %)))
    (mapv #(first %))))


(defn input-dataset-change
  [^JPanel drawing-widget items-point-setters ^ActionEvent e]
  (let [^JComboBox jcb (.getSource e)
        selection      (-> jcb .getSelectedItem str)
        new-fn         (input-y-fns selection)]
    (mapv
      (fn [i]
        ((nth items-point-setters i)
         (new-fn i)))
      (range x-count))
    (ss/repaint! drawing-widget)
    (println "Selected: " selection)))


(defn create-and-show-gui
  [{:keys [sim-stop-start-chan
           ^List xs-best-fn ^List xs-objective-fn ^List ys-best-fn ^List ys-objective-fn
           ^String series-best-fn-label ^String series-objective-fn-label update-loop
           ^String series-scores-label
           ^List xs-scores
           ^List ys-scores]
    :as   gui-data}]
  (SwingUtilities/invokeLater
    (fn []
      (let [my-frame                    (doto (JFrame. "CLJ Symbolic Regression")
                                          (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
                                          (.setSize 1600 1400))

            bottom-container            (doto (JPanel. (BorderLayout.))
                                          ;; (.setSize 1200 100)
                                          (.setBackground Color/LIGHT_GRAY)
                                          (.setLayout (GridLayout. 2 1)))

            info-container              (doto (JPanel. (BorderLayout.))
                                          ;; (.setSize 600 100)
                                          (.setBackground Color/LIGHT_GRAY)
                                          (.setLayout (GridLayout. 2 1)))

            ctls-container              (doto (JPanel. (BorderLayout.))
                                          ;; (.setSize 600 100)
                                          (.setBackground Color/LIGHT_GRAY)
                                          (.setLayout (GridLayout. 3 1)))

            draw-container              (doto (JPanel. (BorderLayout.))
                                          ;; (.setSize 600 100)
                                          (.setBackground Color/LIGHT_GRAY)
                                          (.setLayout (GridLayout. 1 2)))

            top-container               (doto (JPanel. (BorderLayout.))
                                          ;; (.setSize 600 100)
                                          (.setBackground Color/LIGHT_GRAY)
                                          (.setLayout (GridLayout. 1 2)))

            content-pane                (doto (.getContentPane my-frame)
                                          (.setLayout (GridLayout. 2 1)))

            my-label                    (JLabel. "Press Start To Begin Function Search")

            ^XYChart best-fn-chart      (plot/make-plot:2-series series-best-fn-label
                                                                 series-objective-fn-label
                                                                 xs-best-fn
                                                                 xs-objective-fn
                                                                 ys-best-fn
                                                                 ys-objective-fn)
            best-fn-chart-panel         (XChartPanel. best-fn-chart)

            ^XYChart scores-chart       (plot/make-plot:1-series series-scores-label
                                                                 xs-scores
                                                                 ys-scores)
            scores-chart-panel          (XChartPanel. scores-chart)


            [^JPanel drawing-widget items-point-getters items-point-setters] (input-data-items-widget
                                                                               (input-y-fns "sin+cos"))

            ^JButton ctl-start-stop-btn (ss/button
                                          :text "Start"
                                          :listen [:mouse-clicked
                                                   (partial start-stop-on-click
                                                            sim-stop-start-chan
                                                            items-point-getters)])
            ^JButton ctl-reset-btn      (ss/button
                                          :text "Restart"
                                          :listen [:mouse-clicked
                                                   (partial reset-on-click
                                                            ctl-start-stop-btn
                                                            sim-stop-start-chan
                                                            items-point-getters)])]


        (.add info-container ^JComboBox (ss/combobox
                                          :model dataset-fns
                                          :listen [:action
                                                   (partial input-dataset-change
                                                            drawing-widget
                                                            items-point-setters)]))
        (.add info-container my-label)

        (.add draw-container drawing-widget)
        (.add draw-container scores-chart-panel)

        (.add bottom-container draw-container)
        (.add bottom-container info-container)

        (.add ctls-container ctl-start-stop-btn)
        (.add ctls-container ctl-reset-btn)

        (.add top-container ctls-container)
        (.add top-container best-fn-chart-panel)

        (.add content-pane top-container)
        (.add content-pane bottom-container)


        (.pack my-frame)
        (.setVisible my-frame true)

        (update-loop
          {:best-fn-chart      best-fn-chart :best-fn-chart-panel best-fn-chart-panel :info-label my-label
           :scores-chart-panel scores-chart-panel :scores-chart scores-chart}
          gui-data)))))


(defn gui-1
  []
  (let [sim-stop-start-chan (chan)]
    (create-and-show-gui
      {:xs-best-fn                (doto (CopyOnWriteArrayList.) (.add 0.0) (.add 1.0))
       :xs-objective-fn           (doto (CopyOnWriteArrayList.) (.add 0.0) (.add 1.0))
       :ys-best-fn                (doto (CopyOnWriteArrayList.) (.add 2.0) (.add 1.0))
       :ys-objective-fn           (doto (CopyOnWriteArrayList.) (.add 3.0) (.add 1.9))
       :xs-scores                 (doto (CopyOnWriteArrayList.) (.add -3.0) (.add -1.9))
       :ys-scores                 (doto (CopyOnWriteArrayList.) (.add 1.0) (.add 2.0))
       :series-scores-label       "series scores"
       :series-best-fn-label      "series 1"
       :series-objective-fn-label "series 2"
       :sim-stop-start-chan       sim-stop-start-chan
       :update-loop               (fn [{:keys [^XYChart best-fn-chart
                                               ^XYChart scores-chart
                                               ^XChartPanel best-fn-chart-panel
                                               ^XChartPanel scores-chart-panel
                                               ^JLabel info-label]
                                        :as   gui-widgets}
                                       {:keys [^List xs-best-fn ^List ys-best-fn ^List ys-objective-fn ^String series-best-fn-label ^String series-objective-fn-label update-loop]
                                        :as   gui-data}]

                                    (go
                                      (<! sim-stop-start-chan)

                                      (go-loop []

                                        (<! (timeout 4000))

                                        (let [[n ch] (alts! [sim-stop-start-chan] :default :continue :priority true)]
                                          (if (= n :continue)
                                            :ok
                                            (do
                                              (println "Parking updates to chart due to Stop command")
                                              (<! sim-stop-start-chan))))


                                        (println "Draw new points " (.size xs-best-fn))
                                        (.add xs-best-fn (.size xs-best-fn))
                                        (.add ys-best-fn (.size xs-best-fn))
                                        ;; (.remove ys-objective-fn 0)
                                        (.add ys-objective-fn (* 10.0 (Math/random)))

                                        (.updateXYSeries best-fn-chart series-best-fn-label xs-best-fn ys-best-fn nil)
                                        (.updateXYSeries best-fn-chart series-objective-fn-label xs-best-fn ys-objective-fn nil)

                                        (.revalidate best-fn-chart-panel)
                                        (.repaint best-fn-chart-panel)

                                        (.setText info-label (str "size: " (.size xs-best-fn)))
                                        (.revalidate info-label)
                                        (.repaint info-label)

                                        (recur))))})))


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


(comment (gui-2))
(comment (gui-1))
