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
      MouseEvent)
    (java.util
      List)
    (java.util.concurrent
      CopyOnWriteArrayList)
    (javax.swing
      BoxLayout
      JButton
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


(defn input-data-items-widget
  []
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


        x-count             50
        x-scale             15
        pts                 (map
                              (fn [i]
                                [(+ 50.0 (* i x-scale)) (+ 150 (* 50 (Math/sin (/ i 4.0))))])
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

        ^JPanel xyz-p       (ss/xyz-panel
                              :paint draw-grid
                              :id :xyz
                              :background "#222222"
                              :items (conj items bp)
                              :listen [:mouse-clicked
                                       (fn [^MouseEvent e]
                                         (doall
                                           (map-indexed
                                             (fn [i ^JLabel widget]
                                               (let [diff (/ (abs
                                                               (- (.getX (.getLocation widget))
                                                                  (.getX (.getPoint e))))
                                                             500.0)]
                                                 (.setLocation widget
                                                               (+ 50.0 (* i x-scale))
                                                               (+ (* (min 1 (+ 0.65 diff)) (.getY (.getLocation widget)))
                                                                  (* (max 0 (- 0.35 diff)) (.getY (.getPoint e)))))))
                                             items))

                                         (ss/repaint! e))])]
    [xyz-p items-point-getters]))


;; todo: draw on widget: https://stackoverflow.com/questions/10101673/drawing-lines-with-mouse-on-canvas-java-awt

(defn create-and-show-gui
  [{:keys [sim-stop-start-chan
           ^List xs ^List y1s ^List y2s ^String s1l ^String s2l update-loop]
    :as   gui-data}]
  (SwingUtilities/invokeLater
    (fn []
      (let [my-frame          (doto (JFrame. "CLJ Symbolic Regression")
                                (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
                                (.setSize 1600 1400))

            drawing-container (doto (JPanel. (BorderLayout.))
                                (.setSize 1200 100)
                                (.setBackground Color/LIGHT_GRAY)
                                (.setLayout (GridLayout. 2 1)))

            info-container    (doto (JPanel. (BorderLayout.))
                                (.setSize 600 100)
                                (.setBackground Color/LIGHT_GRAY)
                                (.setLayout (GridLayout. 2 1)))

            content-pane      (doto (.getContentPane my-frame)
                                (.setLayout (GridLayout. 2 1)))

            my-label          (JLabel. "Press Start To Begin Function Search")

            chart             (plot/make-plot s1l s2l xs y1s y2s)
            chart-panel       (XChartPanel. chart)
            [^JPanel xyz-p items-point-getters] (input-data-items-widget)

            ^JButton ctl-btn  (ss/button
                                :text "Start"
                                :listen [:mouse-clicked
                                         (fn [^MouseEvent e]
                                           (if-not sim-stop-start-chan
                                             (println "warning: no sim-stop-start-chan provided")
                                             (let [is-start   (= "Start" (ss/get-text* e))
                                                   input-data (mapv (fn [getter]
                                                                      (let [^Point pt (getter)]
                                                                        [(/ (- (.getX pt) 50.0) 15.0)
                                                                         (- 10.0 (/ (.getY pt) 15.0))]))
                                                                    items-point-getters)
                                                   input-x    (mapv first input-data)
                                                   input-y    (mapv second input-data)]
                                               (println "clicked Start/Stop: " is-start)
                                               (put! sim-stop-start-chan {:new-state    is-start
                                                                          :input-data-x input-x
                                                                          :input-data-y input-y})
                                               (ss/set-text* e (if is-start
                                                                 "Stop"
                                                                 "Start")))))])]

        (.add info-container my-label)
        (.add info-container ctl-btn)
        (.add drawing-container info-container)
        (.add drawing-container xyz-p)
        (.add content-pane drawing-container)
        (.add content-pane chart-panel)
        (.pack my-frame)
        (.setVisible my-frame true)

        (update-loop
          {:chart chart :chart-panel chart-panel :info-label my-label}
          gui-data)))))


(defn gui-1
  []
  (let [sim-stop-start-chan (chan)]
    (create-and-show-gui
      {:xs                  (doto (CopyOnWriteArrayList.) (.add 0.0) (.add 1.0))
       :y1s                 (doto (CopyOnWriteArrayList.) (.add 2.0) (.add 1.0))
       :y2s                 (doto (CopyOnWriteArrayList.) (.add 3.0) (.add 1.9))
       :s1l                 "series 1"
       :s2l                 "series 2"
       :sim-stop-start-chan sim-stop-start-chan
       :update-loop         (fn [{:keys [^XYChart chart
                                         ^XChartPanel chart-panel
                                         ^JLabel info-label]
                                  :as   gui-widgets}
                                 {:keys [^List xs ^List y1s ^List y2s ^String s1l ^String s2l update-loop]
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


                                  (println "Draw new points " (.size xs))
                                  (.add xs (.size xs))
                                  (.add y1s (.size xs))
                                  ;; (.remove y2s 0)
                                  (.add y2s (* 10.0 (Math/random)))

                                  (.updateXYSeries chart s1l xs y1s nil)
                                  (.updateXYSeries chart s2l xs y2s nil)

                                  (.revalidate chart-panel)
                                  (.repaint chart-panel)

                                  (.setText info-label (str "size: " (.size xs)))
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
