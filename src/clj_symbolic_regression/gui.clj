(ns clj-symbolic-regression.gui
  (:require
    [clj-symbolic-regression.plot :as plot]
    [clojure.core.async :as async :refer [go go-loop timeout <!! >!! <! >! chan]]
    [seesaw.behave :as sb]
    [seesaw.border :as sbr]
    [seesaw.core :as ss]
    [seesaw.graphics :as sg]
    )
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
    (java.util
      List)
    (java.util.concurrent
      CopyOnWriteArrayList)
    (javax.swing
      BoxLayout
      JFrame
      JLabel
      JPanel
      SwingUtilities)
    (javax.swing.border
      Border)
    (org.knowm.xchart
      XChartPanel
      XYChart)))


(set! *warn-on-reflection* true)


(defn movable
  [w]
  (let [^Point start-point (Point.)]
    (sb/when-mouse-dragged
      w
      ;; When the mouse is pressed, move the widget to the front of the z order
      :start (fn [e]
               (ss/move! e :to-front)
               (.setLocation start-point ^Point (.getPoint e)))
      ;; When the mouse is dragged move the widget
      ;; Unfortunately, the delta passed to this function doesn't work correctly
      ;; if the widget is moved during the drag. So, the move is calculated
      ;; manually.
      :drag (fn [e _]
              (let [^Point p (.getPoint e)]
                (ss/move! e :by [(- (.x p) (.x start-point))
                                 (- (.y p) (.y start-point))])))))
  w)


(defn make-label
  [text]
  (doto
    ; Instead of a boring label, make the label rounded with
    ; some custom drawing. Use the before paint hook to draw
    ; under the label's text.
    (ss/label
      :border 5
      :text text
      :location [(rand-int 300) (rand-int 300)]
      :paint {
              :before (fn [c g]
                        (sg/draw g (sg/rounded-rect 3 3 (- (ss/width c) 6) (- (ss/height c) 6) 9)
                                 (sg/style :foreground "#FFFFaa"
                                           :background "#aaFFFF"
                                           :stroke 2)))})
    ; Set the bounds to its preferred size. Note that this has to be
    ; done after the label is fully constructed.
    (ss/config! :bounds :preferred)))


(defn draw-grid [c g]
  (let [w (ss/width c) h (ss/height c)]
    (doseq [x (range 0 w 10)]
      (.drawLine g x 0 x h))
    (doseq [y (range 0 h 10)]
      (.drawLine g 0 y w y))))

(defn create-and-show-gui
  [{:keys [^List xs ^List y1s ^List y2s ^String s1l ^String s2l update-loop]
    :as   conf}]
  (SwingUtilities/invokeLater
    (fn []
      (let [my-frame                        (doto (JFrame. "My Frame")
                                              (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
                                              (.setSize 1600 1400))

            drawing-container               (doto (JPanel. (BorderLayout.))
                                              (.setSize 1200 100)
                                              (.setBackground Color/RED))


            ^Container content-pane         (doto (.getContentPane my-frame)
                                              (.setLayout (GridLayout. 2 1)))

            ^Container drawing-content-pane (doto drawing-container
                                              (.setLayout (GridLayout. 1 2)))

            my-label                        (JLabel. "Hello UI")


            chart                           (plot/make-plot s1l s2l xs y1s y2s)
            chart-panel                     (XChartPanel. chart)
            ^JPanel bp                      (doto (ss/border-panel
                                                    :border (sbr/line-border :top 15 :color "#AAFFFF")
                                                    :north (ss/label "I'm a draggable label with a text box!")
                                                    :center (ss/text :text "Hey type some stuff here"))
                                              (ss/config! :bounds :preferred)
                                              (movable))

            ^JPanel xyz-p                   (ss/xyz-panel
                                              :paint draw-grid
                                              :id :xyz
                                              :background "#222222"
                                              :items (conj
                                                       (map (comp movable make-label) ["Agent Cooper" "Big Ed" "Leland Palmer"])
                                                       bp))
            ;^JPanel drawing-canvas          (ss/canvas
            ;                                  :background Color/YELLOW
            ;                                  :paint (fn [^JPanel c ^Graphics2D g]
            ;                                           (.drawString g "I'm a canvas" 10 10))
            ;                                  :listen [:mouse-clicked
            ;                                           (fn [e]
            ;                                             (println "CLicked " e))])
            ]



        (.add drawing-content-pane my-label)
        (.add drawing-content-pane xyz-p)
        (.add content-pane drawing-container)
        (.add content-pane chart-panel)
        (.pack my-frame)
        (.setVisible my-frame true)

        (update-loop
          {:chart chart :chart-panel chart-panel :info-label my-label}
          conf)))))


(defn gui-1
  []
  (create-and-show-gui
    {:xs          (doto (CopyOnWriteArrayList.) (.add 0.0) (.add 1.0))
     :y1s         (doto (CopyOnWriteArrayList.) (.add 2.0) (.add 1.0))
     :y2s         (doto (CopyOnWriteArrayList.) (.add 3.0) (.add 1.9))
     :s1l         "series 1"
     :s2l         "series 2"
     :update-loop (fn [{:keys [^XYChart chart
                               ^XChartPanel chart-panel
                               ^JLabel info-label]}
                       {:keys [^List xs ^List y1s ^List y2s ^String s1l ^String s2l update-loop]
                        :as   conf}]



                    (go-loop []
                      (<! (timeout 2000))

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

                      (recur)))}))


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
