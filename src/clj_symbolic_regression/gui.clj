(ns clj-symbolic-regression.gui
  (:require
    [clj-symbolic-regression.dataset.prime-10000 :as data-primes]
    [clj-symbolic-regression.dataset.prime-counting :as data-prime-counting]
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
      JRadioButton
      JRadioButtonMenuItem
      SwingUtilities)
    (javax.swing.border
      Border)
    (javax.swing.text
      AbstractDocument$DefaultDocumentEvent)
    (org.knowm.xchart
      XChartPanel
      XYChart)))


(set! *warn-on-reflection* true)


(def brush-label:skinny "Skinny Brush")
(def brush-label:broad "Broad Brush")
(def brush-label:line "Line Brush")

(def sketch-input-x-count* (atom 50))
(def sketch-input-x-scale* (atom 15))


(def xs->gap
  {100 8
   50  15
   20  27})


(defn ^JPanel panel-grid
  [{:keys [rows cols]}]
  (doto (JPanel. (BorderLayout.))
    ;; (.setSize 1200 100)
    (.setBackground Color/LIGHT_GRAY)
    (.setLayout (GridLayout. rows cols))))


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


(defn sketchpad-on-click:skinny-brush
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


(defn sketchpad-on-click:broad-brush
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
                        (+ (* (min 1 (+ 0.65 diff)) (.getY (.getLocation widget)))
                           (* (max 0 (- 0.35 diff)) (.getY (.getPoint e)))))))
      items))

  (ss/repaint! e))


(defn sketchpad-on-click:line-brush
  [items x-scale ^MouseEvent e]
  (doall
    (map-indexed
      (fn [i ^JLabel widget]
        (.setLocation widget
                      (+ 50.0 (* i x-scale))
                      (.getY (.getPoint e))))
      items))

  (ss/repaint! e))


(def brush-fn* (atom sketchpad-on-click:skinny-brush))


(def brushes-map
  {brush-label:skinny sketchpad-on-click:skinny-brush
   brush-label:broad  sketchpad-on-click:broad-brush
   brush-label:line   sketchpad-on-click:line-brush})


(def items-points-accessors* (atom {}))
(def replace-drawing-widget!* (atom nil))


(defn input-data-items-widget
  [points-fn]
  (let [^JPanel bp             (doto (ss/border-panel
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


        pts                    (map
                                 (fn [i]
                                   [(+ 50.0 (* i @sketch-input-x-scale*)) (points-fn i)])
                                 (range @sketch-input-x-count*))
        items                  (map
                                 (fn [pt]
                                   (movable
                                     (make-label #(do pt)
                                                 (str "x"))
                                     {:disable-x? true}))
                                 pts)

        items-point-getters    (map
                                 (fn [^JLabel widget]
                                   (fn [] (.getLocation widget)))
                                 items)

        items-point-setters    (map
                                 (fn [^JLabel widget]
                                   (fn [^double y] (.setLocation widget (.getX (.getLocation widget)) y)))
                                 items)

        ^JPanel drawing-widget (ss/xyz-panel
                                 :paint draw-grid
                                 :id :xyz
                                 :background "#222222"
                                 :items items #_(conj items bp)
                                 :listen [:mouse-clicked #(@brush-fn* items @sketch-input-x-scale* %)
                                          #_(partial sketchpad-on-click:skinny-brush items @sketch-input-x-scale*)])
        ;; ^JPanel container      (doto (JPanel. (BorderLayout.))
        ;;                         ;; (.setSize 600 100)
        ;;                         (.setBackground Color/LIGHT_GRAY)
        ;;                         (.setLayout (GridLayout. 1 1)))
        ]
    ;; (.add container drawing-widget)
    (reset! items-points-accessors* {:drawing-widget      drawing-widget
                                     :items-point-getters items-point-getters
                                     :items-point-setters items-point-setters})

    {:drawing-widget      drawing-widget
     :items-point-getters items-point-getters
     :items-point-setters items-point-setters}))


(defn getters->input-data
  [items-point-getters]
  (mapv (fn [getter]
          (let [^Point pt (getter)]
            [(/ (- (.getX pt) 50.0) 50.0)
             (- 6.0 (/ (.getY pt) 25.0))]))
        items-point-getters))


(def experiment-settings*
  (atom {:input-iters        200
         :input-phenos-count 40000}))


(defn settings-iters-on-change
  [^MouseEvent e]
  (let [b (.getText ^JRadioButtonMenuItem (.getSource e))]
    (swap! experiment-settings* assoc :input-iters (Integer/parseInt b))
    (println "iters changed to " b)))


(defn settings-pheno-count-on-change
  [^MouseEvent e]
  (let [b (.getText ^JRadioButtonMenuItem (.getSource e))]
    (swap! experiment-settings* assoc :input-phenos-count (Integer/parseInt b))
    (println "pheno count changed to " b)))


(defn ^JPanel experiment-settings-panel
  []
  (let [iters-settings-container             (panel-grid {:rows 1 :cols 4})
        pcount-settings-container            (panel-grid {:rows 1 :cols 4})
        ^JPanel settings-container           (panel-grid {:rows 2 :cols 1})

        btn-group-iters                      (ss/button-group)
        ^JRadioButtonMenuItem iter-radio-1   (ss/radio-menu-item
                                               :text "10"
                                               :group btn-group-iters
                                               :listen [:mouse-clicked settings-iters-on-change])
        ^JRadioButtonMenuItem iter-radio-2   (ss/radio-menu-item
                                               :selected? true
                                               :text "200"
                                               :group btn-group-iters
                                               :listen [:mouse-clicked settings-iters-on-change])
        ^JRadioButtonMenuItem iter-radio-3   (ss/radio-menu-item
                                               :text "1000"
                                               :group btn-group-iters
                                               :listen [:mouse-clicked settings-iters-on-change])
        ^JRadioButtonMenuItem iter-radio-4   (ss/radio-menu-item
                                               :text "10000"
                                               :group btn-group-iters
                                               :listen [:mouse-clicked settings-iters-on-change])

        btn-group-pcounts                    (ss/button-group)
        ^JRadioButtonMenuItem pcount-radio-4 (ss/radio-menu-item
                                               :text "1000"
                                               :group btn-group-pcounts
                                               :listen [:mouse-clicked settings-pheno-count-on-change])
        ^JRadioButtonMenuItem pcount-radio-1 (ss/radio-menu-item
                                               :text "4000"
                                               :group btn-group-pcounts
                                               :listen [:mouse-clicked settings-pheno-count-on-change])
        ^JRadioButtonMenuItem pcount-radio-2 (ss/radio-menu-item
                                               :selected? true
                                               :text "40000"
                                               :group btn-group-pcounts
                                               :listen [:mouse-clicked settings-pheno-count-on-change])
        ^JRadioButtonMenuItem pcount-radio-3 (ss/radio-menu-item
                                               :text "100000"
                                               :group btn-group-pcounts
                                               :listen [:mouse-clicked settings-pheno-count-on-change])]
    (.add pcount-settings-container (JLabel. "Pop Count:"))
    (.add pcount-settings-container pcount-radio-4)
    (.add pcount-settings-container pcount-radio-1)
    (.add pcount-settings-container pcount-radio-2)
    (.add pcount-settings-container pcount-radio-3)

    (.add iters-settings-container (JLabel. "Iterations:"))
    (.add iters-settings-container iter-radio-1)
    (.add iters-settings-container iter-radio-2)
    (.add iters-settings-container iter-radio-3)
    (.add iters-settings-container iter-radio-4)
    (.add settings-container iters-settings-container)
    (.add settings-container pcount-settings-container)
    settings-container))


(defn start-stop-on-click
  [sim-stop-start-chan ^MouseEvent e]
  (let [{:keys [items-point-getters]} @items-points-accessors*]
    (if-not sim-stop-start-chan
      (println "warning: no sim-stop-start-chan provided")
      (let [is-start   (= "Start" (ss/get-text* e))
            input-data (getters->input-data items-point-getters)
            input-x    (mapv first input-data)
            input-y    (mapv second input-data)]
        (println "clicked Start/Stop: " is-start)
        (put! sim-stop-start-chan (merge @experiment-settings*
                                         {:new-state    is-start


                                          :input-data-x input-x
                                          :input-data-y input-y}))
        (ss/set-text* e (if is-start
                          "Stop"
                          "Start"))))))


(defn reset-on-click
  [^JButton start-top-label sim-stop-start-chan ^MouseEvent e]
  (let [{:keys [items-point-getters]} @items-points-accessors*]
    (if-not sim-stop-start-chan
      (println "warning: no sim-stop-start-chan provided")
      (let [input-data (getters->input-data items-point-getters)
            input-x    (mapv first input-data)
            input-y    (mapv second input-data)]
        (println "clicked Reset")
        (put! sim-stop-start-chan (merge @experiment-settings*
                                         {:new-state    true
                                          :reset        true

                                          :input-data-x input-x
                                          :input-data-y input-y}))
        (ss/set-text* start-top-label "Stop")))))


(defn y->gui-coord-y
  [y]
  (+ 150 (* -1 y)))


(def input-y-fns-data
  {"sin+cos"
   {:idx 0
    :fn  (fn [i]
           (y->gui-coord-y
             (+ (* 50 (Math/sin (/ i 4.0)))
                (* 30 (Math/cos (/ i 3.0))))))}
   "cos"
   {:idx 10
    :fn  (fn [i]
           (y->gui-coord-y
             (* 30 (Math/cos (/ i 3.0)))))}
   "sin"
   {:idx 20
    :fn  (fn [i]
           (y->gui-coord-y
             (* 50 (Math/sin (/ i 4.0)))))}

   "log"
   {:idx 30
    :fn  (fn [i]
           (y->gui-coord-y
             (* 50 (Math/log (+ 0.01 (/ i 4.0))))))}

   "hline"
   {:idx 40
    :fn  (fn [i]
           (y->gui-coord-y 0.0))}

   "prime count"
   {:idx 50
    :fn  (let [xys (data-prime-counting/get-data @sketch-input-x-count*)]
           (fn [i]
             (y->gui-coord-y (second (nth xys i)))))}

   "primes"
   {:idx 50
    :fn  (let [xys (data-primes/get-data @sketch-input-x-count*)]
           (fn [i]
             (y->gui-coord-y (second (nth xys i)))))}})


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


(def input-y-fn* (atom "sin+cos"))


(defn input-dataset-change
  [^ActionEvent e]
  (let [{:keys [^JPanel drawing-widget items-point-setters]} @items-points-accessors*
        ^JComboBox jcb (.getSource e)
        selection      (-> jcb .getSelectedItem str)
        new-fn         (input-y-fns selection)]
    (reset! input-y-fn* selection)
    (mapv
      (fn [i]
        ((nth items-point-setters i)
         (new-fn i)))
      (range @sketch-input-x-count*))
    (ss/repaint! drawing-widget)
    (println "Selected: " selection)))


(defn brush-on-change
  [^MouseEvent e]
  (let [b (.getText ^JRadioButtonMenuItem (.getSource e))]
    (reset! brush-fn* (brushes-map b))
    (println "brush change to " b)))


(defn xs-on-change
  [^MouseEvent e]
  (let [xs-str (.getText ^JRadioButtonMenuItem (.getSource e))
        new-xs (Integer/parseInt xs-str)]
    (reset! sketch-input-x-count* new-xs)
    (reset! sketch-input-x-scale* (xs->gap new-xs))
    (@replace-drawing-widget!* (:drawing-widget @items-points-accessors*))
    (println "brush xs to " xs-str " -> " new-xs)))


(defn ^JPanel brush-panel
  []
  (let [brush-config-container          (panel-grid {:rows 1 :cols 3})
        ^JPanel brush-container         (panel-grid {:rows 2 :cols 1})

        btn-group-brush                 (ss/button-group)
        ^JRadioButtonMenuItem b-radio-1 (ss/radio-menu-item
                                          :selected? true
                                          :text brush-label:skinny
                                          :group btn-group-brush
                                          :listen [:mouse-clicked brush-on-change])
        ^JRadioButtonMenuItem b-radio-2 (ss/radio-menu-item
                                          :text brush-label:broad
                                          :group btn-group-brush
                                          :listen [:mouse-clicked brush-on-change])
        ^JRadioButtonMenuItem b-radio-3 (ss/radio-menu-item
                                          :text brush-label:line
                                          :group btn-group-brush
                                          :listen [:mouse-clicked brush-on-change])]
    (.add brush-config-container b-radio-1)
    (.add brush-config-container b-radio-2)
    (.add brush-config-container b-radio-3)
    (.add brush-container brush-config-container)
    brush-container))


(defn ^JPanel xs-panel
  []
  (let [xs-config-container              (panel-grid {:rows 1 :cols 4})
        ^JPanel xs-container             (panel-grid {:rows 2 :cols 1})

        btn-group-xs                     (ss/button-group)
        ^JRadioButtonMenuItem xs-radio-1 (ss/radio-menu-item
                                           :text "20"
                                           :group btn-group-xs
                                           :listen [:mouse-clicked xs-on-change])
        ^JRadioButtonMenuItem xs-radio-2 (ss/radio-menu-item
                                           :selected? true
                                           :text "50"
                                           :group btn-group-xs
                                           :listen [:mouse-clicked xs-on-change])
        ^JRadioButtonMenuItem xs-radio-3 (ss/radio-menu-item
                                           :text "100"
                                           :group btn-group-xs
                                           :listen [:mouse-clicked xs-on-change])]
    (.add xs-config-container (JLabel. "Points: "))
    (.add xs-config-container xs-radio-1)
    (.add xs-config-container xs-radio-2)
    (.add xs-config-container xs-radio-3)
    (.add xs-container xs-config-container)
    xs-container))


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
                                          #_(.setSize 1600 1400))

            bottom-container            (panel-grid {:rows 2 :cols 1})
            info-container              (panel-grid {:rows 2 :cols 1})
            ctls-container              (panel-grid {:rows 3 :cols 1})
            inputs-container            (panel-grid {:rows 2 :cols 2})
            draw-container              (panel-grid {:rows 1 :cols 2})
            top-container               (panel-grid {:rows 1 :cols 2})
            input-fn-container          (panel-grid {:rows 1 :cols 2})

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
                                                                 "Iteration"
                                                                 "Score"
                                                                 xs-scores
                                                                 ys-scores)
            scores-chart-panel          (XChartPanel. scores-chart)


            {:keys [^JPanel drawing-widget]} (input-data-items-widget (input-y-fns @input-y-fn*))

            ^JButton ctl-start-stop-btn (ss/button
                                          :text "Start"
                                          :listen [:mouse-clicked
                                                   (partial start-stop-on-click
                                                            sim-stop-start-chan)])
            ^JButton ctl-reset-btn      (ss/button
                                          :text "Restart"
                                          :listen [:mouse-clicked
                                                   (partial reset-on-click
                                                            ctl-start-stop-btn
                                                            sim-stop-start-chan)])
            brush-container             (brush-panel)
            xs-container                (xs-panel)
            settings-panel              (experiment-settings-panel)
            ^JComboBox input-fn-picker  (ss/combobox
                                          :model dataset-fns
                                          :listen [:action input-dataset-change])]

        (reset! replace-drawing-widget!* (fn [^JPanel drawing-widget]
                                           (println "REPLACE DRAWING WIDGET!")
                                           (ss/replace!
                                             draw-container
                                             drawing-widget
                                             (:drawing-widget
                                               (input-data-items-widget
                                                 (input-y-fns @input-y-fn*))))))


        (.add input-fn-container input-fn-picker)
        (.add input-fn-container brush-container)
        (.add inputs-container input-fn-container)

        (.add inputs-container settings-panel)
        (.add inputs-container xs-container)
        (.add inputs-container (JLabel. ""))
        (.add info-container inputs-container)
        (.add info-container my-label)

        (.add draw-container drawing-widget)
        (.add draw-container scores-chart-panel)

        (.add bottom-container draw-container)
        (.add bottom-container info-container)

        (.add ctls-container ctl-start-stop-btn)
        (.add ctls-container ctl-reset-btn)

        (.add ctls-container (JLabel. ""))

        (.add top-container ctls-container)
        (.add top-container best-fn-chart-panel)

        (.add content-pane top-container)
        (.add content-pane bottom-container)

        (.pack my-frame)
        (.setVisible my-frame true)

        (update-loop
          {:best-fn-chart       best-fn-chart
           :best-fn-chart-panel best-fn-chart-panel
           :info-label          my-label
           :scores-chart-panel  scores-chart-panel
           :scores-chart        scores-chart
           :ctl-start-stop-btn  ctl-start-stop-btn}
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
