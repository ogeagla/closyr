(ns closyr.ui.gui
  (:refer-clojure :exclude [rand rand-int rand-nth shuffle])
  (:require
    [clojure.core.async :as async :refer [go go-loop timeout <!! >!! <! >! chan put! alts!]]
    [closyr.dataset.inputs :as input-data]
    [closyr.dataset.prng :refer :all]
    [closyr.ui.plot :as plot]
    [seesaw.behave :as sb]
    [seesaw.border :as sbr]
    [seesaw.core :as ss]
    [seesaw.graphics :as sg])
  (:import
    (io.materialtheme.darkstackoverflow
      DarkStackOverflowTheme)
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
      BorderFactory
      BoxLayout
      ComboBoxModel
      Icon
      JButton
      JComboBox
      JFrame
      JLabel
      JPanel
      JRadioButton
      JRadioButtonMenuItem
      JTabbedPane
      JTextField
      SwingUtilities
      UIManager
      UnsupportedLookAndFeelException)
    (javax.swing.border
      Border)
    (javax.swing.text
      AbstractDocument$DefaultDocumentEvent)
    (mdlaf
      MaterialLookAndFeel)
    (mdlaf.themes
      JMarsDarkTheme
      MaterialLiteTheme)
    (org.knowm.xchart
      XChartPanel
      XYChart)))


(set! *warn-on-reflection* true)


(def brush-label:skinny "S")
(def brush-label:broad "M")
(def brush-label:huge "L")
(def brush-label:line "Y")

(def sketch-input-x-count* (atom 50))


(def xs->gap
  {100 6
   50  12
   20  28})


(def sketch-input-x-scale* (atom (xs->gap @sketch-input-x-count*)))


(def items-points-accessors* (atom {}))
(def replace-drawing-widget!* (atom nil))

(def sketchpad-size* (atom {}))

(def color:very-light-gray (Color. 204 204 204))
(def color:light-gray Color/LIGHT_GRAY)
(def color:very-light-pink (Color. 250 210 210))


(defn setup-theme
  []
  ;; (LafManager/install (DarculaTheme.))
  ;; (LafManager/install (SolarizedDarkTheme.))

  (try

    (UIManager/setLookAndFeel
      (MaterialLookAndFeel.
        ;; (MaterialLiteTheme.)
        ;; (JMarsDarkTheme.)
        (DarkStackOverflowTheme.)))

    (catch UnsupportedLookAndFeelException e
      (println "Theme error: " e))))


(defn ^JPanel panel-grid
  [{:keys [rows cols ^Border border]}]
  (let [panel (doto (JPanel. (BorderLayout.))
                ;; (.setSize 1200 100)
                ;; (.setBackground color:light-gray)
                (.setLayout (GridLayout. rows cols)))]
    (cond-> panel
      (not (nil? border)) (.setBorder border))
    panel))


(defn radio-controls-border
  [title]
  ;; (BorderFactory/createLineBorder (Color. 80 80 80) 1)
  (BorderFactory/createTitledBorder (BorderFactory/createLineBorder (Color. 80 80 80) 1) title))


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
                        (sg/draw g (sg/rounded-rect 3
                                                    3
                                                    (- (ss/width c) 6)
                                                    (- (ss/width c) 6)
                                                    ;; (- (ss/height c) 6)
                                                    9)
                                 (sg/style :foreground "salmon"
                                           :background "#666"
                                           :stroke 2)))})
    ;; Set the bounds to its preferred size. Note that this has to be
    ;; done after the label is fully constructed.
    (ss/config! :bounds :preferred)))


(defn sketchpad-on-click:skinny-brush
  [items x-scale ^MouseEvent e]
  (let [{items-point-setters :items-point-setters items-point-getters :items-point-getters} @items-points-accessors*]
    (doall
      (map-indexed
        (fn [i getter]
          (let [^Point pt (getter)
                setter    (nth items-point-setters i)
                pt-x      (.getX pt)
                pt-y      (.getY pt)
                diff      (/ (abs
                               (- pt-x
                                  (.getX (.getPoint e))))
                             500.0)]


            (setter
              pt-x
              (+ (* (min 1 (+ 0.95 diff)) pt-y)
                 (* (max 0 (- 0.05 diff)) (.getY (.getPoint e)))))))
        items-point-getters)))

  #_(ss/repaint! e))


(defn sketchpad-on-click:broad-brush
  [items x-scale ^MouseEvent e]
  (let [{items-point-setters :items-point-setters items-point-getters :items-point-getters} @items-points-accessors*]
    (doall
      (map-indexed
        (fn [i getter]
          (let [^Point pt (getter)
                setter    (nth items-point-setters i)
                pt-x      (.getX pt)
                pt-y      (.getY pt)
                diff      (/ (abs
                               (- pt-x
                                  (.getX (.getPoint e))))
                             500.0)]


            (setter
              pt-x
              (+ (* (min 1 (+ 0.85 diff)) pt-y)
                 (* (max 0 (- 0.15 diff)) (.getY (.getPoint e)))))))
        items-point-getters)))

  #_(ss/repaint! e))


(defn sketchpad-on-click:huge-brush
  [items x-scale ^MouseEvent e]
  (let [{items-point-setters :items-point-setters items-point-getters :items-point-getters} @items-points-accessors*]
    (doall
      (map-indexed
        (fn [i getter]
          (let [^Point pt (getter)
                setter    (nth items-point-setters i)
                pt-x      (.getX pt)
                pt-y      (.getY pt)

                diff      (/ (abs
                               (- pt-x
                                  (.getX (.getPoint e))))
                             500.0)]
            (setter
              pt-x
              (+ (* (min 1 (+ 0.65 diff)) pt-y)
                 (* (max 0 (- 0.35 diff)) (.getY (.getPoint e)))))))
        items-point-getters)))

  #_(ss/repaint! e))


(defn sketchpad-on-click:line-brush
  [items x-scale ^MouseEvent e]
  (let [{items-point-setters :items-point-setters items-point-getters :items-point-getters} @items-points-accessors*]
    (doall
      (map-indexed
        (fn [i getter]
          (let [^Point pt (getter)
                setter    (nth items-point-setters i)
                pt-x      (.getX pt)
                pt-y      (.getY pt)]
            (setter pt-x (.getY (.getPoint e)))))
        items-point-getters)))

  #_(ss/repaint! e))


(def brush-fn* (atom sketchpad-on-click:skinny-brush))


(def brushes-map
  {brush-label:skinny sketchpad-on-click:skinny-brush
   brush-label:broad  sketchpad-on-click:broad-brush
   brush-label:huge   sketchpad-on-click:huge-brush
   brush-label:line   sketchpad-on-click:line-brush})


(def selectable-input-fns
  (input-data/input-y-fns-data sketchpad-size* sketch-input-x-count*))


(def input-y-fns
  (into {}
        (map
          (fn [[k v]] [k (:fn v)])
          selectable-input-fns)))


(def dataset-fns
  (->>
    selectable-input-fns
    (sort-by #(:idx (second %)))
    (mapv first)))


(def input-y-fn* (atom input-data/initial-fn))


(defn draw-grid
  [c ^Graphics2D g]
  (let [w (ss/width c) h (ss/height c)]
    (.setColor g (Color. 98 98 98))
    (doseq [x (range 0 w 10)]
      (.drawLine g x 0 x h))
    (doseq [y (range 0 h 10)]
      (.drawLine g 0 y w y)))
  [c g])


(def new-xs?* (atom true))


(defn reposition-labels
  [[c ^Graphics2D g]]
  (let [{items-point-setters :items-point-setters items-point-getters :items-point-getters} @items-points-accessors*
        w     (ss/width c)
        h     (ss/height c)
        old-w (or (:w @sketchpad-size*) w)
        old-h (or (:h @sketchpad-size*) h)]

    (reset! sketchpad-size* {:h h :w w})

    ;; only on resize:
    (when (or (true? @new-xs?*)
              (not= w old-w)
              (not= h old-h))
      (reset! new-xs?* false)

      (mapv
        (fn [i]
          (let [setter (nth items-point-setters i)
                getter (nth items-point-getters i)]
            (setter
              (+ 50.0 (* i @sketch-input-x-scale* (/ w 675)))
              (+ (.getY ^Point (getter))
                 (if (pos? (- h old-h))
                   (Math/ceil (/ (- h old-h) 2))
                   (Math/floor (/ (- h old-h) 2)))))))
        (range @sketch-input-x-count*)))))


(defn set-widget-location
  [^JLabel widget ^double x ^double y]
  (.setLocation widget x y))


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
                                 (fn [pt] (movable (make-label (constantly pt) (str " ")) {:disable-x? true}))
                                 pts)

        items-point-getters    (map
                                 (fn [^JLabel widget] (fn [] (.getLocation widget)))
                                 items)

        items-point-setters    (map
                                 (fn [^JLabel widget]
                                   (fn [x y]
                                     ;; (println "set widget loc: " x y)
                                     (set-widget-location widget x y)))
                                 items)

        ^JPanel drawing-widget (ss/xyz-panel
                                 :paint (comp reposition-labels draw-grid)
                                 :id :xyz
                                 ;; :background color:very-light-gray #_"#BBBBBB" #_"#888888" #_"#222222"
                                 :items items #_(conj items bp)
                                 :listen [:mouse-clicked #(@brush-fn* items @sketch-input-x-scale* %)])]

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
            [(/ (- (.getX pt) 50.0) (/ (:w @sketchpad-size*) 20.0 #_@sketch-input-x-count*))
             (- 7.5 (/ (.getY pt)
                       (/ (:h @sketchpad-size*) 15.0)))]))
        items-point-getters))


(def experiment-settings*
  (atom {:input-iters        100
         :input-phenos-count 5000}))


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
  (let [iters-settings-container               (panel-grid {:rows 1 :cols 4 :border (radio-controls-border "Iterations")})
        pcount-settings-container              (panel-grid {:rows 1 :cols 5 :border (radio-controls-border "Population Size")})
        ^JPanel settings-container             (panel-grid {:rows 1 :cols 2})

        btn-group-iters                        (ss/button-group)
        ^JRadioButtonMenuItem iter-radio-10    (ss/radio-menu-item
                                                 ;; :background color:very-light-gray
                                                 :text "10"
                                                 :group btn-group-iters
                                                 :listen [:mouse-clicked settings-iters-on-change])
        ^JRadioButtonMenuItem iter-radio-100   (ss/radio-menu-item
                                                 ;; :background color:very-light-gray
                                                 :selected? true
                                                 :text "100"
                                                 :group btn-group-iters
                                                 :listen [:mouse-clicked settings-iters-on-change])
        ^JRadioButtonMenuItem iter-radio-1k    (ss/radio-menu-item
                                                 ;; :background color:very-light-gray
                                                 :text "1000"
                                                 :group btn-group-iters
                                                 :listen [:mouse-clicked settings-iters-on-change])
        ^JRadioButtonMenuItem iter-radio-10k   (ss/radio-menu-item
                                                 ;; :background color:very-light-gray
                                                 :text "10000"
                                                 :group btn-group-iters
                                                 :listen [:mouse-clicked settings-iters-on-change])

        btn-group-pcounts                      (ss/button-group)
        ^JRadioButtonMenuItem pcount-radio-1k  (ss/radio-menu-item
                                                 ;; :background color:very-light-gray
                                                 :text "1000"
                                                 :group btn-group-pcounts
                                                 :listen [:mouse-clicked settings-pheno-count-on-change])
        ^JRadioButtonMenuItem pcount-radio-2k  (ss/radio-menu-item
                                                 ;; :background color:very-light-gray
                                                 :text "2000"
                                                 :group btn-group-pcounts
                                                 :listen [:mouse-clicked settings-pheno-count-on-change])
        ^JRadioButtonMenuItem pcount-radio-5k  (ss/radio-menu-item
                                                 ;; :background color:very-light-gray
                                                 :text "5000"
                                                 :selected? true
                                                 :group btn-group-pcounts
                                                 :listen [:mouse-clicked settings-pheno-count-on-change])
        ^JRadioButtonMenuItem pcount-radio-10k (ss/radio-menu-item
                                                 ;; :background color:very-light-gray
                                                 :text "10000"
                                                 :group btn-group-pcounts
                                                 :listen [:mouse-clicked settings-pheno-count-on-change])
        ^JRadioButtonMenuItem pcount-radio-50k (ss/radio-menu-item
                                                 ;; :background color:very-light-gray
                                                 :text "50000"
                                                 :group btn-group-pcounts
                                                 :listen [:mouse-clicked settings-pheno-count-on-change])]
    ;; (.add pcount-settings-container (JLabel. "Pop Count:"))
    (.add pcount-settings-container pcount-radio-1k)
    (.add pcount-settings-container pcount-radio-2k)
    (.add pcount-settings-container pcount-radio-5k)
    (.add pcount-settings-container pcount-radio-10k)
    ;; (.add pcount-settings-container pcount-radio-50k)

    ;; (.add iters-settings-container (JLabel. "Iterations:"))
    (.add iters-settings-container iter-radio-10)
    (.add iters-settings-container iter-radio-100)
    (.add iters-settings-container iter-radio-1k)
    (.add iters-settings-container iter-radio-10k)
    (.add settings-container iters-settings-container)
    (.add settings-container pcount-settings-container)
    settings-container))


(def ctl:start "Start")
(def ctl:stop "Pause")
(def ctl:restart "Restart")

(def experiment-is-running?* (atom false))
(def ctl-reset-btn* (atom nil))


(defn start-stop-on-click
  [sim-stop-start-chan ^JLabel status-label ^MouseEvent e]
  (let [{:keys [items-point-getters]} @items-points-accessors*]
    (if-not sim-stop-start-chan
      (println "warning: no sim-stop-start-chan provided")
      (let [is-start   (= ctl:start (ss/get-text* e))
            input-data (getters->input-data items-point-getters)
            input-x    (mapv first input-data)
            input-y    (mapv second input-data)]

        (println "clicked Start/Stop: " is-start)

        (reset! experiment-is-running?* is-start)
        (.setEnabled ^JButton @ctl-reset-btn* true)
        (put! sim-stop-start-chan (merge @experiment-settings*
                                         {:new-state    is-start
                                          :input-data-x input-x
                                          :input-data-y input-y}))
        (ss/set-text* e
                      (if is-start
                        ctl:stop
                        ctl:start))

        (ss/set-text* status-label
                      (if is-start
                        "Running"
                        "Paused"))))))


(defn reset-on-click
  [^JButton start-top-label sim-stop-start-chan ^JLabel status-label ^MouseEvent e]
  (let [{:keys [items-point-getters]} @items-points-accessors*]
    (if-not sim-stop-start-chan
      (println "warning: no sim-stop-start-chan provided")
      (let [input-data (getters->input-data items-point-getters)
            input-x    (mapv first input-data)
            input-y    (mapv second input-data)]
        (reset! experiment-is-running?* true)
        (println "clicked Reset")
        (put! sim-stop-start-chan (merge @experiment-settings*
                                         {:new-state    true
                                          :reset        true
                                          :input-data-x input-x
                                          :input-data-y input-y}))
        (ss/set-text* start-top-label ctl:stop)
        (ss/set-text* status-label "Running")))))


(defn input-dataset-change
  [^ActionEvent e]
  (let [{:keys [^JPanel drawing-widget items-point-setters items-point-getters]} @items-points-accessors*
        ^JComboBox jcb (.getSource e)
        selection      (-> jcb .getSelectedItem str)
        new-fn         (input-y-fns selection)]
    (reset! input-y-fn* selection)
    (doseq [i (range @sketch-input-x-count*)]
      ((nth items-point-setters i)
       (.getX ^Point ((nth items-point-getters i)))
       (new-fn i)))
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
  (let [brush-config-container          (panel-grid {:rows 1 :cols 4 :border (radio-controls-border "Brush")})
        ^JPanel brush-container         (panel-grid {:rows 1 :cols 3})

        btn-group-brush                 (ss/button-group)
        ^JRadioButtonMenuItem b-radio-0 (ss/radio-menu-item
                                          ;; :background color:very-light-gray
                                          :selected? true
                                          :text brush-label:skinny
                                          :group btn-group-brush
                                          :listen [:mouse-clicked brush-on-change])
        ^JRadioButtonMenuItem b-radio-1 (ss/radio-menu-item
                                          ;; :background color:very-light-gray
                                          :text brush-label:broad
                                          :group btn-group-brush
                                          :listen [:mouse-clicked brush-on-change])
        ^JRadioButtonMenuItem b-radio-2 (ss/radio-menu-item
                                          ;; :background color:very-light-gray
                                          :text brush-label:huge
                                          :group btn-group-brush
                                          :listen [:mouse-clicked brush-on-change])
        ^JRadioButtonMenuItem b-radio-3 (ss/radio-menu-item
                                          ;; :background color:very-light-gray
                                          :text brush-label:line
                                          :group btn-group-brush
                                          :listen [:mouse-clicked brush-on-change])]
    ;; (.add brush-config-container (JLabel. "Brush: "))
    (.add brush-config-container b-radio-0)
    (.add brush-config-container b-radio-1)
    (.add brush-config-container b-radio-2)
    (.add brush-config-container b-radio-3)
    (.add brush-container brush-config-container)
    brush-container))


(defn ^JPanel xs-panel
  []
  (let [xs-config-container              (panel-grid {:rows 1 :cols 3 :border (radio-controls-border "Points Count")})
        ^JPanel xs-container             (panel-grid {:rows 1 :cols 1})

        btn-group-xs                     (ss/button-group)
        ^JRadioButtonMenuItem xs-radio-1 (ss/radio-menu-item
                                           ;; :background color:very-light-gray
                                           :text "20"
                                           :group btn-group-xs
                                           :listen [:mouse-clicked xs-on-change])
        ^JRadioButtonMenuItem xs-radio-2 (ss/radio-menu-item
                                           ;; :background color:very-light-gray
                                           :selected? true
                                           :text "50"
                                           :group btn-group-xs
                                           :listen [:mouse-clicked xs-on-change])
        ^JRadioButtonMenuItem xs-radio-3 (ss/radio-menu-item
                                           ;; :background color:very-light-gray
                                           :text "100"
                                           :group btn-group-xs
                                           :listen [:mouse-clicked xs-on-change])]
    ;; (.add xs-config-container (JLabel. "Points: "))
    (.add xs-config-container xs-radio-1)
    (.add xs-config-container xs-radio-2)
    (.add xs-config-container xs-radio-3)
    (.add xs-container xs-config-container)
    xs-container))


(defn update-replace-drawing-widget
  [draw-container]
  (reset! replace-drawing-widget!*
          (fn [^JPanel drawing-widget]
            (println "REPLACE DRAWING WIDGET!")
            (reset! new-xs?* true)
            (let [new-widget (:drawing-widget
                               (input-data-items-widget
                                 (input-y-fns @input-y-fn*)))]

              (ss/replace!
                draw-container
                drawing-widget
                new-widget)
              #_(ss/repaint! new-widget)))))


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

      (setup-theme)

      (let [my-frame                        (doto (JFrame. "CLOSYR")
                                              (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE #_DISPOSE_ON_CLOSE))

            bottom-container                (panel-grid {:rows 2 :cols 1})
            inputs-and-info-container       (panel-grid {:rows 3 :cols 1})
            ctls-container                  (panel-grid {:rows 2 :cols 1})
            row-3-container                 (panel-grid {:rows 1 :cols 2})
            draw-parent                     (panel-grid {:rows 1 :cols 1})
            top-container                   (panel-grid {:rows 1 :cols 2})
            input-fn-container              (panel-grid {:rows 1 :cols 1})

            page-pane                       (panel-grid {:rows 2 :cols 1})
            content-pane                    (doto (.getContentPane my-frame)
                                              (.setLayout (GridLayout. 1 1)))

            sim-info-label                  (JLabel. "Press Start To Begin Function Search")
            ^JTextField sim-selectable-text (doto (JTextField. "")
                                              (.setEditable false))

            ^XYChart best-fn-chart          (plot/make-plot:2-series
                                              series-best-fn-label
                                              series-objective-fn-label
                                              xs-best-fn
                                              xs-objective-fn
                                              ys-best-fn
                                              ys-objective-fn)
            best-fn-chart-panel             (XChartPanel. best-fn-chart)

            ^XYChart scores-chart           (plot/make-plot:1-series
                                              series-scores-label
                                              "Iteration"
                                              "Score"
                                              xs-scores
                                              ys-scores)
            scores-chart-panel              (XChartPanel. scores-chart)


            {:keys [^JPanel drawing-widget]} (input-data-items-widget (input-y-fns @input-y-fn*))
            status-label                    (JLabel. "Waiting to start...")

            ^JButton ctl-start-stop-btn     (ss/button
                                              ;; :background color:very-light-gray
                                              :text ctl:start
                                              :listen [:mouse-clicked
                                                       (partial start-stop-on-click
                                                                sim-stop-start-chan
                                                                status-label)])
            ^JButton ctl-reset-btn          (reset! ctl-reset-btn*
                                                    (doto
                                                      ^JButton (ss/button
                                                                 ;; :background color:very-light-gray
                                                                 :text ctl:restart
                                                                 :listen [:mouse-clicked
                                                                          (partial reset-on-click
                                                                                   ctl-start-stop-btn
                                                                                   sim-stop-start-chan
                                                                                   status-label)])
                                                      (.setEnabled false)))
            brush-container                 (brush-panel)
            xs-container                    (xs-panel)
            settings-panel                  (experiment-settings-panel)
            ^JComboBox input-fn-picker      (ss/combobox
                                              ;; :background color:very-light-gray
                                              :model dataset-fns
                                              :listen [:action input-dataset-change])

            icon-test                       (JLabel. ^Icon (UIManager/getIcon "OptionPane.informationIcon"))

            btns-row                        (doto (panel-grid {:rows 1 :cols 2})
                                              (.add ctl-start-stop-btn)
                                              (.add ctl-reset-btn))

            status-row                      (doto (panel-grid {:rows 1 :cols 1})
                                              ;; (.add (JLabel. "Status:"))
                                              (.add status-label))

            btns-container                  (doto (panel-grid {:rows 2 :cols 1})
                                              (.add status-row)
                                              (.add btns-row))

            settings-container              (doto (panel-grid {:rows 2 :cols 1})
                                              (.add settings-panel)
                                              (.add input-fn-container))

            history-container               (doto (panel-grid {:rows 1 :cols 1})
                                              (.add (JLabel. "Hello")))

            page-pane-tabbed                (doto (JTabbedPane.)
                                              (.setBounds 50 50 200 200)
                                              (.add "Main" ctls-container)
                                              (.add "Info" history-container))]

        (update-replace-drawing-widget draw-parent)

        (.add brush-container xs-container)
        (.add brush-container input-fn-picker)

        ;; (.add input-fn-container input-fn-picker)
        (.add input-fn-container brush-container)

        (.add inputs-and-info-container sim-info-label)
        (.add inputs-and-info-container sim-selectable-text)

        (.add draw-parent drawing-widget)
        (.add row-3-container btns-container)
        (.add row-3-container scores-chart-panel)

        (.add bottom-container row-3-container)
        (.add bottom-container inputs-and-info-container)

        (.add ctls-container settings-container)
        (.add ctls-container draw-parent)

        (.add top-container ctls-container)
        (.add top-container best-fn-chart-panel)

        (.add page-pane top-container)
        (.add page-pane bottom-container)
        (.add content-pane page-pane)

        (.pack my-frame)
        (.setVisible my-frame true)
        (.setSize my-frame 1500 800)

        (update-loop
          {:best-fn-chart       best-fn-chart
           :best-fn-chart-panel best-fn-chart-panel
           :info-label          sim-info-label
           :status-label        status-label
           :sim-selectable-text sim-selectable-text
           :scores-chart-panel  scores-chart-panel
           :scores-chart        scores-chart
           :ctl-start-stop-btn  ctl-start-stop-btn}
          gui-data)))))


(defn test-gui-1
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
       :update-loop
       (fn [{:keys [^XYChart best-fn-chart
                    ^XYChart scores-chart
                    ^XChartPanel best-fn-chart-panel
                    ^XChartPanel scores-chart-panel
                    ^JLabel info-label]
             :as   gui-widgets}
            {:keys [^List xs-best-fn ^List ys-best-fn ^List ys-objective-fn
                    ^String series-best-fn-label ^String series-objective-fn-label update-loop]
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
             ;; (println "Draw new points " (.size xs-best-fn))
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


(defn test-gui-2
  []
  (ss/invoke-later

    (setup-theme)

    (-> (ss/frame :title "Hello",
                  :width 1600
                  :height 1400
                  :content "Hello, Seesaw",
                  :on-close :exit)
        ss/pack!
        ss/show!)))


(comment (test-gui-2))
(comment (test-gui-1))
