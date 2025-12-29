(ns closyr.ui.gui
  (:refer-clojure :exclude [rand rand-int rand-nth shuffle])
  (:require
    [clojure.core.async :as async :refer [go go-loop timeout <!! >!! <! >! chan put! alts!]]
    [closyr.dataset.inputs :as input-data]
    [closyr.ui.components :as ui-comp]
    [closyr.ui.plot :as plot]
    [closyr.ui.settings.advanced :as settings-adv]
    [closyr.ui.settings.experiment :as settings-exp]
    [closyr.ui.settings.mutations :as settings-mut]
    [closyr.ui.sketchpad :as sketchpad]
    [closyr.ui.theme :as ui-theme]
    [closyr.util.csv :as input-csv]
    [closyr.util.log :as log]
    [closyr.util.prng :refer [rand rand-int rand-nth shuffle]]
    [seesaw.core :as ss])
  (:import
    (java.awt
      BorderLayout
      Color
      Container
      Dimension
      FlowLayout
      Font
      GridBagConstraints
      GridBagLayout
      GridLayout
      Image
      Point
      Toolkit)
    (java.awt.event
      ActionEvent
      ActionListener
      MouseEvent)
    (java.io
      File
      FileFilter)
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
      JFileChooser
      JFrame
      JLabel
      JPanel
      JTabbedPane
      JTextField
      SwingUtilities
      UIManager)
    (javax.swing.filechooser
      FileNameExtensionFilter)
    (javax.swing.text
      AbstractDocument$DefaultDocumentEvent)
    (org.knowm.xchart
      XChartPanel
      XYChart)
    (org.knowm.xchart.style.markers
      SeriesMarkers)))


(set! *warn-on-reflection* true)


(def ctl:start
  "Button start text"
  "Start")


(def ^:private ctl:stop "Pause")
(def ^:private ctl:restart "Restart")

(def ^:private experiment-is-running?* (atom false))


(def ctl-reset-btn*
  "Button which signals to restart GA"
  (atom nil))


(def ^:private objective-formula-field* (atom nil))

(def ^:private input-y-fn* (atom input-data/initial-fn))


(def ^:private selectable-input-fns
  (input-data/input-y-fns-data sketchpad/sketchpad-size* sketchpad/sketch-input-x-count*))


(def ^:private input-y-fns
  (into {}
        (map
          (fn [[k v]] [k (:fn v)])
          selectable-input-fns)))


(def ^:private input-y-formulas
  (into {}
        (map
          (fn [[k v]] [k (:formula v)])
          selectable-input-fns)))


(def ^:private dataset-fns
  (->>
    selectable-input-fns
    (sort-by #(:idx (second %)))
    (mapv first)))




(defn- start-stop-on-click
  [sim-stop-start-chan ^JLabel status-label ^MouseEvent e]
  (let [{:keys [items-point-getters]} (sketchpad/get-items-points-accessors)
        is-start   (= ctl:start (ss/get-text* e))
        input-data (sketchpad/getters->input-data items-point-getters)
        input-x    (mapv first input-data)
        input-y    (mapv second input-data)]

    (log/info "clicked Start/Pause: " is-start)

    (reset! experiment-is-running?* is-start)
    (.setEnabled ^JButton @ctl-reset-btn* true)
    (put! sim-stop-start-chan (merge @settings-exp/experiment-settings*
                                     {:new-state    (if is-start :start :pause)
                                      :input-data-x input-x
                                      :input-data-y input-y}))
    (ss/set-text* e
                  (if is-start
                    ctl:stop
                    ctl:start))

    (ss/set-text* status-label
                  (if is-start
                    "Running"
                    "Paused"))
    (.setForeground status-label
                    (if is-start
                      (Color. 0 200 0)
                      (Color. 255 180 0)))))


(defn- reset-on-click
  [^JButton start-top-label sim-stop-start-chan ^JLabel status-label ^MouseEvent e]
  (let [{:keys [items-point-getters]} (sketchpad/get-items-points-accessors)
        input-data (sketchpad/getters->input-data items-point-getters)
        input-x    (mapv first input-data)
        input-y    (mapv second input-data)]
    (reset! experiment-is-running?* true)
    (log/info "clicked Reset")
    (put! sim-stop-start-chan (merge @settings-exp/experiment-settings*
                                     {:new-state    :restart
                                      :input-data-x input-x
                                      :input-data-y input-y}))
    (ss/set-text* start-top-label ctl:stop)
    (ss/set-text* status-label "Running")
    (.setForeground status-label (Color. 0 200 0))))


(defn- input-dataset-change
  [^ActionEvent e]
  (let [^JComboBox jcb (.getSource e)
        selection      (-> jcb .getSelectedItem str)
        new-fn         (input-y-fns selection)
        new-formula    (input-y-formulas selection)]
    (reset! sketchpad/xs* nil)
    (reset! input-y-fn* selection)
    (sketchpad/update-points-from-setters! new-fn)
    (when-let [^JTextField formula-field @objective-formula-field*]
      (.setText formula-field (or new-formula "")))
    (sketchpad/repaint-drawing-widget!)
    (log/info "Selected: " selection)))


(defn- parse-seed-value
  "Parse text as a Long seed value. Returns nil for empty/invalid input."
  [^String text]
  (when (and text (not (empty? (.trim text))))
    (try
      (Long/parseLong (.trim text))
      (catch NumberFormatException _
        nil))))


(defn- update-seed-warning-visibility!
  "Update the visibility and text of the warning label based on seed value."
  [^JLabel warning-label seed-value]
  (if seed-value
    (do
      (.setText warning-label "Deterministic mode: slower single-threaded")
      (.setVisible warning-label true))
    (do
      (.setText warning-label "")
      (.setVisible warning-label false))))


(defn- ^JPanel random-seed-panel
  "Create a panel for random seed input with performance warning and clear button."
  []
  (let [^JPanel container          (ui-comp/panel-grid {:rows 1 :cols 1 :border (ui-comp/radio-controls-border "Random Seed")})
        ^JPanel inner-panel        (doto (JPanel.)
                                     (.setLayout (FlowLayout. FlowLayout/LEFT 5 2)))

        ^JLabel seed-label         (JLabel. "Seed:")
        ^JTextField seed-field     (doto (JTextField. 10)
                                     (.setToolTipText "Enter a number for deterministic mode, or leave empty for parallel mode"))

        ^JButton clear-btn         (doto ^JButton (ss/button :text "Clear (Parallel)")
                                     (.setToolTipText "Clear seed to return to parallel (non-deterministic) mode"))

        ^JLabel warning-label      (doto (JLabel. "")
                                     (.setForeground (Color. 255 180 0))
                                     (.setVisible false))

        update-seed!               (fn [seed-value]
                                     (swap! settings-exp/experiment-settings* assoc :random-seed seed-value)
                                     (update-seed-warning-visibility! warning-label seed-value)
                                     (log/info "Random seed changed to:" seed-value
                                               (if seed-value "(deterministic mode)" "(parallel mode)")))]

    ;; Listen for text changes in the seed field
    (ss/listen seed-field
               :document
               (fn [^AbstractDocument$DefaultDocumentEvent e]
                 (let [doc       (.getDocument e)
                       doc-txt   (.getText doc 0 (.getLength doc))
                       new-seed  (parse-seed-value doc-txt)]
                   (update-seed! new-seed))))

    ;; Clear button resets to parallel mode
    (ss/listen clear-btn
               :mouse-clicked
               (fn [^MouseEvent _]
                 (.setText seed-field "")
                 (update-seed! nil)))

    ;; Build the panel
    (.add inner-panel seed-label)
    (.add inner-panel seed-field)
    (.add inner-panel clear-btn)
    (.add inner-panel warning-label)
    (.add container inner-panel)
    container))




(defn- update-replace-drawing-widget
  [draw-container]
  (sketchpad/set-replace-drawing-widget-fn!
    (fn [^JPanel drawing-widget]
      (log/info "REPLACE DRAWING WIDGET!")
      (ss/replace!
        draw-container
        drawing-widget
        (:drawing-widget
          (sketchpad/input-data-items-widget
            (input-y-fns @input-y-fn*)))))))


(defn- set-input-data!
  [input-data-maps]
  (let [{:keys [^JPanel drawing-widget]} (sketchpad/get-items-points-accessors)
        canvas-w (ss/width drawing-widget)
        canvas-h (ss/height drawing-widget)]
    (sketchpad/set-xs-count! (count input-data-maps))
    (sketchpad/redraw-sketch-widget!)
    (let [{:keys [items-point-setters items-point-getters]} (sketchpad/get-items-points-accessors)
          xs            (map :x input-data-maps)
          ys            (map :y input-data-maps)
          max-x         (reduce max xs)
          min-x         (reduce min xs)
          max-y         (reduce max ys)
          min-y         (reduce min ys)

          diff-y        (- max-y min-y)
          diff-x        (- max-x min-x)

          x-scalar      (/ (- canvas-w 200) diff-x)
          y-scalar      (min 10.0 (/ (- canvas-h 100) diff-y))

          scaled-inputs (map (fn [{:keys [x y]}]
                               {:x (* x-scalar x)
                                :y (* y-scalar y)})
                             input-data-maps)]
      (reset! sketchpad/xs* (mapv :x scaled-inputs))
      (doseq [[i {:keys [x y]}] (map-indexed (fn [i d] [i d]) scaled-inputs)]
        (log/info "Set ixy: " i x y
                  " scalars: " x-scalar y-scalar
                  " diff: " diff-x diff-y
                  " canvas: " canvas-w canvas-h)
        ((nth items-point-setters i)
         x
         (input-data/y->gui-coord-y sketchpad/sketchpad-size* y))))))


(defn- ^JPanel input-file-picker-widget
  [parent-widget]
  (let [file-filter                  (FileNameExtensionFilter. "CSV Text File" (into-array ["csv"]))

        input-file-picker            (doto (JFileChooser.)
                                       (.setCurrentDirectory (File. (System/getProperty "user.home")))
                                       (.setFileFilter file-filter))

        input-file-label             (JLabel. "")

        ^JButton select-file-button  (ss/button
                                       :text "Choose Input Data CSV"
                                       :listen
                                       [:mouse-clicked
                                        (fn [^MouseEvent e]
                                          (let [res      (.showOpenDialog input-file-picker parent-widget)
                                                sel-file (.getSelectedFile input-file-picker)]
                                            (when (and (= JFileChooser/APPROVE_OPTION res)
                                                       sel-file)
                                              (log/info "Got file: " (.getAbsolutePath sel-file))

                                              (try
                                                (let [csv-data (input-csv/get-csv-data sel-file)]
                                                  (set-input-data! csv-data)
                                                  (ss/set-text* input-file-label
                                                                (str "Points: " (count csv-data)
                                                                     " From file: " (.getName sel-file))))
                                                (catch Exception e
                                                  (log/error e)
                                                  (ss/set-text* input-file-label
                                                                (str "Error: " (.getMessage e))))))))])

        ^JPanel input-file-container (doto (ui-comp/panel-grid {:rows 2 :cols 1})
                                       (.add select-file-button)
                                       (.add input-file-label))]
    input-file-container))




(defn- setup-ui-frame
  [{:keys [sim-stop-start-chan
           ^List xs-best-fn ^List xs-objective-fn ^List ys-best-fn ^List ys-objective-fn
           ^String series-best-fn-label ^String series-objective-fn-label update-loop
           ^String series-scores-best-label

           ^String series-scores-p99-label
           ^String series-scores-p95-label
           ^String series-scores-p90-label

           ^List xs-scores-best
           ^List ys-scores-best

           ^List xs-scores-p99
           ^List ys-scores-p99
           ^List xs-scores-p95
           ^List ys-scores-p95
           ^List xs-scores-p90
           ^List ys-scores-p90]
    :as   gui-data}]
  (let [my-frame-atom                       (atom nil)
        my-frame                            (doto (JFrame. "CLOSYR")
                                              (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE #_DISPOSE_ON_CLOSE)
                                              (ui-theme/set-app-icon))

        bottom-container                    (ui-comp/panel-grid {:rows 2 :cols 1})
        inputs-and-info-container           (ui-comp/panel-grid {:rows 3 :cols 1})
        ctls-container                      (ui-comp/panel-grid {:rows 2 :cols 1})
        row-3-container                     (ui-comp/panel-grid {:rows 1 :cols 2})
        draw-parent                         (ui-comp/panel-grid {:rows 1 :cols 1})
        top-container                       (ui-comp/panel-grid {:rows 1 :cols 2})
        input-fn-container                  (ui-comp/panel-grid {:rows 1 :cols 1})

        page-pane                           (ui-comp/panel-grid {:rows 2 :cols 1})
        content-pane                        (doto (.getContentPane my-frame)
                                              (.setLayout (GridLayout. 1 1)))

        unicode-font                        (ui-theme/find-unicode-font 14)
        label-width                         130
        sim-info-label                      (let [lbl (JLabel. "")]
                                              (when unicode-font
                                                (.setFont lbl unicode-font))
                                              lbl)
        initial-formula                     (str (or (input-y-formulas input-data/initial-fn) ""))
        ^JLabel objective-label             (let [lbl (doto (JLabel. "ObjectiveFn(x_) :=")
                                                        (.setPreferredSize (Dimension. label-width 20)))]
                                              (when unicode-font
                                                (.setFont lbl unicode-font))
                                              lbl)
        ^JTextField objective-formula-text  (let [tf (doto (JTextField. initial-formula)
                                                       (.setEditable false))]
                                              (when unicode-font
                                                (.setFont tf unicode-font))
                                              (reset! objective-formula-field* tf))
        ^JPanel objective-row               (doto (JPanel. (BorderLayout.))
                                              (.add objective-label BorderLayout/WEST)
                                              (.add objective-formula-text BorderLayout/CENTER))
        ^JLabel best-label                  (let [lbl (doto (JLabel. "BestFitFn(x_) :=")
                                                        (.setPreferredSize (Dimension. label-width 20)))]
                                              (when unicode-font
                                                (.setFont lbl unicode-font))
                                              lbl)
        ^JTextField best-fn-selectable-text (let [tf (doto (JTextField. "")
                                                       (.setEditable false))]
                                              (when unicode-font
                                                (.setFont tf unicode-font))
                                              tf)
        ^JPanel best-row                    (doto (JPanel. (BorderLayout.))
                                              (.add best-label BorderLayout/WEST)
                                              (.add best-fn-selectable-text BorderLayout/CENTER))

        ^XYChart best-fn-chart              (plot/make-plot:n-series
                                              {:x-axis-title "X"
                                               :y-axis-title "Y"
                                               :chart-title  "Start to see functions..."
                                               :series       [{:label  series-objective-fn-label
                                                               :xs     xs-objective-fn
                                                               :ys     ys-objective-fn
                                                               :marker SeriesMarkers/CIRCLE}
                                                              {:label  series-best-fn-label
                                                               :xs     xs-best-fn
                                                               :ys     ys-best-fn
                                                               :marker SeriesMarkers/PLUS}]
                                               :width        400
                                               :height       200})
        best-fn-chart-panel                 (XChartPanel. best-fn-chart)

        ^XYChart scores-chart               (plot/make-plot:n-series
                                              {:x-axis-title "Iteration"
                                               :y-axis-title "Score"
                                               :chart-title  "No data to show"
                                               :series       [{:label  series-scores-p90-label
                                                               :xs     xs-scores-p90
                                                               :ys     ys-scores-p90
                                                               :marker SeriesMarkers/CROSS}

                                                              {:label  series-scores-p95-label
                                                               :xs     xs-scores-p95
                                                               :ys     ys-scores-p95
                                                               :marker SeriesMarkers/CROSS}

                                                              {:label  series-scores-p99-label
                                                               :xs     xs-scores-p99
                                                               :ys     ys-scores-p99
                                                               :marker SeriesMarkers/CROSS}

                                                              {:label  series-scores-best-label
                                                               :xs     xs-scores-best
                                                               :ys     ys-scores-best
                                                               :marker SeriesMarkers/PLUS}]
                                               :width        400
                                               :height       200})
        scores-chart-panel                  (XChartPanel. scores-chart)

        {:keys [^JPanel drawing-widget]} (sketchpad/input-data-items-widget (input-y-fns @input-y-fn*))

        status-label                        (doto (JLabel. "Press Start To Find Function")
                                              (.setFont (Font. "SansSerif" Font/BOLD 18))
                                              (.setForeground (Color. 180 180 180)))
        adv-settings-value-label            (JLabel. "Auto")
        ^JButton gear-btn                   (doto ^JButton (ss/button
                                                             :text "\u2699"
                                                             :listen [:mouse-clicked
                                                                      (fn [_]
                                                                        (when-let [frame @my-frame-atom]
                                                                          (settings-adv/show-advanced-settings-dialog! frame adv-settings-value-label settings-exp/experiment-settings*)))])
                                              (.setToolTipText "Advanced settings")
                                              (.setFont (Font. "SansSerif" Font/PLAIN 16))
                                              (.setPreferredSize (Dimension. 40 30)))
        status-with-gear                    (doto (JPanel. (BorderLayout.))
                                              (.add status-label BorderLayout/CENTER)
                                              (.add gear-btn BorderLayout/EAST))
        status-column                       (doto (ui-comp/panel-grid {:rows 2 :cols 1})
                                              (.add status-with-gear)
                                              (.add (settings-exp/max-leafs-settings-panel)))

        ^JButton ctl-start-stop-btn         (ss/button
                                              :text ctl:start
                                              :listen [:mouse-clicked
                                                       (partial start-stop-on-click
                                                                sim-stop-start-chan
                                                                status-label)])
        ^JButton ctl-reset-btn              (reset! ctl-reset-btn*
                                                    (doto
                                                      ^JButton (ss/button
                                                                 :text ctl:restart
                                                                 :listen [:mouse-clicked
                                                                          (partial reset-on-click
                                                                                   ctl-start-stop-btn
                                                                                   sim-stop-start-chan
                                                                                   status-label)])
                                                      (.setEnabled false)))
        brush-container                     (sketchpad/brush-panel)
        xs-container                        (sketchpad/xs-panel)
        settings-panel                      (settings-exp/experiment-settings-panel)
        ^JComboBox input-fn-picker          (ss/combobox
                                              :model dataset-fns
                                              :listen [:action input-dataset-change])

        icon-test                           (JLabel. ^Icon (UIManager/getIcon "OptionPane.informationIcon"))

        btns-row                            (doto (ui-comp/panel-grid {:rows 1 :cols 2})
                                              (.add ctl-start-stop-btn)
                                              (.add ctl-reset-btn))

        status-row                          (ui-comp/panel-grid {:rows 1 :cols 2})

        ^JPanel input-file-container        (input-file-picker-widget status-row)

        status-row                          (doto status-row
                                              (.add status-column)
                                              (.add input-file-container))

        btns-container                      (doto (ui-comp/panel-grid {:rows 2 :cols 1})
                                              (.add btns-row)
                                              (.add status-row))

        ^JPanel random-seed-panel-widget    (random-seed-panel)

        ^JPanel mutations-panel-widget      (settings-mut/mutations-selection-panel my-frame-atom settings-exp/experiment-settings*)

        ;; Combine random seed and mutations panels on the same row
        seed-and-mutations-row              (doto (ui-comp/panel-grid {:rows 1 :cols 2})
                                              (.add random-seed-panel-widget)
                                              (.add mutations-panel-widget))

        settings-container                  (doto (ui-comp/panel-grid {:rows 3 :cols 1})
                                              (.add seed-and-mutations-row)
                                              (.add settings-panel)
                                              (.add input-fn-container))

        history-container                   (doto (ui-comp/panel-grid {:rows 1 :cols 1})
                                              (.add (JLabel. "Hello")))

        page-pane-tabbed                    (doto (JTabbedPane.)
                                              (.setBounds 50 50 200 200)
                                              (.add "Main" ctls-container)
                                              (.add "Info" history-container))]

    (update-replace-drawing-widget draw-parent)

    (.add brush-container xs-container)
    (.add brush-container input-fn-picker)

    (.add input-fn-container brush-container)

    (.add inputs-and-info-container sim-info-label)
    (.add inputs-and-info-container objective-row)
    (.add inputs-and-info-container best-row)

    (.add draw-parent drawing-widget)
    (.add row-3-container draw-parent)
    (.add row-3-container scores-chart-panel)

    (.add bottom-container row-3-container)
    (.add bottom-container inputs-and-info-container)

    (.add ctls-container btns-container)
    (.add ctls-container settings-container)


    (.add top-container ctls-container)
    (.add top-container best-fn-chart-panel)

    (.add page-pane top-container)
    (.add page-pane bottom-container)
    (.add content-pane page-pane)

    (.pack my-frame)
    (.setVisible my-frame true)
    (.setSize my-frame 1500 800)

    ;; Set the frame atom so dialogs can reference it
    (reset! my-frame-atom my-frame)

    (update-loop
      {:best-fn-chart           best-fn-chart
       :best-fn-chart-panel     best-fn-chart-panel
       :info-label              sim-info-label
       :status-label            status-label
       :best-fn-selectable-text best-fn-selectable-text
       :scores-chart-panel      scores-chart-panel
       :scores-chart            scores-chart
       :ctl-start-stop-btn      ctl-start-stop-btn}
      gui-data)))


(defn create-and-show-gui
  "Create a show Swing GUI"
  [gui-data]
  (SwingUtilities/invokeLater
    (fn []
      (try
        (ui-theme/setup-theme)
        (setup-ui-frame gui-data)
        (catch Exception e
          (log/error "Error in GUI: " e))))))


(defn- test-gui-1
  []
  (let [sim-stop-start-chan (chan)]
    (create-and-show-gui
      {:xs-best-fn                (doto (CopyOnWriteArrayList.) (.add 0.0) (.add 1.0))
       :xs-objective-fn           (doto (CopyOnWriteArrayList.) (.add 0.0) (.add 1.0))
       :ys-best-fn                (doto (CopyOnWriteArrayList.) (.add 2.0) (.add 1.0))
       :ys-objective-fn           (doto (CopyOnWriteArrayList.) (.add 3.0) (.add 1.9))
       :xs-scores-best            (doto (CopyOnWriteArrayList.) (.add -3.0) (.add -1.9))
       :ys-scores-best            (doto (CopyOnWriteArrayList.) (.add 1.0) (.add 2.0))

       :xs-scores-p99             (doto (CopyOnWriteArrayList.) (.add -3.0) (.add -1.9))
       :ys-scores-p99             (doto (CopyOnWriteArrayList.) (.add 2.0) (.add 2.0))
       :xs-scores-p95             (doto (CopyOnWriteArrayList.) (.add -3.0) (.add -1.9))
       :ys-scores-p95             (doto (CopyOnWriteArrayList.) (.add 2.0) (.add 2.0))
       :xs-scores-p90             (doto (CopyOnWriteArrayList.) (.add -3.0) (.add -1.9))
       :ys-scores-p90             (doto (CopyOnWriteArrayList.) (.add 3.0) (.add 2.0))

       :series-scores-best-label  "series scores"
       :series-scores-p99-label   "p99 score"
       :series-scores-p95-label   "p95 score"
       :series-scores-p90-label   "p90 score"

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
             (<! (timeout 2000))
             (let [[n ch] (alts! [sim-stop-start-chan] :default :continue :priority true)]
               (if (= n :continue)
                 :ok
                 (do
                   (log/info "Test GUI: Parking updates to chart due to Stop command")
                   (log/info "Test GUI: Resuming: " (<! sim-stop-start-chan)))))
             (.add xs-best-fn (.size xs-best-fn))
             (.add ys-best-fn (.size xs-best-fn))
             (.add ys-objective-fn (* 10.0 (rand)))

             (.updateXYSeries best-fn-chart series-best-fn-label xs-best-fn ys-best-fn nil)
             (.updateXYSeries best-fn-chart series-objective-fn-label xs-best-fn ys-objective-fn nil)

             (.revalidate best-fn-chart-panel)
             (.repaint best-fn-chart-panel)

             (.setText info-label (str "size: " (.size xs-best-fn)))
             (.revalidate info-label)
             (.repaint info-label)

             (recur))))})))


(defn- test-gui-2
  []
  (ss/invoke-later

    (ui-theme/setup-theme)

    (-> (ss/frame :title "Hello",
                  :width 1600
                  :height 1400
                  :content "Hello, Seesaw",
                  :on-close :exit)
        ss/pack!
        ss/show!)))


(comment (test-gui-2))
(comment (test-gui-1))
