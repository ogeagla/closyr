(ns closyr.ui.sketchpad
  "Sketchpad drawing widget and brush controls"
  (:require
    [closyr.ui.components :as ui-comp]
    [closyr.util.log :as log]
    [seesaw.core :as ss])
  (:import
    (java.awt
      Color
      Cursor
      Graphics2D
      Point)
    (java.awt.event
      MouseEvent)
    (javax.swing
      JLabel
      JPanel
      JRadioButtonMenuItem)))


(set! *warn-on-reflection* true)


;; =============================================================================
;; Brush labels
;; =============================================================================

(def ^:private brush-label:skinny ".")
(def ^:private brush-label:broad "o")
(def ^:private brush-label:huge "O")
(def ^:private brush-label:line "Y")


;; =============================================================================
;; State atoms (some exported for use by other modules)
;; =============================================================================

(def sketch-input-x-count*
  "Number of input points in the sketchpad"
  (atom 50))


(def ^:private xs->gap
  {200 3
   100 6
   50  12
   25  24
   20  28
   10  56})


(def sketch-input-x-scale*
  "Scale factor for x coordinates"
  (atom (xs->gap @sketch-input-x-count*)))


(def sketchpad-size*
  "Current size of the sketchpad {:w width :h height}"
  (atom {}))


(def ^:private items-points-accessors* (atom {}))
(def ^:private replace-drawing-widget!* (atom nil))
(def ^:private new-xs?* (atom true))
(def ^:private on-data-change-callback* (atom nil))

(def xs*
  "Current x coordinates when loaded from file"
  (atom nil))


(defn set-on-data-change-callback!
  "Set a callback function to be called when sketchpad data changes.
   The callback receives the current Y values as a vector."
  [callback-fn]
  (reset! on-data-change-callback* callback-fn))


(defn- notify-data-change!
  "Call the data change callback if set"
  []
  (when-let [callback @on-data-change-callback*]
    (let [{:keys [items-point-getters]} @items-points-accessors*]
      (when items-point-getters
        (let [y-values (mapv (fn [getter]
                               (let [^Point pt (getter)]
                                 (- 7.5 (/ (.getY pt)
                                           (/ (:h @sketchpad-size*) 15.0)))))
                             items-point-getters)]
          (callback y-values))))))


;; =============================================================================
;; Brush functions
;; =============================================================================

(defn- sketchpad-on-click:skinny-brush
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
        items-point-getters))))


(defn- sketchpad-on-click:broad-brush
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
        items-point-getters))))


(defn- sketchpad-on-click:huge-brush
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
        items-point-getters))))


(defn- sketchpad-on-click:line-brush
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
        items-point-getters))))


(def ^:private brush-fn* (atom sketchpad-on-click:broad-brush))


(def ^:private brushes-map
  {brush-label:skinny sketchpad-on-click:skinny-brush
   brush-label:broad  sketchpad-on-click:broad-brush
   brush-label:huge   sketchpad-on-click:huge-brush
   brush-label:line   sketchpad-on-click:line-brush})


;; =============================================================================
;; Drawing functions
;; =============================================================================

(defn- draw-grid
  [c ^Graphics2D g]
  (let [w (ss/width c) h (ss/height c)]
    (.setColor g (Color. 98 98 98))
    (doseq [x (range 0 w 10)]
      (.drawLine g x 0 x h))
    (doseq [y (range 0 h 10)]
      (.drawLine g 0 y w y)))
  [c g])


(defn- reposition-labels
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
      (if-let [xs @xs*]
        (mapv
          (fn [i x]
            (let [setter (nth items-point-setters i)
                  getter (nth items-point-getters i)]
              (setter
                (+ 50.0 (* x (/ w 675)))
                (+ (.getY ^Point (getter))
                   (if (pos? (- h old-h))
                     (Math/ceil (/ (- h old-h) 2))
                     (Math/floor (/ (- h old-h) 2)))))))
          (range @sketch-input-x-count*)
          xs)

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
          (range @sketch-input-x-count*))))))


(defn- set-widget-location
  [^JLabel widget ^double x ^double y]
  (.setLocation widget x y))


;; =============================================================================
;; Widget creation
;; =============================================================================

(defn input-data-items-widget
  "Create the sketchpad drawing widget with draggable points"
  [points-fn]
  (log/info "Create input-data-items-widget")
  (let [pts                    (map
                                 (fn [i]
                                   [(+ 50.0 (* i @sketch-input-x-scale*)) (points-fn i)])
                                 (range @sketch-input-x-count*))

        items                  (map
                                 (fn [pt] (ui-comp/movable (ui-comp/make-label (constantly pt) (str " ")) {:disable-x? true}))
                                 pts)

        items-point-getters    (map
                                 (fn [^JLabel widget] (fn [] (.getLocation widget)))
                                 items)

        items-point-setters    (map
                                 (fn [^JLabel widget]
                                   (fn [x y]
                                     (set-widget-location widget x y)))
                                 items)

        ^JPanel drawing-widget (ss/xyz-panel
                                 :paint (comp reposition-labels draw-grid)
                                 :id :xyz
                                 :items items
                                 :listen [:mouse-clicked (fn [e]
                                                           (@brush-fn* items @sketch-input-x-scale* e)
                                                           (notify-data-change!))])]

    (.setCursor drawing-widget (Cursor/getPredefinedCursor Cursor/HAND_CURSOR))
    (log/info "Set hand cursor for sketchpad widget: " (.getCursor drawing-widget))

    (reset! items-points-accessors* {:drawing-widget      drawing-widget
                                     :items-point-getters items-point-getters
                                     :items-point-setters items-point-setters})

    {:drawing-widget      drawing-widget
     :items-point-getters items-point-getters
     :items-point-setters items-point-setters}))


(defn get-items-points-accessors
  "Get the current items-points-accessors atom value"
  []
  @items-points-accessors*)


(defn getters->input-data
  "Convert point getters to input data coordinates"
  [items-point-getters]
  (mapv (fn [getter]
          (let [^Point pt (getter)]
            [(/ (- (.getX pt) 50.0) (/ (:w @sketchpad-size*) 20.0))
             (- 7.5 (/ (.getY pt)
                       (/ (:h @sketchpad-size*) 15.0)))]))
        items-point-getters))


(defn set-replace-drawing-widget-fn!
  "Set the function used to replace the drawing widget"
  [f]
  (reset! replace-drawing-widget!* f))


(defn init-drag-callback!
  "Initialize the drag finish callback to notify data changes"
  []
  (ui-comp/set-on-drag-finish-callback! notify-data-change!))


(defn redraw-sketch-widget!
  "Redraw the sketchpad widget"
  []
  (@replace-drawing-widget!* (:drawing-widget @items-points-accessors*)))


;; =============================================================================
;; Control panels
;; =============================================================================

(defn- brush-on-change
  [^MouseEvent e]
  (let [b (.getText ^JRadioButtonMenuItem (.getSource e))]
    (reset! brush-fn* (brushes-map b))
    (log/info "brush change to " b)))


(defn- xs-on-change
  [^MouseEvent e]
  (let [xs-str (.getText ^JRadioButtonMenuItem (.getSource e))
        new-xs (Integer/parseInt xs-str)]
    (reset! xs* nil)
    (reset! sketch-input-x-count* new-xs)
    (reset! sketch-input-x-scale* (xs->gap new-xs))
    (redraw-sketch-widget!)
    (log/info "brush xs to " xs-str " -> " new-xs)))


(defn ^JPanel brush-panel
  "Create the brush selection panel"
  []
  (let [brush-config-container          (ui-comp/panel-grid {:rows 1 :cols 4 :border (ui-comp/radio-controls-border "Brush")})
        ^JPanel brush-container         (ui-comp/panel-grid {:rows 1 :cols 3})

        btn-group-brush                 (ss/button-group)
        ^JRadioButtonMenuItem b-radio-0 (ss/radio-menu-item
                                          :text brush-label:skinny
                                          :group btn-group-brush
                                          :listen [:mouse-clicked brush-on-change])
        ^JRadioButtonMenuItem b-radio-1 (ss/radio-menu-item
                                          :selected? true
                                          :text brush-label:broad
                                          :group btn-group-brush
                                          :listen [:mouse-clicked brush-on-change])
        ^JRadioButtonMenuItem b-radio-2 (ss/radio-menu-item
                                          :text brush-label:huge
                                          :group btn-group-brush
                                          :listen [:mouse-clicked brush-on-change])
        ^JRadioButtonMenuItem b-radio-3 (ss/radio-menu-item
                                          :text brush-label:line
                                          :group btn-group-brush
                                          :listen [:mouse-clicked brush-on-change])]
    (.add brush-config-container b-radio-0)
    (.add brush-config-container b-radio-1)
    (.add brush-config-container b-radio-2)
    (.add brush-config-container b-radio-3)
    (.add brush-container brush-config-container)
    brush-container))


(defn ^JPanel xs-panel
  "Create the points count selection panel"
  []
  (let [xs-config-container                (ui-comp/panel-grid {:rows 1 :cols 5 :border (ui-comp/radio-controls-border "Points Count")})
        ^JPanel xs-container               (ui-comp/panel-grid {:rows 1 :cols 1})

        btn-group-xs                       (ss/button-group)
        ^JRadioButtonMenuItem xs-radio-10  (ss/radio-menu-item
                                             :text "10"
                                             :group btn-group-xs
                                             :listen [:mouse-clicked xs-on-change])
        ^JRadioButtonMenuItem xs-radio-25  (ss/radio-menu-item
                                             :text "25"
                                             :group btn-group-xs
                                             :listen [:mouse-clicked xs-on-change])
        ^JRadioButtonMenuItem xs-radio-50  (ss/radio-menu-item
                                             :selected? true
                                             :text "50"
                                             :group btn-group-xs
                                             :listen [:mouse-clicked xs-on-change])
        ^JRadioButtonMenuItem xs-radio-100 (ss/radio-menu-item
                                             :text "100"
                                             :group btn-group-xs
                                             :listen [:mouse-clicked xs-on-change])

        ^JRadioButtonMenuItem xs-radio-200 (ss/radio-menu-item
                                             :text "200"
                                             :group btn-group-xs
                                             :listen [:mouse-clicked xs-on-change])]
    (.add xs-config-container xs-radio-10)
    (.add xs-config-container xs-radio-25)
    (.add xs-config-container xs-radio-50)
    (.add xs-config-container xs-radio-100)
    ;; (.add xs-config-container xs-radio-200)
    (.add xs-container xs-config-container)
    xs-container))


(defn update-points-from-setters!
  "Update points using setters with values from a function"
  [new-fn]
  (let [{:keys [items-point-setters items-point-getters]} @items-points-accessors*]
    (doseq [i (range @sketch-input-x-count*)]
      ((nth items-point-setters i)
       (.getX ^Point ((nth items-point-getters i)))
       (new-fn i)))))


(defn repaint-drawing-widget!
  "Repaint the drawing widget"
  []
  (when-let [drawing-widget (:drawing-widget @items-points-accessors*)]
    (ss/repaint! drawing-widget)))


(defn set-xs-count!
  "Set the number of x points and trigger redraw"
  [n]
  (reset! sketch-input-x-count* n)
  (reset! new-xs?* true))
