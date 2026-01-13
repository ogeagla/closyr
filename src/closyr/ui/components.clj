(ns closyr.ui.components
  "Reusable UI component utilities"
  (:require
    [seesaw.behave :as sb]
    [seesaw.core :as ss]
    [seesaw.graphics :as sg])
  (:import
    (java.awt
      BorderLayout
      Color
      GridLayout
      Point)
    (java.awt.event
      MouseEvent)
    (javax.swing
      BorderFactory
      JPanel)
    (javax.swing.border
      Border)))


(set! *warn-on-reflection* true)


(defn ^JPanel panel-grid
  "Create a JPanel with GridLayout and optional border"
  [{:keys [rows cols ^Border border]}]
  (let [panel (doto (JPanel. (BorderLayout.))
                (.setLayout (GridLayout. rows cols)))]
    (cond-> panel
      (not (nil? border)) (.setBorder border))
    panel))


(defn radio-controls-border
  "Create a titled border for radio button groups"
  [title]
  (BorderFactory/createTitledBorder (BorderFactory/createLineBorder (Color. 80 80 80) 1) title))


(def ^:private on-drag-finish-callback* (atom nil))

(defn set-on-drag-finish-callback!
  "Set a callback to be called when any movable widget finishes being dragged"
  [callback-fn]
  (reset! on-drag-finish-callback* callback-fn))

(defn movable
  "Make a widget draggable with mouse. Options: {:disable-x? true} to lock horizontal movement."
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
                                  (- (.y p) (.y start-point))])))
       ;; When the drag finishes, call the callback if set
       :finish (fn [_]
                 (when-let [callback @on-drag-finish-callback*]
                   (callback)))))
     w))


(defn make-label
  "Create a styled rounded label at the given location"
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
                                                    9)
                                 (sg/style :foreground "salmon"
                                           :background "#666"
                                           :stroke 2)))})
    ;; Set the bounds to its preferred size. Note that this has to be
    ;; done after the label is fully constructed.
    (ss/config! :bounds :preferred)))
