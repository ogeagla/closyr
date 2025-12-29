(ns closyr.ui.theme
  "UI theme setup and font utilities"
  (:require
    [clojure.java.io :as io]
    [closyr.util.log :as log])
  (:import
    (io.materialtheme.darkstackoverflow
      DarkStackOverflowTheme)
    (java.awt
      Font
      GraphicsEnvironment
      Image
      Toolkit)
    (javax.swing
      JFrame
      UIManager
      UnsupportedLookAndFeelException)
    (mdlaf
      MaterialLookAndFeel)))


(set! *warn-on-reflection* true)


(defn setup-theme
  "Initialize the Material Design dark theme for the application"
  []
  (try
    (UIManager/setLookAndFeel
      (MaterialLookAndFeel.
        (DarkStackOverflowTheme.)))

    (catch UnsupportedLookAndFeelException e
      (log/error "Theme error: " e))))


(defn find-unicode-font
  "Find a font that supports Unicode math symbols. Returns a Font or nil."
  [size]
  (let [preferred-fonts ["DejaVu Sans" "Noto Sans" "Segoe UI Symbol"
                         "Arial Unicode MS" "Lucida Sans Unicode"
                         "FreeSans" "Liberation Sans"]
        available-fonts (set (.getAvailableFontFamilyNames
                               (GraphicsEnvironment/getLocalGraphicsEnvironment)))
        found-font      (first (filter available-fonts preferred-fonts))]
    (when found-font
      (Font. found-font Font/PLAIN size))))


(defn set-app-icon
  "Set the application window icon"
  [^JFrame frame]
  (let [^Image icon (.getImage (Toolkit/getDefaultToolkit) (io/resource "icons/icon_v5_qtr.png"))]
    (doto frame
      (.setIconImage icon))))
