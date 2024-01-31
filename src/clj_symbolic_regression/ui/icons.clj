(ns clj-symbolic-regression.ui.icons
  (:require
    [clojure.string :as str])
  (:import
    (javax.swing
      UIManager)))


;; https://en-human-begin.blogspot.com/2007/11/javas-icons-by-default.html
;; https://coderanch.com/t/571308/java/built-Java-icons

(def all-icons
  ["Button.rolloverIconType"
   "Button.textIconGap"
   "CheckBox.textIconGap"
   "CheckBoxMenuItem.arrowIcon"
   "CheckBoxMenuItem.checkIcon"
   "DesktopIcon.background"
   "DesktopIcon.border"
   "DesktopIcon.font"
   "DesktopIcon.foreground"
   "DesktopIcon.width"
   "DesktopIconUI"
   "FileChooser.detailsViewIcon"
   "FileChooser.directoryIcon"
   "FileChooser.fileIcon"
   "FileChooser.floppyDriveIcon"
   "FileChooser.homeFolderIcon"
   "FileChooser.listViewIcon"
   "FileChooser.newFolderIcon"
   "FileChooser.upFolderIcon"
   "FileView.computerIcon"
   "FileView.directoryIcon"
   "FileView.fileIcon"
   "FileView.floppyDriveIcon"
   "FileView.hardDriveIcon"
   "InternalFrame.closeIcon"
   "InternalFrame.iconifyIcon"
   "InternalFrame.maximizeIcon"
   "InternalFrame.minimizeIcon"
   "InternalFrame.paletteCloseIcon"
   "Menu.arrowIcon"
   "Menu.checkIcon"
   "MenuItem.arrowIcon"
   "MenuItem.checkIcon"
   "OptionPane.errorIcon"
   "OptionPane.informationIcon"
   "OptionPane.questionIcon"
   "OptionPane.warningIcon"
   "RadioButton.icon"
   "RadioButton.textIconGap"
   "RadioButtonMenuItem.arrowIcon"
   "RadioButtonMenuItem.checkIcon"
   "Slider.horizontalThumbIcon"
   "Slider.verticalThumbIcon"
   "TabbedPane.textIconGap"
   "Table.ascendingSortIcon"
   "Table.descendingSortIcon"
   "Table.sortIconColor"
   "ToggleButton.textIconGap"
   "ToolBar.handleIcon"
   "Tree.closedIcon"
   "Tree.collapsedIcon"
   "Tree.drawsFocusBorderAroundIcon"
   "Tree.expandedIcon"
   "Tree.leafIcon"
   "Tree.openIcon"])


(defn get-builtin-icons
  []
  ;; Hashtable<Object,Object> defs = UIManager.getDefaults();
  ;;       Enumeration en = defs.keys();
  ;;       while (en.hasMoreElements()) {
  ;;           Object o = en.nextElement();
  ;;           if (o instanceof String) {
  ;;               String key = (String)o;
  ;;               if (key.indexOf("Icon") >= 0)
  ;;                   System.out.println(key);
  ;;           }
  ;;       }

  (let [defaults  (UIManager/getDefaults)
        keys-enum (.keys defaults)]
    (loop [keys-enum keys-enum]
      (when (.hasMoreElements keys-enum)
        (let [o (.nextElement keys-enum)]
          (when (instance? String o)
            (let [s (str o)]
              (when (>= (.indexOf s "Icon") 0)
                (println "Found icon: " s)))))
        (recur keys-enum)))))


(comment (get-builtin-icons))
(comment (println (str/join "\n" all-icons)))
