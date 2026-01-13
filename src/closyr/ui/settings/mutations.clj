(ns closyr.ui.settings.mutations
  "Mutation selection dialog and panel"
  (:require
    [closyr.ops.initialize :as ops-init]
    [closyr.ui.components :as ui-comp]
    [closyr.util.log :as log])
  (:import
    (java.awt
      BorderLayout
      FlowLayout)
    (java.awt.event
      ActionListener)
    (javax.swing
      BoxLayout
      JButton
      JCheckBox
      JDialog
      JFrame
      JLabel
      JPanel
      JScrollPane)))


(set! *warn-on-reflection* true)


(def all-mutation-labels
  "All available mutation labels"
  (ops-init/mutation-labels))


(def selected-mutations*
  "Set of currently selected mutation labels (all selected by default)"
  (atom (set all-mutation-labels)))


(defn update-mutations-blacklist!
  "Update the mutations blacklist in experiment-settings based on deselected mutations"
  [experiment-settings*]
  (let [selected  @selected-mutations*
        blacklist (vec (remove selected all-mutation-labels))]
    (swap! experiment-settings* assoc :mutations-blacklist
           (when (seq blacklist) blacklist))
    (log/info "Mutations blacklist updated:" (count blacklist) "mutations excluded")))


(defn show-mutations-dialog!
  "Show a dialog to select/deselect mutations"
  [^JFrame parent-frame ^JLabel count-label experiment-settings*]
  (let [^JDialog dialog (doto (JDialog. parent-frame "Select Mutations" true)
                          (.setSize 400 500)
                          (.setLocationRelativeTo parent-frame))

        checkboxes (atom {})

        ^JPanel checkbox-panel (JPanel.)
        _ (.setLayout checkbox-panel (BoxLayout. checkbox-panel BoxLayout/Y_AXIS))

        _ (doseq [^String label (sort all-mutation-labels)]
            (let [^JCheckBox cb (doto (JCheckBox. label ^Boolean (contains? @selected-mutations* label))
                                  (.addActionListener
                                    (reify ActionListener
                                      (actionPerformed [_ e]
                                        (let [selected? (.isSelected ^JCheckBox (.getSource e))]
                                          (if selected?
                                            (swap! selected-mutations* conj label)
                                            (swap! selected-mutations* disj label)))))))]
              (swap! checkboxes assoc label cb)
              (.add checkbox-panel cb)))

        ^JScrollPane scroll-pane (doto (JScrollPane. checkbox-panel)
                                   (.setVerticalScrollBarPolicy JScrollPane/VERTICAL_SCROLLBAR_ALWAYS))

        ^JButton select-all-btn (doto (JButton. "Select All")
                                  (.addActionListener
                                    (reify ActionListener
                                      (actionPerformed [_ _]
                                        (reset! selected-mutations* (set all-mutation-labels))
                                        (doseq [[_ ^JCheckBox cb] @checkboxes]
                                          (.setSelected cb true))))))

        ^JButton select-none-btn (doto (JButton. "Select None")
                                   (.addActionListener
                                     (reify ActionListener
                                       (actionPerformed [_ _]
                                         (reset! selected-mutations* #{})
                                         (doseq [[_ ^JCheckBox cb] @checkboxes]
                                           (.setSelected cb false))))))

        ^JButton ok-btn (doto (JButton. "OK")
                          (.addActionListener
                            (reify ActionListener
                              (actionPerformed [_ _]
                                (update-mutations-blacklist! experiment-settings*)
                                (.setText count-label (str (count @selected-mutations*) "/" (count all-mutation-labels)))
                                (.dispose dialog)))))

        ^JPanel buttons-panel (doto (JPanel. (FlowLayout.))
                                (.add select-all-btn)
                                (.add select-none-btn)
                                (.add ok-btn))

        ^JPanel main-panel (doto (JPanel. (BorderLayout.))
                             (.add (JLabel. "Select mutations to use during evolution:") BorderLayout/NORTH)
                             (.add scroll-pane BorderLayout/CENTER)
                             (.add buttons-panel BorderLayout/SOUTH))]

    (.setContentPane dialog main-panel)
    (.setVisible dialog true)))


(defn ^JPanel mutations-selection-panel
  "Create a panel with a button to open mutation selection dialog"
  [parent-frame-atom experiment-settings*]
  (let [^JPanel container   (ui-comp/panel-grid {:rows 1 :cols 1 :border (ui-comp/radio-controls-border "Mutations")})
        ^JPanel inner-panel (doto (JPanel.)
                              (.setLayout (FlowLayout. FlowLayout/LEFT 5 2)))

        ^JLabel count-label (JLabel. (str (count @selected-mutations*) "/" (count all-mutation-labels)))

        ^JButton select-btn (doto (JButton. "Select Mutations...")
                              (.setToolTipText "Choose which mutations to use during evolution")
                              (.addActionListener
                                (reify ActionListener
                                  (actionPerformed [_ _]
                                    (when-let [frame @parent-frame-atom]
                                      (show-mutations-dialog! frame count-label experiment-settings*))))))]

    (.add inner-panel select-btn)
    (.add inner-panel count-label)
    (.add container inner-panel)
    container))
