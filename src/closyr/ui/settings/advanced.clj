(ns closyr.ui.settings.advanced
  "Advanced settings dialog for log interval configuration"
  (:require
    [closyr.util.log :as log])
  (:import
    (java.awt
      BorderLayout
      FlowLayout)
    (java.awt.event
      ActionListener)
    (javax.swing
      BoxLayout
      ButtonGroup
      JButton
      JDialog
      JFrame
      JLabel
      JPanel
      JRadioButton)))


(set! *warn-on-reflection* true)


(defn show-advanced-settings-dialog!
  "Show a dialog with advanced settings for log interval configuration"
  [^JFrame parent-frame ^JLabel current-value-label experiment-settings*]
  (let [^JDialog dialog (doto (JDialog. parent-frame "Advanced Settings" true)
                          (.setSize 350 250)
                          (.setLocationRelativeTo parent-frame))

        current-log-steps (:log-steps @experiment-settings*)
        selected-value    (atom current-log-steps)

        btn-group         (ButtonGroup.)
        options           [["Auto" nil] ["1" 1] ["5" 5] ["10" 10] ["25" 25]]

        ^JPanel radio-panel (JPanel.)
        _                 (.setLayout radio-panel (BoxLayout. radio-panel BoxLayout/Y_AXIS))

        _                 (doseq [[label value] options]
                           (let [^JRadioButton rb (doto (JRadioButton. ^String label)
                                                    (.setSelected (= value current-log-steps))
                                                    (.addActionListener
                                                      (reify ActionListener
                                                        (actionPerformed [_ _]
                                                          (reset! selected-value value)))))]
                             (.add btn-group rb)
                             (.add radio-panel rb)))

        ^JButton ok-btn   (doto (JButton. "OK")
                            (.addActionListener
                              (reify ActionListener
                                (actionPerformed [_ _]
                                  (swap! experiment-settings* assoc :log-steps @selected-value)
                                  (.setText current-value-label (if @selected-value
                                                                  (str @selected-value)
                                                                  "Auto"))
                                  (log/info "Log steps changed to:" (or @selected-value "Auto"))
                                  (.dispose dialog)))))

        ^JButton cancel-btn (doto (JButton. "Cancel")
                              (.addActionListener
                                (reify ActionListener
                                  (actionPerformed [_ _]
                                    (.dispose dialog)))))

        ^JPanel buttons-panel (doto (JPanel. (FlowLayout.))
                                (.add ok-btn)
                                (.add cancel-btn))

        ^JPanel main-panel (doto (JPanel. (BorderLayout.))
                             (.add (JLabel. "Log/Chart Update Interval (iterations):") BorderLayout/NORTH)
                             (.add radio-panel BorderLayout/CENTER)
                             (.add buttons-panel BorderLayout/SOUTH))]

    (.setContentPane dialog main-panel)
    (.setVisible dialog true)))
