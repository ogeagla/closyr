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
      JCheckBox
      JDialog
      JFrame
      JLabel
      JPanel
      JRadioButton)))


(set! *warn-on-reflection* true)


(defn show-advanced-settings-dialog!
  "Show a dialog with advanced settings for log interval, adaptive mode, and quiet logging"
  [^JFrame parent-frame ^JLabel current-value-label experiment-settings*]
  (let [^JDialog dialog (doto (JDialog. parent-frame "Advanced Settings" true)
                          (.setSize 400 350)
                          (.setLocationRelativeTo parent-frame))

        current-log-steps (:log-steps @experiment-settings*)
        current-adaptive (:adaptive-mode @experiment-settings*)
        current-quiet (:quiet-logs @experiment-settings*)
        current-eval-cache (:use-eval-cache @experiment-settings*)
        selected-value (atom current-log-steps)
        selected-adaptive (atom current-adaptive)
        selected-quiet (atom current-quiet)
        selected-eval-cache (atom current-eval-cache)

        btn-group (ButtonGroup.)
        options [["Auto" nil] ["1" 1] ["5" 5] ["10" 10] ["25" 25]]

        ^JPanel radio-panel (JPanel.)
        _ (.setLayout radio-panel (BoxLayout. radio-panel BoxLayout/Y_AXIS))

        _ (doseq [[label value] options]
            (let [^JRadioButton rb (doto (JRadioButton. ^String label)
                                     (.setSelected (= value current-log-steps))
                                     (.addActionListener
                                       (reify ActionListener
                                         (actionPerformed [_ _]
                                           (reset! selected-value value)))))]
              (.add btn-group rb)
              (.add radio-panel rb)))

        ;; Adaptive mode checkbox
        ^JCheckBox adaptive-cb (JCheckBox. "Adaptive Mutations")
        _ (doto adaptive-cb
            (.setSelected (boolean current-adaptive))
            (.setToolTipText "Dynamically adjust mutation rates based on population diversity and stagnation")
            (.addActionListener
              (reify ActionListener
                (actionPerformed [_ _]
                  (reset! selected-adaptive (.isSelected adaptive-cb))))))

        ;; Quiet logging checkbox
        ^JCheckBox quiet-cb (JCheckBox. "Quiet Logging")
        _ (doto quiet-cb
            (.setSelected (boolean current-quiet))
            (.setToolTipText "Suppress detailed iteration logs for cleaner output")
            (.addActionListener
              (reify ActionListener
                (actionPerformed [_ _]
                  (reset! selected-quiet (.isSelected quiet-cb))))))

        ;; Eval cache checkbox
        ^JCheckBox eval-cache-cb (JCheckBox. "Evaluation Cache")
        _ (doto eval-cache-cb
            (.setSelected (boolean current-eval-cache))
            (.setToolTipText "Cache evaluation results by expression to avoid redundant calculations")
            (.addActionListener
              (reify ActionListener
                (actionPerformed [_ _]
                  (reset! selected-eval-cache (.isSelected eval-cache-cb))))))

        ;; Checkboxes panel
        ^JPanel checkbox-panel (JPanel.)
        _ (doto checkbox-panel
            (.setLayout (BoxLayout. checkbox-panel BoxLayout/Y_AXIS))
            (.add adaptive-cb)
            (.add quiet-cb)
            (.add eval-cache-cb))

        ^JButton ok-btn (doto (JButton. "OK")
                          (.addActionListener
                            (reify ActionListener
                              (actionPerformed [_ _]
                                (swap! experiment-settings* assoc
                                       :log-steps @selected-value
                                       :adaptive-mode @selected-adaptive
                                       :quiet-logs @selected-quiet
                                       :use-eval-cache @selected-eval-cache)
                                (.setText current-value-label (if @selected-value
                                                                (str @selected-value)
                                                                "Auto"))
                                (log/info "Log steps changed to:" (or @selected-value "Auto"))
                                (log/info "Adaptive mode:" @selected-adaptive)
                                (log/info "Quiet logging:" @selected-quiet)
                                (log/info "Eval cache:" @selected-eval-cache)
                                (.dispose dialog)))))

        ^JButton cancel-btn (doto (JButton. "Cancel")
                              (.addActionListener
                                (reify ActionListener
                                  (actionPerformed [_ _]
                                    (.dispose dialog)))))

        ^JPanel buttons-panel (doto (JPanel. (FlowLayout.))
                                (.add ok-btn)
                                (.add cancel-btn))

        ;; Center panel with both sections
        ^JPanel center-panel (JPanel.)
        _ (doto center-panel
            (.setLayout (BoxLayout. center-panel BoxLayout/Y_AXIS))
            (.add (JLabel. "Log/Chart Update Interval (iterations):"))
            (.add radio-panel)
            (.add (JLabel. " "))
            (.add (JLabel. "Mutation Settings:"))
            (.add checkbox-panel))

        ^JPanel main-panel (doto (JPanel. (BorderLayout.))
                             (.add center-panel BorderLayout/CENTER)
                             (.add buttons-panel BorderLayout/SOUTH))]

    (.setContentPane dialog main-panel)
    (.setVisible dialog true)))
