(ns closyr.ui.settings.experiment
  "Experiment settings panels for max leafs, iterations, and population size"
  (:require
    [closyr.ui.components :as ui-comp]
    [closyr.util.log :as log]
    [seesaw.core :as ss])
  (:import
    (java.awt.event
      MouseEvent)
    (javax.swing
      JPanel
      JRadioButtonMenuItem)))


(set! *warn-on-reflection* true)


;; =============================================================================
;; State
;; =============================================================================

(def experiment-settings*
  "Atom containing experiment settings"
  (atom {:max-leafs           40
         :input-iters         100
         :input-phenos-count  2000
         :random-seed         nil
         :mutations-blacklist nil
         :log-steps           nil}))


(def ^:private amount->number
  {"10"    10
   "100"   100
   "500"   500
   "1000"  1000
   "2000"  2000
   "5000"  5000
   "10000" 10000
   "1K"    1000
   "2K"    2000
   "5K"    5000
   "10K"   10000
   "20K"   20000
   "50K"   50000})


;; =============================================================================
;; Event handlers
;; =============================================================================

(defn- settings-max-leafs-on-change
  [^MouseEvent e]
  (let [b (.getText ^JRadioButtonMenuItem (.getSource e))]
    (swap! experiment-settings* assoc :max-leafs (Integer/parseInt b))
    (log/info "max leafs changed to " b)))


(defn- settings-iters-on-change
  [^MouseEvent e]
  (let [b (.getText ^JRadioButtonMenuItem (.getSource e))]
    (swap! experiment-settings* assoc :input-iters (amount->number b))
    (log/info "iters changed to " b)))


(defn- settings-pheno-count-on-change
  [^MouseEvent e]
  (let [b (.getText ^JRadioButtonMenuItem (.getSource e))]
    (swap! experiment-settings* assoc :input-phenos-count (amount->number b))
    (log/info "pheno count changed to " b)))


;; =============================================================================
;; Panels
;; =============================================================================

(defn ^JPanel max-leafs-settings-panel
  "Create the max function leafs settings panel"
  []
  (let [max-leafs-settings-container              (ui-comp/panel-grid
                                                    {:rows 1 :cols 4 :border (ui-comp/radio-controls-border "Max Function Leafs")})

        ^JPanel settings-container                (ui-comp/panel-grid {:rows 1 :cols 1})

        btn-group-max-leafs                       (ss/button-group)
        ^JRadioButtonMenuItem max-leafs-radio-10  (ss/radio-menu-item
                                                    :text "20"
                                                    :group btn-group-max-leafs
                                                    :listen [:mouse-clicked settings-max-leafs-on-change])
        ^JRadioButtonMenuItem max-leafs-radio-100 (ss/radio-menu-item
                                                    :selected? true
                                                    :text "40"
                                                    :group btn-group-max-leafs
                                                    :listen [:mouse-clicked settings-max-leafs-on-change])
        ^JRadioButtonMenuItem max-leafs-radio-1k  (ss/radio-menu-item
                                                    :text "60"
                                                    :group btn-group-max-leafs
                                                    :listen [:mouse-clicked settings-max-leafs-on-change])
        ^JRadioButtonMenuItem max-leafs-radio-10k (ss/radio-menu-item
                                                    :text "120"
                                                    :group btn-group-max-leafs
                                                    :listen [:mouse-clicked settings-max-leafs-on-change])]


    (.add max-leafs-settings-container max-leafs-radio-10)
    (.add max-leafs-settings-container max-leafs-radio-100)
    (.add max-leafs-settings-container max-leafs-radio-1k)
    (.add max-leafs-settings-container max-leafs-radio-10k)
    (.add settings-container max-leafs-settings-container)
    settings-container))


(defn ^JPanel experiment-settings-panel
  "Create the experiment settings panel with iterations and population size"
  []
  (let [iters-settings-container               (ui-comp/panel-grid
                                                 {:rows 1 :cols 4 :border (ui-comp/radio-controls-border "Iterations")})
        pcount-settings-container              (ui-comp/panel-grid
                                                 {:rows 1 :cols 5 :border (ui-comp/radio-controls-border "Population Size")})
        ^JPanel settings-container             (ui-comp/panel-grid {:rows 1 :cols 2})

        btn-group-iters                        (ss/button-group)
        ^JRadioButtonMenuItem iter-radio-10    (ss/radio-menu-item
                                                 :text "10"
                                                 :group btn-group-iters
                                                 :listen [:mouse-clicked settings-iters-on-change])
        ^JRadioButtonMenuItem iter-radio-100   (ss/radio-menu-item
                                                 :selected? true
                                                 :text "100"
                                                 :group btn-group-iters
                                                 :listen [:mouse-clicked settings-iters-on-change])
        ^JRadioButtonMenuItem iter-radio-1k    (ss/radio-menu-item
                                                 :text "1K"
                                                 :group btn-group-iters
                                                 :listen [:mouse-clicked settings-iters-on-change])
        ^JRadioButtonMenuItem iter-radio-10k   (ss/radio-menu-item
                                                 :text "10K"
                                                 :group btn-group-iters
                                                 :listen [:mouse-clicked settings-iters-on-change])

        btn-group-pcounts                      (ss/button-group)
        ^JRadioButtonMenuItem pcount-radio-500 (ss/radio-menu-item
                                                 :text "500"
                                                 :group btn-group-pcounts
                                                 :listen [:mouse-clicked settings-pheno-count-on-change])
        ^JRadioButtonMenuItem pcount-radio-1k  (ss/radio-menu-item
                                                 :text "1K"
                                                 :group btn-group-pcounts
                                                 :listen [:mouse-clicked settings-pheno-count-on-change])
        ^JRadioButtonMenuItem pcount-radio-2k  (ss/radio-menu-item
                                                 :text "2K"
                                                 :selected? true
                                                 :group btn-group-pcounts
                                                 :listen [:mouse-clicked settings-pheno-count-on-change])
        ^JRadioButtonMenuItem pcount-radio-10k (ss/radio-menu-item
                                                 :text "5K"
                                                 :group btn-group-pcounts
                                                 :listen [:mouse-clicked settings-pheno-count-on-change])
        ^JRadioButtonMenuItem pcount-radio-50k (ss/radio-menu-item
                                                 :text "50K"
                                                 :group btn-group-pcounts
                                                 :listen [:mouse-clicked settings-pheno-count-on-change])]
    (.add pcount-settings-container pcount-radio-500)
    (.add pcount-settings-container pcount-radio-1k)
    (.add pcount-settings-container pcount-radio-2k)
    (.add pcount-settings-container pcount-radio-10k)
    (.add pcount-settings-container pcount-radio-50k)

    (.add iters-settings-container iter-radio-10)
    (.add iters-settings-container iter-radio-100)
    (.add iters-settings-container iter-radio-1k)
    (.add iters-settings-container iter-radio-10k)
    (.add settings-container iters-settings-container)
    (.add settings-container pcount-settings-container)
    settings-container))
