(ns closyr.core
  (:gen-class)
  (:require [closyr.symreg :as symreg]))

(set! *warn-on-reflection* true)


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (symreg/run-app-with-gui))



