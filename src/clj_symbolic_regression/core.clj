(ns clj-symbolic-regression.core
  (:gen-class)
  (:require [clj-symbolic-regression.symreg :as symreg]))

(set! *warn-on-reflection* true)


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (symreg/run-test))



