(ns closyr.dataset-inputs-test
  (:require
    [clojure.test :refer :all]
    [closyr.dataset.inputs :as input-data]))


(deftest eval-all-input-fns
  (doseq [[fn-name {fn-body :fn fn-idx :idx
                    :as     fn-data}] (input-data/input-y-fns-data (atom 1000) (atom 10))]
    (testing (str "initial fn: " fn-name)
      (is (number? fn-idx))
      (is (number? (fn-body 0))))))
