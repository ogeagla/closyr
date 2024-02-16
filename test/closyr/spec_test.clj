(ns closyr.spec-test
  (:require
    [clojure.test :refer :all]
    [closyr.spec :as specs]
    [closyr.symbolic-regression :as symreg]))


(deftest ga-phenotypes
  (testing "failure case"
    (is (thrown? Exception
          (specs/check-schema!
            "test-pheno"
            #'symreg/GAPhenotype
            {})))))
