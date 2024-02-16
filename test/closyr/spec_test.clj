(ns closyr.spec-test
  (:require
    [clojure.pprint :as pp]
    [clojure.test :refer :all]
    [closyr.spec :as specs]
    [closyr.symbolic-regression :as symreg]
    [malli.core :as m]))


(deftest defined-schemas
  (testing "GAPhenotype failure case"
    (is (thrown? Exception
          (specs/check-schema!
            "test-pheno"
            #'symreg/GAPhenotype
            {}))))

  (testing "GAMutation failure case"
    (is (thrown? Exception
          (specs/check-schema!
            "test-mutation"
            #'symreg/GAMutation
            {}))))

  (testing "GAPopulation failure case"
    (is (thrown? Exception
          (specs/check-schema!
            "test-pop"
            #'symreg/GAPopulation
            {}))))


  (testing "SolverRunConfig failure case"
    (is (thrown? Exception
          (specs/check-schema!
            "test-solver-run-config"
            #'symreg/SolverRunConfig
            {}))))

  (testing "SolverRunArgs failure case"
    (is (thrown? Exception
          (specs/check-schema!
            "test-solver-run-args"
            #'symreg/SolverRunArgs
            {}))))

  (testing "SolverRunResults failure case"
    (is (thrown? Exception
          (specs/check-schema!
            "test-solver-run-results"
            #'symreg/SolverRunResults
            {})))))


(def ^:private all-specs
  "{closyr.ops.common
 {extend-xs
  {:schema [:=> [:cat [:sequential number?]] map?],
   :ns closyr.ops.common,
   :name extend-xs}},
 closyr.ops
 {compute-residual
  {:schema [:=> [:cat number? number?] number?],
   :ns closyr.ops,
   :name compute-residual}},
 closyr.symbolic-regression
 {run-ga-iterations-using-record
  {:schema
   [:=> [:cat #'closyr.symbolic-regression/SolverRunConfig #'closyr.symbolic-regression/SolverRunArgs] #'closyr.symbolic-regression/SolverRunResults],
   :ns closyr.symbolic-regression,
   :name run-ga-iterations-using-record}}}
")


(deftest check-instrumented

  (testing "All default defns"
    (is (=
          (with-out-str (pp/pprint (m/function-schemas)))
          all-specs))))
