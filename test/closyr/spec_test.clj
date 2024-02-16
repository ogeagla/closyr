(ns closyr.spec-test
  (:require
    [clojure.pprint :as pp]
    [clojure.test :refer :all]
    [closyr.spec :as specs]
    [malli.core :as m]))


(deftest defined-schemas
  (testing "GAPhenotype failure case"
    (is (thrown? Exception
          (specs/check-schema!
            "test-pheno"
            #'specs/GAPhenotype
            {}))))

  (testing "GAMutation failure case"
    (is (thrown? Exception
          (specs/check-schema!
            "test-mutation"
            #'specs/GAMutation
            {}))))

  (testing "GAPopulation failure case"
    (is (thrown? Exception
          (specs/check-schema!
            "test-pop"
            #'specs/GAPopulation
            {}))))


  (testing "SolverRunConfig failure case"
    (is (thrown? Exception
          (specs/check-schema!
            "test-solver-run-config"
            #'specs/SolverRunConfig
            {}))))

  (testing "SolverRunArgs failure case"
    (is (thrown? Exception
          (specs/check-schema!
            "test-solver-run-args"
            #'specs/SolverRunArgs
            {}))))

  (testing "SolverRunResults failure case"
    (is (thrown? Exception
          (specs/check-schema!
            "test-solver-run-results"
            #'specs/SolverRunResults
            {})))))


(def ^:private all-specs
  "{closyr.ops.common
 {extend-xs
  {:schema [:=> [:cat [:sequential number?]] map?],
   :ns closyr.ops.common,
   :name extend-xs}},
 closyr.ops.eval
 {eval-phenotype-on-expr-args
  {:schema [:=> [:cat #'closyr.spec/GAPhenotype some?] any?],
   :ns closyr.ops.eval,
   :name eval-phenotype-on-expr-args},
  eval-vec-pheno
  {:schema
   [:=> [:cat #'closyr.spec/GAPhenotype #'closyr.spec/SolverEvalArgs] [:or [:vector number?] nil?]],
   :ns closyr.ops.eval,
   :name eval-vec-pheno}},
 closyr.ops
 {compute-residual
  {:schema [:=> [:cat number? number?] number?],
   :ns closyr.ops,
   :name compute-residual}},
 closyr.symbolic-regression
 {run-app-from-cli-args
  {:schema
   [:=> [:cat #'closyr.symbolic-regression/CLIArgs] #'closyr.spec/SolverRunResults],
   :ns closyr.symbolic-regression,
   :name run-app-from-cli-args},
  run-ga-iterations-using-record
  {:schema
   [:=> [:cat #'closyr.spec/SolverRunConfig #'closyr.spec/SolverRunArgs] #'closyr.spec/SolverRunResults],
   :ns closyr.symbolic-regression,
   :name run-ga-iterations-using-record}}}
")


(deftest check-instrumented

  (testing "All default defns"
    (is (=
          (do
            (require 'closyr.symbolic-regression)
            (with-out-str (pp/pprint (m/function-schemas))))
          all-specs))))
