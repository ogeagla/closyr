(ns closyr.spec-test
  (:require
    [clojure.pprint :as pp]
    [clojure.test :refer :all]
    [closyr.spec :as specs]
    [malli.core :as m]
    [malli.instrument :as mi]))


(deftest defined-schemas
  (testing "GAPhenotype failure case"
    (is (thrown? Exception
          (specs/validate!
            "test-pheno"
            #'specs/GAPhenotype
            {}))))

  (testing "GAMutation failure case"
    (is (thrown? Exception
          (specs/validate!
            "test-mutation"
            #'specs/GAMutation
            {}))))

  (testing "GAPopulation failure case"
    (is (thrown? Exception
          (specs/validate!
            "test-pop"
            #'specs/GAPopulation
            {}))))


  (testing "SolverRunConfig failure case"
    (is (thrown? Exception
          (specs/validate!
            "test-solver-run-config"
            #'specs/SolverRunConfig
            {}))))

  (testing "SolverRunArgs failure case"
    (is (thrown? Exception
          (specs/validate!
            "test-solver-run-args"
            #'specs/SolverRunArgs
            {}))))

  (testing "SolverRunResults failure case"
    (is (thrown? Exception
          (specs/validate!
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
 closyr.ops.modify
 {apply-modifications
  {:schema
   [:=> [:cat pos-int? int? [:sequential #'closyr.spec/GAMutation] #'closyr.spec/GAPhenotype #'closyr.spec/GAPhenotype] [:map {:closed true} [:new-pheno #'closyr.spec/GAPhenotype] [:iters int?] [:mods [:sequential #'closyr.spec/GAMutation]]]],
   :ns closyr.ops.modify,
   :name apply-modifications}},
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


(def ^:private all-instrumented
  "(#'closyr.ops.common/extend-xs
 #'closyr.ops.eval/eval-phenotype-on-expr-args
 #'closyr.ops.eval/eval-vec-pheno
 #'closyr.ops.modify/apply-modifications
 #'closyr.ops/compute-residual
 #'closyr.symbolic-regression/run-app-from-cli-args
 #'closyr.symbolic-regression/run-ga-iterations-using-record)
")


(deftest check-instrumented

  (testing "All default defns"
    (is (=
          (do
            (require 'closyr.symbolic-regression)
            (with-out-str (pp/pprint (m/function-schemas))))
          all-specs))))


#_(deftest check-can-uninstrument
  (testing "Can de-instrument"
    (let [_ (#'specs/disable-validate-instrumentation!)]
      (mi/unstrument!)



      (is (= (specs/validate!
               "test-solver-run-results"
               #'specs/SolverRunResults
               {})
             true))

      (is (= (#'ops/compute-residual nil nil)
             ops/max-resid))


      (is (= (ops-common/extend-xs [0.1 "a"])
             [])))))
