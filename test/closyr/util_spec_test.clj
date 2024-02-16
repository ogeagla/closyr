(ns closyr.util-spec-test
  (:require
    [clojure.pprint :as pp]
    [clojure.test :refer :all]
    [closyr.util.spec :as specs]
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
            #'specs/GAPopulationPhenotypes
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


(deftest check-instrumented
  (testing "All default defns"
    (do
      (require 'closyr.symbolic-regression)
      (let [ss (m/function-schemas)]
        (is (=
              (count ss)
              ;; the number of ns which contain defns which have malli/schema metadata in entire src:
              6))

        (is (=
              (reduce + 0 (map (fn [[k v]] (count v)) ss))
              ;; the number of total defns which have malli/schema metadata in entire src:
              14))))))


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
