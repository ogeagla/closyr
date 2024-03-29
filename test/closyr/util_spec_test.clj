(ns closyr.util-spec-test
  (:require
    [clojure.string :as str]
    [clojure.test :refer :all]
    [closyr.util.spec :as specs]
    [malli.core :as m]
    [malli.error :as me]
    [malli.generator :as mg]
    [malli.instrument :as mi]
    [malli.transform :as mt])
  (:import
    (org.matheclipse.core.eval
      EvalEngine
      ExprEvaluator)
    (org.matheclipse.core.expression
      F)
    (org.matheclipse.core.interfaces
      IExpr
      ISymbol)))


(deftest inputs-from-gui-spec
  (testing "GUI input data"
    (is (= (me/humanize
             (m/explain #'specs/SolverGUIMessage
                        {:max-leafs          20,
                         :input-iters        10000,
                         :input-phenos-count 50000,
                         :new-state          :start,
                         :input-data-x
                         [0.0
                          1.6331994645247656
                          3.2931726907630523
                          4.953145917001338
                          6.613119143239625
                          8.273092369477911
                          9.933065595716197
                          11.593038821954485
                          13.25301204819277
                          14.912985274431056],
                         :input-data-y
                         [2.3704663212435237
                          3.2253886010362693
                          3.7694300518134716
                          3.9248704663212437
                          1.1269430051813476
                          -5.090673575129534
                          1.204663212435233
                          2.215025906735751
                          1.5155440414507773
                          6.955958549222798]}))
           nil))))


(deftest generates-custom-types
  (testing "valid Expr"
    (is (instance?
          IExpr
          (mg/generate #'specs/SymbolicExpr)))

    (is (=
          (m/explain #'specs/SymbolicExpr F/C1)
          nil)))

  (testing "invalid Expr"
    (is (=
          (-> (m/explain #'specs/SymbolicExpr 123) :errors count)
          1))

    (is (=
          (me/humanize (m/explain #'specs/SymbolicExpr 0))
          ["should be an IExpr, got 0"])))

  (testing "valid Expr[]"
    (is (instance?
          specs/iexpr-array-class
          (mg/generate #'specs/PrimitiveArrayOfIExpr)))

    (is (=
          (m/explain #'specs/PrimitiveArrayOfIExpr (into-array IExpr [F/C1 F/CN1 (F/Sin (F/Dummy "x"))]))
          nil)))

  (testing "invalid Expr[]"
    (is (=
          (-> (m/explain #'specs/PrimitiveArrayOfIExpr 123) :errors count)
          1))

    (is (=
          (me/humanize (m/explain #'specs/PrimitiveArrayOfIExpr 0))
          ["should be a IExpr[], got 0"])))

  (testing "valid Symbol"
    (is (instance?
          ISymbol
          (mg/generate #'specs/SymbolicVariable)))

    (is (=
          (m/explain #'specs/SymbolicVariable (F/Dummy "x"))
          nil)))

  (testing "invalid Symbol"
    (is (=
          (-> (m/explain #'specs/SymbolicVariable 123) :errors count)
          1))

    (is (=
          (me/humanize (m/explain #'specs/SymbolicVariable 0))
          ["should be an ISymbol, got 0"])))

  (testing "valid Evaluator"
    (is (=
          (class (mg/generate #'specs/SymbolicEvaluator))
          ExprEvaluator))

    (is (=
          (m/explain #'specs/SymbolicEvaluator (ExprEvaluator.
                                                 (doto (EvalEngine. true)
                                                   (.setQuietMode true))
                                                 true
                                                 0))
          nil)))

  (testing "invalid Evaluator"
    (is (=
          (-> (m/explain #'specs/SymbolicEvaluator 123) :errors count)
          1))

    (is (=
          (me/humanize (m/explain #'specs/SymbolicEvaluator 0))
          ["should be an ExprEvaluator, got 0"]))))


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
              17))))))


#_(deftest decode-test
    (testing "example"
      (is (=
            (m/decode
              [:vector {:decode/string #(str/split % #",")} int?]
              "1,2,3,4"
              (mt/string-transformer))
            [1 2 3 4]))))


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
