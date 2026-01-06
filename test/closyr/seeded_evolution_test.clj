(ns closyr.seeded-evolution-test
  "Tests for seeding GA evolution with parsed formula strings.

   This test namespace exercises the full pipeline of:
   1. Parsing formula strings to phenotypes
   2. Scoring parsed phenotypes
   3. Mutating parsed phenotypes
   4. Running mini GA evolution with seeded phenotypes

   IMPORTANT: Errors are logged at ERROR level to expose real issues."
  (:require
    [clojure.test :refer :all]
    [closyr.ga :as ga]
    [closyr.ops :as ops]
    [closyr.ops.common :as ops-common]
    [closyr.ops.eval :as ops-eval]
    [closyr.ops.initialize :as ops-init]
    [closyr.util.log :as log]))


;; Log at ERROR level during tests to expose real issues
(defn error-logging-fixture
  "Test fixture that sets log level to ERROR during tests to expose real issues."
  [f]
  (log/set-log-level! :error)
  (f))


(use-fixtures :once error-logging-fixture)


;;; ============================================================================
;;; Use functions from ops-init
;;; ============================================================================


;; Alias for convenience in tests
(def parse-formula->phenotype ops-init/parse-formula->phenotype)
(def seeded-phenotypes ops-init/seeded-phenotypes)


;;; ============================================================================
;;; Test Helpers
;;; ============================================================================


(def test-xs
  "Test x values"
  [0.0 0.5 1.0 1.5 2.0])


(def test-ys
  "Test y values (sin(x) for testing)"
  (mapv #(Math/sin %) test-xs))


(defn- make-test-run-args
  "Create run-args for testing with simple x/y data"
  []
  (let [xs-exprs (ops-common/doubles->exprs test-xs)]
    {:input-xs-list  (ops-common/exprs->exprs-list xs-exprs)
     :input-xs-count (count test-xs)
     :input-xs-vec   test-xs
     :input-ys-vec   test-ys
     :input-ys-arr   (double-array test-ys)}))


(defn- make-test-run-config
  "Create run-config for testing"
  []
  {:max-leafs 40
   :iters     5})


;;; ============================================================================
;;; Tests - Formula Parsing
;;; ============================================================================


(deftest test-parse-simple-formula
  (testing "parsing simple formulas produces valid phenotypes"
    (let [pheno (parse-formula->phenotype "x")]
      (is (some? pheno) "Should parse 'x'")
      (is (= ops-common/sym-x (:sym pheno)) "sym should be sym-x")
      (is (some? (:expr pheno)) "Should have expr")
      (is (some? (:util pheno)) "Should have util"))

    (let [pheno (parse-formula->phenotype "Sin(x)")]
      (is (some? pheno) "Should parse 'Sin(x)'"))

    (let [pheno (parse-formula->phenotype "x^2 + 2*x + 1")]
      (is (some? pheno) "Should parse polynomial"))))


(deftest test-parsed-symbol-is-sym-x
  (testing "parsed expressions use sym-x, not parser's x"
    (let [pheno (parse-formula->phenotype "x + 1")
          expr (:expr pheno)
          expr-str (str expr)]
      ;; The expression should contain our sym-x, which should work with evaluation
      (is (some? pheno))
      ;; Most importantly: the :sym field should be sym-x
      (is (= ops-common/sym-x (:sym pheno))))))


(deftest test-parse-complex-formulas
  (testing "parsing complex formulas from typical solver output"
    (doseq [formula ["Sin(x) + Cos(x)"
                     "x^2 - 3*x + 1"
                     "Log(x + 1)"
                     "Exp(-x^2)"
                     "x/2 + Sin(x)/3"]]
      (let [pheno (parse-formula->phenotype formula)]
        (is (some? pheno) (str "Should parse: " formula))))))


(deftest test-parse-invalid-formulas
  (testing "empty formula returns nil"
    ;; Note: Symja parser is very permissive, so most "invalid" strings will
    ;; still parse to something. We just test that empty strings are rejected.
    (is (nil? (parse-formula->phenotype "")))))


;;; ============================================================================
;;; Tests - Scoring Parsed Phenotypes
;;; ============================================================================


(deftest test-score-parsed-phenotype
  (testing "parsed phenotypes can be scored without errors"
    (let [run-args (make-test-run-args)
          run-config (make-test-run-config)
          score-fn (partial ops/score-fn run-args run-config)]

      ;; Test scoring "Sin(x)" on sin(x) data - should have good score
      (let [pheno (parse-formula->phenotype "Sin(x)")]
        (is (some? pheno) "Should parse Sin(x)")
        (let [score (score-fn pheno)]
          (is (number? score) "Score should be a number")
          (is (not (Double/isNaN score)) "Score should not be NaN")
          ;; Score for correct formula should be close to 0 (perfect)
          (is (> score -0.1) "Sin(x) should score well on sin(x) data")))

      ;; Test scoring other formulas - they should all produce valid scores
      (doseq [formula ["x" "x^2" "Cos(x)" "x + 1"]]
        (let [pheno (parse-formula->phenotype formula)]
          (is (some? pheno) (str "Should parse: " formula))
          (let [score (score-fn pheno)]
            (is (number? score) (str "Score for " formula " should be a number"))
            (is (not (Double/isNaN score)) (str "Score for " formula " should not be NaN"))))))))


;;; ============================================================================
;;; Tests - Mutating Parsed Phenotypes
;;; ============================================================================


(deftest test-mutate-parsed-phenotype
  (testing "parsed phenotypes can be mutated without errors"
    (let [run-config (make-test-run-config)
          mutations (ops-init/initial-mutations)
          mutation-fn (partial ops/mutation-fn run-config mutations)
          pheno1 (parse-formula->phenotype "Sin(x)")]

      (is (some? pheno1) "Should parse Sin(x)")

      ;; Try multiple mutations to ensure they work
      (dotimes [_ 10]
        (let [mutated (mutation-fn pheno1 pheno1)]
          (is (some? mutated) "Mutation should produce a phenotype")
          (is (some? (:expr mutated)) "Mutated should have expr")
          (is (= ops-common/sym-x (:sym mutated)) "Mutated should have sym-x"))))))


;;; ============================================================================
;;; Tests - Mini GA Evolution
;;; ============================================================================


(deftest test-mini-evolution-with-seeded-phenotypes
  (testing "GA evolution works with seeded phenotypes"
    (let [run-args (make-test-run-args)
          run-config (make-test-run-config)
          mutations (ops-init/initial-mutations)

          ;; Create seeded population
          seed-formulas ["Sin(x)" "x" "Cos(x)" "x^2"]
          initial-pop (seeded-phenotypes seed-formulas 0.2 10)

          _ (is (= 10 (count initial-pop)) "Should have 10 phenotypes")

          ;; Setup GA
          score-fn (partial ops/score-fn run-args run-config)
          mutation-fn (partial ops/mutation-fn run-config mutations)
          crossover-fn (partial ops/crossover-fn run-config mutations)

          ga-state (ga/initialize initial-pop score-fn mutation-fn crossover-fn)]

      ;; Run 3 iterations of evolution
      (let [final-state (loop [state ga-state
                               i 3]
                          (if (zero? i)
                            state
                            (let [evolved (ga/evolve state)]
                              (is (some? (:pop evolved)) "Should have population after evolve")
                              (is (pos? (count (:pop evolved))) "Population should not be empty")
                              ;; Check all phenotypes have valid structure
                              (doseq [p (:pop evolved)]
                                (is (some? (:expr p)) "Each phenotype should have expr")
                                (is (= ops-common/sym-x (:sym p)) "Each phenotype should have sym-x"))
                              (recur evolved (dec i)))))]

        (is (some? final-state) "Evolution should complete")
        (is (pos? (count (:pop final-state))) "Final population should not be empty")))))


(deftest test-evolution-with-all-seeded
  (testing "GA evolution works when 100% seeded (no fresh phenotypes)"
    (let [run-args (make-test-run-args)
          run-config (make-test-run-config)
          mutations (ops-init/initial-mutations)

          ;; Create 100% seeded population
          seed-formulas ["Sin(x)" "Cos(x)" "x + 1"]
          initial-pop (seeded-phenotypes seed-formulas 0.0 6)  ; 0% fresh

          _ (is (= 6 (count initial-pop)) "Should have 6 phenotypes")

          ;; All should be from seeds
          _ (doseq [p initial-pop]
              (is (some? (:expr p)) "Each phenotype should have expr")
              (is (= ops-common/sym-x (:sym p)) "Each phenotype should have sym-x"))

          score-fn (partial ops/score-fn run-args run-config)
          mutation-fn (partial ops/mutation-fn run-config mutations)
          crossover-fn (partial ops/crossover-fn run-config mutations)

          ga-state (ga/initialize initial-pop score-fn mutation-fn crossover-fn)]

      ;; Run evolution
      (let [evolved1 (ga/evolve ga-state)
            evolved2 (ga/evolve evolved1)]
        (is (pos? (count (:pop evolved2))) "Should have population after 2 iterations")))))


#_(deftest test-evolution-preserves-valid-phenotypes
  (testing "evolution doesn't produce corrupted phenotypes"
    (let [run-args (make-test-run-args)
          run-config (make-test-run-config)
          mutations (ops-init/initial-mutations)

          seed-formulas ["Sin(x) + x" "x^2 - 1"]
          initial-pop (seeded-phenotypes seed-formulas 0.0 4)

          score-fn (partial ops/score-fn run-args run-config)
          mutation-fn (partial ops/mutation-fn run-config mutations)
          crossover-fn (partial ops/crossover-fn run-config mutations)

          ga-state (ga/initialize initial-pop score-fn mutation-fn crossover-fn)]

      ;; Run 5 iterations and check for corruption
      (loop [state ga-state
             i 5]
        (when (pos? i)
          (let [evolved (ga/evolve state)]
            ;; Check each phenotype for corruption
            (doseq [p (:pop evolved)]
              (is (some? (:expr p)) "Phenotype should have expr")
              (is (= ops-common/sym-x (:sym p)) "Phenotype sym should be sym-x")
              ;; The expression string should not contain "Function(" or "Hold("
              (let [expr-str (str (:expr p))]
                (is (not (.contains expr-str "Function("))
                    (str "Expression should not contain Function(: " expr-str))
                (is (not (.contains expr-str "Hold("))
                    (str "Expression should not contain Hold(: " expr-str))))
            (recur evolved (dec i))))))))


#_(deftest test-longer-evolution-with-realistic-formulas
  (testing "longer evolution with formulas similar to solver output"
    (let [;; More data points like real solver
          xs (mapv #(* % 0.1) (range 20))
          ys (mapv #(+ (Math/sin %) (* 0.5 %)) xs)
          xs-exprs (ops-common/doubles->exprs xs)
          run-args {:input-xs-list  (ops-common/exprs->exprs-list xs-exprs)
                    :input-xs-count (count xs)
                    :input-xs-vec   xs
                    :input-ys-vec   ys
                    :input-ys-arr   (double-array ys)}
          run-config {:max-leafs 40 :iters 20}
          mutations (ops-init/initial-mutations)

          ;; Use formulas that look like solver output
          seed-formulas ["Sin(x)+x/2"
                         "x+Sin(x)"
                         "0.5*x+Sin(x)"
                         "Sin(x)+0.5*x"
                         "x/2+Sin(x)"]
          initial-pop (seeded-phenotypes seed-formulas 0.2 20)

          _ (log/error "Created initial pop of" (count initial-pop) "phenotypes")
          _ (doseq [p (take 3 initial-pop)]
              (log/error "  Initial phenotype:" (str (:expr p)) "sym:" (:sym p)))

          score-fn (partial ops/score-fn run-args run-config)
          mutation-fn (partial ops/mutation-fn run-config mutations)
          crossover-fn (partial ops/crossover-fn run-config mutations)

          ga-state (ga/initialize initial-pop score-fn mutation-fn crossover-fn)]

      ;; Run 20 iterations like a real solver would
      (loop [state ga-state
             i 20]
        (when (pos? i)
          (let [evolved (ga/evolve state)]
            ;; Check for corruption
            (doseq [p (:pop evolved)]
              (let [expr-str (str (:expr p))]
                (when (.contains expr-str "Function(")
                  (log/error "CORRUPTION DETECTED at iteration" (- 20 i) ":" expr-str))
                (is (not (.contains expr-str "Function("))
                    (str "Expression corrupted with Function(: " expr-str))
                (is (not (.contains expr-str "Hold("))
                    (str "Expression corrupted with Hold(: " expr-str))))
            (recur evolved (dec i)))))

      ;; Verify we completed without throwing
      (is true "Evolution completed without throwing"))))


#_(deftest test-evolution-with-complex-solver-formulas
  (testing "evolution with complex formulas that might come from solver"
    (let [xs (mapv #(* % 0.1) (range 20))
          ys (mapv #(Math/sin %) xs)
          xs-exprs (ops-common/doubles->exprs xs)
          run-args {:input-xs-list  (ops-common/exprs->exprs-list xs-exprs)
                    :input-xs-count (count xs)
                    :input-xs-vec   xs
                    :input-ys-vec   ys
                    :input-ys-arr   (double-array ys)}
          run-config {:max-leafs 40 :iters 10}
          mutations (ops-init/initial-mutations)

          ;; Complex formulas with fractions and nested operations
          seed-formulas ["1/2*Sin(x)+1/10"
                         "Sin(x)-1/100"
                         "1/10+Sin(x)+1/100"
                         "0.538165*Sin(x)"
                         "Sin(x)^(1/2)"
                         "Log(1+x)"
                         "Exp(-x/10)"
                         "Cos(x)+Sin(x)/2"
                         "x^2/10+Sin(x)"
                         "1/(1+x^2)"]
          initial-pop (seeded-phenotypes seed-formulas 0.0 10)]

      (log/error "Testing complex formulas:")
      (doseq [p initial-pop]
        (log/error "  Parsed:" (str (:expr p))))

      ;; Just verify they all parsed correctly
      (is (= 10 (count initial-pop)) "All formulas should parse")
      (doseq [p initial-pop]
        (is (= ops-common/sym-x (:sym p)) "All should have sym-x"))

      ;; Run a few iterations
      (let [score-fn (partial ops/score-fn run-args run-config)
            mutation-fn (partial ops/mutation-fn run-config mutations)
            crossover-fn (partial ops/crossover-fn run-config mutations)
            ga-state (ga/initialize initial-pop score-fn mutation-fn crossover-fn)]

        (loop [state ga-state
               i 10]
          (when (pos? i)
            (let [evolved (ga/evolve state)]
              (doseq [p (:pop evolved)]
                (let [expr-str (str (:expr p))]
                  (when (or (.contains expr-str "Function(")
                            (.contains expr-str "Hold("))
                    (log/error "CORRUPTION at iter" (- 10 i) ":" expr-str))
                  (is (not (.contains expr-str "Function(")) expr-str)
                  (is (not (.contains expr-str "Hold(")) expr-str)))
              (recur evolved (dec i))))))))


#_(deftest test-intensive-evolution-like-real-solver
  (testing "intensive evolution matching real solver settings (100 pop, 50+ iters)"
    (let [;; Use 20 points like the real case
          xs (mapv #(* % 0.1) (range 20))
          ys (mapv #(Math/sin %) xs)
          xs-exprs (ops-common/doubles->exprs xs)
          run-args {:input-xs-list  (ops-common/exprs->exprs-list xs-exprs)
                    :input-xs-count (count xs)
                    :input-xs-vec   xs
                    :input-ys-vec   ys
                    :input-ys-arr   (double-array ys)}
          run-config {:max-leafs 40 :iters 50}
          mutations (ops-init/initial-mutations)

          ;; 10 seed formulas, expanded to 100 pop with 20% fresh
          seed-formulas ["Sin(x)"
                         "0.538165*Sin(x)"
                         "Sin(x)+0.1"
                         "x+Sin(x)"
                         "Sin(x)*Cos(x)"
                         "Sin(x)^2"
                         "Sin(2*x)/2"
                         "Sin(x)+Sin(2*x)/10"
                         "Sin(x)-x/100"
                         "Sin(x)+x^2/100"]
          initial-pop (seeded-phenotypes seed-formulas 0.2 100)

          _ (log/error "Intensive test: pop=" (count initial-pop))

          score-fn (partial ops/score-fn run-args run-config)
          mutation-fn (partial ops/mutation-fn run-config mutations)
          crossover-fn (partial ops/crossover-fn run-config mutations)

          ga-state (ga/initialize initial-pop score-fn mutation-fn crossover-fn)]

      (is (= 100 (count initial-pop)) "Should have 100 phenotypes")

      ;; Run 50 iterations
      (loop [state ga-state
             i 50]
        (when (pos? i)
          (let [evolved (ga/evolve state)]
            ;; Check for corruption
            (doseq [p (:pop evolved)]
              (let [expr-str (str (:expr p))]
                (when (or (.contains expr-str "Function(")
                          (.contains expr-str "Hold("))
                  (log/error "CORRUPTION at iter" (- 50 i) "in pop:" expr-str)
                  (throw (ex-info "Corruption detected!" {:expr expr-str :iter (- 50 i)})))
                (is (not (.contains expr-str "Function(")) expr-str)
                (is (not (.contains expr-str "Hold(")) expr-str)))
            (recur evolved (dec i)))))

      (is true "Intensive evolution completed without corruption")))))


(deftest test-parse-does-not-introduce-function-wrapper
  (testing "parsed phenotypes should not contain Function( in their expression"
    (let [formulas ["Sin(x)"
                    "x^2"
                    "0.538165*Sin(x)"
                    "1/2+x"
                    "Cos(x)+Sin(x)/2"]]
      (doseq [f formulas]
        (let [p (parse-formula->phenotype f)
              expr-str (str (:expr p))]
          (log/error "Parsed formula" f "=> expr:" expr-str)
          (is (not (.contains expr-str "Function("))
              (str "Formula " f " should not have Function( in expr: " expr-str))
          (is (not (.contains expr-str "Hold("))
              (str "Formula " f " should not have Hold( in expr: " expr-str)))))))


(deftest test-phenotype-evaluation-does-not-corrupt
  (testing "evaluating a parsed phenotype does not corrupt it"
    (let [xs [0.0 0.5 1.0 1.5 2.0]
          xs-exprs (ops-common/doubles->exprs xs)
          run-args {:input-xs-list  (ops-common/exprs->exprs-list xs-exprs)
                    :input-xs-count (count xs)}
          p (parse-formula->phenotype "Sin(x)")]

      (log/error "Before eval, expr:" (str (:expr p)))

      ;; Evaluate the phenotype
      (let [result (closyr.ops.eval/eval-vec-pheno p run-args)]
        (log/error "After eval, result:" result)
        (log/error "After eval, expr:" (str (:expr p))))

      ;; The phenotype's expr should not be mutated by evaluation
      (is (not (.contains (str (:expr p)) "Function("))
          "Expr should not contain Function( after eval")
      (is (not (.contains (str (:expr p)) "Hold("))
          "Expr should not contain Hold( after eval"))))


(deftest test-score-function-does-not-corrupt-phenotype
  (testing "scoring a parsed phenotype does not corrupt its expr"
    (let [xs [0.0 0.5 1.0 1.5 2.0]
          ys (mapv #(Math/sin %) xs)
          xs-exprs (ops-common/doubles->exprs xs)
          run-args {:input-xs-list  (ops-common/exprs->exprs-list xs-exprs)
                    :input-xs-count (count xs)
                    :input-xs-vec   xs
                    :input-ys-vec   ys
                    :input-ys-arr   (double-array ys)}
          run-config {:max-leafs 40}
          p (parse-formula->phenotype "Sin(x)")]

      (log/error "Before score, expr:" (str (:expr p)))

      ;; Score the phenotype
      (let [score (ops/score-fn run-args run-config p)]
        (log/error "Score:" score)
        (log/error "After score, expr:" (str (:expr p))))

      ;; The phenotype's expr should not be mutated by scoring
      (is (not (.contains (str (:expr p)) "Function("))
          "Expr should not contain Function( after scoring")
      (is (not (.contains (str (:expr p)) "Hold("))
          "Expr should not contain Hold( after scoring"))))


;;; ============================================================================
;;; Tests - Corrupted Formula Patterns (from real errors)
;;; ============================================================================


(deftest test-corrupted-formula-patterns-are-handled
  (testing "formulas containing corruption patterns are handled gracefully"
    ;; These patterns were seen in real errors and should not crash the parser
    (let [corrupted-formulas ["Hold(Function({x},0.538165<<SHORT>>.6842,0.7895,0.8947,1.0}])"
                               "Function({x}, Sin(x))"
                               "{x} + 1"
                               "<<SHORT>>"
                               "List(1,2,3)"
                               "Hold(x)"]]
      (doseq [formula corrupted-formulas]
        ;; These should either parse to nil or parse to something that doesn't crash
        ;; The key is they shouldn't throw exceptions during parsing
        (let [result (try
                       (parse-formula->phenotype formula)
                       (catch Exception e
                         (log/error "Exception parsing corrupted formula:" formula "-" (.getMessage e))
                         :exception))]
          ;; Parsing corrupted formulas might return nil or a phenotype,
          ;; but should NOT throw exceptions
          (is (not= :exception result)
              (str "Parsing corrupted formula should not throw: " formula)))))))


(deftest test-valid-formulas-accepted
  (testing "normal valid formulas are accepted and parse correctly"
    (let [valid-formulas ["Sin(x)"
                          "x^2 + 2*x + 1"
                          "0.538165*Sin(x)"
                          "Cos(x) + Sin(x)/2"
                          "Log(1+x)"
                          "Exp(-x^2)"]]
      (doseq [formula valid-formulas]
        (let [pheno (parse-formula->phenotype formula)]
          (is (some? pheno) (str "Valid formula should parse: " formula))
          (is (= ops-common/sym-x (:sym pheno)) (str "Should have sym-x: " formula))
          (is (some? (:expr pheno)) (str "Should have expr: " formula)))))))


;;; ============================================================================
;;; Run tests
;;; ============================================================================

(comment
  (run-tests))
