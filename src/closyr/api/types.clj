(ns closyr.api.types
  "Java-friendly types for the symbolic regression API.
   These types implement Java interfaces for seamless Java interop."
  (:import
    (org.closyr.api
      IFormulaConfig
      IFormulaResult
      IFormulaSolution)
    (org.matheclipse.core.interfaces
      IExpr)))


(set! *warn-on-reflection* true)


;; ============================================================================
;; FormulaSolution - implements IFormulaSolution
;; ============================================================================

(deftype FormulaSolution [^String formula
                          ^double score
                          ^IExpr expr
                          ^int leaf-count]

  IFormulaSolution

  (getFormula [_] formula)

  (getScore [_] score)

  (getExpr [_] expr)

  (getLeafCount [_] leaf-count)

  Object

  (toString [_]
    (str "FormulaSolution{formula='" formula "', score=" score
         ", leafCount=" leaf-count "}")))


(defn ->formula-solution
  "Create a FormulaSolution from a phenotype map."
  [{:keys [expr score]}]
  (let [^IExpr e expr
        formula-str (if e (str e) "unknown")
        leaf-count (if e (.leafCount e) 0)
        score-val (if (number? score) (double score) Double/NEGATIVE_INFINITY)]
    (FormulaSolution. formula-str score-val e leaf-count)))


;; ============================================================================
;; FormulaResult - implements IFormulaResult
;; ============================================================================

(deftype FormulaResult [^IFormulaSolution best-solution
                        ^java.util.List all-solutions
                        ^int iterations-done]

  IFormulaResult

  (getBestSolution [_] best-solution)

  (getAllSolutions [_] all-solutions)

  (getIterationsDone [_] iterations-done)

  Object

  (toString [_]
    (str "FormulaResult{bestFormula='"
         (when best-solution (.getFormula best-solution))
         "', bestScore="
         (when best-solution (.getScore best-solution))
         ", iterations=" iterations-done
         ", solutionCount=" (count all-solutions) "}")))


(defn ->formula-result
  "Create a FormulaResult from solver output."
  [{:keys [iters-done final-population]}]
  (let [phenotypes (get final-population :pop [])
        ;; Convert all phenotypes to solutions
        solutions (->> phenotypes
                       (map ->formula-solution)
                       ;; Sort by score descending (higher/closer to 0 is better)
                       (sort-by #(.getScore ^IFormulaSolution %) #(compare %2 %1))
                       vec)
        best (first solutions)
        solutions-list (java.util.ArrayList. ^java.util.Collection solutions)]
    (FormulaResult. best solutions-list (int (or iters-done 0)))))


;; ============================================================================
;; FormulaConfig - implements IFormulaConfig
;; ============================================================================

(deftype FormulaConfig [^int iterations
                        ^int population-size
                        ^int max-leafs
                        ^long random-seed
                        ^boolean adaptive-mode
                        ^boolean quiet-logs
                        ^boolean use-eval-cache
                        ^String scoring-method]

  IFormulaConfig

  (getIterations [_] iterations)

  (getPopulationSize [_] population-size)

  (getMaxLeafs [_] max-leafs)

  (getRandomSeed [_] random-seed)

  (isAdaptiveMode [_] adaptive-mode)

  (isQuietLogs [_] quiet-logs)

  (isUseEvalCache [_] use-eval-cache)

  (getScoringMethod [_] scoring-method)

  Object

  (toString [_]
    (str "FormulaConfig{iterations=" iterations
         ", populationSize=" population-size
         ", maxLeafs=" max-leafs
         ", randomSeed=" random-seed
         ", adaptiveMode=" adaptive-mode
         ", quietLogs=" quiet-logs
         ", useEvalCache=" use-eval-cache
         ", scoringMethod=" scoring-method "}")))


(defn config
  "Create a FormulaConfig with the given options.

   Options:
     :iterations      - Number of GA iterations (default: 20)
     :population-size - Population size (default: 100)
     :max-leafs       - Max expression tree leaves (default: 40)
     :random-seed     - Random seed for reproducibility (default: -1, meaning no seed)
     :adaptive-mode   - Enable adaptive mutation rates (default: false)
     :quiet-logs      - Suppress detailed iteration logs (default: false)
     :use-eval-cache  - Enable evaluation caching (default: false)
     :scoring-method  - Scoring method: \"mae-max\", \"log-cosh\", or \"r-squared\" (default: \"mae-max\")"
  ([]
   (config {}))
  ([{:keys [iterations population-size max-leafs random-seed adaptive-mode quiet-logs use-eval-cache scoring-method]
     :or   {iterations      20
            population-size 100
            max-leafs       40
            random-seed     -1
            adaptive-mode   false
            quiet-logs      false
            use-eval-cache  false
            scoring-method  "mae-max"}}]
   (FormulaConfig. (int iterations)
                   (int population-size)
                   (int max-leafs)
                   (long random-seed)
                   (boolean adaptive-mode)
                   (boolean quiet-logs)
                   (boolean use-eval-cache)
                   (str scoring-method))))


(defn config->map
  "Convert an IFormulaConfig to a Clojure map."
  [^IFormulaConfig cfg]
  {:iterations      (.getIterations cfg)
   :population-size (.getPopulationSize cfg)
   :max-leafs       (.getMaxLeafs cfg)
   :random-seed     (.getRandomSeed cfg)
   :adaptive-mode   (.isAdaptiveMode cfg)
   :quiet-logs      (.isQuietLogs cfg)
   :use-eval-cache  (.isUseEvalCache cfg)
   :scoring-method  (.getScoringMethod cfg)})
