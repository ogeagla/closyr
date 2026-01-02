(ns closyr.api.finder
  "Main entry point for the Java API.
   This namespace uses gen-class to create a Java-friendly FormulaFinder class."
  (:require
    [closyr.api.types :as types]
    [closyr.ga :as ga]
    [closyr.ops.common :as ops-common]
    [closyr.ops.initialize :as ops-init]
    [closyr.symbolic-regression :as symreg]
    [closyr.util.log :as log]
    [closyr.util.prng :as prng])
  (:import
    (org.closyr.api
      IFormulaConfig
      IFormulaFinder
      IFormulaResult))
  (:gen-class
    :name org.closyr.api.FormulaFinder
    :implements [org.closyr.api.IFormulaFinder]
    :methods [^:static [create [] org.closyr.api.IFormulaFinder]
              ^:static [createConfig [] org.closyr.api.IFormulaConfig]
              ^:static [createConfig [int int int] org.closyr.api.IFormulaConfig]
              ^:static [find ["[D" "[D"] org.closyr.api.IFormulaResult]
              ^:static [find ["[D" "[D" org.closyr.api.IFormulaConfig] org.closyr.api.IFormulaResult]]))


(set! *warn-on-reflection* true)


(defn- array->vec
  "Convert Java array to Clojure vector, handling nil"
  [^"[Ljava.lang.String;" arr]
  (when arr
    (vec arr)))


(defn run-solver
  "Run the symbolic regression solver with the given parameters."
  [xs ys config]
  (let [^doubles xs-arr xs
        ^doubles ys-arr ys
        ^IFormulaConfig cfg config
        xs-vec (vec xs-arr)
        ys-vec (vec ys-arr)
        iterations (.getIterations cfg)
        population-size (.getPopulationSize cfg)
        max-leafs (.getMaxLeafs cfg)
        random-seed (.getRandomSeed cfg)
        adaptive-mode (.isAdaptiveMode cfg)
        quiet-logs (.isQuietLogs cfg)
        whitelist (array->vec (.getMutationsWhitelist cfg))
        blacklist (array->vec (.getMutationsBlacklist cfg))
        initial-muts (if (or whitelist blacklist)
                       (ops-init/filter-mutations {:whitelist whitelist
                                                   :blacklist blacklist})
                       (ops-init/initial-mutations))
        run-config {:initial-phenos (ops-init/initial-phenotypes population-size)
                    :initial-muts   initial-muts
                    :iters          iterations
                    :use-gui?       false
                    :use-flamechart false
                    :max-leafs      max-leafs
                    :random-seed    random-seed
                    :adaptive-mode  adaptive-mode
                    :quiet-logs     quiet-logs
                    :input-xs-exprs (ops-common/doubles->exprs xs-vec)
                    :input-ys-exprs (ops-common/doubles->exprs ys-vec)}

        _ (log/info "API: using" (count initial-muts) "mutations"
                    "adaptive:" adaptive-mode "quiet:" quiet-logs)
        result (symreg/run-find-formula run-config)]

    (types/->formula-result result)))


(defn validate-inputs
  "Validate xs and ys arrays."
  [xs ys]
  (when (nil? xs)
    (throw (IllegalArgumentException. "xs array cannot be null")))
  (when (nil? ys)
    (throw (IllegalArgumentException. "ys array cannot be null")))
  (let [^doubles xs-arr xs
        ^doubles ys-arr ys]
    (when (not= (alength xs-arr) (alength ys-arr))
      (throw (IllegalArgumentException.
               (str "xs and ys arrays must have the same length, got "
                    (alength xs-arr) " and " (alength ys-arr)))))
    (when (< (alength xs-arr) 2)
      (throw (IllegalArgumentException.
               (str "At least 2 data points are required, got " (alength xs-arr)))))))


;; ============================================================================
;; Instance methods (IFormulaFinder implementation)
;; ============================================================================

(defn -findFormula
  "Find a formula that fits the given data points."
  ([this xs ys]
   (-findFormula this xs ys (types/config)))
  ([_ xs ys config]
   (let [^doubles xs-arr xs
         ^doubles ys-arr ys
         ^IFormulaConfig cfg config]
     (validate-inputs xs-arr ys-arr)
     (run-solver xs-arr ys-arr cfg))))


;; ============================================================================
;; Static factory methods
;; ============================================================================

(defn -create
  "Create a new FormulaFinder instance."
  []
  ;; The instance is created by gen-class, we just return it
  ;; This is called via FormulaFinder.create()
  (eval '(org.closyr.api.FormulaFinder.)))


(defn -createConfig
  "Create a configuration with default or custom values."
  ([]
   (types/config))
  ([iterations population-size max-leafs]
   (types/config {:iterations      (int iterations)
                  :population-size (int population-size)
                  :max-leafs       (int max-leafs)})))


(defn -find
  "Static method to find a formula without creating an instance."
  ([xs ys]
   (-find xs ys (types/config)))
  ([xs ys config]
   (let [^doubles xs-arr xs
         ^doubles ys-arr ys
         ^IFormulaConfig cfg config]
     (validate-inputs xs-arr ys-arr)
     (run-solver xs-arr ys-arr cfg))))
