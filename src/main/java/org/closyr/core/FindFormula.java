package org.closyr.core;

import org.closyr.api.ClojureBridge;
import org.closyr.api.FormulaConfigBuilder;
import org.closyr.api.IFormulaConfig;
import org.closyr.api.IFormulaResult;
import org.closyr.api.IFormulaSolution;
import org.matheclipse.core.eval.EvalEngine;
import org.matheclipse.core.eval.interfaces.AbstractFunctionOptionEvaluator;
import org.matheclipse.core.eval.interfaces.IFunctionEvaluator;
import org.matheclipse.core.expression.F;
import org.matheclipse.core.expression.ImplementationStatus;
import org.matheclipse.core.interfaces.IAST;
import org.matheclipse.core.interfaces.IExpr;
import org.matheclipse.core.interfaces.ISymbol;

import java.util.ArrayList;
import java.util.List;


/**
 * FindFormula - A Symja function that uses genetic algorithm-based symbolic regression
 * to find a formula that best fits the given data points.
 *
 * <p>Usage from Symja:</p>
 * <pre>
 *   FindFormula[{{x1, y1}, {x2, y2}, ...}, x]
 *   FindFormula[{{x1, y1}, {x2, y2}, ...}, x, n]
 *   FindFormula[{{x1, y1}, {x2, y2}, ...}, x, n, All]
 * </pre>
 *
 * <p>Usage from Java:</p>
 * <pre>
 *   double[] xs = {1.0, 2.0, 3.0, 4.0, 5.0};
 *   double[] ys = {2.0, 4.0, 6.0, 8.0, 10.0};
 *   FindFormula.Result result = FindFormula.findFormula(xs, ys);
 *   System.out.println("Best formula: " + result.getFormulaString());
 *   System.out.println("Score: " + result.getScore());
 * </pre>
 *
 * <p>Or use the newer API directly:</p>
 * <pre>
 *   import org.closyr.api.*;
 *   IFormulaResult result = FormulaFinder.find(xs, ys);
 * </pre>
 */
public class FindFormula extends AbstractFunctionOptionEvaluator {

    /**
     * Result of a symbolic regression run.
     */
    public static class Result {
        private final String formulaString;
        private final double score;
        private final IExpr formulaExpr;
        private final int iterationsDone;
        private final List<Solution> allSolutions;

        public Result(String formulaString, double score, IExpr formulaExpr,
                      int iterationsDone, List<Solution> allSolutions) {
            this.formulaString = formulaString;
            this.score = score;
            this.formulaExpr = formulaExpr;
            this.iterationsDone = iterationsDone;
            this.allSolutions = allSolutions;
        }

        /** The formula as a human-readable string (e.g., "2*x + 1") */
        public String getFormulaString() {
            return formulaString;
        }

        /** The fitness score (higher/closer to 0 is better, negative values) */
        public double getScore() {
            return score;
        }

        /** The formula as a Symja IExpr for further symbolic computation */
        public IExpr getFormulaExpr() {
            return formulaExpr;
        }

        /** Number of GA iterations completed */
        public int getIterationsDone() {
            return iterationsDone;
        }

        /** All solutions from the final population, sorted by score (best first) */
        public List<Solution> getAllSolutions() {
            return allSolutions;
        }

        @Override
        public String toString() {
            return "Result{formula='" + formulaString + "', score=" + score +
                    ", iterations=" + iterationsDone + "}";
        }
    }

    /**
     * A single solution (phenotype) from the genetic algorithm.
     */
    public static class Solution {
        private final String formulaString;
        private final double score;
        private final IExpr formulaExpr;

        public Solution(String formulaString, double score, IExpr formulaExpr) {
            this.formulaString = formulaString;
            this.score = score;
            this.formulaExpr = formulaExpr;
        }

        public String getFormulaString() {
            return formulaString;
        }

        public double getScore() {
            return score;
        }

        public IExpr getFormulaExpr() {
            return formulaExpr;
        }

        @Override
        public String toString() {
            return "Solution{formula='" + formulaString + "', score=" + score + "}";
        }
    }

    /**
     * Configuration for the symbolic regression solver.
     */
    public static class Config {
        private int iterations = 20;
        private int populationSize = 100;
        private int maxLeafs = 40;
        private long randomSeed = -1L;
        private String[] mutationsWhitelist = null;
        private String[] mutationsBlacklist = null;
        private boolean adaptiveMode = false;
        private boolean quietLogs = false;
        private boolean useEvalCache = false;
        private String scoringMethod = "mae-max";

        public Config() {}

        /** Number of GA iterations (default: 20) */
        public Config iterations(int iterations) {
            this.iterations = iterations;
            return this;
        }

        /** Population size (default: 100) */
        public Config populationSize(int populationSize) {
            this.populationSize = populationSize;
            return this;
        }

        /** Maximum number of leaves in expression tree (default: 40) */
        public Config maxLeafs(int maxLeafs) {
            this.maxLeafs = maxLeafs;
            return this;
        }

        /** Random seed for reproducible results (default: -1, meaning non-deterministic) */
        public Config randomSeed(long randomSeed) {
            this.randomSeed = randomSeed;
            return this;
        }

        /** Whitelist of mutation labels to use (only these mutations will be used) */
        public Config mutationsWhitelist(String... labels) {
            this.mutationsWhitelist = labels;
            return this;
        }

        /** Blacklist of mutation labels to exclude (these mutations will not be used) */
        public Config mutationsBlacklist(String... labels) {
            this.mutationsBlacklist = labels;
            return this;
        }

        /** Enable adaptive mutation rates (default: false) */
        public Config adaptiveMode(boolean adaptiveMode) {
            this.adaptiveMode = adaptiveMode;
            return this;
        }

        /** Enable quiet logging mode (default: false) */
        public Config quietLogs(boolean quietLogs) {
            this.quietLogs = quietLogs;
            return this;
        }

        /** Enable evaluation cache (default: false) */
        public Config useEvalCache(boolean useEvalCache) {
            this.useEvalCache = useEvalCache;
            return this;
        }

        /**
         * Set the scoring method for fitness evaluation.
         * Valid values: "mae-max" (default), "log-cosh", "r-squared".
         * All methods return 0 for perfect fit, negative for worse fits.
         */
        public Config scoringMethod(String scoringMethod) {
            this.scoringMethod = scoringMethod;
            return this;
        }

        public int getIterations() { return iterations; }
        public int getPopulationSize() { return populationSize; }
        public int getMaxLeafs() { return maxLeafs; }
        public long getRandomSeed() { return randomSeed; }
        public String[] getMutationsWhitelist() { return mutationsWhitelist; }
        public String[] getMutationsBlacklist() { return mutationsBlacklist; }
        public boolean isAdaptiveMode() { return adaptiveMode; }
        public boolean isQuietLogs() { return quietLogs; }
        public boolean isUseEvalCache() { return useEvalCache; }
        public String getScoringMethod() { return scoringMethod; }

        /** Convert to IFormulaConfig for the new API */
        IFormulaConfig toFormulaConfig() {
            return FormulaConfigBuilder.builder()
                    .iterations(iterations)
                    .populationSize(populationSize)
                    .maxLeafs(maxLeafs)
                    .randomSeed(randomSeed)
                    .mutationsWhitelist(mutationsWhitelist)
                    .mutationsBlacklist(mutationsBlacklist)
                    .adaptiveMode(adaptiveMode)
                    .quietLogs(quietLogs)
                    .useEvalCache(useEvalCache)
                    .scoringMethod(scoringMethod)
                    .build();
        }
    }

    public FindFormula() {
        // empty constructor for Symja
    }

    /**
     * Find a formula that fits the given x,y data points using default settings.
     *
     * @param xs array of x values
     * @param ys array of y values (same length as xs)
     * @return Result containing the best formula found
     */
    public static Result findFormula(double[] xs, double[] ys) {
        return findFormula(xs, ys, new Config());
    }

    /**
     * Find a formula that fits the given x,y data points with custom configuration.
     *
     * @param xs     array of x values
     * @param ys     array of y values (same length as xs)
     * @param config configuration for the solver
     * @return Result containing the best formula found
     */
    public static Result findFormula(double[] xs, double[] ys, Config config) {
        // Delegate to the new API via ClojureBridge
        IFormulaConfig formulaConfig = config.toFormulaConfig();
        IFormulaResult apiResult = ClojureBridge.find(xs, ys, formulaConfig);

        // Convert to legacy Result format
        return convertResult(apiResult);
    }

    /**
     * Convert from the new API result to the legacy Result format.
     */
    private static Result convertResult(IFormulaResult apiResult) {
        List<Solution> solutions = new ArrayList<>();

        for (IFormulaSolution apiSolution : apiResult.getAllSolutions()) {
            solutions.add(new Solution(
                    apiSolution.getFormula(),
                    apiSolution.getScore(),
                    apiSolution.getExpr()
            ));
        }

        IFormulaSolution best = apiResult.getBestSolution();
        String bestFormula = best != null ? best.getFormula() : "x";
        double bestScore = best != null ? best.getScore() : Double.NEGATIVE_INFINITY;
        IExpr bestExpr = best != null ? best.getExpr() : null;

        return new Result(
                bestFormula,
                bestScore,
                bestExpr,
                apiResult.getIterationsDone(),
                solutions
        );
    }

    // ==================== Symja Integration ====================

    /**
     * Symja function evaluator implementation.
     * Called when FindFormula[data, x] is evaluated in Symja.
     */
    @Override
    public IExpr evaluate(IAST ast, final int argSize, final IExpr[] options,
                          final EvalEngine engine, IAST originalAST) {
        IExpr data = ast.arg1();
        IExpr x = ast.arg2();

        if (!x.isVariable()) {
            return F.NIL;
        }

        int[] isMatrix = data.isMatrix();
        if (isMatrix == null || isMatrix[1] != 2 || !data.isList()) {
            return F.NIL;
        }

        IAST matrix = (IAST) data;
        double[][] doubleMatrix = matrix.toDoubleMatrix();
        if (doubleMatrix == null) {
            return F.NIL;
        }

        // Extract x and y values from the matrix
        double[] xs = new double[doubleMatrix.length];
        double[] ys = new double[doubleMatrix.length];
        for (int i = 0; i < doubleMatrix.length; i++) {
            xs[i] = doubleMatrix[i][0];
            ys[i] = doubleMatrix[i][1];
        }

        // Parse optional arguments
        Config config = new Config();
        if (argSize > 2) {
            int n = ast.arg3().toIntDefault(-1);
            if (n > 0) {
                config.iterations(n);
            }
        }

        try {
            Result result = findFormula(xs, ys, config);
            IExpr formulaExpr = result.getFormulaExpr();

            if (formulaExpr != null) {
                return formulaExpr;
            }
        } catch (Exception e) {
            System.err.println("FindFormula: " + e.getMessage());
        }

        return F.NIL;
    }

    @Override
    public int status() {
        return ImplementationStatus.EXPERIMENTAL;
    }

    @Override
    public int[] expectedArgSize(IAST ast) {
        return IFunctionEvaluator.ARGS_2_4;
    }

    @Override
    public void setUp(final ISymbol newSymbol) {
        super.setUp(newSymbol);
    }
}
