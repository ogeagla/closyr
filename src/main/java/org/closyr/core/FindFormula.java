package org.closyr.core;

import clojure.java.api.Clojure;
import clojure.lang.IFn;
import clojure.lang.IPersistentMap;
import clojure.lang.Keyword;
import clojure.lang.PersistentVector;
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
import java.util.Map;


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
 */
public class FindFormula extends AbstractFunctionOptionEvaluator {

    private static volatile boolean clojureInitialized = false;
    private static IFn runAppWithoutGuiFn;
    private static IFn initialPhenotypesFn;
    private static IFn initialMutationsFn;
    private static IFn doublesToExprsFn;
    private static IFn runFindFormulaFn;

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

        public int getIterations() { return iterations; }
        public int getPopulationSize() { return populationSize; }
        public int getMaxLeafs() { return maxLeafs; }
    }

    public FindFormula() {
        // empty constructor for Symja
    }

    /**
     * Initialize the Clojure runtime and load required namespaces.
     * This is called automatically but can be called explicitly to warm up.
     */
    public static synchronized void initializeClojure() {
        if (clojureInitialized) {
            return;
        }

        // Require the necessary Clojure namespaces
        IFn require = Clojure.var("clojure.core", "require");
        require.invoke(Clojure.read("closyr.symbolic-regression"));
        require.invoke(Clojure.read("closyr.ops.initialize"));
        require.invoke(Clojure.read("closyr.ops.common"));

        // Get references to the Clojure functions we need
        runAppWithoutGuiFn = Clojure.var("closyr.symbolic-regression", "run-app-without-gui");
        runFindFormulaFn = Clojure.var("closyr.symbolic-regression", "run-find-formula");
        initialPhenotypesFn = Clojure.var("closyr.ops.initialize", "initial-phenotypes");
        initialMutationsFn = Clojure.var("closyr.ops.initialize", "initial-mutations");
        doublesToExprsFn = Clojure.var("closyr.ops.common", "doubles->exprs");

        clojureInitialized = true;
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
        if (xs == null || ys == null) {
            throw new IllegalArgumentException("xs and ys arrays cannot be null");
        }
        if (xs.length != ys.length) {
            throw new IllegalArgumentException("xs and ys arrays must have the same length");
        }
        if (xs.length < 2) {
            throw new IllegalArgumentException("At least 2 data points are required");
        }

        initializeClojure();

        // Convert Java arrays to Clojure vectors
        List<Double> xsList = new ArrayList<>(xs.length);
        List<Double> ysList = new ArrayList<>(ys.length);
        for (int i = 0; i < xs.length; i++) {
            xsList.add(xs[i]);
            ysList.add(ys[i]);
        }
        PersistentVector xsVec = PersistentVector.create(xsList);
        PersistentVector ysVec = PersistentVector.create(ysList);

        // Convert to IExpr vectors
        Object xsExprs = doublesToExprsFn.invoke(xsVec);
        Object ysExprs = doublesToExprsFn.invoke(ysVec);

        // Create initial population and mutations
        Object initialPhenos = initialPhenotypesFn.invoke(config.getPopulationSize());
        Object initialMuts = initialMutationsFn.invoke();

        // Build the run configuration map
        Keyword initialPhenosKey = Keyword.intern("initial-phenos");
        Keyword initialMutsKey = Keyword.intern("initial-muts");
        Keyword itersKey = Keyword.intern("iters");
        Keyword useGuiKey = Keyword.intern("use-gui?");
        Keyword useFlamechartKey = Keyword.intern("use-flamechart");
        Keyword inputXsExprsKey = Keyword.intern("input-xs-exprs");
        Keyword inputYsExprsKey = Keyword.intern("input-ys-exprs");
        Keyword maxLeafsKey = Keyword.intern("max-leafs");

        IFn hashMap = Clojure.var("clojure.core", "hash-map");
        Object runConfig = hashMap.invoke(
                initialPhenosKey, initialPhenos,
                initialMutsKey, initialMuts,
                itersKey, config.getIterations(),
                useGuiKey, false,
                useFlamechartKey, false,
                maxLeafsKey, config.getMaxLeafs(),
                inputXsExprsKey, xsExprs,
                inputYsExprsKey, ysExprs
        );

        // Run the solver
        Object result = runFindFormulaFn.invoke(runConfig);

        // Extract results from the Clojure map
        return extractResult(result);
    }

    @SuppressWarnings("unchecked")
    private static Result extractResult(Object result) {
        if (!(result instanceof IPersistentMap)) {
            throw new RuntimeException("Unexpected result type from Clojure: " + result.getClass());
        }

        IPersistentMap resultMap = (IPersistentMap) result;

        Keyword itersDoneKey = Keyword.intern("iters-done");
        Keyword finalPopulationKey = Keyword.intern("final-population");
        Keyword popKey = Keyword.intern("pop");
        Keyword exprKey = Keyword.intern("expr");
        Keyword scoreKey = Keyword.intern("score");

        int itersDone = ((Number) resultMap.valAt(itersDoneKey)).intValue();
        IPersistentMap finalPopulation = (IPersistentMap) resultMap.valAt(finalPopulationKey);
        Object pop = finalPopulation.valAt(popKey);

        List<Solution> allSolutions = new ArrayList<>();

        if (pop instanceof Iterable) {
            for (Object phenotype : (Iterable<?>) pop) {
                if (phenotype instanceof IPersistentMap) {
                    IPersistentMap phenoMap = (IPersistentMap) phenotype;
                    Object exprObj = phenoMap.valAt(exprKey);
                    Object scoreObj = phenoMap.valAt(scoreKey);

                    IExpr expr = (exprObj instanceof IExpr) ? (IExpr) exprObj : null;
                    String formulaStr = (expr != null) ? expr.toString() : "unknown";
                    double score = (scoreObj instanceof Number) ? ((Number) scoreObj).doubleValue() : Double.NEGATIVE_INFINITY;

                    allSolutions.add(new Solution(formulaStr, score, expr));
                }
            }
        }

        // Sort by score (descending - higher/closer to 0 is better)
        allSolutions.sort((a, b) -> Double.compare(b.getScore(), a.getScore()));

        Solution best = allSolutions.isEmpty() ? new Solution("x", Double.NEGATIVE_INFINITY, null) : allSolutions.get(0);

        return new Result(
                best.getFormulaString(),
                best.getScore(),
                best.getFormulaExpr(),
                itersDone,
                allSolutions
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
                // Substitute the Symja variable for our internal 'x'
                // The formula uses a dummy symbol 'x', we need to map it to the user's variable
                return formulaExpr;
            }
        } catch (Exception e) {
            System.out.println("FindFormula: " + e.getMessage());
//            engine.printMessage("FindFormula: " + e.getMessage());
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
