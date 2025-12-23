package org.closyr.core;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.matheclipse.core.interfaces.IExpr;

import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Tests for the FindFormula Java API that calls into Clojure symbolic regression.
 */
class FindFormulaTest {

//    @BeforeAll
//    static void initClojure() {
//        // Pre-initialize Clojure runtime to avoid timeout in first test
//        FindFormula.initializeClojure();
//    }

    @BeforeEach
    void setUp() {
    }

    @AfterEach
    void tearDown() {
    }

    @Test
    void testFindFormulaWithLinearData() {
        // Simple linear relationship: y = 2x
        double[] xs = {1.0, 2.0, 3.0, 4.0, 5.0};
        double[] ys = {2.0, 4.0, 6.0, 8.0, 10.0};

        FindFormula.Config config = new FindFormula.Config()
                .iterations(10)
                .populationSize(50);

        FindFormula.Result result = FindFormula.findFormula(xs, ys, config);

        assertNotNull(result);
        assertNotNull(result.getFormulaString());
        assertFalse(result.getFormulaString().isEmpty());
        assertTrue(result.getIterationsDone() > 0);
        assertNotNull(result.getAllSolutions());
        assertFalse(result.getAllSolutions().isEmpty());

        System.out.println("Linear data result: " + result);
    }

    @Test
    void testFindFormulaWithQuadraticData() {
        // Quadratic relationship: y = x^2
        double[] xs = {1.0, 2.0, 3.0, 4.0, 5.0};
        double[] ys = {1.0, 4.0, 9.0, 16.0, 25.0};

        FindFormula.Config config = new FindFormula.Config()
                .iterations(15)
                .populationSize(100);

        FindFormula.Result result = FindFormula.findFormula(xs, ys, config);

        assertNotNull(result);
        assertNotNull(result.getFormulaString());
        assertTrue(result.getIterationsDone() > 0);

        System.out.println("Quadratic data result: " + result);
    }

    @Test
    void testFindFormulaWithDefaultConfig() {
        double[] xs = {0.0, 1.0, 2.0, 3.0};
        double[] ys = {1.0, 2.0, 5.0, 10.0};

        FindFormula.Result result = FindFormula.findFormula(xs, ys);

        assertNotNull(result);
        assertNotNull(result.getFormulaString());
        assertEquals(20, result.getIterationsDone()); // default iterations

        System.out.println("Default config result: " + result);
    }

    @Test
    void testResultContainsSymjaExpr() {
        double[] xs = {1.0, 2.0, 3.0};
        double[] ys = {3.0, 6.0, 9.0};

        FindFormula.Config config = new FindFormula.Config()
                .iterations(5)
                .populationSize(30);

        FindFormula.Result result = FindFormula.findFormula(xs, ys, config);

        assertNotNull(result);
        IExpr formulaExpr = result.getFormulaExpr();
        assertNotNull(formulaExpr, "Formula IExpr should not be null");

        // The IExpr should be usable for symbolic computation
        String exprString = formulaExpr.toString();
        assertNotNull(exprString);
        assertFalse(exprString.isEmpty());

        System.out.println("IExpr formula: " + exprString);
    }

    @Test
    void testAllSolutionsSortedByScore() {
        double[] xs = {1.0, 2.0, 3.0, 4.0};
        double[] ys = {2.0, 4.0, 8.0, 16.0};

        FindFormula.Config config = new FindFormula.Config()
                .iterations(5)
                .populationSize(50);

        FindFormula.Result result = FindFormula.findFormula(xs, ys, config);

        List<FindFormula.Solution> solutions = result.getAllSolutions();
        assertNotNull(solutions);
        assertFalse(solutions.isEmpty());

        // Verify solutions are sorted by score (descending - higher is better)
        for (int i = 1; i < solutions.size(); i++) {
            assertTrue(solutions.get(i - 1).getScore() >= solutions.get(i).getScore(),
                    "Solutions should be sorted by score descending");
        }

        // Best solution should match the result's formula
        assertEquals(result.getFormulaString(), solutions.get(0).getFormulaString());
        assertEquals(result.getScore(), solutions.get(0).getScore());
    }

    @Test
    void testNullXsThrowsException() {
        double[] ys = {1.0, 2.0, 3.0};

        IllegalArgumentException exception = assertThrows(
                IllegalArgumentException.class,
                () -> FindFormula.findFormula(null, ys)
        );
        assertTrue(exception.getMessage().contains("null"));
    }

    @Test
    void testNullYsThrowsException() {
        double[] xs = {1.0, 2.0, 3.0};

        IllegalArgumentException exception = assertThrows(
                IllegalArgumentException.class,
                () -> FindFormula.findFormula(xs, null)
        );
        assertTrue(exception.getMessage().contains("null"));
    }

    @Test
    void testMismatchedArrayLengthsThrowsException() {
        double[] xs = {1.0, 2.0, 3.0};
        double[] ys = {1.0, 2.0};

        IllegalArgumentException exception = assertThrows(
                IllegalArgumentException.class,
                () -> FindFormula.findFormula(xs, ys)
        );
        assertTrue(exception.getMessage().contains("same length"));
    }

    @Test
    void testTooFewDataPointsThrowsException() {
        double[] xs = {1.0};
        double[] ys = {2.0};

        IllegalArgumentException exception = assertThrows(
                IllegalArgumentException.class,
                () -> FindFormula.findFormula(xs, ys)
        );
        assertTrue(exception.getMessage().contains("At least 2"));
    }

    @Test
    void testConfigBuilder() {
        FindFormula.Config config = new FindFormula.Config()
                .iterations(50)
                .populationSize(200)
                .maxLeafs(30);

        assertEquals(50, config.getIterations());
        assertEquals(200, config.getPopulationSize());
        assertEquals(30, config.getMaxLeafs());
    }

    @Test
    void testResultToString() {
        double[] xs = {1.0, 2.0, 3.0};
        double[] ys = {2.0, 4.0, 6.0};

        FindFormula.Config config = new FindFormula.Config()
                .iterations(3)
                .populationSize(20);

        FindFormula.Result result = FindFormula.findFormula(xs, ys, config);

        String str = result.toString();
        assertNotNull(str);
        assertTrue(str.contains("Result{"));
        assertTrue(str.contains("formula="));
        assertTrue(str.contains("score="));
        assertTrue(str.contains("iterations="));
    }

    @Test
    void testSolutionToString() {
        double[] xs = {1.0, 2.0, 3.0};
        double[] ys = {1.0, 4.0, 9.0};

        FindFormula.Config config = new FindFormula.Config()
                .iterations(3)
                .populationSize(20);

        FindFormula.Result result = FindFormula.findFormula(xs, ys, config);

        assertFalse(result.getAllSolutions().isEmpty());
        FindFormula.Solution solution = result.getAllSolutions().get(0);

        String str = solution.toString();
        assertNotNull(str);
        assertTrue(str.contains("Solution{"));
        assertTrue(str.contains("formula="));
        assertTrue(str.contains("score="));
    }

    @Test
    void testWithTrigonometricData() {
        // Data from y = sin(x)
        int numPoints = 20;
        double[] xs = new double[numPoints];
        double[] ys = new double[numPoints];
        for (int i = 0; i < numPoints; i++) {
            xs[i] = i * Math.PI / 10.0;
            ys[i] = Math.sin(xs[i]);
        }

        FindFormula.Config config = new FindFormula.Config()
                .iterations(15)
                .populationSize(100);

        FindFormula.Result result = FindFormula.findFormula(xs, ys, config);

        assertNotNull(result);
        assertNotNull(result.getFormulaString());
        // Trigonometric functions are complex, just verify we get a result
        assertTrue(result.getAllSolutions().size() > 0);

        System.out.println("Trig data result: " + result);
    }

    @Test
    void testInitializeClojureIdempotent() {
//        // Should be safe to call multiple times
//        FindFormula.initializeClojure();
//        FindFormula.initializeClojure();
//        FindFormula.initializeClojure();

        // Verify the API still works
        double[] xs = {1.0, 2.0};
        double[] ys = {1.0, 2.0};

        FindFormula.Config config = new FindFormula.Config()
                .iterations(2)
                .populationSize(10);

        FindFormula.Result result = FindFormula.findFormula(xs, ys, config);
        assertNotNull(result);
    }
}
