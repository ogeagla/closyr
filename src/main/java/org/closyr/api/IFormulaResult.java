package org.closyr.api;

import java.util.List;

/**
 * Represents the result of running the symbolic regression solver.
 */
public interface IFormulaResult {

    /**
     * Get the best solution found.
     */
    IFormulaSolution getBestSolution();

    /**
     * Get all solutions from the final population, sorted by score (best first).
     */
    List<IFormulaSolution> getAllSolutions();

    /**
     * Get the number of GA iterations that were completed.
     */
    int getIterationsDone();

    /**
     * Get the best formula as a string (convenience method).
     * Equivalent to getBestSolution().getFormula().
     */
    default String getBestFormula() {
        IFormulaSolution best = getBestSolution();
        return best != null ? best.getFormula() : null;
    }

    /**
     * Get the best score (convenience method).
     * Equivalent to getBestSolution().getScore().
     */
    default double getBestScore() {
        IFormulaSolution best = getBestSolution();
        return best != null ? best.getScore() : Double.NEGATIVE_INFINITY;
    }
}
