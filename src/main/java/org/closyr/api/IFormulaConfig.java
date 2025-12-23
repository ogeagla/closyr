package org.closyr.api;

/**
 * Configuration for the symbolic regression solver.
 */
public interface IFormulaConfig {

    /**
     * Get the number of GA iterations to run.
     */
    int getIterations();

    /**
     * Get the population size (number of candidate formulas).
     */
    int getPopulationSize();

    /**
     * Get the maximum number of leaf nodes allowed in expression trees.
     */
    int getMaxLeafs();
}
