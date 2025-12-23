package org.closyr.api;

/**
 * Main interface for finding formulas via symbolic regression.
 */
public interface IFormulaFinder {

    /**
     * Find a formula that fits the given data points.
     *
     * @param xs array of x values (independent variable)
     * @param ys array of y values (dependent variable, same length as xs)
     * @return the result containing the best formula and all solutions
     */
    IFormulaResult findFormula(double[] xs, double[] ys);

    /**
     * Find a formula with custom configuration.
     *
     * @param xs     array of x values
     * @param ys     array of y values
     * @param config solver configuration
     * @return the result containing the best formula and all solutions
     */
    IFormulaResult findFormula(double[] xs, double[] ys, IFormulaConfig config);
}
