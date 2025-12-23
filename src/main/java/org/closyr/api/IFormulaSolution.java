package org.closyr.api;

import org.matheclipse.core.interfaces.IExpr;

/**
 * Represents a single formula solution from the symbolic regression solver.
 */
public interface IFormulaSolution {

    /**
     * Get the formula as a human-readable string (e.g., "2*x + 1").
     */
    String getFormula();

    /**
     * Get the fitness score. Higher (closer to 0) is better.
     * Scores are typically negative, with 0 being a perfect fit.
     */
    double getScore();

    /**
     * Get the formula as a Symja IExpr for further symbolic computation.
     * May be null if the expression couldn't be extracted.
     */
    IExpr getExpr();

    /**
     * Get the number of leaf nodes in the expression tree.
     * Smaller values indicate simpler formulas.
     */
    int getLeafCount();
}
