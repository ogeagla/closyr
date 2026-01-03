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

    /**
     * Get the random seed for reproducible results.
     * A value of -1 means no seed is set (non-deterministic).
     */
    default long getRandomSeed() {
        return -1L;
    }

    /**
     * Get the whitelist of mutation labels to use.
     * If null or empty, all mutations are used (subject to blacklist).
     */
    default String[] getMutationsWhitelist() {
        return null;
    }

    /**
     * Get the blacklist of mutation labels to exclude.
     * If null or empty, no mutations are excluded.
     */
    default String[] getMutationsBlacklist() {
        return null;
    }

    /**
     * Check if adaptive mutation mode is enabled.
     * When enabled, mutation rates adjust dynamically based on population diversity.
     */
    default boolean isAdaptiveMode() {
        return false;
    }

    /**
     * Check if quiet logging mode is enabled.
     * When enabled, detailed iteration logs are suppressed.
     */
    default boolean isQuietLogs() {
        return false;
    }

    /**
     * Check if evaluation cache is enabled.
     * When enabled, expression evaluation results are cached to avoid redundant calculations.
     */
    default boolean isUseEvalCache() {
        return false;
    }
}
