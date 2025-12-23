package org.closyr.api;

/**
 * Fluent builder for creating IFormulaConfig instances.
 *
 * <p>Usage:</p>
 * <pre>
 *   IFormulaConfig config = FormulaConfigBuilder.builder()
 *       .iterations(50)
 *       .populationSize(200)
 *       .maxLeafs(30)
 *       .build();
 * </pre>
 */
public class FormulaConfigBuilder {

    private int iterations = 20;
    private int populationSize = 100;
    private int maxLeafs = 40;
    private long randomSeed = -1L;

    private FormulaConfigBuilder() {
    }

    /**
     * Create a new builder with default values.
     */
    public static FormulaConfigBuilder builder() {
        return new FormulaConfigBuilder();
    }

    /**
     * Set the number of GA iterations.
     */
    public FormulaConfigBuilder iterations(int iterations) {
        this.iterations = iterations;
        return this;
    }

    /**
     * Set the population size.
     */
    public FormulaConfigBuilder populationSize(int populationSize) {
        this.populationSize = populationSize;
        return this;
    }

    /**
     * Set the maximum expression tree leaf count.
     */
    public FormulaConfigBuilder maxLeafs(int maxLeafs) {
        this.maxLeafs = maxLeafs;
        return this;
    }

    /**
     * Set the random seed for reproducible results.
     * Use -1 for non-deterministic behavior (default).
     */
    public FormulaConfigBuilder randomSeed(long randomSeed) {
        this.randomSeed = randomSeed;
        return this;
    }

    /**
     * Build the configuration.
     */
    public IFormulaConfig build() {
        return new SimpleFormulaConfig(iterations, populationSize, maxLeafs, randomSeed);
    }

    /**
     * Simple implementation of IFormulaConfig.
     */
    private static class SimpleFormulaConfig implements IFormulaConfig {
        private final int iterations;
        private final int populationSize;
        private final int maxLeafs;
        private final long randomSeed;

        SimpleFormulaConfig(int iterations, int populationSize, int maxLeafs, long randomSeed) {
            this.iterations = iterations;
            this.populationSize = populationSize;
            this.maxLeafs = maxLeafs;
            this.randomSeed = randomSeed;
        }

        @Override
        public int getIterations() {
            return iterations;
        }

        @Override
        public int getPopulationSize() {
            return populationSize;
        }

        @Override
        public int getMaxLeafs() {
            return maxLeafs;
        }

        @Override
        public long getRandomSeed() {
            return randomSeed;
        }

        @Override
        public String toString() {
            return "FormulaConfig{iterations=" + iterations +
                    ", populationSize=" + populationSize +
                    ", maxLeafs=" + maxLeafs +
                    ", randomSeed=" + randomSeed + "}";
        }
    }
}
