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
    private String[] mutationsWhitelist = null;
    private String[] mutationsBlacklist = null;

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
     * Set a whitelist of mutation labels to use (only these mutations will be used).
     */
    public FormulaConfigBuilder mutationsWhitelist(String... labels) {
        this.mutationsWhitelist = labels;
        return this;
    }

    /**
     * Set a blacklist of mutation labels to exclude (these mutations will not be used).
     */
    public FormulaConfigBuilder mutationsBlacklist(String... labels) {
        this.mutationsBlacklist = labels;
        return this;
    }

    /**
     * Build the configuration.
     */
    public IFormulaConfig build() {
        return new SimpleFormulaConfig(iterations, populationSize, maxLeafs, randomSeed,
                mutationsWhitelist, mutationsBlacklist);
    }

    /**
     * Simple implementation of IFormulaConfig.
     */
    private static class SimpleFormulaConfig implements IFormulaConfig {
        private final int iterations;
        private final int populationSize;
        private final int maxLeafs;
        private final long randomSeed;
        private final String[] mutationsWhitelist;
        private final String[] mutationsBlacklist;

        SimpleFormulaConfig(int iterations, int populationSize, int maxLeafs, long randomSeed,
                           String[] mutationsWhitelist, String[] mutationsBlacklist) {
            this.iterations = iterations;
            this.populationSize = populationSize;
            this.maxLeafs = maxLeafs;
            this.randomSeed = randomSeed;
            this.mutationsWhitelist = mutationsWhitelist;
            this.mutationsBlacklist = mutationsBlacklist;
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
        public String[] getMutationsWhitelist() {
            return mutationsWhitelist;
        }

        @Override
        public String[] getMutationsBlacklist() {
            return mutationsBlacklist;
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
