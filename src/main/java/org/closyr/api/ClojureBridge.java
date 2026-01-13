package org.closyr.api;

import clojure.java.api.Clojure;
import clojure.lang.IFn;

/**
 * Bridge class that lazily loads Clojure namespaces and provides
 * access to the symbolic regression solver.
 *
 * This class exists to avoid circular dependencies between Java
 * compilation and Clojure AOT compilation.
 */
public class ClojureBridge {

    private static volatile boolean initialized = false;
    private static IFn findFn;
    private static IFn configFn;

    private ClojureBridge() {
        // Utility class
    }

    /**
     * Initialize the Clojure runtime. Safe to call multiple times.
     */
    public static synchronized void initialize() {
        if (initialized) {
            return;
        }

        // Require the API namespaces
        IFn require = Clojure.var("clojure.core", "require");
        require.invoke(Clojure.read("closyr.api.finder"));
        require.invoke(Clojure.read("closyr.api.types"));

        // Get references to the functions we need
        findFn = Clojure.var("closyr.api.finder", "-find");
        configFn = Clojure.var("closyr.api.types", "config");

        initialized = true;
    }

    /**
     * Find a formula using default configuration.
     */
    public static IFormulaResult find(double[] xs, double[] ys) {
        initialize();
        return (IFormulaResult) findFn.invoke(xs, ys);
    }

    /**
     * Find a formula with custom configuration.
     */
    public static IFormulaResult find(double[] xs, double[] ys, IFormulaConfig config) {
        initialize();
        return (IFormulaResult) findFn.invoke(xs, ys, config);
    }

    /**
     * Create a default configuration.
     */
    public static IFormulaConfig createConfig() {
        initialize();
        return (IFormulaConfig) configFn.invoke();
    }
}
