# closyr

[![Clojure CI](https://github.com/ogeagla/closyr/actions/workflows/clojure.yml/badge.svg?branch=master)](https://github.com/ogeagla/closyr/actions/workflows/clojure.yml)

![icon_v5_qtr.png](resources%2Ficons%2Ficon_v5_qtr.png)

A Symbolic Regression tool to search for symbolic expressions which minimize residuals to an objective, written in Clojure.

![gui_evolution_sample_Peek 2024-02-14 08-21.gif](screenshots%2Fgui_evolution_sample_Peek%202024-02-14%2008-21.gif)

Warning: This is experimental software, with an unstable API. Expect breaking changes prior to a major release.

- Draw an objective function, select a built-in dataset, or provide a CSV
- Select number of points, iterations
- See progress 
  - Chart of best fitting function
  - Chart of loss over time
  - A selectable text version of the function

## Installation


Run the application from binary, download a release JAR and see the Usage section below.  
This project is not yet available in a JAR repository (though I'd use Clojars if enough requests).


Or, you can run the app from source, using Clojure development tools (`leiningen`) in this project:

    $ lein deps/repl/run/uberjar...

## Usage

Use the application via GUI or in the terminal. 

There are 4 ways to run the application.  See next section for details about the options you can provide when running the app.


### Run the provided release JAR

Requirements: Java, Leiningen

    $ lein uberjar
    $ java -jar closyr-0.1.0-SNAPSHOT-standalone.jar ...options here...


### Lein Run

Requirements: Java, Leiningen


    $ lein run

Or headless (just in the terminal), where you can specify options and input data:

    $ lein run -t -p 25 -i 5 -x 0,1,2,3,4,5,6 -y 1,2,30,4,5,6,10

Which is the same as:

    $ lein run --headless --population 1000 --iterations 200 --xs 0,1,2,3,4,5,6 --ys 1,2,30,4,5,6,10 

###  In Clojure REPL

Requirements: Java, Leiningen (I will provide a deps file if enough interest)


    (require '[closyr.symbolic-regression :as symreg])
    (symreg/run-app-with-gui)

### Build and run JAR

Requirements: Java, Leiningen


    $ lein uberjar
    $ java -jar target/uberjar/closyr-0.1.0-SNAPSHOT-standalone.jar

You can also provide the same command-line options to `java` command, like:

    $ java -jar target/uberjar/closyr-0.1.0-SNAPSHOT-standalone.jar -t -p 25 -i 5 -x 0,1,2,3,4,5,6 -y 1,2,30,4,5,6,10

## Options
| Short, Long Option      | Required?       | Example | Default | Description                                                                                                                   |
|-------------------------|-----------------|---------|---------|-------------------------------------------------------------------------------------------------------------------------------|
| `-t`,`--headless`       | no              | `-t`    | `false` | run without GUI, in terminal only                                                                                             |
| `-c`,`--use-flamechart` | no              | `-c`    | `false` | run with flamecharts, run then visit http://localhost:54321/flames.svg                                                        |
| `-p`,`--population`     | no              | `100`   | `20`    | size of population which will evolve; the number of functions we create and modify                                            |
| `-i`,`--iterations`     | no              | `50`    | `10`    | number of iterations to run for                                                                                               |
| `-l`,`--max-leafs`      | no              | `40`    | `40`    | max number of AST tree leafs in candidate functions                                                                           |
| `-x`,`--xs`             | no, unless `ys` | `1,3,4` | random  | the xs for the points in the dataset to fit against; if provided, must also provide `ys` and be the same count                |
| `-y`,`--ys`             | no, unless `xs` | `2,4,8` | random  | the ys for the points in the dataset to fit against; if provided, must also provide `xs` and be the same count                |
| `-f`,`--infile`         | no              | `f.csv` |         | A CSV file. Contains either 2 columns without titles in first row, or has columns `x` and `y` to be used as objective dataset |
| `-s`,`--seed`           | no              | `42`    |         | Random seed for reproducible results. **Warning:** Enables deterministic mode which disables CPU parallelism                  |
| `-w`,`--mutations-whitelist` | no         | `+Sin,-Sin` |     | Comma-separated list of mutation labels to use (only these mutations will be applied)                                         |
| `-b`,`--mutations-blacklist` | no         | `Derivative` |    | Comma-separated list of mutation labels to exclude                                                                            |

### Reproducible Results with Random Seed

You can use the `-s` or `--seed` option to get reproducible results:

    $ lein run -t -p 20 -i 5 -x 1,2,3,4,5 -y 2,4,6,8,10 -s 42

Running the same command with the same seed will produce identical results.

**Warning:** When a random seed is set, the solver enters **deterministic mode** which disables CPU parallelism. This ensures reproducibility but may result in slower execution times for large populations.

### Filtering Mutations with Whitelist/Blacklist

You can control which mutations are used during evolution using whitelist and blacklist options:

```bash
# Only use trigonometric mutations
$ lein run -t -p 100 -i 50 -x 1,2,3,4,5 -y 2,4,6,8,10 -w "+Sin,-Sin,+Cos,-Cos,*Sin,*Cos"

# Exclude derivative and logarithm mutations
$ lein run -t -p 100 -i 50 -x 1,2,3,4,5 -y 2,4,6,8,10 -b "Derivative,+Log,-Log"

# Combine both: start with trig mutations, exclude +Sin
$ lein run -t -p 100 -i 50 -x 1,2,3,4,5 -y 2,4,6,8,10 -w "+Sin,-Sin,+Cos,-Cos" -b "+Sin"
```

Common mutation labels include: `Derivative`, `+Sin`, `-Sin`, `+Cos`, `-Cos`, `*Sin`, `*Cos`, `+Log`, `-Log`, `+Exp`, `-Exp`, `+x`, `-x`, `*x`, `/x`, `+1/2`, `-1/2`, `*2`, `/2`, and many more.

## Example Screenshots

On successful application start, you can start and run a search.  You might see something like this when done:

![gui_done_2024-02-12_08-44.png](screenshots%2Fgui_done_2024-02-12_08-44.png)

An example of using the GUI:

![gui_usage_clip_Peek 2024-02-12 09-03.gif](screenshots%2Fgui_usage_clip_Peek%202024-02-12%2009-03.gif)

## Tests

    $ lein test

Coverage looks like this if you run `lein cloverage`:

![test_coverage_2025-12-29_17-44.png](screenshots/test_coverage_2025-12-29_17-44.png)

## Benchmarks

The project includes benchmark tests using standard symbolic regression test functions from the literature:

### Running Benchmarks

```bash
# Run all benchmark tests
$ lein test :only closyr.benchmark-functions-test

# Run a specific benchmark
$ lein test :only closyr.benchmark-functions-test/nguyen-4-benchmark
```

### Benchmark Functions

| Benchmark | Formula | Domain | Description |
|-----------|---------|--------|-------------|
| **Nguyen-4** | x⁶ + x⁵ + x⁴ + x³ + x² + x | [-1, 1] | High-degree polynomial |
| **Nguyen-5** | sin(x²)·cos(x) - 1 | [-1, 1] | Trigonometric composition |
| **Feynman Lorentz** | 1/√(1 - v²/c²) | [0, 0.95] | Relativistic Lorentz factor |
| **Feynman Wave** | sin(kx - ωt) | [0, 4π] | Traveling wave equation |

These benchmarks are from the [Nguyen benchmark suite](https://gpbenchmarks.org/) and [AI Feynman dataset](https://space.mit.edu/home/tegmark/aifeynman.html), commonly used in symbolic regression research.

### Example Results

Results from running benchmarks with 200 population, 100 iterations, seed=42:

| Benchmark        | Best Score | Time  | Best Formula Found |
|------------------|------------|-------|-------------------|
| Nguyen-4         | -0.91      | 4.91s | `-1/100+Sin(x)+x*(x+1/50*x*Csc(x)*(-1/100+E^(2*(-1/10+E)^x)-11/10*Sin(121.0*x)))` |
| Nguyen-5         | -0.29      | 2.00s | `-9601/10000` |
| Feynman Lorentz  | -0.72      | 2.67s | `1/2+x-Cos(x)+Cos(1/2-x)*(-x^2+0.9*Log(-1/100+3.05997*x^4+Cos(x)))` |
| Feynman Wave     | -0.14      | 2.37s | `-Cos(3/5+x)` |

*Score is negative sum of residuals (closer to 0 is better). Times measured on AMD Ryzen 9.*

**Note:** Benchmarks use deterministic mode (random seed) for reproducibility, which disables parallelism. Production runs without a seed will be faster.

## How It Works

- We use Genetic Algorithms to allow candidate functions of best fit to compete.
- They compete on a computed score on the input data, and we use the sum of residuals to generate the score.
- Evolution consists of mutation and crossover. 
- Mutations act on the function's AST and modify branches, leafs, or the whole tree.  Operations like `+0.1`, `*x`, `/Sin(x)` are applied to functions.
- Crossovers combine two functions' ASTs at a random point, and combine using various operators like `+`, and `*`.

### Java API

You can use closyr as a library from Java or any JVM language. The Java API provides a clean interface layered on top of the Clojure implementation:

```
┌─────────────────────────────────────────────────────────────────────────┐
│                           Your Java Application                         │
└─────────────────────────────────────────────────────────────────────────┘
                                    │
                                    ▼
┌─────────────────────────────────────────────────────────────────────────┐
│                        Java API (org.closyr.api)                        │
│  ┌─────────────────┐  ┌──────────────────┐  ┌────────────────────────┐  │
│  │  FormulaFinder  │  │  FormulaConfig   │  │  IFormulaResult        │  │
│  │  .find(xs, ys)  │  │  .iterations()   │  │  .getBestSolution()    │  │
│  │  .create()      │  │  .populationSize │  │  .getAllSolutions()    │  │
│  │                 │  │  .randomSeed()   │  │  .getIterationsRun()   │  │
│  └─────────────────┘  └──────────────────┘  └────────────────────────┘  │
└─────────────────────────────────────────────────────────────────────────┘
                                    │
                                    ▼
┌─────────────────────────────────────────────────────────────────────────┐
│                    Clojure Implementation Layer                         │
│  ┌─────────────────────────────────────────────────────────────────┐    │
│  │  closyr.api.finder  ──▶  closyr.symbolic-regression             │    │
│  │         │                         │                             │    │
│  │         ▼                         ▼                             │    │
│  │  closyr.api.types        closyr.ga (genetic algorithm)          │    │
│  │                                   │                             │    │
│  │                                   ▼                             │    │
│  │                          closyr.ops (mutations/crossover)       │    │
│  │                                   │                             │    │
│  │                                   ▼                             │    │
│  │                          Symja (symbolic math engine)           │    │
│  └─────────────────────────────────────────────────────────────────┘    │
└─────────────────────────────────────────────────────────────────────────┘
```

#### Java Usage Example

```java
import org.closyr.api.*;

public class Example {
    public static void main(String[] args) {
        // Input data points
        double[] xs = {1.0, 2.0, 3.0, 4.0, 5.0};
        double[] ys = {2.0, 4.0, 6.0, 8.0, 10.0};

        // Simple usage with defaults
        IFormulaResult result = FormulaFinder.find(xs, ys);

        // Or with custom configuration
        IFormulaConfig config = FormulaConfigBuilder.builder()
            .iterations(50)
            .populationSize(100)
            .maxLeafs(40)
            .randomSeed(42)  // Optional: for reproducible results
            .build();

        result = FormulaFinder.find(xs, ys, config);

        // Get the best formula found
        IFormulaSolution best = result.getBestSolution();
        System.out.println("Formula: " + best.getFormula());
        System.out.println("Score: " + best.getScore());
    }
}
```

#### Reproducible Results in Java

Use `randomSeed()` in the config builder for reproducible results:

```java
IFormulaConfig config = FormulaConfigBuilder.builder()
    .iterations(20)
    .populationSize(50)
    .randomSeed(12345)  // Same seed = same results
    .build();
```

**Warning:** Setting a random seed enables **deterministic mode** which disables CPU parallelism. This ensures reproducibility but may result in slower execution.

#### Filtering Mutations in Java

Use `mutationsWhitelist()` and `mutationsBlacklist()` to control which mutations are used:

```java
// Only use trigonometric mutations
IFormulaConfig config = FormulaConfigBuilder.builder()
    .iterations(50)
    .populationSize(100)
    .mutationsWhitelist("+Sin", "-Sin", "+Cos", "-Cos", "*Sin", "*Cos")
    .build();

// Exclude specific mutations
IFormulaConfig config = FormulaConfigBuilder.builder()
    .iterations(50)
    .populationSize(100)
    .mutationsBlacklist("Derivative", "+Log", "-Log")
    .build();

// Or using FindFormula.Config directly
FindFormula.Config config = new FindFormula.Config()
    .iterations(50)
    .populationSize(100)
    .mutationsWhitelist("+Sin", "-Sin", "+Cos", "-Cos")
    .mutationsBlacklist("+Sin");  // Further exclude from whitelist
```

## Roadmap

- [x] CLI options accept a CSV file (GUI already supports this)
- [ ] More tests
- [ ] Use something like ProGuard to shrink the JAR for releases 
  - https://www.guardsquare.com/manual/configuration/examples
  - https://stackoverflow.com/questions/12281365/obfuscating-clojure-uberjars-with-proguard
  - https://github.com/eiffelqiu/obfuscate-clojure-project-demo/tree/master
- [ ] A different frontend.  The current Java Swing frontend does the job, but it's not easy to maintain and it's hard to make look better.
- [ ] Can this be a follow-up to this issue, asking for a symbolic regression tool on the JVM? https://github.com/axkr/symja_android_library/issues/850
- [ ] When I created this and my other symbolic regression tools, I didn't know about the formal field of symbolic regression.  I've since found some great libraries that I should review and apply the lessons to this project: https://github.com/MilesCranmer/PySR


## Architecture

```
┌─────────────────────────────────────────────────────────────────────────────────┐
│                              ENTRY POINTS                                       │
├─────────────────────┬─────────────────────┬─────────────────────────────────────┤
│   closyr.core       │  closyr.api.finder  │  closyr.symbolic-regression         │
│   (CLI -main)       │  (Java API)         │  (Main Orchestrator)                │
└─────────┬───────────┴──────────┬──────────┴──────────────┬──────────────────────┘
          │                      │                         │
          └──────────────────────┼─────────────────────────┘
                                 ▼
┌─────────────────────────────────────────────────────────────────────────────────┐
│                           GENETIC ALGORITHM                                     │
│  ┌─────────────────────────────────────────────────────────────────────────┐    │
│  │                           closyr.ga                                     │    │
│  │            (Selection, Crossover, Mutation Loop)                        │    │
│  └─────────────────────────────────────────────────────────────────────────┘    │
└───────────────────────────────────┬─────────────────────────────────────────────┘
                                    ▼
┌─────────────────────────────────────────────────────────────────────────────────┐
│                         EXPRESSION OPERATIONS                                   │
│  ┌───────────────┐    ┌───────────────┐    ┌───────────────┐                    │
│  │  closyr.ops   │───▶│ ops.modify    │    │ ops.eval      │                    │
│  │   (Facade)    │    │ (Mutations &  │    │ (Evaluate     │                    │
│  │               │───▶│  Crossover)   │    │  Expressions) │                    │
│  └───────┬───────┘    └───────────────┘    └───────────────┘                    │
│          │            ┌───────────────┐    ┌───────────────┐                    │
│          └───────────▶│ ops.initialize│    │ ops.common    │                    │
│                       │ (Population   │    │ (Shared Utils)│                    │
│                       │  Seeding)     │    │               │                    │
│                       └───────────────┘    └───────────────┘                    │
└───────────────────────────────────┬─────────────────────────────────────────────┘
                                    ▼
┌─────────────────────────────────────────────────────────────────────────────────┐
│                         SYMJA (External)                                        │
│              Symbolic Math Engine - AST Manipulation & Evaluation               │
│                    github.com/axkr/symja_android_library                        │
└─────────────────────────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────────────────────────┐
│                              UI LAYER                                           │
│  ┌─────────────────────────┐    ┌─────────────────┐    ┌─────────────────┐      │
│  │     closyr.ui.gui       │    │  closyr.ui.plot │    │ closyr.ui.icons │      │
│  │   (Swing/Seesaw GUI)    │───▶│   (XChart)      │    │                 │      │
│  │   - Sketchpad           │    │   - Best Fn     │    │                 │      │
│  │   - Controls            │    │   - Scores      │    │                 │      │
│  │   - Function Display    │    │                 │    │                 │      │
│  └─────────────────────────┘    └─────────────────┘    └─────────────────┘      │
└─────────────────────────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────────────────────────┐
│                            DATA LAYER                                           │
│  ┌─────────────────────────┐    ┌───────────────────────────────────────────┐   │
│  │  closyr.dataset.inputs  │    │  Built-in Datasets                        │   │
│  │  (Benchmark Functions)  │    │  - prime-10000 (nth prime)                │   │
│  │  - Nguyen-4, Nguyen-5   │    │  - prime-counting (π(x))                  │   │
│  │  - Feynman Lorentz/Wave │    │                                           │   │
│  │  - Trig, Log, Gaussian  │    │                                           │   │
│  └─────────────────────────┘    └───────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────────────────────────┐
│                            UTILITIES                                            │
│  ┌───────────────┐  ┌───────────────┐  ┌───────────────┐  ┌───────────────┐     │
│  │ util.log      │  │ util.csv      │  │ util.prng     │  │ util.spec     │     │
│  │ (Logging)     │  │ (CSV Import)  │  │ (Seeded RNG)  │  │ (Malli Specs) │     │
│  └───────────────┘  └───────────────┘  └───────────────┘  └───────────────┘     │
└─────────────────────────────────────────────────────────────────────────────────┘

Data Flow:
  1. User provides (x,y) data via GUI sketchpad, CSV file, or CLI args
  2. GA creates initial population of random Symja expressions
  3. Each iteration: evaluate fitness (sum of residuals), select, mutate, crossover
  4. Best expressions shown in real-time on charts
  5. Final best-fit function returned as Symja-compatible expression
```

## Credits

- This project would not have been possible without the symbolic math library `Symja` (https://github.com/axkr/symja_android_library), which has been great to use and I consider it to be like a `MathJs` (https://github.com/josdejong/mathjs) on the JVM.


## Contribute

I accept PRs, please open an issue to discuss beforehand to make sure we are in alignment. Thanks!



## License

Copyright © 2024 Octavian Geagla

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.

``` 
________/\\\\\\\\\__/\\\___________________/\\\\\__________/\\\\\\\\\\\____/\\\________/\\\____/\\\\\\\\\_____
 _____/\\\////////__\/\\\_________________/\\\///\\\______/\\\/////////\\\_\///\\\____/\\\/___/\\\///////\\\___
  ___/\\\/___________\/\\\_______________/\\\/__\///\\\___\//\\\______\///____\///\\\/\\\/____\/\\\_____\/\\\___
   __/\\\_____________\/\\\______________/\\\______\//\\\___\////\\\_____________\///\\\/______\/\\\\\\\\\\\/____
    _\/\\\_____________\/\\\_____________\/\\\_______\/\\\______\////\\\____________\/\\\_______\/\\\//////\\\____
     _\//\\\____________\/\\\_____________\//\\\______/\\\__________\////\\\_________\/\\\_______\/\\\____\//\\\___
      __\///\\\__________\/\\\______________\///\\\__/\\\_____/\\\______\//\\\________\/\\\_______\/\\\_____\//\\\__
       ____\////\\\\\\\\\_\/\\\\\\\\\\\\\\\____\///\\\\\/_____\///\\\\\\\\\\\/_________\/\\\_______\/\\\______\//\\\_
        _______\/////////__\///////////////_______\/////_________\///////////___________\///________\///________\///__

```
