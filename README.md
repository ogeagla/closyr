# closyr

![icon_v4_qtr.png](resources%2Ficons%2Ficon_v4_qtr.png)

A Symbolic Regression tool, written in Clojure.

Warning: This is experimental software, with an unstable API. Expect breaking changes prior to a major release.

- Draw an objective function or select a built-in dataset
- Select number of points, iterations
- See progress 
  - Chart of best fitting function
  - Chart of loss over time
  - A selectable text version of the function

## Installation

Currently this application is not packaged, so you have to run it from source, using Clojure development tools (`leiningen`)

    $ lein deps

## Usage

Use the application via GUI or in the terminal. 

There are 4 options.


### Run the provided realase JAR

Requirements:
- Java


Then run:


    $ java -jar closyr-0.1.0-SNAPSHOT-standalone.jar ...options here...


### Lein Run

Requirements:
- Java
- Leiningen


Then run:


    $ lein run

Or headless (just in the terminal), where you can specify options and input data:

    $ lein run -t -p 25 -i 5 -x 0,1,2,3,4,5,6 -y 1,2,30,4,5,6,10

Which is the same as:

    $ lein run --headless --population 1000 --iterations 200 --xs 0,1,2,3,4,5,6 --ys 1,2,30,4,5,6,10 

###  In Clojure REPL

Requirements:
- Java
- Leiningen (I will provide a deps file if enough interest)


Then run:


    (require '[closyr.symreg :as symreg]) 
    (symreg/run-app-with-gui)

### Build and run JAR

Requirements:
- Java


Then run:


    $ lein uberjar
    $ java -jar target/uberjar/closyr-0.1.0-standalone.jar

You can also provide the same command-line options to `java` command, like:

    $ java -jar target/uberjar/closyr-0.1.0-standalone.jar -t -p 25 -i 5 -x 0,1,2,3,4,5,6 -y 1,2,30,4,5,6,10

## Run Options

- `-t` `--headless`   : [optional] run without GUI, in terminal only
- `-p` `--population` : [optional] size of population which will evolve; the number of functions we create and modify
- `-i` `--iterations` : [optional] number of iterations to run for
- `-x` `--xs`         : [optional] the xs for the points in the dataset to fit against; if provided, must also provide `ys`
- `-y` `--ys`         : [optional] the ys for the points in the dataset to fit against; if provided, must also provide `xs`
- `-f` `--infile`     : [optional] A CSV file which either contains 2 columns without titles in first row, or has columns `x` and `y` to be used as objective dataset

## Examples

On successful application start, you should see this:

![gui_after_startup_2024-02-01_09-27.png](screenshots%2Fgui_after_startup_2024-02-01_09-27.png)

While a function search is running:

![gui_running_2024-02-01_09-29.png](screenshots%2Fgui_running_2024-02-01_09-29.png)

## Tests

    $ lein test

Coverage looks like this if you run `lein cloverage`:

![test_coverage_2024-02-07_10-32.png](screenshots%2Ftest_coverage_2024-02-07_10-32.png)

## How It Works

- We use Genetic Algorithms to allow candidate functions of best fit to compete.
- They compete on a computed score on the input data, and we use the sum of residuals to generate the score.
- Evolution consists of mutation and crossover. 
- Mutations act on the function's AST and modify branches, leafs, or the whole tree.  Operations like `+0.1`, `*x`, `/Sin(x)` are applied to functions.
- Crossovers combine two functions' ASTs at a random point, and combine using various operators like `+`, and `*`.

## Roadmap

- [ ] CLI options accept a CSV file (GUI already supports this)
- [ ] More tests
- [ ] A different frontend.  The current Java Swing frontend does the job, but it's not easy to maintain and it's hard to make look better.
- [ ] Can this be a follow-up to this issue, asking for a symbolic regression tool on the JVM? https://github.com/axkr/symja_android_library/issues/850
- [ ] When I created this and my other symbolic regression tools, I didn't know about the formal field of symbolic regression.  I've since found some great libraries that I should review and apply the lessons to this project: https://github.com/MilesCranmer/PySR


## Credits

- This project would not have been possible without the symbolic math library `Symja` (https://github.com/axkr/symja_android_library), which has been great to use and I consider it to be like a `MathJs` (https://github.com/josdejong/mathjs) on the JVM.



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
