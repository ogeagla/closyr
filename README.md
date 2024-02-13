# closyr

![icon_v5_qtr.png](resources%2Ficons%2Ficon_v5_qtr.png)

A Symbolic Regression tool, written in Clojure.

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


### Run the provided realase JAR

Requirements: Java


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
    $ java -jar target/uberjar/closyr-0.1.0-standalone.jar

You can also provide the same command-line options to `java` command, like:

    $ java -jar target/uberjar/closyr-0.1.0-standalone.jar -t -p 25 -i 5 -x 0,1,2,3,4,5,6 -y 1,2,30,4,5,6,10

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

## Example Screenshots

On successful application start, you can start an run a search.  You might see something like this when done:

![gui_done_2024-02-12_08-44.png](screenshots%2Fgui_done_2024-02-12_08-44.png)

An example of using the GUI:

![gui_usage_clip_Peek 2024-02-12 09-03.gif](screenshots%2Fgui_usage_clip_Peek%202024-02-12%2009-03.gif)

## Tests

    $ lein test

Coverage looks like this if you run `lein cloverage`:

![test_coverage_2024-02-13_09-25.png](screenshots%2Ftest_coverage_2024-02-13_09-25.png)

## How It Works

- We use Genetic Algorithms to allow candidate functions of best fit to compete.
- They compete on a computed score on the input data, and we use the sum of residuals to generate the score.
- Evolution consists of mutation and crossover. 
- Mutations act on the function's AST and modify branches, leafs, or the whole tree.  Operations like `+0.1`, `*x`, `/Sin(x)` are applied to functions.
- Crossovers combine two functions' ASTs at a random point, and combine using various operators like `+`, and `*`.

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
