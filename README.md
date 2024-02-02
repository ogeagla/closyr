# closyr

![icon_v3_qtr.png](resources%2Ficons%2Ficon_v3_qtr.png)

A Symbolic Regression tool, written in Clojure.

- Draw an objective function or select a built-in dataset
- Select number of points, iterations
- See progress 
  - Chart of best fitting function
  - Chart of loss over time
  - A selectable text version of the function

## Installation

Currently this application is not packed, so you have to run it from source, using Clojure development tools (`leiningen`)

    $ lein deps

## Usage

There are 3 options.


### Lein Run

    
    $ lein run

Or headless (just in the terminal), where you can specify options and input data:

    $ lein run -t -p 25 -i 5 -x 0,1,2,3,4,5,6 -y 1,2,30,4,5,6,10

Which is the same as:

    $ lein run --headless --population 1000 --iterations 200 --xs 0,1,2,3,4,5,6 --ys 1,2,30,4,5,6,10 

###  In Clojure REPL


    (require '[closyr.symreg :as symreg]) 
    (symreg/run-app-with-gui)

### Build and run JAR


    $ lein uberjar
    $ java -jar target/uberjar/closyr-0.1.0-standalone.jar

You can also provide the same command-line options to `java` command, like:

    $ java -jar target/uberjar/closyr-0.1.0-standalone.jar -t -p 25 -i 5 -x 0,1,2,3,4,5,6 -y 1,2,30,4,5,6,10

## Examples

On successful application start, you should see this:

![gui_after_startup_2024-02-01_09-27.png](screenshots%2Fgui_after_startup_2024-02-01_09-27.png)

While a function search is running:

![gui_running_2024-02-01_09-29.png](screenshots%2Fgui_running_2024-02-01_09-29.png)

### Tests

    $ lein test

Coverage looks like this:
![test_coverage_2024-02-02_10-30.png](screenshots%2Ftest_coverage_2024-02-02_10-30.png)



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
