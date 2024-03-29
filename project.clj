(defproject closyr "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url  "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.clojure/core.async "1.6.681"]
                 [metosin/malli "0.14.0"]

                 [org.clojure/tools.cli "1.1.230"]

                 [org.clojure/data.csv "1.1.0"]

                 [seesaw/seesaw "1.5.0"]
                 [flames "0.5.0"]
                 [org.knowm.xchart/xchart "3.8.7"]
                 ;; [com.github.weisj/darklaf-core "3.0.2"]
                 [io.github.vincenzopalazzo/material-ui-swing "1.1.4"]
                 [io.github.material-ui-swing/DarkStackOverflowTheme "0.0.1-rc3"]

                 [ch.qos.logback/logback-classic "1.5.0"]
                 [org.slf4j/jcl-over-slf4j "2.0.12"]

                 [org.slf4j/slf4j-api "2.0.12"]
                 [org.apache.logging.log4j/log4j-core "2.22.1"]
                 [org.apache.logging.log4j/log4j-api "2.22.1"]

                 [org.matheclipse/matheclipse-core "3.1.0-SNAPSHOT"
                  :exclusions [org.slf4j/slf4j-api com.fasterxml.jackson.core/jackson-core]]
                 [org.matheclipse/matheclipse-gpl "3.1.0-SNAPSHOT"
                  :exclusions [org.slf4j/slf4j-api com.fasterxml.jackson.core/jackson-core]]]

  :repositories {"snapshots-repo" {:url       "https://oss.sonatype.org/content/repositories/snapshots"
                                   :releases  false
                                   :snapshots true}}
  :main ^:skip-aot closyr.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot      :all
                       :manifest {"Multi-Release" true}
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}}

  :plugins [[lein-cloverage "1.2.4"]
            [lein-vanity "0.2.0"]
            [lein-nomis-ns-graph "0.14.6"]
            [lein-ancient "0.7.0"]
            [jonase/eastwood "1.4.2"]
            [lein-kibit "0.1.8"]
            [lein-bikeshed "0.5.2"]
            [venantius/yagni "0.1.7"]
            [lein-check-namespace-decls "1.0.4"]
            [docstring-checker "1.1.0"]])
