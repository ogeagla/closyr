(defproject clj-symbolic-regression "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url  "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.clojure/core.async "1.6.681"]
                 [seesaw/seesaw "1.5.0"]
                 [flames "0.5.0"]
                 [org.knowm.xchart/xchart "3.8.7"]
                 [com.github.weisj/darklaf-core "3.0.2"]
                 [io.github.vincenzopalazzo/material-ui-swing "1.1.4"]
                 [org.slf4j/slf4j-api "2.0.11"]
                 [org.apache.logging.log4j/log4j-api "2.22.1"]
                 [org.apache.logging.log4j/log4j-core "2.22.1"]
                 [org.matheclipse/matheclipse-core "3.0.0"
                  :exclusions [org.slf4j/slf4j-api
                               org.apache.logging.log4j/log4j-api
                               org.apache.logging.log4j/log4j-core]]
                 [org.matheclipse/matheclipse-gpl "3.0.0"
                  :exclusions [org.slf4j/slf4j-api
                               org.apache.logging.log4j/log4j-api
                               org.apache.logging.log4j/log4j-core]]
                 ]
  ;:java-source-paths ["src/main/java"]
  ;:repositories {
  ;               "snapshots-repo" {:url       "https://oss.sonatype.org/content/repositories/snapshots"
  ;                                 :releases  false
  ;                                 :snapshots true}}
  :main ^:skip-aot clj-symbolic-regression.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot      :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}}
  :plugins [[lein-ring "0.12.6"]
            [lein-garden "0.3.0"]
            [lein-vanity "0.2.0"]
            [lein-nomis-ns-graph "0.14.6"]
            [lein-ancient "0.7.0"]
            [jonase/eastwood "1.4.2"]
            [lein-kibit "0.1.8"]
            [lein-bikeshed "0.5.2"]
            [venantius/yagni "0.1.7"]
            [lein-check-namespace-decls "1.0.4"]
            [docstring-checker "1.1.0"]]

  )
