(defproject clj-symbolic-regression "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url  "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.slf4j/slf4j-api "2.0.7"]
                 [org.apache.logging.log4j/log4j-api "2.17.1"]
                 [org.matheclipse/matheclipse-core "3.0.0"
                  :exclusions [org.slf4j/slf4j-api org.apache.logging.log4j/log4j-api]]
                 [org.matheclipse/matheclipse-gpl "3.0.0"
                  :exclusions [org.slf4j/slf4j-api org.apache.logging.log4j/log4j-api]]
                 ]
  ;:repositories {
  ;               "snapshots-repo" {:url       "https://oss.sonatype.org/content/repositories/snapshots"
  ;                                 :releases  false
  ;                                 :snapshots true}}
  :main ^:skip-aot clj-symbolic-regression.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot      :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})