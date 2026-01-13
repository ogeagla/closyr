(defproject closyr "0.1.0-SNAPSHOT"
  :description "Symbolic Regression in Clojure"
  :url "https://github.com/ogeagla/closyr"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url  "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.12.4"]
                 [org.clojure/core.async "1.8.741"]
                 [metosin/malli "0.20.0"]

                 [org.clojure/tools.cli "1.3.250"]

                 [org.clojure/data.csv "1.1.1"]

                 [seesaw/seesaw "1.5.0"]
                 [flames "0.5.0"]
                 [org.knowm.xchart/xchart "3.8.8"]
                 ;; [com.github.weisj/darklaf-core "3.0.2"]
                 [io.github.vincenzopalazzo/material-ui-swing "1.1.4"]
                 [io.github.material-ui-swing/DarkStackOverflowTheme "0.0.1-rc3"]

                 [ch.qos.logback/logback-classic "1.5.24"]
                 [org.slf4j/jcl-over-slf4j "2.0.17"]

                 [org.slf4j/slf4j-api "2.0.17"]
                 [org.apache.logging.log4j/log4j-core "2.25.3"]
                 [org.apache.logging.log4j/log4j-api "2.25.3"]

                 [org.matheclipse/matheclipse-core "3.1.0-SNAPSHOT"
                  :exclusions [org.slf4j/slf4j-api com.fasterxml.jackson.core/jackson-core]]
                 [org.matheclipse/matheclipse-gpl "3.1.0-SNAPSHOT"
                  :exclusions [org.slf4j/slf4j-api com.fasterxml.jackson.core/jackson-core]]

                 ;; Web server
                 [ring/ring-core "1.15.3"]
                 [ring/ring-jetty-adapter "1.15.3"]
                 [ring/ring-defaults "0.7.0"]
                 [metosin/reitit "0.10.0"]
                 [selmer "1.12.70"]
                 [cheshire "6.1.0"]]

  :repositories {"snapshots-repo" {:url       "https://oss.sonatype.org/content/repositories/snapshots"
                                   :releases  false
                                   :snapshots true}}
  :java-source-paths ["src/main/java"]
  :java-test-paths ["test"]
  :main ^:skip-aot closyr.core
  :target-path "target/%s"

  ;; JVM tuning for better GA performance
  :jvm-opts ["-Xms4g"                                   ; Initial heap = max (avoid resizing)
             "-Xmx4g"                                   ; Max heap size


             ;"-XX:+UseG1GC"                             ; G1 garbage collector (better for large heaps)

             ;"-XX:+UseZGC"
             ;"-XX:+ZGenerational"

             "-XX:+UseParallelGC"


             "-XX:MaxGCPauseMillis=100"                 ; Target max GC pause
             "-XX:+UseStringDeduplication"             ; Reduce memory for duplicate strings
             "-XX:+AlwaysPreTouch"                     ; Pre-touch memory pages at startup
             "-XX:+UseNUMA"                            ; Optimize for NUMA architectures
             "-XX:+OptimizeStringConcat"              ; Optimize string concatenation
             "-XX:AutoBoxCacheMax=20000"              ; Cache more Integer objects (reduce boxing overhead)
             "-XX:+DisableExplicitGC"]                ; Ignore System.gc() calls
  ;; Note: direct-linking only in uberjar profile (breaks dynamic var rebinding in tests)

  ;; AOT compile API namespaces for Java interop
  :aot [closyr.api.types
        closyr.api.finder]

  :profiles {:uberjar {:aot      :all
                       :manifest {"Multi-Release" true}
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}
             :test {:dependencies [[org.junit.jupiter/junit-jupiter-api "6.0.2"]
                                   [org.junit.jupiter/junit-jupiter-engine "6.0.2"]
                                   [org.junit.platform/junit-platform-launcher "6.0.2"]
                                   [org.junit.platform/junit-platform-console-standalone "6.0.2"]]}}

  :cloverage {:ns-exclude-regex [#"closyr\.ops\.common"
                                  #"closyr\.ops\.eval"
                                  #"closyr\.ops\.initialize"
                                  #"closyr\.ops\.modify"
                                  #"closyr\.ops"
                                  #"closyr\.ui\."
                                  #"closyr\.util\.spec"]}

  :plugins [[lein-cloverage "1.2.4"]
            [lein-vanity "0.2.0"]
            [lein-nomis-ns-graph "0.14.6"]
            [lein-ancient "0.7.0"]
            [jonase/eastwood "1.4.3"]
            [lein-kibit "0.1.11"]
            [lein-bikeshed "0.5.2"]
            [venantius/yagni "0.1.7"]
            [lein-check-namespace-decls "1.0.4"]
            [docstring-checker "1.1.0"]])
