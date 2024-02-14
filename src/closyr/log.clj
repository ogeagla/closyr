(ns closyr.log
  (:require
    [clojure.pprint :as pprint]
    [clojure.string :as str])
  (:import
    (ch.qos.logback.classic
      Level
      Logger)
    (java.io
      StringWriter)
    (org.slf4j
      LoggerFactory
      MDC)))


(set! *warn-on-reflection* true)


(def log-level* (atom nil))


(def ^Logger default-logger
  "The logger for the app"
  (LoggerFactory/getLogger "closyr"))


(defn ^Logger get-ns-logger_unmemo
  "Create a logger for the provided ns str"
  [^String ns-name-str]
  (.info default-logger (str "Create logger for ns: " ns-name-str))
  (LoggerFactory/getLogger (if (str/includes? ns-name-str "closyr")
                             ns-name-str
                             (str "closyr/" ns-name-str))))


(def get-ns-logger
  "Memoized logger for each ns"
  (memoize get-ns-logger_unmemo))


(defn ^Logger get-ns-logger-w-level
  "Get logger for ns and set to current level"
  [^String ns-name-str]
  (if @log-level*
    (doto ^Logger (get-ns-logger ns-name-str)
      (.setLevel ^Level @log-level*))
    (get-ns-logger ns-name-str)))


(defn set-log-level!
  "Pass keyword :error :warn :info :debug"
  [level]
  (let [log-level (case level
                    :debug Level/DEBUG
                    :info Level/INFO
                    :warn Level/WARN
                    :error Level/ERROR)]
    (.setLevel default-logger log-level)
    (.setLevel (get-ns-logger-w-level (str (ns-name *ns*))) log-level)
    (reset! log-level* log-level)))


(defmacro with-logging-context
  "Use this to add a map to any logging wrapped in the macro. Macro can be nested.
  (with-logging-context {:key \"value\"} (log/info \"yay\"))"
  [context & body]
  `(let [wrapped-context# ~context
         ctx#             (MDC/getCopyOfContextMap)]
     (try
       (when (map? wrapped-context#)
         (doall (map (fn [[k# v#]] (MDC/put (name k#) (str v#))) wrapped-context#)))
       ~@body
       (finally
         (if ctx#
           (MDC/setContextMap ctx#)
           (MDC/clear))))))


(defmacro debug
  "Log debug"
  [& msg]
  `(.debug (get-ns-logger-w-level (str (ns-name *ns*))) (print-str ~@msg)))


(defmacro info
  "Log info"
  [& msg]
  `(.info (get-ns-logger-w-level (str (ns-name *ns*))) (print-str ~@msg)))


(defmacro warn
  "Log warn"
  [& msg]
  `(.warn (get-ns-logger-w-level (str (ns-name *ns*))) (print-str ~@msg)))


(defmacro error
  "Log error. Can be a throwable as first arg."
  [throwable & msg]
  `(if (instance? Throwable ~throwable)
     (.error (get-ns-logger-w-level (str (ns-name *ns*))) (print-str ~@msg) ~throwable)
     (.error (get-ns-logger-w-level (str (ns-name *ns*))) (print-str ~throwable ~@msg))))


(defmacro spy
  "Log spy"
  [expr]
  `(let [a# ~expr
         w# (StringWriter.)]
     (pprint/pprint '~expr w#)
     (.append w# " => ")
     (pprint/pprint a# w#)
     (error (.toString w#))
     a#))
