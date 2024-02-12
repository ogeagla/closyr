(ns closyr.log
  (:require [clojure.pprint :as pprint])
  (:import [ch.qos.logback.classic Level Logger]
           [java.io StringWriter]
           [org.slf4j LoggerFactory MDC]))

(set! *warn-on-reflection* true)
(def ^Logger logger (LoggerFactory/getLogger "closyr"))

(defn set-log-level!
  "Pass keyword :error :warn :info :debug"
  [level]
  (case level
    :debug (.setLevel logger Level/DEBUG)
    :info (.setLevel logger Level/INFO)
    :warn (.setLevel logger Level/WARN)
    :error (.setLevel logger Level/ERROR)))

(defmacro with-logging-context [context & body]
  "Use this to add a map to any logging wrapped in the macro. Macro can be nested.
  (with-logging-context {:key \"value\"} (log/info \"yay\"))
  "
  `(let [wrapped-context# ~context
         ctx# (MDC/getCopyOfContextMap)]
     (try
       (if (map? wrapped-context#)
         (doall (map (fn [[k# v#]] (MDC/put (name k#) (str v#))) wrapped-context#)))
       ~@body
       (finally
         (if ctx#
           (MDC/setContextMap ctx#)
           (MDC/clear))))))

(defmacro debug [& msg]
  `(.debug logger (print-str ~@msg)))

(defmacro info [& msg]
  `(.info logger (print-str ~@msg)))

(defmacro warn [& msg]
  `(.warn logger (print-str ~@msg)))

(defmacro error [throwable & msg]
  `(if (instance? Throwable ~throwable)
     (.error logger (print-str ~@msg) ~throwable)
     (.error logger (print-str ~throwable ~@msg))))

(defmacro spy
  [expr]
  `(let [a# ~expr
         w# (StringWriter.)]
     (pprint/pprint '~expr w#)
     (.append w# " => ")
     (pprint/pprint a# w#)
     (error (.toString w#))
     a#))
