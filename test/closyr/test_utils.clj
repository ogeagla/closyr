(ns closyr.test-utils
  "Test utilities and fixtures for quieter logging during tests"
  (:require
    [closyr.util.log :as log]))


(defn quiet-logging-fixture
  "Test fixture that sets log level to WARN during tests.
  Use with: (use-fixtures :once quiet-logging-fixture)"
  [f]
  (log/set-log-level! :warn)
  (f))
