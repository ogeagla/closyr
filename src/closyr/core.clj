(ns closyr.core
  (:gen-class)
  (:require
    [clojure.string :as str]
    [clojure.tools.cli :as cli]
    [closyr.log :as log]
    [closyr.dataset.csv :as input-csv]
    [closyr.symbolic-regression :as symreg]
    ;[unilog.config  :refer [start-logging!]]
    )
  (:import
    (java.io
      File)))

(set! *warn-on-reflection* true)
#_(start-logging! {
                 :level :info
                 :overrides  {"edu.jas.ufd" :error}

                 })

(defn- str->doubles-vec
  [numbers-str]
  (try
    (let [ns (->>
               #"\,"
               (str/split numbers-str)
               (mapv #(Double/parseDouble %)))]
      ns)
    (catch Exception e
      (log/error "Cant parse numbers str: " numbers-str " : " e))))


(def ^:private cli-options
  [["-i" "--iterations ITERATIONS" "Number of iterations"
    :default 10
    :parse-fn #(Integer/parseInt %)
    :validate [#(< 1 % 10001) "Must be a number between 1 and 10001"]]

   ["-f" "--infile INFILE" "CSV Input File"
    :default nil
    :parse-fn #(File. ^String %)
    :validate [#(.exists ^File %) "CSV File must exist"]]

   ["-y" "--ys YS" "Y values to fit against. Without xs, will use 0,...,N"
    :default nil
    :parse-fn str->doubles-vec
    :validate [#(< 2 (count %) 101) "Y values count must be between 2 and 101, separated by ,"]]

   ["-x" "--xs XS" "X values to fit against. Must provide ys if providing xs"
    :default nil
    :parse-fn str->doubles-vec
    :validate [#(< 2 (count %) 101) "X values count must be between 2 and 101, separated by ,"]]

   ["-p" "--population POPULATION" "Population size"
    :default 20
    :parse-fn #(Integer/parseInt %)
    :validate [#(< 19 % 10001) "Must be a number between 19 and 10001"]]

   ["-t" "--headless" "Headless mode, no GUI, run in terminal"
    :default false
    :id :headless]

   #_["-v" nil "Verbosity level"
      :id :verbosity
      :default 0
      :update-fn inc]
   #_["-h" "--help"]])


(defn- parse-main-opts
  [args]
  (let [{errors  :errors
         options :options
         :as     main-opts} (cli/parse-opts args cli-options)]
    (when (seq errors)
      (log/warn "Warning(s) from CLI input processor: \n Warning:" (str/join "\n Warning: " errors))
      (log/info "From CLI args: " args))
    options))


(defn- validate-symreg-opts
  [{:keys [ys xs infile]
    :as   opts}]
  (let [opts (if-not (nil? infile)
               (let [csv-data (input-csv/get-csv-data infile)
                     xs       (mapv :x csv-data)
                     ys       (mapv :y csv-data)]
                 (assoc opts :xs xs :ys ys))
               opts)
        opts (cond
               (and ys xs (not= (count ys) (count xs))) (log/error "Error: XS and YS count mismatch. XS/YS: " xs ys)
               (and xs (nil? ys)) (log/error "Error: only XS provided, please provide YS. XS/YS: " xs ys)
               (and ys (nil? xs)) (log/error "Error: only YS provided, please provide XS. XS/YS: " xs ys)
               :else opts)]
    (dissoc opts :infile)))


(def ^:private big-text
  (str #"
________/\\\\\\\\\__/\\\___________________/\\\\\__________/\\\\\\\\\\\____/\\\________/\\\____/\\\\\\\\\_____
 _____/\\\////////__\/\\\_________________/\\\///\\\______/\\\/////////\\\_\///\\\____/\\\/___/\\\///////\\\___
  ___/\\\/___________\/\\\_______________/\\\/__\///\\\___\//\\\______\///____\///\\\/\\\/____\/\\\_____\/\\\___
   __/\\\_____________\/\\\______________/\\\______\//\\\___\////\\\_____________\///\\\/______\/\\\\\\\\\\\/____
    _\/\\\_____________\/\\\_____________\/\\\_______\/\\\______\////\\\____________\/\\\_______\/\\\//////\\\____
     _\//\\\____________\/\\\_____________\//\\\______/\\\__________\////\\\_________\/\\\_______\/\\\____\//\\\___
      __\///\\\__________\/\\\______________\///\\\__/\\\_____/\\\______\//\\\________\/\\\_______\/\\\_____\//\\\__
       ____\////\\\\\\\\\_\/\\\\\\\\\\\\\\\____\///\\\\\/_____\///\\\\\\\\\\\/_________\/\\\_______\/\\\______\//\\\_
        _______\/////////__\///////////////_______\/////_________\///////////___________\///________\///________\///__
  "))


(defn -main
  "Validate CLI args and run symbolic regression"
  [& args]
  (log/info big-text)

  (some->
    (parse-main-opts args)
    (validate-symreg-opts)
    (symreg/run-app-from-cli-args)))
