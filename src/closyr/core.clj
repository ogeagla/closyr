(ns closyr.core
  (:gen-class)
  (:require
    [clojure.string :as str]
    [clojure.tools.cli :as cli]
    [closyr.dataset.csv :as input-csv]
    [closyr.symreg :as symreg])
  (:import
    (java.io
      File)))


(set! *warn-on-reflection* true)


(defn str->doubles-vec
  [numbers-str]
  (try
    (let [ns (->>
               #"\,"
               (str/split numbers-str)
               (mapv #(Double/parseDouble %)))]
      ns)
    (catch Exception e
      (println "Cant parse numbers str: " numbers-str " : " e))))


(def cli-options
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
    :validate [#(< 4 (count %) 101) "Y values count must be between 4 and 101, separated by ,"]]

   ["-x" "--xs XS" "X values to fit against. Must provide ys if providing xs"
    :default nil
    :parse-fn str->doubles-vec
    :validate [#(< 4 (count %) 101) "X values count must be between 4 and 101, separated by ,"]]

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


(defn parse-main-opts
  [args]
  (let [{errors  :errors
         options :options
         :as     main-opts} (cli/parse-opts args cli-options)]
    ;; (println "Got main args: " args)
    ;; (println "Got main opts: " main-opts)
    (when (seq errors)
      (println "Warning(s) from CLI input processor: \n Warning:" (str/join "\n Warning: " errors)))
    options))


(defn validate-symreg-opts
  [{:keys [ys xs infile]
    :as   opts}]
  (let [opts (if-not (nil? infile)
               (let [csv-data (input-csv/get-csv-data infile)
                     xs       (mapv :x csv-data)
                     ys       (mapv :y csv-data)]
                 (assoc opts :xs xs :ys ys))
               opts)
        opts (cond
               (and ys xs (not= (count ys) (count xs))) (println "Error: XS and YS count mismatch")
               (and xs (nil? ys)) (println "Error: only XS provided, please provide YS")
               :else opts)]
    (dissoc opts :infile)))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (some->
    (parse-main-opts args)
    (validate-symreg-opts)
    (symreg/run-app-from-cli-args)))


(defn test-cli
  []
  [(let [test-input '("-vvvp80000" "foo" "--help" "--invalid-opt" "-i50000" "-y1,1,1" "-x1,1,1,1,1,1,1,1,1")]
     (validate-symreg-opts (parse-main-opts test-input)))

   (let [test-input '("-t" "-p1000" "foo" "-i" "200" "-y" "1,2,30,4,5,6,10" "-x" "0,1,2,3,4,5,6")]
     (validate-symreg-opts (parse-main-opts test-input)))

   (let [test-input '("--headless" "--population" "1000" "--iterations" "200" "--ys" "1,2,30,4,5,6,10" "--xs" "0,1,2,3,4,5,6")]
     (validate-symreg-opts (parse-main-opts test-input)))])


(comment (test-cli))
