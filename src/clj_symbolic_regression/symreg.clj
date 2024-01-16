(ns clj-symbolic-regression.symreg
  (:require
    [clj-symbolic-regression.ga :as ga]
    [clj-symbolic-regression.gui :as gui]
    [clj-symbolic-regression.ops :as ops]
    [clj-symbolic-regression.plot :as plot]
    [clojure.core.async :as async :refer [go go-loop timeout <!! >!! <! >! chan put! take! alts!!]]
    [clojure.string :as str]
    [flames.core :as flames])
  (:import
    (java.util
      Date
      List)
    (java.util.concurrent
      CopyOnWriteArrayList)
    (javax.swing
      JLabel)
    (org.apache.log4j
      Level
      LogManager
      Logger)
    (org.knowm.xchart
      XChartPanel
      XYChart)
    (org.matheclipse.core.expression
      F)
    (org.matheclipse.core.interfaces
      IExpr
      ISymbol)))


(set! *warn-on-reflection* true)

(def ^ISymbol sym-x (F/Dummy "x"))


(defn doubles->exprs
  [numbers]
  (mapv
    (fn [^double n] (.add F/C0 n))
    numbers))


(defn exprs->doubles
  [exprs]
  (mapv #(.doubleValue (.toNumber ^IExpr %)) exprs))


(defn ^"[Lorg.matheclipse.core.interfaces.IExpr;" exprs->input-exprs-list
  [exprs]
  (let [^"[Lorg.matheclipse.core.interfaces.IExpr;" exprs-arr
        (into-array IExpr exprs)
        ^"[Lorg.matheclipse.core.interfaces.IExpr;" exprs-list
        (into-array IExpr [(F/List exprs-arr)])]
    exprs-list))


(defn eval-vec-pheno
  [p
   {:keys [input-exprs-list input-exprs-count output-exprs-vec]
    :as   run-args}]
  (let [^IExpr new-expr (:expr p)
        new-is-const    (.isNumber new-expr)
        ^IExpr eval-p   (ops/eval-phenotype-on-expr-args p input-exprs-list)
        ;; ^IExpr eval-p   (ops/eval-phenotype-on-string-args p input-exprs-F-strings)
        vs              (vec (map
                               (fn [i]
                                 (try
                                   (.doubleValue
                                     (.toNumber (.getArg eval-p (inc i) F/Infinity)))
                                   (catch Exception e
                                     Double/POSITIVE_INFINITY)))
                               (range (dec (.size eval-p)))))
        vs              (if (seq vs)
                          vs
                          (vec (map
                                 (fn [i]
                                   (.doubleValue
                                     (.toNumber (if new-is-const
                                                  new-expr
                                                  (.getArg eval-p 0 F/Infinity)))))
                                 (range input-exprs-count))))]
    vs))


(defn extend-xs
  [input-exprs-vec]
  (let [x-min                (first input-exprs-vec)
        x-max                (last input-exprs-vec)
        x-range-sz           (- x-max x-min)
        extra-pts            25
        x-range-pct-extend   0.5
        x-range-extend-pt-sz (/ (* x-range-pct-extend x-range-sz) extra-pts)

        x-head               (reverse
                               (mapv
                                 (fn [i]
                                   (- x-min (* (inc i) x-range-extend-pt-sz)))
                                 (range extra-pts)))

        x-tail               (mapv
                               (fn [i]
                                 (+ x-max (* (inc i) x-range-extend-pt-sz)))
                               (range extra-pts))

        x-tail-list          (exprs->input-exprs-list (doubles->exprs x-tail))
        x-head-list          (exprs->input-exprs-list (doubles->exprs x-head))
        xs                   (concat x-head input-exprs-vec x-tail)]
    {:xs          xs
     :x-head      x-head
     :x-head-list x-head-list
     :x-tail      x-tail
     :x-tail-list x-tail-list}))


(defn eval-vec-pheno-oversample-from-orig-xs
  "Eval xs but oversample in range and add a head and tail for plotting more points on curve"
  [p
   {:keys [input-exprs-list input-exprs-count input-exprs-vec output-exprs-vec]
    :as   run-args}]
  (let [{x-head      :x-head
         x-head-list :x-head-list
         x-tail      :x-tail
         x-tail-list :x-tail-list} (extend-xs input-exprs-vec)

        _            (println "Got range extensions: head: " (count x-head) "tail: " (count x-tail))

        xs           (concat x-head (:input-exprs-vec run-args) x-tail)

        evaluated-ys (concat
                       (eval-vec-pheno p (assoc run-args :input-exprs-list x-head-list :input-exprs-count (count x-head)))
                       (eval-vec-pheno p run-args)
                       (eval-vec-pheno p (assoc run-args :input-exprs-list x-tail-list :input-exprs-count (count x-tail))))]

    {:xs xs
     :ys evaluated-ys}))


(defn eval-vec-pheno-oversample
  "Eval xs but oversample in range and add a head and tail for plotting more points on curve"
  [p
   {:keys [input-exprs-list input-exprs-count input-exprs-vec output-exprs-vec]
    :as   run-args}
   {xs          :xs
    x-head      :x-head
    x-head-list :x-head-list
    x-tail      :x-tail
    x-tail-list :x-tail-list}]
  (let [_            (println "Got range extensions: head: " (count x-head) "tail: " (count x-tail))

        ;; xs           (concat x-head (:input-exprs-vec run-args) x-tail)

        evaluated-ys (concat
                       (eval-vec-pheno p (assoc run-args :input-exprs-list x-head-list :input-exprs-count (count x-head)))
                       (eval-vec-pheno p run-args)
                       (eval-vec-pheno p (assoc run-args :input-exprs-list x-tail-list :input-exprs-count (count x-tail))))]

    {:xs xs
     :ys evaluated-ys}))


(defn sum
  [coll]
  (reduce + 0.0 coll))


(def sim-stats* (atom {}))


(defn score-fn
  [{:keys [input-exprs-list input-exprs-count output-exprs-vec
           sim-stop-start-chan sim->gui-chan]
    :as   run-args}
   v]
  (try
    (let [leafs            (.leafCount ^IExpr (:expr v))
          f-of-xs          (eval-vec-pheno v run-args)
          resids           (map - output-exprs-vec f-of-xs)
          resid            (->>
                             resids
                             (map #(min 100000 (abs %)))
                             (sum))
          score            (* -1 (abs resid))
          length-deduction (* 0.0001 leafs)
          overall-score    (- score length-deduction)]

      (when (zero? resid) (println "warning: zero resid " resids))
      (when (neg? length-deduction) (println "warning: negative deduction increases score: " leafs length-deduction v))
      (swap! sim-stats* update-in [:scoring :len-deductions] #(into (or % []) [length-deduction]))

      overall-score)
    (catch Exception e
      -1000000)))


(def mutations-sampler
  [1 1 1 1 1 1 1 1 1
   1 1 1 1 1 1 1 1 1
   2 2 2 2 2 2
   2 2 2 2 2 2
   3 3 3 3
   3 3 3 3
   4 4 4
   4 4 4
   5 5 5
   5 5
   6 6
   6 6
   7 7
   7
   8
   8
   9
   10])


(defn rand-mut
  [initial-muts]
  (rand-nth initial-muts))


(defn mutation-fn
  [initial-muts v v-discard]
  (try
    (let [c         (rand-nth mutations-sampler)
          new-pheno (loop [c          c
                           v          v
                           first-run? true]
                      (if (zero? c)
                        v
                        (let [new-v (ops/modify (rand-mut initial-muts) (if first-run?
                                                                          (assoc v :util (:util v-discard))
                                                                          v))]
                          (recur
                            (if (and (< 200 (count (str (:expr v))))
                                     (< (count (str (:expr v))) (count (str (:expr new-v)))))
                              0
                              (dec c))
                            new-v
                            false))))
          old-leafs (.leafCount ^IExpr (:expr v))
          new-leafs (.leafCount ^IExpr (:expr new-pheno))]
      (swap! sim-stats* update-in [:mutations :counts c] #(inc (or % 0)))
      (swap! sim-stats* update-in [:mutations :size-in] #(into (or % []) [old-leafs]))
      (swap! sim-stats* update-in [:mutations :size-out] #(into (or % []) [new-leafs]))
      new-pheno)
    (catch Exception e
      (println "Err in mutation: " e))))


(defn crossover-fn
  [initial-muts v v-discard]
  ;; todo do something for crossover
  (mutation-fn initial-muts v v-discard))


(defn sort-population
  [pops]
  (->>
    (:pop pops)
    (remove #(nil? (:score %)))
    (sort-by :score)
    (reverse)))


(defn reportable-phen-str
  [p]

  (str
    " id: " (-> p :id)
    " score: " (-> p :score)
    " fn: " (-> p :expr str)))


(defn summarize-sim-stats
  []
  (let [{{cs     :counts
          sz-in  :size-in
          sz-out :size-out}               :mutations
         {len-deductions :len-deductions} :scoring
         :as                              dat} @sim-stats*]
    (let [data-str (-> dat
                       (assoc :scoring {:len-deductions (/ (sum len-deductions) (count len-deductions))})
                       (assoc :mutations {:counts        cs
                                          :size-in-mean  (/ (sum sz-in) (count sz-in))
                                          :size-out-mean (/ (sum sz-out) (count sz-out))}))]
      (str " mut size: " (count sz-in) " len deduction: " (count len-deductions)
           " "
           data-str))))


(def test-timer* (atom nil))

(def log-steps 2)


(defn report-iteration
  [i
   iters
   ga-result
   {:keys [input-exprs-list input-exprs-count output-exprs-vec
           sim-stop-start-chan sim->gui-chan extended-domain-args]
    :as   run-args}]
  (when (or (= 1 i) (zero? (mod i log-steps)))
    (let [old-score  (:pop-old-score ga-result)
          old-scores (:pop-old-scores ga-result)
          end        (Date.)
          diff       (- (.getTime end) (.getTime ^Date @test-timer*))
          bests      (sort-population ga-result)
          took-s     (/ diff 1000.0)
          pop-size   (count (:pop ga-result))
          best-v     (first bests)
          evaled     (eval-vec-pheno best-v run-args)
          {evaled-extended :ys xs-extended :xs} (eval-vec-pheno-oversample best-v run-args extended-domain-args)]
      ;(println "Biggest: vals x: " (take-last 5 (sort xs-extended)))
      ;(println "Biggest: vals y: " (take-last 5 (sort evaled-extended)))
      ;(println "eval extended pts count: xs: " (count xs-extended) "ys: " (count evaled-extended))

      (reset! test-timer* end)
      (println i "-step pop size: " pop-size " took secs: " took-s " phenos/s: " (Math/round ^double (/ (* pop-size log-steps) took-s)))
      (println i " pop score: " old-score
               " mean: " (Math/round (float (/ old-score (count (:pop ga-result))))))
      (println i " top best:\n"
               (->> (take 5 bests)
                    (map reportable-phen-str)
                    (str/join "\n")))
      (println i " sim stats: " (summarize-sim-stats))
      ;; (println i " fn eval cache: " @fn-eval-cache-stats*)

      (put! sim->gui-chan {:iters                    iters
                           :i                        (- iters i)
                           :best-eval                evaled
                           :input-exprs-vec-extended xs-extended
                           :best-eval-extended       evaled-extended
                           :best-f-str               (str (:expr best-v))
                           :best-score               (:score best-v)})))
  (reset! sim-stats* {}))


(defn near-exact-solution
  [i old-score old-scores]
  (println "Perfect score! " i old-score " all scores: " old-scores)
  0)


(def input-exprs
  (->>
    (range 50)
    (map (fn [i]
           (.add F/C0 (* Math/PI (/ i 15.0)))))
    vec))


(def output-exprs
  (->>
    (range 50)
    (map (fn [i]
           (let [x (* Math/PI (/ i 15.0))]
             (.add F/C0 0.0 #_(+ (* 0.5 x x) 2.0 (* 4.0 (Math/sin x)))))))
    vec))


(defn setup-gui
  [plot-args*]
  (let [sim->gui-chan       (chan)
        sim-stop-start-chan (chan)
        {:keys [input-exprs-vec output-exprs-vec]} @plot-args*]
    (gui/create-and-show-gui
      {:sim-stop-start-chan sim-stop-start-chan
       :x1s                 (doto (CopyOnWriteArrayList.) (.addAll input-exprs-vec))
       :x2s                 (doto (CopyOnWriteArrayList.) (.addAll input-exprs-vec))
       :y1s                 (doto (CopyOnWriteArrayList.) (.addAll (repeat (count input-exprs-vec) 0.0)))
       :y2s                 (doto (CopyOnWriteArrayList.) (.addAll output-exprs-vec))
       :s1l                 "best fn"
       :s2l                 "objective fn"
       :update-loop         (fn [{:keys [^XYChart chart
                                         ^XChartPanel chart-panel
                                         ^JLabel info-label]}
                                 {:keys [^List x1s ^List x2s ^List y1s ^List y2s ^String s1l ^String s2l]
                                  :as   conf}]
                              (go-loop []
                                (<! (timeout 1000))
                                (when-let [{:keys [input-exprs-vec-extended
                                                   best-eval-extended
                                                   best-eval best-score best-f-str i iters]
                                            :as   sim-msg} (<! sim->gui-chan)]

                                  (let [{:keys [input-exprs-vec output-exprs-vec]} @plot-args*]
                                    (.clear y1s)
                                    (.addAll y1s (if best-eval-extended
                                                   (mapv (fn [v]
                                                           (cond
                                                             (infinite? v) 0.0
                                                             (> (abs v) 10e9) 0.0
                                                             :else v))
                                                         best-eval-extended)
                                                   best-eval))

                                    (.clear y2s)
                                    (.addAll y2s output-exprs-vec)

                                    (.clear x1s)
                                    (.addAll x1s (or input-exprs-vec-extended input-exprs-vec))

                                    (.clear x2s)
                                    (.addAll x2s input-exprs-vec)

                                    (.setTitle chart "Best vs Objective Functions")
                                    (.updateXYSeries chart s1l x1s y1s nil)
                                    (.updateXYSeries chart s2l x2s y2s nil)

                                    (.setText info-label (str "<html>Iteration: " i "/" iters
                                                              "<br>Best Function: "
                                                              "<br><small> y = " best-f-str "</small>"
                                                              "<br>Score: " best-score
                                                              "</html>"))
                                    (.revalidate info-label)
                                    (.repaint info-label)

                                    (.revalidate chart-panel)
                                    (.repaint chart-panel)

                                    (recur)))))})
    {:sim->gui-chan       sim->gui-chan
     :sim-stop-start-chan sim-stop-start-chan}))


(defn check-start-stop-state
  [{:keys [input-exprs-list input-exprs-count output-exprs-vec
           sim-stop-start-chan sim->gui-chan]
    :as   run-args}]
  (let [[n ch] (alts!! [sim-stop-start-chan] :default :continue :priority true)]
    (if (= n :continue)
      :ok
      (do
        (println "Parking updates due to Stop command")
        (<!! sim-stop-start-chan)
        (println "Resuming updates")))))


(defn get-input-data
  [{:keys [iters initial-phenos initial-muts input-exprs output-exprs] :as run-config}]
  ;; to not use the GUI, pass the initial values through
  (let [input-exprs-vec                                              (exprs->doubles input-exprs)
        output-exprs-vec                                             (exprs->doubles output-exprs)

        plot-args*                                                   (atom {:input-exprs-vec  input-exprs-vec
                                                                            :output-exprs-vec output-exprs-vec})


        {sim->gui-chan       :sim->gui-chan
         sim-stop-start-chan :sim-stop-start-chan
         :as                 gui-comms} (setup-gui plot-args*)

        ;; wait for GUI to press Start, which submits the new xs/ys data:
        {new-state    :new-state
         input-data-x :input-data-x
         input-data-y :input-data-y} (<!! sim-stop-start-chan)

        _                                                            (println "Got state req: " (if new-state "Start" "Stop"))

        input-exprs                                                  (if input-data-x
                                                                       (mapv (fn [^double pt-x] (.add F/C0 pt-x)) input-data-x)
                                                                       input-exprs)
        output-exprs                                                 (if input-data-y
                                                                       (mapv (fn [^double pt-y] (.add F/C0 pt-y)) input-data-y)
                                                                       output-exprs)

        output-exprs-vec                                             (exprs->doubles output-exprs)

        ^"[Lorg.matheclipse.core.interfaces.IExpr;" input-exprs-list (exprs->input-exprs-list input-exprs)

        input-exprs-count                                            (count input-exprs)
        input-exprs-vec                                              (exprs->doubles input-exprs)
        extended-domain-args                                         (extend-xs input-exprs-vec)]

    (reset! plot-args* {:input-exprs-vec  input-exprs-vec
                        :output-exprs-vec output-exprs-vec})

    (merge gui-comms
           {:extended-domain-args extended-domain-args
            :input-exprs-list     input-exprs-list
            :input-exprs-count    input-exprs-count
            :input-exprs-vec      input-exprs-vec
            :output-exprs-vec     output-exprs-vec})))


(defn run-experiment
  [{:keys [iters initial-phenos initial-muts input-exprs output-exprs] :as run-config}]
  (println-str "run for iters: " iters)
  (println "initial pop: " (count initial-phenos))
  (println "initial muts: " (count initial-muts))

  (let [{:keys [input-exprs-list input-exprs-count output-exprs-vec
                sim-stop-start-chan sim->gui-chan]
         :as   run-args} (get-input-data run-config)
        start (Date.)
        pop1  (ga/initialize
                initial-phenos
                (partial score-fn run-args)
                (partial mutation-fn initial-muts)
                (partial crossover-fn initial-muts))]
    (println "start " start)
    (reset! test-timer* start)

    (loop [pop pop1
           i   iters]
      (if (zero? i)
        pop
        (do
          (check-start-stop-state run-args)
          (let [{old-scores :pop-old-scores old-score :pop-old-score :as ga-result} (ga/evolve pop)]
            (report-iteration i iters ga-result run-args)
            (recur ga-result
                   (if (or (zero? old-score) (some #(> % -1e-3) old-scores))
                     (near-exact-solution i old-score old-scores)
                     (dec i)))))))

    (let [end  (Date.)
          diff (- (.getTime end) (.getTime start))]

      (println "Took " (/ diff 1000.0) " seconds"))))


(defn in-flames
  [f]
  ;; http://localhost:54321/flames.svg
  (let [flames (flames/start! {:port 54321, :host "localhost"})]
    (f)
    (flames/stop! flames)))


;; (def ^Logger logger (LogManager/getRootLogger))
;; (println "Root logger name: " (.getName logger) (.setLevel logger Level/ERROR))

(defn run-test
  []
  (let [experiment-fn (fn []
                        (run-experiment
                          {:initial-phenos (ops/initial-phenotypes sym-x 1000)
                           :initial-muts   (ops/initial-mutations)
                           :input-exprs    input-exprs
                           :output-exprs   output-exprs
                           :iters          500}))]
    ;; with flame graph analysis:
    ;; (in-flames experiment-fn)
    ;; plain experiment:
    (experiment-fn)))


;; todo: pick iters / phenos count / datasets in gui

(comment (run-test))
