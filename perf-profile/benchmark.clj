(ns benchmark
  (:require [closyr.ops.common :as ops-common]
            [closyr.ops.eval :as ops-eval]
            [closyr.ops.initialize :as ops-init]
            [closyr.ops :as ops]
            [closyr.ga :as ga]
            [closyr.util.prng :as prng]))

(defn benchmark-evolution
  "Benchmark evolution with optional deterministic mode.
   When random-seed is provided, runs in deterministic (single-threaded) mode."
  [pop-size n-iters max-leafs xs-max & {:keys [random-seed]}]
  (binding [ga/*deterministic-mode* (some? random-seed)]
    (when random-seed
      (prng/set-random-seed! random-seed))
    (let [phenos (ops-init/initial-phenotypes pop-size)
          muts (ops-init/initial-mutations)
          xs (vec (range 0.1 xs-max 0.2))
          ys (mapv #(+ (* % %) (Math/sin %)) xs)
          input-xs-list (ops-common/exprs->exprs-list (ops-common/doubles->exprs xs))
          run-args {:input-xs-list  input-xs-list
                    :input-xs-count (count xs)
                    :input-xs-vec   xs
                    :input-ys-vec   ys
                    :input-ys-arr   (double-array ys)}
          run-config {:max-leafs max-leafs}
          score-fn (partial ops/score-fn run-args run-config)
          mut-fn (partial ops/mutation-fn run-config muts)
          cross-fn (partial ops/crossover-fn run-config muts)
          ga-config (ga/initialize phenos score-fn mut-fn cross-fn)]

      ;; Warmup
      (dotimes [_ 3] (ga/evolve ga-config))

      ;; Timed run
      (let [start (System/nanoTime)
            result (loop [config ga-config, i 0]
                     (if (< i n-iters)
                       (recur (ga/evolve config) (inc i))
                       config))
            elapsed-ms (/ (- (System/nanoTime) start) 1e6)]
        {:pop-size       pop-size
         :iters          n-iters
         :elapsed-ms     elapsed-ms
         :ms-per-iter    (/ elapsed-ms n-iters)
         :phenos-per-sec (/ (* pop-size n-iters 1000) elapsed-ms)
         :deterministic  (some? random-seed)}))))

(println "\n=== Evolution Benchmark ===\n")

(def results* (atom []))

(doseq [pop-size [10 100 500 1000 20000]]
  (swap! results* concat ["\n"])
  (doseq [xs-max [5.0 20.0]]
    (doseq [max-leafs [40 120]]
      (let [result (benchmark-evolution pop-size 10 max-leafs xs-max)]
        (swap! results* concat [(format "Pop %4d, XsMax %4f, MaxLeafs %4d: %6.1f ms/iter, %8.0f phenos/sec\n"
                                        pop-size
                                        xs-max
                                        max-leafs
                                        (:ms-per-iter result)
                                        (:phenos-per-sec result))])))))

(println "\n=== Parallel Benchmark Done ===")
(println "\n=== Results (Parallel Mode) ===")
(println (apply str @results*))

;; Deterministic benchmark
(println "\n=== Deterministic Benchmark (single-threaded) ===\n")

(def deterministic-results* (atom []))

(doseq [pop-size [10 100 500 1000]]
  (swap! deterministic-results* concat ["\n"])
  (doseq [xs-max [5.0 20.0]]
    (doseq [max-leafs [40 120]]
      (let [result (benchmark-evolution pop-size 10 max-leafs xs-max :random-seed 12345)]
        (swap! deterministic-results* concat [(format "Pop %4d, XsMax %4.1f, MaxLeafs %4d: %6.1f ms/iter, %8.0f phenos/sec (deterministic)\n"
                                                      pop-size
                                                      xs-max
                                                      max-leafs
                                                      (:ms-per-iter result)
                                                      (:phenos-per-sec result))])))))

(println "\n=== Results (Deterministic Mode) ===")
(println (apply str @deterministic-results*))

;; Comparison summary
(println "\n=== Parallel vs Deterministic Comparison ===\n")

(doseq [pop-size [100 1000]]
  (let [parallel-result (benchmark-evolution pop-size 10 40 5.0)
        deterministic-result (benchmark-evolution pop-size 10 40 5.0 :random-seed 42)
        speedup (/ (:phenos-per-sec parallel-result) (:phenos-per-sec deterministic-result))]
    (println (format "Pop %4d: Parallel %8.0f phenos/sec, Deterministic %8.0f phenos/sec, Speedup: %.2fx"
                     pop-size
                     (:phenos-per-sec parallel-result)
                     (:phenos-per-sec deterministic-result)
                     speedup))))

(println "\n=== All Benchmarks Complete ===")

(System/exit 0)
