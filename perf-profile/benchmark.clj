(ns benchmark
  (:require [closyr.ops.common :as ops-common]
            [closyr.ops.eval :as ops-eval]
            [closyr.ops.initialize :as ops-init]
            [closyr.ops :as ops]
            [closyr.ga :as ga]))

(defn benchmark-evolution [pop-size n-iters]
  (let [phenos (ops-init/initial-phenotypes pop-size)
        muts (ops-init/initial-mutations)
        xs (vec (range 0.1 10.0 0.2))
        ys (mapv #(+ (* % %) (Math/sin %)) xs)
        input-xs-list (ops-common/exprs->exprs-list (ops-common/doubles->exprs xs))
        run-args {:input-xs-list  input-xs-list
                  :input-xs-count (count xs)
                  :input-xs-vec   xs
                  :input-ys-vec   ys
                  :input-ys-arr   (double-array ys)}
        run-config {:max-leafs 40}
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
       :phenos-per-sec (/ (* pop-size n-iters 1000) elapsed-ms)})))

(println "\n=== Evolution Benchmark ===\n")

(doseq [pop-size [10 50 100 500 1000 2000 4000 10000 50000]]
  (let [result (benchmark-evolution pop-size 10)]
    (printf "Pop %4d: %6.1f ms/iter, %8.0f phenos/sec\n"
            pop-size
            (:ms-per-iter result)
            (:phenos-per-sec result))))

(println "\n=== Done ===")
(System/exit 0)
