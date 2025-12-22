(ns score-only-bench
  "Benchmark just the score-fn to isolate residual computation"
  (:require [closyr.ops.common :as ops-common]
            [closyr.ops.eval :as ops-eval]
            [closyr.ops :as ops])
  (:import [org.matheclipse.core.expression F]))

(defn bench-score-fn [n-points n-calls use-arr?]
  (let [x ops-common/sym-x
        expr (F/Plus (F/Sin x) (F/Times x x))
        pheno (ops-common/->phenotype x expr nil)
        xs (vec (range 0.1 (+ 0.1 (* n-points 0.1)) 0.1))
        ys (mapv #(+ (Math/sin %) (* % %)) xs)
        xs-list (ops-common/exprs->exprs-list (ops-common/doubles->exprs xs))
        run-args (cond-> {:input-xs-list  xs-list
                          :input-xs-count (count xs)
                          :input-xs-vec   xs
                          :input-ys-vec   ys}
                   use-arr? (assoc :input-ys-arr (double-array ys)))
        run-config {:max-leafs 40}]

    ;; Warmup
    (dotimes [_ 100]
      (ops/score-fn run-args run-config pheno))

    ;; Timed
    (let [start (System/nanoTime)]
      (dotimes [_ n-calls]
        (ops/score-fn run-args run-config pheno))
      (/ (- (System/nanoTime) start) 1e6))))

(println "\n=== Score Function Benchmark (isolating residual computation) ===\n")

(doseq [n-points [50 100 200 500]]
  (let [n-calls 1000
        time-without (bench-score-fn n-points n-calls false)
        time-with (bench-score-fn n-points n-calls true)
        speedup (/ time-without time-with)]
    (printf "Points: %3d | WITHOUT arr: %7.1f ms | WITH arr: %7.1f ms | Speedup: %.2fx\n"
            n-points time-without time-with speedup)))

(println "\n=== Done ===")
(System/exit 0)
