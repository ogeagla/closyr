(ns score-benchmark
  (:require [closyr.ops.common :as ops-common]
            [closyr.ops.eval :as ops-eval]
            [closyr.ops :as ops]
            [closyr.ops.initialize :as ops-init])
  (:import [org.matheclipse.core.expression F]))

(defn benchmark-scoring []
  (let [x ops-common/sym-x
        expr (F/Plus (F/Sin x) (F/Times x x))
        pheno (ops-common/->phenotype x expr nil)

        ;; Create test data
        xs (vec (range 0.1 10.0 0.1))  ; 100 points
        ys (mapv #(+ (Math/sin %) (* % %)) xs)
        n (count xs)

        xs-list (ops-common/exprs->exprs-list (ops-common/doubles->exprs xs))
        run-args {:input-xs-list xs-list :input-xs-count n
                  :input-xs-vec xs :input-ys-vec ys}
        run-config {:max-leafs 40}

        ;; Pre-compute f(x) for scoring benchmarks
        f-of-xs (ops-eval/eval-vec-pheno pheno run-args)]

    (println "\n=== Scoring Benchmark ===\n")
    (println "Points:" n)
    (println "f(x) sample:" (take 3 f-of-xs))

    ;; Warmup
    (dotimes [_ 100]
      (ops/compute-score-from-actuals-and-expecteds pheno f-of-xs ys 5))

    ;; Current implementation
    (println "\n1. Current compute-score (10000x):")
    (time (dotimes [_ 10000]
            (ops/compute-score-from-actuals-and-expecteds pheno f-of-xs ys 5)))

    ;; Primitive array version
    (println "\n2. Primitive array version (10000x):")
    (let [^doubles ys-arr (double-array ys)
          ^doubles fx-arr (double-array f-of-xs)
          max-resid (double 1000000)]
      (time (dotimes [_ 10000]
              (let [n (alength ys-arr)
                    ;; Compute sum and max in single pass
                    [sum-resid max-resid-val]
                    (loop [i (int 0), sum (double 0.0), mx (double 0.0)]
                      (if (< i n)
                        (let [expected (aget ys-arr i)
                              actual (aget fx-arr i)
                              resid (if (Double/isNaN actual)
                                      max-resid
                                      (Math/abs (- expected actual)))
                              resid (Math/min max-resid resid)]
                          (recur (unchecked-inc-int i)
                                 (+ sum resid)
                                 (Math/max mx resid)))
                        [sum mx]))]
                (* -1.0 (+ (* 2.0 (/ sum-resid n)) max-resid-val))))))

    ;; Just the residual computation part
    (println "\n3. Just map compute-residual (10000x):")
    (time (dotimes [_ 10000]
            (map ops/compute-residual ys f-of-xs)))

    ;; Realized map
    (println "\n4. Realized map compute-residual (10000x):")
    (time (dotimes [_ 10000]
            (doall (map ops/compute-residual ys f-of-xs))))

    ;; reduce + and reduce max
    (println "\n5. reduce + and reduce max (10000x):")
    (let [resids (mapv #(Math/abs (- %1 %2)) ys f-of-xs)]
      (time (dotimes [_ 10000]
              (let [s (reduce + 0.0 resids)
                    m (reduce max resids)]
                (+ s m)))))

    ;; areduce version
    (println "\n6. areduce sum and max (10000x):")
    (let [^doubles resids-arr (double-array (mapv #(Math/abs (- %1 %2)) ys f-of-xs))]
      (time (dotimes [_ 10000]
              (let [s (areduce resids-arr i ret 0.0 (+ ret (aget resids-arr i)))
                    m (areduce resids-arr i ret 0.0 (Math/max ret (aget resids-arr i)))]
                (+ s m)))))

    (println "\n=== Done ===")))

(benchmark-scoring)
