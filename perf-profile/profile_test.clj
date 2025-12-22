(ns profile-mutations
  (:require [closyr.ops.common :as ops-common]
            [closyr.ops.modify :as ops-modify]
            [closyr.ops.initialize :as ops-init])
  (:import [org.matheclipse.core.expression F]))

(defn profile-mutation-internals []
  (let [x ops-common/sym-x
        pheno (ops-common/->phenotype x (F/Plus (F/Sin x) (F/Times x x)) nil)
        muts (ops-init/initial-mutations)]

    (println "\n=== Mutation Internals ===\n")

    ;; Profile single modification types
    (println "1. Single modify-fn mutation (100x):")
    (let [mod-fn (first (filter #(= :modify-fn (:op %)) muts))]
      (time (dotimes [_ 100]
              (ops-modify/modify mod-fn pheno))))

    (println "\n2. Single modify-leafs mutation (100x):")
    (let [mod-leafs (first (filter #(= :modify-leafs (:op %)) muts))]
      (time (dotimes [_ 100]
              (ops-modify/modify mod-leafs pheno))))

    (println "\n3. Single modify-branches mutation (100x):")
    (let [mod-branch (first (filter #(= :modify-branches (:op %)) muts))]
      (time (dotimes [_ 100]
              (ops-modify/modify mod-branch pheno))))

    (println "\n4. apply-modifications with 1 mod (100x):")
    (time (dotimes [_ 100]
            (ops-modify/apply-modifications 40 1 muts pheno pheno)))

    (println "\n5. apply-modifications with 5 mods (100x):")
    (time (dotimes [_ 100]
            (ops-modify/apply-modifications 40 5 muts pheno pheno)))

    (println "\n6. Just F/Sin creation (1000x):")
    (time (dotimes [_ 1000]
            (F/Sin x)))

    (println "\n7. Just ->phenotype from expr (1000x):")
    (let [expr (F/Plus (F/Sin x) x)]
      (time (dotimes [_ 1000]
              (ops-common/->phenotype x expr nil))))

    (println "\n=== Done ===\n")))

(profile-mutation-internals)
