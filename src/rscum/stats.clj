(ns rscum.stats
  (:use [clojure.set]))

(defn similarity [a b]
  (/ (count (intersection a b)) (count (union a b))))
