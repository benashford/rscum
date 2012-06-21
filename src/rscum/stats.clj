(ns rscum.stats
  (:use [clojure.set]))

(defn similarity [a b]
  (float (/ (count (intersection a b)) (count (union a b)))))