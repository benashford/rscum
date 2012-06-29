(ns rscum.results
  (:require [rscum.util :as util])
  (:use [incanter.core :only [view]])
  (:use [incanter.charts :only [scatter-plot histogram add-pointer add-text]]))

(defn show-clusters [clustered]
  (let [cluster-points (map (fn [[k [_ x y]]] [k x y]) (util/flatten-nested clustered))
    plot (scatter-plot
            (map #(nth % 1) cluster-points)
            (map #(nth % 2) cluster-points)
            :group-by (map first cluster-points)
            :title "Similar GitHub users"
            :x-label "x"
            :y-label "y")]
    (doseq [flattened (map second clustered)]
      (doseq [item flattened]
        (add-pointer plot (nth item 1) (nth item 2) :text (nth item 0) :angle :sw)))
    (view plot)))
