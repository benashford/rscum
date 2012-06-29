(ns rscum.results
  (:require [rscum.util :as util])
  (:use [incanter.core :only [view]])
  (:use [incanter.charts :only [scatter-plot histogram add-pointer add-text]])
  (:use [incanter.pdf :only [save-pdf]]))

;;
;; GRAPHS
;;

(defn- make-clusters [clustered]
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
    plot))

(defn show-clusters [clustered]
  (let [plot (make-clusters clustered)]
    (view plot)))

(defn save-clusters [clustered filename]
  (let [plot (make-clusters clustered)]
    (save-pdf plot filename :height 2000 :width 2000)))

;;
;; TEXT
;;

(defn produce-one-cluster-information [cluster members watching ranks callback-f]
  (callback-f (str "CLUSTER " cluster))
  (callback-f " - members:")
  (doseq [
    line
      (util/space-columns
        8
        (util/zip
          (->>
            members
            (map first)
            (map (fn [user] [user (ranks user)]))
            (sort-by second)
            reverse
            (map #(apply (partial format "username: %s (%f)") %)))
          (->>
            members
            (map first)
            (map watching)
            (apply concat)
            frequencies
            (sort-by second)
            reverse
            (map #(apply (partial format "repo: %s (%d)") %)))))]
    (callback-f line)))

(defn produce-cluster-information [clustered watching ranks callback-f]
  "Prints a list of clusters, their members, and the top watched repos"
  (doseq [[cluster members] clustered]
    (produce-one-cluster-information cluster members watching ranks callback-f)
    (callback-f "")))
