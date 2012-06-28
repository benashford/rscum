(ns rscum.core
  (:require [rscum.data :as data])
  (:require [rscum.github :as github])
  (:require [rscum.rank :as rank])
  (:require [rscum.stats :as stats])
  (:require [rscum.util :as util])
  (:use [incanter.core :only [view]])
  (:use [incanter.charts :only [scatter-plot histogram add-pointer add-text]]))

(defn save-following [username]
  (data/save-following username (github/following-users username)))

(defn save-watching [username]
  (data/save-watching username (github/watching-repos username)))

(defn crawl-iteration []
  (when-let [username (data/next-crawl-username)]
    (println "Crawling: " username)
    (if (not (data/crawled? username))
      (do
        (save-following username)
        (save-watching username)))))

(defn crawl
  ([target]
    (letfn [
      (crawl-f []
        (if (and (< (data/crawled-count) target) (> (data/crawl-queue-len) 0))
          (do
            (crawl-iteration)
            crawl-f)
          nil))]
      (trampoline crawl-f)))
   ([username target]
    (do
      (save-following username)
      (save-watching username)
      (crawl target))))

(defn rank []
  (time
    (let [following-hash (data/load-following)
          user-ranks (rank/page-rank following-hash)]
      (data/save-ranks user-ranks))))

(defn show-rank []
  (let [ordered-users (data/get-users-by-rank)]
    (doseq [[user rank] (take 100 ordered-users)]
      (println "User: " user "\tRank: " (format "%.8f" rank)))))

(defn similarity [watching-data user-a user-b]
  (stats/similarity (watching-data user-a) (watching-data user-b)))

(defn most-similar-to [watching-data user-a]
  (->>
    (keys watching-data)
    (remove #{user-a})
    (map (fn [user-b] [user-b (similarity watching-data user-a user-b)]))
    (filter #(> (second %) 0))
    (sort-by second)))

(defn show-flattened-plots []
  (time
    (let [watching (data/load-watching)
          flattened (stats/reduce-dimensions (keys watching) watching)
          plot (scatter-plot (map #(nth % 1) flattened) (map #(nth % 2) flattened))]
      (doseq [item flattened]
        (add-pointer plot (nth item 1) (nth item 2) :text (nth item 0) :angle :sw))
      (view plot))))

(defn cluster [k]
  (time
    (let [watching (data/load-watching)
          flattened (stats/reduce-dimensions (keys watching) watching)
          clustered (stats/k-means-cluster flattened k)
          cluster-points (map (fn [[k [_ x y]]] [k x y]) (util/flatten-nested clustered))
          plot (scatter-plot (map #(nth % 1) cluster-points) (map #(nth % 2) cluster-points) :group-by (map first cluster-points))]
      (doseq [item flattened]
        (add-pointer plot (nth item 1) (nth item 2) :text (nth item 0) :angle :sw))
      (view plot))))

(defn save-clusters [k]
  (time
    (let [watching (data/load-watching)
          flattened (stats/reduce-dimensions (keys watching) watching)
          clustered (stats/k-means-cluster flattened k)
          numbered-clusters (util/rearrange [1 0] (util/zip (map second clustered) (range)))]
      (doseq [[number elements] numbered-clusters]
        (data/save-cluster number elements)))))
