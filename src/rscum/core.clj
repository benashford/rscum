(ns rscum.core
  (:require [rscum.data :as data])
  (:require [rscum.github :as github])
  (:require [rscum.rank :as rank])
  (:require [rscum.results :as results])
  (:require [rscum.stats :as stats])
  (:require [rscum.util :as util])
  (:use [incanter.core :only [view]])
  (:use [incanter.charts :only [scatter-plot histogram add-pointer add-text]])
  (:use [clojure.stacktrace]))

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

(defn show-clusters []
  "Loads and shows the saved clustered data"
  (results/show-clusters (data/load-clusters)))

(defn pdf-clusters [filename]
  "Loads and saves as PDF the clustered data"
  (results/save-clusters (data/load-clusters) filename))

(defn save-clusters [k]
  "Plot the 2d graph, calculate the clusters, and saves to the database - WARNING: takes many minutes"
  (time
    (let [watching (data/load-watching)
          flattened (stats/reduce-dimensions (keys watching) watching)
          clustered (stats/k-means-cluster flattened k)
          numbered-clusters (util/rearrange [1 0] (util/zip (map second clustered) (range)))]
      (doseq [[number elements] numbered-clusters]
        (data/save-clusters number elements)))))

(defn print-clusters []
  (results/produce-cluster-information
    (sort-by first (data/load-clusters))
    (data/load-watching)
    data/get-rank))
