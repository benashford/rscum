(ns rscum.core
  (:require [rscum.data :as data])
  (:require [rscum.github :as github])
  (:require [rscum.rank :as rank])
  (:require [rscum.results :as results])
  (:require [rscum.stats :as stats])
  (:require [rscum.util :as util])
  (:use [incanter.core :only [view]])
  (:use [incanter.charts :only [scatter-plot histogram add-pointer add-text]])
  (:use [clojure.java.io])
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

(defn show-clusters
  "Loads and shows the saved clustered data"
  []
  (results/show-clusters (data/load-clusters)))

(defn pdf-clusters
  "Loads and saves as PDF the clustered data"
  [filename]
  (results/save-clusters (data/load-clusters) filename))

(defn save-clusters
  "Plot the 2d graph, calculate the clusters, and saves to the database - WARNING: takes many minutes"
  [k]
  (time
    (let [watching (data/load-watching)
          flattened (stats/reduce-dimensions (keys watching) watching)
          clustered (stats/k-means-cluster flattened k)
          numbered-clusters (util/rearrange [1 0] (util/zip (map second clustered) (range)))]
      (doseq [[number elements] numbered-clusters]
        (data/save-clusters number elements)))))

(defn print-cluster-report
  ([]
    (results/produce-cluster-information
      (sort-by first (data/load-clusters))
      (data/load-watching)
      data/get-rank
      println))
  ([cluster]
    (results/produce-cluster-information
      (filter #(= (first %) cluster) (data/load-clusters))
      (data/load-watching)
      data/get-rank
      println)))

(defn show-details [username]
  (data/load-user username))

(defn save-cluster-report [filename]
  (with-open [wrtr (writer filename)]
    (results/produce-cluster-information
      (sort-by first (data/load-clusters))
      (data/load-watching)
      data/get-rank
      (fn [line] (.write wrtr (str line \newline))))))

(defn similarity-histogram
  "Show a histogram of similarity scores, to show suitability of particular function"
  [sim-f post-f]
  (let [watching (data/load-watching)
        edges (stats/similarity-edges sim-f post-f watching)
        scores (filter #(< % 1.0) (map second edges))]
    (view (histogram scores :nbins 100))))
