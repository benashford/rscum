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

(defn crawl-and-rank [target rank-every]
  (rank)
  (loop [next-target rank-every]
    (crawl next-target)
    (rank)
    (if
      (< next-target target)
      (recur (+ next-target rank-every)))))

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
    (sort-by second)
    (take 50)))

(defn show-clusters
  "Loads and shows the saved clustered data"
  []
  (results/show-clusters (data/load-clusters)))

(defn pdf-clusters
  "Loads and saves as PDF the clustered data"
  [filename]
  (results/save-clusters (data/load-clusters) filename))

(defn similarity-histogram
  "Show a histogram of similarity scores, to show suitability of particular function"
  [sim-f post-f]
  (let [watching (data/load-watching)
        edges (stats/similarity-edges sim-f post-f watching)]
    (results/show-similarity-histogram edges)))

(defn pdf-similarity-histogram
  [sim-f post-f filename]
  (let [watching (data/load-watching)
        edges (stats/similarity-edges sim-f post-f watching)]
    (results/save-similarity-histogram edges filename)))

(defn plot-2d
  "Uses a stress minimisation algorithm to plot users in 2 dimensions - WARNING: takes many minutes"
  []
  (let [watching (data/load-watching)]
    (doseq [user-2d (stats/reduce-dimensions (keys watching) watching)]
      (data/save-2d user-2d))))

(defn save-clusters
  "Calculate the clusters, and saves to the database - WARNING: plot-2d must have been called first"
  [k]
  (time
    (let [watching (data/load-watching)
          flattened (data/load-2d)
          clustered (stats/cluster flattened k)
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
