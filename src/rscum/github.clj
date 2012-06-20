(ns rscum.github
  (:use [tentacles.users :only [following]])
  (:use [tentacles.repos :only [watching]]))

(def +per-page+ 100)

(defn- fetch-all [f & params]
  (loop [results []
         page 1]
    (let [next-results (apply #(f % {:page page :per-page +per-page+}) params)
          concat-results (concat results next-results)]
      (if (< (count next-results) +per-page+)
        concat-results
        (recur concat-results (inc page))))))

(defn following-users [username]
  (map :login (fetch-all following username)))

(defn watching-repos [username]
  (map :full_name (fetch-all watching username)))
