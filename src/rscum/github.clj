(ns rscum.github
  (:use [tentacles.users :only [following]])
  (:use [tentacles.repos :only [watching]])
  (:use [tentacles.core :only [api-call]]))

(def +per-page+ 100)

(defn- fetch-all [f & params]
  (loop [results []
         page 1]
    (let [next-results (apply #(f % {:page page :per-page +per-page+}) params)
          concat-results (concat results next-results)]
      (if (< (count next-results) +per-page+)
        concat-results
        (recur concat-results (inc page))))))

(defmacro fromgithub [fn-name params after before]
  `(defn ~fn-name ~params
    (~@after (fetch-all ~@before))))

(fromgithub following-users [username] (map :login) (following username))
(fromgithub watching-repos [username] (map :full_name) (watching username))

;; Temporary until a newer version of Tentacles is published on Clojars

(defn rate-limit []
  (api-call :get "rate_limit" nil nil))