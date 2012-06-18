(ns rscum.github
  (:use [tentacles.users :only [following]])
  (:use [tentacles.repos :only [watching]]))

(defn following-users [username]
  (map :login (following username)))

(defn watching-repos [username]
  (map :full_name (watching username)))
