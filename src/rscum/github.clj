(ns rscum.github
  (:use [tentacles.users :only [following]])
  (:use [tentacles.repos :only [watching]])
  (:use [tentacles.core :only [api-call]]))

(def +per-page+ 100)

(defn rate-limit
  "Temporary until a newer version of Tentacles is published on Clojars"
  []
  ((api-call :get "rate_limit" nil nil) :rate))

(let [rl (rate-limit)]
  (def +limit+ (rl :limit))
  (def remaining (atom (rl :remaining))))

(defn- wait-for-quota
  []
  (println "Quota limit reached, sleeping for one minute...")
  (Thread/sleep 60000)
  (println "...re-checking quota...")
  (swap! remaining + ((rate-limit) :remaining))
  (println "...remaining: " @remaining)
  (if (<= @remaining 0) (recur)))

(defn- fetch-all [f & params]
  (loop [results []
         page 1]
    (if (<= @remaining 0)
      (wait-for-quota))
    (let [next-results (apply #(f % {:page page :per-page +per-page+}) params)
          concat-results (concat results next-results)]
      (swap! remaining dec)
      (if (< (count next-results) +per-page+)
        concat-results
        (recur concat-results (inc page))))))

(defmacro fromgithub [fn-name params after before]
  `(defn ~fn-name ~params
    (~@after (fetch-all ~@before))))

(fromgithub following-users [username] (map :login) (following username))
(fromgithub watching-repos [username] (map :full_name) (watching username))
