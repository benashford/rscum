(ns rscum.stats
  (:use [clojure.set]))

(defn similarity [a b]
  (let [pcount (count (union a b))]
    (if (= pcount 0)
      0
      (double (/ (count (intersection a b)) pcount)))))

;; DELETE
(defn similarity-graph [watching]
  (loop [[user & users] (keys watching)
         graph []]
    (if (nil? users)
      graph
      (let [scores (map (fn [other-user] [#{user other-user} (similarity (watching user) (watching other-user))]) users)]
        (println "USERS count:" (count users))
        (recur users (concat graph scores))))))

;; MAKE PRIVATE
(defn pairing-users [users map-f filter-f]
  (time
    (loop [[user & other-users] users
           results []]
      (println "USER:" user "count:" (count other-users) "Results count:" (count results))
      (if (nil? other-users)
        results
        (let [nresults (map (fn [user-b] #{user user-b}) other-users)]
          (recur other-users (into results (filter filter-f (map map-f nresults)))))))))
