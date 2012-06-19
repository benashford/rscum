(ns rscum.data
  (:require [redis.core :as redis]))

(def +redis-server+ {:host "localhost" :port 6379})

(def +crawl-queue+ "crawl-queue")
(def +crawled-users+ "crawled-users")

(defn crawled? [username]
  (redis/with-server +redis-server+
    (redis/sismember +crawled-users+ username)))

(defn- user-following-key [username]
  (format "user:%s:following" username))

(defn save-following [username following]
  (redis/with-server +redis-server+
    (redis/sadd +crawled-users+ username)
    (doseq [follow following]
      (if (not (crawled? follow))
        (redis/rpush +crawl-queue+ follow))
      (redis/sadd (user-following-key username) follow))))

(defn save-watching [username watching]
  (redis/with-server +redis-server+
    (doseq [watch watching]
      (redis/sadd (format "user:%s:watching" username) watch))))

(defn next-crawl-username []
  (redis/with-server +redis-server+
    (redis/lpop +crawl-queue+)))

(defn crawled-count []
  (redis/with-server +redis-server+
    (redis/scard +crawled-users+)))

(defn- load-users []
  (redis/smembers +crawled-users+))

(defn- load-user-following [username]
  (redis/smembers (user-following-key username)))

(defn load-following []
  (redis/with-server +redis-server+
    (let [users (load-users)]
      (into {} (map (fn [user] [user (load-user-following user)]) users)))))
