(ns rscum.data
  (:require [redis.core :as redis]))

(def +redis-server+ {:host "localhost" :port 6379})

(def +crawled-users+ "crawled-users")
(def +ordered-crawl-queue+ "ordered-crawl-queue")

(defn crawled? [username]
  (redis/with-server +redis-server+
    (redis/sismember +crawled-users+ username)))

(defn- user-following-key [username]
  (format "user:%s:following" username))

(defn save-following [username following]
  (redis/with-server +redis-server+
    (redis/sadd +crawled-users+ username)
    (doseq [follow following]
      (redis/sadd (user-following-key username) follow))))

(defn save-watching [username watching]
  (redis/with-server +redis-server+
    (doseq [watch watching]
      (redis/sadd (format "user:%s:watching" username) watch))))

(defn next-crawl-username []
  (redis/with-server +redis-server+
    (when-let [next-username (first (redis/zrevrange +ordered-crawl-queue+ 0 0))]
      (redis/zrem +ordered-crawl-queue+ next-username)
      next-username)))

(defn crawled-count []
  (redis/with-server +redis-server+
    (redis/scard +crawled-users+)))

(defn crawl-queue-len []
  (redis/with-server +redis-server+
    (redis/zcard +ordered-crawl-queue+)))

(defn- load-users []
  (redis/smembers +crawled-users+))

(defn- load-user-following [username]
  (redis/smembers (user-following-key username)))

(defn load-following []
  (redis/with-server +redis-server+
    (let [users (load-users)]
      (into {} (map (fn [user] [user (load-user-following user)]) users)))))

(defn- user-rank-key [username]
  (format "user:%s:rank" username))

(defn save-ranks [user-ranks]
  (redis/with-server +redis-server+
    (doseq [user (keys user-ranks)]
      (let [rank (user-ranks user)]
        (redis/set (user-rank-key user) rank)
        (if (not (crawled? user))
          (redis/zadd +ordered-crawl-queue+ rank user))))))

(defn get-users-by-rank []
  (redis/with-server +redis-server+
    (let [users (load-users)
          get-f (fn [user] [user (->> (user-rank-key user) redis/get Double/parseDouble)])]
      (->> (map get-f users)
           (sort-by second)
           reverse))))

(defn delete-users [usernames]
  (redis/with-server +redis-server+
    (doseq [username usernames]
      (redis/srem +crawled-users+ username))))
