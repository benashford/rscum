(ns rscum.data
  (:require [redis.core :as redis]))

(def +redis-server+ {:host "localhost" :port 6379})

(def +crawled-users+ "crawled-users")
(def +ordered-crawl-queue+ "ordered-crawl-queue")

(defmacro defredf [fn-name args & body]
  `(defn ~fn-name ~args
    (redis/with-server +redis-server+
      ~@body)))

(defredf crawled? [username]
  (redis/sismember +crawled-users+ username))

(defn- user-following-key [username]
  (format "user:%s:following" username))

(defn- user-watching-key [username]
  (format "user:%s:watching" username))

(defn- user-rank-key [username]
  (format "user:%s:rank" username))

(defn- user-info-key [username]
  (format "user:%s:info" username))

(defredf save-following [username following]
  (redis/sadd +crawled-users+ username)
  (doseq [follow following]
    (redis/sadd (user-following-key username) follow)))

(defredf save-watching [username watching]
  (doseq [watch watching]
    (redis/sadd (user-watching-key username) watch)))

(defredf next-crawl-username []
  (when-let [next-username (first (redis/zrevrange +ordered-crawl-queue+ 0 0))]
    (redis/zrem +ordered-crawl-queue+ next-username)
    next-username))

(defredf crawled-count []
  (redis/scard +crawled-users+))

(defredf crawl-queue-len []
  (redis/zcard +ordered-crawl-queue+))

(defredf load-users []
  (redis/smembers +crawled-users+))

(defn- load-user-following [username]
  (redis/smembers (user-following-key username)))

(defn- load-user-watching [username]
  (redis/smembers (user-watching-key username)))

(defredf load-following []
  (let [users (load-users)]
    (into {} (map (fn [user] [user (load-user-following user)]) users))))

(defredf load-watching []
  (let [users (load-users)]
    (into {} (map (fn [user] [user (load-user-watching user)]) users))))

(defredf save-ranks [user-ranks]
  (doseq [user (keys user-ranks)]
    (let [rank (user-ranks user)]
      (redis/set (user-rank-key user) rank)
      (if (not (crawled? user))
        (redis/zadd +ordered-crawl-queue+ rank user)))))

(defredf get-users-by-rank []
  (let [users (load-users)
        get-f (fn [user] [user (->> user user-rank-key redis/get Double/parseDouble)])]
    (->>
      users
      (map get-f)
      (sort-by second)
      reverse)))

(defredf get-rank [user]
  (Double/parseDouble (redis/get (user-rank-key user))))

(defredf delete-users [usernames]
  (doseq [username usernames]
    (redis/srem +crawled-users+ username)))

(defredf save-2d [[user x y]]
  (redis/hmset
    (user-info-key user)
    "x" x
    "y" y))

(defredf load-2d []
  (mapv
    (fn [user]
      (let [[x y] (redis/hmget (user-info-key user) "x" "y")]
        [user (Double/parseDouble x) (Double/parseDouble y)]))
    (load-users)))

(defredf save-clusters [cluster-num elements]
  (doseq [[user x y] elements]
    (redis/hmset
      (user-info-key user)
      "cluster" cluster-num)))

(defredf load-clusters []
  (map
    (fn [[k v]]
      [k (map (fn [[_ u x y]] [u x y]) v)])
    (group-by first
      (map
        (fn [user]
          (let [[c x y] (redis/hmget (user-info-key user) "cluster" "x" "y")]
            [(Integer/parseInt c) user (Double/parseDouble x) (Double/parseDouble y)]))
        (load-users)))))

(defredf load-user [username]
  (let [uik (user-info-key username)]
    {:rank (get-rank username)
     :cluster (Integer/parseInt (redis/hget uik "cluster"))
     :x (Double/parseDouble (redis/hget uik "x"))
     :y (Double/parseDouble (redis/hget uik "y"))}))
