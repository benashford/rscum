(ns rscum.stats
  (:use [clojure.set]))

(defn similarity [a b]
  (let [pcount (count (union a b))]
    (if (= pcount 0)
      0
      (double (/ (count (intersection a b)) pcount)))))

(defn pair-map [map-f users]
  (letfn [(outer-f [users]
    (let [user (first users)]
      (when-let [other-users (next users)]
        (letfn [(inner-f [inner-users]
          (if (empty? inner-users)
            (outer-f other-users)
            (let [other-inner-users (rest inner-users)
                  v (map-f user (first inner-users))]
              (if-not (nil? v)
                (lazy-seq (cons v (inner-f other-inner-users)))
                (recur other-inner-users)))))]
          (inner-f other-users)))))]
    (outer-f users)))

(defn similarity-graph [watching]
  (into {}
    (pair-map
      (fn [user-a user-b]
        (let [sscore (similarity (watching user-a) (watching user-b))]
          (if (= sscore 0.0) nil [#{user-a user-b} sscore])))
      (keys watching))))
