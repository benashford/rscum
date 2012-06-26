(ns rscum.stats
  (:use [clojure.set]))

(defn similarity
  "Calculate the similarity between two sets - 0 being lowest"
  [a b]
  (let [pcount (count (union a b))]
    (if (= pcount 0)
      0
      (Math/sqrt (double (/ (count (intersection a b)) pcount))))))

(defn pair-map
  "Apply a map function on each pair of users"
  [map-f users]
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

(defn- normalize-second
  "Normalise the second item in each tuple in a sequence"
  [s]
  (let [seconds (map second s)
        lowest (apply min seconds)
        multiplier (/ 1 (- (apply max seconds) lowest))]
    (map
      (fn [[k v]]
        [k (- 1 (* (- v lowest) multiplier))])
      s)))

(defn similarity-edges
  "The edges of a graph, defined as similarity"
  [watching]
  (normalize-second
    (pair-map
      (fn [user-a user-b]
        (let [sscore (similarity (watching user-a) (watching user-b))]
          [#{user-a user-b} sscore]))
      (keys watching))))
