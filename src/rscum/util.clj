(ns rscum.util)

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

(defn zip [& xs]
  (when (first (first xs))
    (lazy-seq
      (cons
        (map first xs)
        (apply zip (map next xs))))))

(defn flatten-nested [nested]
  (->>
    (map
      (fn [[k v]]
        ;;(println "flatten-nested map fn, k:" k "v:" v)
        (map (fn [vi] [k vi]) v))
      nested)
    (apply concat)))
