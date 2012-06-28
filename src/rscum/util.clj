(ns rscum.util
  (:use [clojure.string :only [trim]]))

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

(defn rearrange [order s]
  (map
    (fn [row]
      (map #(nth row %) order))
    s))

(defn flatten-nested [nested]
  (->>
    (map
      (fn [[k v]]
        (map (fn [vi] [k vi]) v))
      nested)
    (apply concat)))

(defn spaces [n]
  (apply str (repeat n \space)))

(defn space-columns [min-space rows-raw]
  (let [
    rows (map (fn [string] (map str string)) rows-raw)
    max-width (fn [idx] (apply max (map #(+ (count (nth % idx)) min-space) rows)))
    max-widths
      (map
        (fn [column-idx]
          (max-width column-idx))
        (range (count (first rows-raw))))]
    (map
      (fn [row]
        (str
          (spaces min-space)
          (trim
            (apply str
              (map
                (fn [[text max-space]]
                  (str text (spaces (- max-space (count text)))))
                (zip row max-widths))))))
      rows)))
