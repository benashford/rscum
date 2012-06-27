(ns rscum.stats
  (:use [clojure.set]))

;; Move to util package
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

;;
;; Similarity scoring
;;
(defn similarity
  "Calculate the similarity between two sets - 0 being lowest"
  [a b]
  (let [pcount (count (union a b))]
    (if (= pcount 0)
      0
      (Math/sqrt (double (/ (count (intersection a b)) pcount))))))

(defn- normalize-second
  "Normalise the second item in each tuple in a sequence"
  [s]
  (let [seconds (map second s)
        lowest (apply min seconds)
        multiplier (/ 1 (- (apply max seconds) lowest))]
    (map
      (fn [[k v]]
        [k (- 1 v)]) ;; (* (- v lowest) multiplier))])
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

(defn- make-similarity-edge [watching]
  (let [edges (into {} (similarity-edges watching))]
    (fn [a b]
      (edges #{a b}))))

;;
;; Random numbers
;;
(let [rand (java.util.Random.)]
  (defn rand-double [lower upper]
    (->>
      (.nextDouble rand)
      (* (- upper lower))
      (+ lower))))

(defn rand-double-seq [lower upper]
  (lazy-seq (cons (rand-double lower upper) (rand-double-seq lower upper))))

(defn zip [& xs]
  (when (first (first xs))
    (lazy-seq
      (cons
        (map first xs)
        (apply zip (map next xs))))))

;;
;; Dimension reduceing
;;
(defn distance [a b]
  (Math/sqrt
    (+
      (Math/pow (- (first a) (first b)) 2)
      (Math/pow (- (last a) (last b)) 2))))

(defn calc-error-terms [user-distances user-edge]
  (map
    (fn [[other-user distance]]
      (let [real-distance (user-edge other-user)]
        (case real-distance
          0.0 distance
          1.0 0.0
          (/ (- distance real-distance) real-distance))))
    user-distances))

(defn calc-grad [position pde]
  (reduce
    (fn [x y]
      (map
        (fn [idx]
          (+ (nth x idx) (nth y idx)))
        (range (count x))))
    [0.0 0.0]
    (map
      (fn [[other-position distance error-term]]
        (map
          (fn [x]
            (* (/ (- (nth position x) (nth other-position x)) distance) error-term))
          (range 1 3)))
      pde)))

(defn reduce-dimensions-position-iteration
  [position positions edge]
  (let [user (first position)
        distances (map (fn [other-position] (distance [(nth position 1) (nth position 2)] [(nth other-position 1) (nth other-position 2)])) positions)
        error-terms (calc-error-terms (zip (map first positions) distances) (partial edge user))
        grad (calc-grad position (zip positions distances error-terms))
        move (fn [p gidx] (- p (* 0.01 (nth grad gidx))))]
      ;;(println "distances:" distances "error-terms:" error-terms)
    [user (move (nth position 1) 0) (move (nth position 2) 1)]))

(defn reduce-dimensions-iteration
  [positions edge]
  (map
    (fn [position]
      (reduce-dimensions-position-iteration
        position
        (filter #(not (= position %)) positions)
        edge))
    positions))

(defn reduce-dimensions [users watching]
  (let [initial-positions (zip users (rand-double-seq -10 10) (rand-double-seq -10 10))
        edge (make-similarity-edge watching)]
    (loop [positions initial-positions
           iterations 10]
      (if
        (<= iterations 0)
        positions
        (recur (reduce-dimensions-iteration positions edge) (dec iterations))))))
