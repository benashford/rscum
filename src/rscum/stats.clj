(ns rscum.stats
  (:use [clojure.set])
  (:use [rscum.util]))

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
  (let [seconds (sort (map second s))
        lowest (first seconds)
        multiplier (/ 1 (- (last seconds) lowest))]
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
          0.0 distance ;; Not sure about thiss
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
        (mapv
          (fn [x]
            (* (/ (- (nth position x) (nth other-position x)) distance) error-term))
          (range 1 3)))
      pde)))

(defn reduce-dimensions-position-iteration
  "Called once-per-node-per-iteration, returns the new position"
  [[user pos-x pos-y] positions edge]
  (let [distances (map (fn [[o-pos-x o-pos-y]] (distance [pos-x pos-y] [o-pos-x o-pos-y])) positions)
        error-terms (calc-error-terms (zip (map first positions) distances) (partial edge user))
        grad (calc-grad [pos-x pos-y] (zip positions distances error-terms))
        move (fn [p gidx] (- p (* (/ 1 (count positions)) (nth grad gidx))))]
      (if (some #(> (Math/abs %) 1000) grad) (println "grad:" grad))
    [user (move pos-x 0) (move pos-y 1)]))

(defn reduce-dimensions-iteration
  [positions edge]
  (pmap
    (fn [position]
      (reduce-dimensions-position-iteration
        position
        (filter #(not (= position %)) positions)
        edge))
    positions))

(defn reduce-dimensions [users watching]
  (let [initial-positions (zip users (rand-double-seq -0.5 0.5) (rand-double-seq -0.5 0.5))
        edge (make-similarity-edge watching)]
    (loop [positions initial-positions
           iterations 15]
      (if
        (<= iterations 0)
        positions
        (recur (reduce-dimensions-iteration positions edge) (dec iterations))))))
