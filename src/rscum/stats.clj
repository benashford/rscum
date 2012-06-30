(ns rscum.stats
  (:use [clojure.set])
  (:use [rscum.util]))

;;
;; Miscellaneous
;;

(defn- log2 [x]
  (/ (Math/log x) (Math/log 2)))

;;
;; Similarity scoring - the functions
;;
(defn null-sim [a b]
  0.5)

(defn set-overlap
  "Calculate the Jaccard Index between two sets - 0 being lowest"
  [a b]
  (-
    1
    (let [pcount (count (union a b))]
      (if (= pcount 0)
        0.0
        (Math/sqrt (double (/ (count (intersection a b)) pcount)))))))

(defn tanimoto [ff a b]
  (let [so (- 1 (set-overlap a b))]
    (if (= 0.0 so) ff (* -1 (log2 so)))))

;; The default similarity scoring function
(def similarity (partial tanimoto 10.0))

;;
;; Similarity scoring - post-processing
;;
(defn null-pp [s]
  s)

(defn normalise-second
  "Normalise the second item in each tuple in a sequence"
  [s]
  (let [seconds (sort (map second s))
        lowest (first seconds)
        multiplier (/ 1 (- (last seconds) lowest))]
    (map
      (fn [[k v]]
        [k (* (- v lowest) multiplier)])
      s)))

(def post-process normalise-second)

(defn similarity-edges
  "The edges of a graph, defined as similarity"
  ([watching]
    (similarity-edges similarity post-process watching))
  ([sim-f post-f watching]
    (post-f
      (pair-map
        (fn [user-a user-b]
          (let [sscore (sim-f (watching user-a) (watching user-b))]
            [#{user-a user-b} sscore]))
        (keys watching)))))

(defn- make-similarity-edge
  "For a given map of watched repos, produce a function that returns the similarity score between two users"
  [watching]
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
  (Math/hypot
    (- (first a) (first b))
    (- (last a) (last b))))

(defn calc-error-terms [user-distances user-edge]
  (map
    (fn [[other-user distance]]
      (let [real-distance (user-edge other-user)]
        (case real-distance
          0.0 distance ;; Not sure about thiss
          (/ (- distance real-distance) real-distance))))
    user-distances))

(defn calc-grad [positions pde]
  (reduce
    (fn [x y]
      (map
        (fn [idx]
          (+ (nth x idx) (nth y idx)))
        (range (count x))))
    [0.0 0.0]
    (map
      (fn [[other-positions distance error-term]]
        (mapv
          (fn [[position other-position]]
            (* (/ (- position other-position) distance) error-term))
          (zip positions other-positions)))
      pde)))

(defn reduce-dimensions-position-iteration
  "Called once-per-node-per-iteration, returns the new position"
  [[user pos-x pos-y] full-positions edge]
  {:pre [(> (count full-positions) 0)]}
  (let [pos [pos-x pos-y]
        positions (map (fn [[_ x y]] [x y]) full-positions)
        distances (map (fn [o-pos] (distance pos o-pos)) positions)
        error-terms (calc-error-terms (zip (map first full-positions) distances) (partial edge user))
        total-error-term (->> error-terms (map #(Math/abs %)) (reduce +))
        grad (calc-grad pos (zip positions distances error-terms))
        move (fn [p gidx] (- p (* (/ 1 (count positions)) (nth grad gidx))))]
    [user (move pos-x 0) (move pos-y 1) total-error-term]))

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
        (let [next-positions-with-errors (reduce-dimensions-iteration positions edge)]
          (println "TOTAL error, iteration:" iterations "=" (reduce + (map #(nth % 3) next-positions-with-errors)))
          (recur (map (fn [[a b c _]] [a b c]) next-positions-with-errors) (dec iterations)))))))

;;
;; CLUSTERING
;;

;; k-means clustering
;;
(defn- by-centoid [data centoids]
  {:pre [(> (count centoids) 0)]
   :post [(= (count %) (count centoids))]}
  (letfn [
    (do-distance [[_ x y] centoid]
      (distance [x y] centoid))
    (find-centoid [point]
      (->>
        (map (fn [centoid] [(do-distance point centoid) centoid]) centoids)
        (sort-by first)
        first
        second))]
    (->>
      (map (fn [point] {(find-centoid point) [point]}) data)
      (reduce
        (partial merge-with concat)
        (reduce (fn [h k] (assoc h k [])) {} centoids)))))

(defn- re-centre-centoids [points-by-centoid]
  {:pre [(> (count points-by-centoid) 0)]
   :post [(= (count %) (count points-by-centoid))]}
  (map
    (fn [points-for-centoid]
      (let [xes (map #(nth % 1) points-for-centoid)
            yes (map #(nth % 2) points-for-centoid)
            x-count (count xes)
            y-count (count yes)
            new-x (if (= x-count 0) (rand-double -0.5 0.5) (/ (reduce + xes) x-count))
            new-y (if (= y-count 0) (rand-double -0.5 0.5) (/ (reduce + yes) y-count))]
        [new-x new-y]))
    points-by-centoid))

(defn k-means-cluster [data k]
  (loop [centoids (map (fn [_] [(rand-double -0.5 0.5) (rand-double -0.5 0.5)]) (range k))
         iterations 50]
    (let [points-by-centoid (by-centoid data centoids)]
      (if (<= iterations 0)
        points-by-centoid
        (recur (re-centre-centoids (map second points-by-centoid)) (dec iterations))))))

;; Angular clustering
;;
(defn angular-cluster [data k]
  (let [angle-per-cluster (/ (* 2 Math/PI) k)]
    (->>
      data
      (map
        (fn [[u x y :as point]]
          {(int (Math/floor (/ (+ (Math/atan2 x y) Math/PI) angle-per-cluster))) [point]}))
      (reduce (partial merge-with concat) {}))))

(def cluster angular-cluster)
