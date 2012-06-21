(ns rscum.rank
  (:import (edu.uci.ics.jung.graph DirectedSparseGraph)
           (edu.uci.ics.jung.graph.util EdgeType)
           (edu.uci.ics.jung.algorithms.scoring PageRank)))

(defn- make-directed-sparse-graph []
  (DirectedSparseGraph.))

(defn- add-vertex [graph vertex]
  (.addVertex graph vertex))

(defn- add-edge [graph start end]
  (.addEdge graph [start end] start end EdgeType/DIRECTED))

(defn- make-ranker [graph damping-factor]
  (PageRank. graph damping-factor))

(defn- evaluate-rank [ranker]
  (.evaluate ranker))

(defn- get-vertex-score [ranker key]
  (.getVertexScore ranker key))

(defn- get-vertices [graph]
  (.getVertices graph))

(defn page-rank [tree]
  (let [graph (make-directed-sparse-graph)]
    (doseq [key (keys tree)]
      (doseq [value (tree key)]
        (add-vertex graph key)
        (add-vertex graph value)
        (add-edge graph key value)))
    (let [ranker (make-ranker graph 0.15)]
      (evaluate-rank ranker)
      (let [sorted (->> (map (fn [key] [key (get-vertex-score ranker key)]) (get-vertices graph))
                        (sort-by second)
                        reverse)
            multiplier (/ 1 (second (first sorted)))]
        (println "HIGHEST RANKED: " (first sorted) " MULTIPLIER: " multiplier " OUT OF: " (count sorted))
        (into {} (map (fn [[k v]] [k (* v multiplier)]) sorted))))))
