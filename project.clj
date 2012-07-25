(defproject rscum "1.0.0-SNAPSHOT"
  :description "TBC"
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [tentacles "0.1.3"]
                 [org.clojars.tavisrudd/redis-clojure "1.3.1"]
                 [net.sf.jung/jung-api "2.0.1"]
                 [net.sf.jung/jung-graph-impl "2.0.1"]
                 [net.sf.jung/jung-algorithms "2.0.1"]
                 [incanter "1.3.0"]]
  :main rscum.core
  :plugins [[lein-swank "1.4.4"]]
  :jvm-opts ["-Xmx1g"])
