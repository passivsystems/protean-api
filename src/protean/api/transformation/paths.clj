(ns protean.api.transformation.paths
  "Path calculation functions"
  (:require [clojure.string :as s]
            [clojure.core.reducers :as reducers]
            [protean.api.codex.document :as d]
            [protean.api.protocol.http :as h]))

;; =============================================================================
;; Helper functions
;; =============================================================================

(defn- encode [svc path method codex]
  {:svc svc
   :path path
   :method method
   :tree (d/to-seq codex svc path method)})

(defn- methods-range [svc paths codex]
  (let [endpoints (first (vals paths))
        methods (filter h/method? (map #(key %) endpoints))]
    (map #(encode svc (first (keys paths)) % codex) methods)))

(defn- combi-paths [codex combi]
  (let [svc (first combi)
        paths-loc (rest combi)
        paths (map #(hash-map % (get-in codex [svc %])) paths-loc)]
    (map #(methods-range svc % codex) paths)))

(defn- svc-paths [codex svc]
  (let [paths-raw (get-in codex [svc])
        paths (map #(hash-map (first %) (last %)) paths-raw)]
    (map #(methods-range svc % codex) paths)))

(defn- proc-group [codex group path-fn coll]
  (if (seq group)
    (flatten (reduce conj (map #(path-fn codex %) coll)))
    '()))

(defn- locs-range [codex locs]
  (let [groups ((juxt filter remove) #(= (count (s/split % #" ")) 1) locs)
        combi (map #(s/split (apply str %) #" ") (second groups))
        combi-paths (proc-group codex (second groups) combi-paths combi)
        svc-paths (proc-group codex (first groups) svc-paths (first groups))]
    (concat combi-paths svc-paths)))

;; =============================================================================
;; Path calculation functions
;; =============================================================================

(defn services
  "Get services for a codex and a vector of locs.

   If the locs vector is empty every resource under every service Protean knows
   about will be included in the path extraction transformation.

   If the corpus includes a 'locs' vector 'sample simple' a
   path extraction datastructure will be generated for one 'sample' service like:

    {
      :svc :sample
      :path simple
      :method :get
      :spec {:doc Simplest example of a resource - doc is optional}
    }
  "
  [codex locs]
  (let [res (locs-range codex locs)]
    (when (empty? res) (println "WARNING locs" locs "did not resolve to any path"))
    res))

(defn paths
  [codices]
  (reducers/reduce
    (fn [xs {:keys [svc path method tree]}] (assoc-in xs [svc path method] tree))
    {}
    (flatten (mapv #(services % (seq (remove keyword? (keys %)))) codices))))

(defn uri [host port svc path]
  (if (= path "/")
    (str "http://" host ":" port "/" svc)
    (str "http://" host ":" port "/" svc "/" path)))
