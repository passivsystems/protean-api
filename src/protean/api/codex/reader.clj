(ns protean.api.codex.reader
  (:require [clojure.edn :as edn]
            [clojure.string :as s]
            [protean.api.codex.document :as d])
  (:import java.io.File))

(defn- ordered-resources [tree svc]
  (let [matched-path-ks (re-seq #"\"[A-Za-z0-9-_~#\*;=\[\]\(\)\.\$\{\}/]+\"[\s]+\{(?!:type)" tree)]
    (->> (last (s/split (s/join "," matched-path-ks) (re-pattern svc)))
         (re-seq #"\"[A-Za-z0-9-_~#\*;=\[\]\(\)\.\$\{\}/]+\"")
         (map #(s/replace % (re-pattern "\"") "")))))

(defn- read-codex-part
  "will read the codex eden file, merging with any referenced files"
  [protean-home codex-dir file]
  (defn- merge-includes [[k v]]
    (cond
      (= :includes k) (reduce merge-with merge (map (partial read-codex-part protean-home codex-dir) v))
      (map? v)        {k (into {} (apply merge-with merge (map merge-includes v)))}
      :else           {k v}))
  (let [afile (if (string? file) (d/to-path-dir protean-home file codex-dir) file)
        file-content (slurp afile)
        read (edn/read-string file-content)
        tree (apply merge-with merge (map merge-includes read))]
    (assoc tree :ordered-resources (when-let [svc (d/service tree)]
                                     (ordered-resources file-content svc)))))

(defn read-codex
  "will read the codex eden file, merging with any referenced files"
  [protean-home file]
  (let [codex-dir (.getParent (.getAbsoluteFile file))]
    ;; TODO review this - alternatives are setting a binding, updating system
    ;; property (env caches values at startup)?
    (assoc (read-codex-part protean-home codex-dir file) :codex-dir codex-dir)))
