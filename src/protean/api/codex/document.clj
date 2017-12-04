(ns protean.api.codex.document
  "Codex data extraction and truthiness functionality."
  (:require [me.rossputin.diskops :as dsk]
            [protean.utils :as u]
            [protean.api.protocol.http :as h]))

(defn custom-keys
  "returns only keys which are not keywords"
  [c] (seq (remove keyword? (keys c))))

(defn to-seq
  "creates a sequence (for now aka 'tree' - needs renaming) that can be
   traversed to resolve required references in scope"
  [codices svc path method]
  [(get-in codices [svc path method])
   (get-in codices [svc path])
   (get-in codices [svc])
   (get-in codices [method])
   codices])

(defn get-in-tree
  "returns the first result for given sequence of keys from a tree (scope)"
  [tree ks]
  (first (remove nil? (map #(get-in % ks) tree))))

(defn assoc-tree-item->
  "Extracts first out-ks in tree and assocs to target as in-k."
  [tree out-ks in-ks target]
  (if-let [v (get-in-tree tree out-ks)]
    (if (empty? v) target (assoc-in target in-ks v))
    target))

(defn assoc-item->
  "Extracts out-ks in source and assocs to target as in-ks."
  [source out-ks in-ks target]
  (if-let [v (get-in source out-ks)]
    (if (empty? v) target (assoc-in target in-ks v))
    target))

(defn service [tree] (ffirst (filter #(= (type (key %)) String) tree)))

(defn get-path-locations
  "Returns all locations that correspond to a relative path, provided a codex-dir"
  [protean-home path codex-dir]
  (let [current-dir (dsk/pwd)
        locations [(str codex-dir "/" path)
                   (str current-dir "/" path)
                   (str protean-home "/" path)]]
    locations))

(defn to-path-dir
  "Resolves relative paths to absolute, provided a codex-dir"
  [protean-home path codex-dir]
  (if (dsk/as-relative path)
    (let [locations (get-path-locations protean-home path codex-dir)
          abs-path (u/find dsk/exists? locations)]
      (if abs-path
        abs-path
        (throw (Exception.
          (str "Could not find relative path: '" path "', looked in " locations)))))
    path))

(defn to-path
  "Resolves relative paths to absolute, provided a tree"
  [protean-home path tree]
  (to-path-dir protean-home path (get-in-tree tree [:codex-dir])))

;; =============================================================================
;; Codex request
;; =============================================================================

(defn- f [include-optional [k [v & attr]]]
  (if (or include-optional (not (contains? (into #{} attr) :optional)))
    [k v]))

(defn qps [t include-optional]
  (->> (get-in-tree t [:req :query-params])
       (map (partial f include-optional))
       (into {})))

(defn fps [t include-optional]
  (->> (get-in-tree t [:req :form-params])
       (map (partial f include-optional))
       (into {})))

(defn mps
  "Get matrix parameters from codex."
  [t include-optional]
  (->> (tree-seq map? vals (first t))
       (filter #(and (map? %) (contains? % :struct)))
       first
       :struct
       (map (partial f include-optional))
       (map first)
       (remove nil?)
       (into #{})))

(defn- codex-req-hdrs [tree]
  ; we don't use get-in-tree as we want to merge definitions in all scopes here
  (into {} (remove nil? (map #(get-in % [:req :headers]) tree))))

(defn req-ctype [tree]
  (let [hdrs (codex-req-hdrs tree)
        ctype (get hdrs h/ctype)
        body-schema (get-in-tree tree [:req :body-schema])
        body-example (get-in-tree tree [0 :req :body-examples])
        body (get-in-tree tree [:req :body])
        default-ctype (get-in-tree tree [:default-content-type])]
    (cond
      ctype ctype
      body-schema (h/mime-schema body-schema)
      body-example (h/mime body-example)
      body default-ctype)))

(defn req-hdrs [tree]
  (let [ctype (req-ctype tree)
        ctype-hdr (if ctype {h/ctype ctype} {})]
    (merge ctype-hdr (codex-req-hdrs tree))))

(defn body-req [t] (get-in-tree t [:req :body]))

;; =============================================================================
;; Codex response
;; =============================================================================

(defn- codex-rsp-hdrs [rsp-code tree]
  (merge
    (get-in-tree tree [:rsp :headers])
    (get-in-tree tree [:rsp rsp-code :headers])))

(defn rsp-ctype [rsp-code tree]
  (let [ctype (get (codex-rsp-hdrs rsp-code tree) h/ctype)
        body-schema (get-in-tree tree [:rsp rsp-code :body-schema])
        body-example (first (get-in-tree tree [:rsp rsp-code :body-examples]))]
    (cond
      ctype ctype
      body-schema (h/mime-schema body-schema)
      body-example (h/mime body-example))))

(defn rsp-hdrs [rsp-code tree]
  (let [ctype (rsp-ctype rsp-code tree)
        ctype-hdr (if ctype {h/ctype ctype} {})]
    (merge ctype-hdr (codex-rsp-hdrs rsp-code tree))))

(defn status-matching [tree f-e]
  (let [filter (fn [m] (seq (filter #(re-matches f-e (name (key %))) (:rsp m))))
        statuses (some identity (map filter tree))
        include-defaults (fn [[k v]]
      [k (update-in v [:headers] #(merge (get-in-tree tree [:rsp :headers]) %))])]
    (seq (into {} (map include-defaults statuses)))))

(defn success-status [tree] (status-matching tree #"[123]\d\d"))

(defn error-status [tree] (status-matching tree #"[45]\d\d"))

;; =============================================================================
;; Codex fragment functions (codex fragments that travel with tests etc)
;; =============================================================================

(defn azn [c] (get-in c [:headers h/azn]))
