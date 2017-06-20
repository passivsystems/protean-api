(ns protean.api.codex.document
  "Codex data extraction and truthiness functionality."
  (:require [clojure.java.io :as io]
            [environ.core :refer [env]]
            [me.rossputin.diskops :as dsk]
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
  [tree ks] (first (remove nil? (map #(get-in % ks) tree))))

(defn git [tree ks] (get-in-tree tree ks))

(defn assoc-tree-item->
  "Extracts first out-ks in tree and assocs to target as in-k."
  [tree out-ks in-ks target]
  (if-let [v (git tree out-ks)]
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
  (->> (git t [:req :query-params])
       (map (partial f include-optional))
       (into {})))

(defn fps [t include-optional]
  (->> (git t [:req :form-params])
       (map (partial f include-optional))
       (into {})))

(defn recursive-filter [m k]
  (filter #(and (map? %) (contains? % k)) (tree-seq map? vals m)))

(defn mps
  "Get matrix parameters from codex."
  [t include-optional]
  (->> (recursive-filter t :struct)
       (first)
       (:struct)
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
        body-schema (git tree [:req :body-schema])
        body-example (git tree [0 :req :body-examples])
        body (git tree [:req :body])
        default-ctype (git tree [:default-content-type])]
    (cond
      ctype ctype
      body-schema (h/mime-schema body-schema)
      body-example (h/mime body-example)
      body default-ctype)))

(defn req-hdrs [tree]
  (let [ctype (req-ctype tree)
        ctype-hdr (if ctype {h/ctype ctype} {})]
    (merge ctype-hdr (codex-req-hdrs tree))))

(defn body-req [t] (git t [:req :body]))

;; =============================================================================
;; Codex response
;; =============================================================================

(defn- codex-rsp-hdrs [rsp-code tree]
  (merge
    (git tree [:rsp :headers])
    (git tree [:rsp rsp-code :headers])))

(defn rsp-ctype [rsp-code tree]
  (let [ctype (get-in (codex-rsp-hdrs rsp-code tree) h/ctype)
        body-schema (git tree [:rsp rsp-code :body-schema])
        body-example (first (git tree [:rsp rsp-code :body-examples]))]
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
      [k (update-in v [:headers] #(merge (git tree [:rsp :headers]) %))])]
    (seq (into {} (map include-defaults statuses)))))

(defn success-status [tree] (status-matching tree #"[123]\d\d"))

(defn error-status [tree] (status-matching tree #"[45]\d\d"))

(defn success-responses
  "Note here request is a request with tree and other data blended in"
  [request]
  (get-in request [:response :success]))

(defn error-responses
  "Note here request is a request with tree and other data blended in"
  [request]
  (get-in request [:response :error]))

(defn responses [request]
  (concat (success-responses request) (error-responses request)))


;; =============================================================================
;; Codex fragment functions (codex fragments that travel with tests etc)
;; =============================================================================

(defn azn [c] (get-in c [:headers h/azn]))
