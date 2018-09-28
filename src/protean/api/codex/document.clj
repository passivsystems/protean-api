(ns protean.api.codex.document
  "Codex data extraction and truthiness functionality."
  (:require [clojure.string :as s]
            [me.rossputin.diskops :as dsk]
            [protean.utils :as u]
            [protean.api.protocol.http :as h]))

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
  "Extracts nested values taking into account tree inheritance."
  [tree ks]
  (let [xs (reverse (remove false? (map #(get-in % ks false) tree)))]
    (if (some map? xs)
      (u/remove-nils (into {} xs))
      (first xs))))

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
        (throw (java.io.FileNotFoundException.
          (str "Could not find relative path: '" path "', looked in " locations)))))
    path))

(defn to-path
  "Resolves relative paths to absolute, provided a tree"
  [protean-home path tree]
  (to-path-dir protean-home path (get-in-tree tree [:codex-dir])))

(defn- param-fix
  "Vectors param if not of type collection and marks as required"
  [params]
  (u/update-vals params #(if (coll? %) % (vector % :required))))

;; =============================================================================
;; Codex request
;; =============================================================================

(defn qps [t] (param-fix (get-in-tree t [:req :query-params])))

(defn fps [t] (param-fix (get-in-tree t [:req :form-params])))

(defn mps [t names]
  (->> (filter #(s/starts-with? % ";") names)
       (map (fn [n] (u/update-keys (get-in-tree t [:vars n :struct]) #(str n "." %))))
       (into {})
       param-fix))

(defn req-ctype [tree]
  (let [hdrs (get-in-tree tree [:req :headers])
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

(defn req-hdrs
  [tree]
  (let [ctype (req-ctype tree)
        ctype-hdr (if ctype {h/ctype ctype} {})]
    (param-fix (merge ctype-hdr (get-in-tree tree [:req :headers])))))

(defn req-body [t] (get-in-tree t [:req :body]))

(defn req-body-examples [t] (get-in-tree t [:req :body-examples]))

;; =============================================================================
;; Codex response
;; =============================================================================

(defn- codex-rsp-hdrs [rsp-code tree]
  (param-fix (merge (get-in-tree tree [:rsp :headers])
                    (get-in-tree tree [:rsp rsp-code :headers]))))

(defn rsp-ctype [rsp-code tree]
  (let [ctype (get-in (codex-rsp-hdrs rsp-code tree) [h/ctype 0])
        body-schema (get-in-tree tree [:rsp rsp-code :body-schema])
        body-example (first (get-in-tree tree [:rsp rsp-code :body-examples]))]
    (cond
      ctype ctype
      body-schema (h/mime-schema body-schema)
      body-example (h/mime body-example))))

(defn rsp-hdrs [rsp-code tree]
  (let [ctype (rsp-ctype rsp-code tree)]
    (merge (when ctype (param-fix {h/ctype ctype}))
           (codex-rsp-hdrs rsp-code tree))))

(defn status-matching [tree f-e]
 (->> tree
      (map (fn [{:keys [rsp]}] (filter #(re-matches f-e (name (key %))) rsp)))
      (some identity)
      ; includes default headers, content type + unapply param-fix
      (map (fn [[k v]] [k (assoc v :headers (u/update-vals (rsp-hdrs k tree) first))]))
      (into {})
      seq))

(defn success-status [tree] (status-matching tree #"[123]\d\d"))

(defn error-status [tree] (status-matching tree #"[45]\d\d"))
