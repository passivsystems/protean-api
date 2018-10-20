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
  (let [xs (remove nil? (map #(get-in % ks) tree))]
    (if (some map? xs)
      (u/remove-vals (into {} (reverse xs)) #(= % :remove))
      (first xs))))

(defn service [tree] (ffirst (filter #(= (type (key %)) String) tree)))

(defn get-path-locations
  "Returns all locations that correspond to a relative path, provided a codex-dir"
  [protean-home path codex-dir]
  [(str codex-dir "/" path)
   (str (dsk/pwd) "/" path)
   (str protean-home "/" path)])

(defn to-path-dir
  "Resolves relative paths to absolute, provided a codex-dir"
  [protean-home path codex-dir]
  (if (dsk/as-relative path)
    (let [locs (get-path-locations protean-home path codex-dir)]
      (or (u/find dsk/exists? locs)
          (throw (java.io.FileNotFoundException. (str
            "Could not find relative path: '" path "', looked in: " locs)))))
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
  (or (get-in (codex-rsp-hdrs rsp-code tree) [h/ctype 0])
      (when-let [bs (get-in-tree tree [:rsp rsp-code :body-schema])]
        (h/mime-schema bs))
      (when-let [be (first (get-in-tree tree [:rsp rsp-code :body-examples]))]
        (h/mime be))))

(defn rsp-hdrs [rsp-code tree]
  (merge (when-let [ct (rsp-ctype rsp-code tree)] (param-fix {h/ctype ct}))
         (codex-rsp-hdrs rsp-code tree)))

(defn status-matching [tree f-e]
 (->> (get-in-tree tree [:rsp])
      (filter (fn [[k _]] (re-matches f-e (name k))))
      ; includes default headers, content type + unapply param-fix
      (map (fn [[k v]] [k (assoc v :headers (u/update-vals (rsp-hdrs k tree) first))]))))

(defn success-status [tree] (sort-by :status (status-matching tree #"[123]\d\d")))

(defn error-status [tree] (sort-by :status (status-matching tree #"[45]\d\d")))
