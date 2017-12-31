(ns protean.api.transformation.validation
  (:require [clojure.string :as s]
            [clojure.set :refer [subset?]]
            [clojure.xml :as x]
            [clojure.zip :as z]
            [clojure.data :as data]
            [cheshire.core :as jsn]
            [protean.api.codex.document :as d]
            [protean.api.codex.placeholder :as ph]
            [protean.api.protocol.http :as h]
            [protean.api.protocol.protean :as pp]
            [protean.api.transformation.coerce :as c]
            [protean.api.transformation.jsonvalidation :as jv]
            [protean.api.transformation.xmlvalidation :as xv])
  (:import java.io.ByteArrayInputStream))

(defn- to-regex
  [tree v]
  (let [lookup #(str (d/get-in-tree tree [:types (d/get-in-tree tree [:vars % :type])]))]
    (loop [s v items (re-seq ph/ph v)]
      (if items
        (let [[[p n]] items] (recur (s/replace s p (lookup n)) (next items)))
        (re-pattern s)))))

(defn- invalid-values
  [expectations tree items]
  (let [invalid (fn [pattern value]
                  (when (and pattern (nil? (re-matches pattern value)))
                    (str " value: '" value "' does not match: " pattern)))
        patterns (into {} (for [[k v] expectations] {k (to-regex tree v)}))
        all (merge-with invalid patterns (select-keys items (keys patterns)))]
    (into {} (remove (fn [[k v]] (nil? v)) all))))

(defn validate-status [expected-status payload]
  (when-not (= (str (:status payload)) expected-status)
    (str "expected status " expected-status " (was " (:status payload) ")")))

(defn validate-headers
  [{:keys [headers tree]}]
  (when-let [expected-hdrs (d/req-hdrs tree)]
    (let [expected (set (map #(s/lower-case %) (keys expected-hdrs)))
          received (set (map #(s/lower-case %) (keys headers)))
          invalids (invalid-values expected-hdrs tree headers)]
      (cond
        (not (subset? expected received)) (str "expected headers: " (s/join ", " expected) " (was " (s/join "," received) ")")
        (not (empty? invalids)) (str "invalid headers: " (s/join ", " (map (fn [[k v]] (str k v)) invalids)))
        :else nil))))

(defn validate-query-params
  [{:keys [query-params tree]}]
  (when-let [expected-qps (d/qps tree false)]
    (let [expected (set (keys expected-qps))
          received (set (map name (keys query-params)))
          invalids (invalid-values expected-qps tree query-params)]
      (cond
        (not (every? received expected)) (str "expected query params: " (s/join ", " expected) " (was " (s/join "," received) ")")
        (not (empty? invalids)) (str "invalid query params: " (s/join ", " (map (fn [[k v]] (str k v)) invalids)))
        :else nil))))

(defn validate-form-params
  [{:keys [form-params tree]}]
  (when-let [expected-fps (d/fps tree false)]
    (let [expected (set (keys expected-fps))
          received (set (keys form-params))
          invalids (invalid-values expected-fps tree form-params)]
      (cond
        (not (every? received expected)) (str "expected form params: " (s/join ", " expected) " (was " (s/join "," received) ")")
        (not (empty? invalids)) (str "invalid form params: " (s/join ", " (map (fn [[k v]] (str k v)) invalids)))
        :else nil))))

(defn validate-matrix-params
  [{:keys [uri path-params tree] :as request}]
  (when-let [expected-mps (into {} (map #(d/mps % tree false) (filter #(s/starts-with? % ";") (keys path-params))))]
    (let [matrix-params (into {} (map #(s/split % #"=") (rest (s/split uri #";"))))
          expected (set (keys expected-mps))
          received (set (keys matrix-params))
          invalids (invalid-values expected-mps tree matrix-params)]
      (cond
        (not (every? received expected)) (str "expected matrix params: " (s/join ", " expected) " (was " (s/join "," received) ")")
        (not (empty? invalids)) (str "invalid matrix params: " (s/join ", " (map (fn [[k v]] (str k v)) invalids)))
        :else nil))))

(defn- zip-str [s] (z/xml-zip (x/parse (ByteArrayInputStream. (.getBytes s)))))

(defn- map-vals [m k] (set (keep k (tree-seq #(or (map? %) (vector? %)) identity m))))

(defn- validate-xml-body [{:keys [body]} schema codex-body]
  (if schema
    (let [validation (xv/validate schema body)]
      (when-not (:success validation)
        (str "Xml body: " body "\ndid not conform to xml schema " schema " : " (:message validation))))
    (if codex-body
      (let [tags-in-str (fn [s] (map-vals (zip-str s) :tag))
            expected-tags (tags-in-str (c/pretty-xml codex-body))
            received-tags (tags-in-str body)]
        (if (= received-tags expected-tags)
          (str "Xml body not valid - expected " expected-tags " but received " received-tags))))))

(defn- validate-jsn-body [{:keys [body]} schema codex-body]
  (if schema
    (let [validation (jv/validate schema body)]
      (when-not (:success validation)
        (str "Json body: " body "\ndid not conform to json schema " schema " : " (:message validation))))
    (when codex-body
      (try
        (let [body-jsn (jsn/parse-string body)]
          (if (map? codex-body)
            (let [expected-keys (set (keys codex-body))
                  received-keys (set (keys body-jsn))]
              (when-not (= received-keys expected-keys)
                (str "Json body not valid - expected " expected-keys " but received " received-keys)))
            (contains? codex-body body-jsn)))
        (catch Exception e (str "Could not parse json:" body "\n" (.getMessage e)))))))

(defn parse-hdr [hdr]
  (let [parse-qlf (fn [qlf] (when qlf
                              (let [[k v _] (map s/trim (s/split qlf #"="))]
                                {k v})))
        [value rest] (s/split (str hdr) #";")
        qlfs (into {} (parse-qlf rest))]
      [(s/trim value) qlfs]))

(defn- validate-ctype [expected-ctype actual-ctype]
  (when expected-ctype
    (let [[expected expected-qlfs](parse-hdr expected-ctype)
          [actual actual-qlfs] (parse-hdr actual-ctype)
          fix (fn [q] (assoc q "charset" (s/upper-case (str (or (get q "charset") "utf-8")))))]
      (or
        (not (= expected actual))
        (first (data/diff (fix expected-qlfs) (fix actual-qlfs)))))))

(defn validate-body [payload expected-ctype schema codex-body]
  (if payload
    (let [ctype (pp/ctype payload)]
      (cond
        (validate-ctype expected-ctype ctype)
          (str "expected content-type " expected-ctype " (was " ctype ")")
        (h/xml? ctype)
          (validate-xml-body payload schema codex-body)
        (h/txt? ctype)
          nil
        ; TODO should we validate that the ctype is set as json?
        :else
          (validate-jsn-body payload schema codex-body)))
    (str "expected body but was empty")))
