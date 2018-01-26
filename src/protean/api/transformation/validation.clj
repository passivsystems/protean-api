(ns protean.api.transformation.validation
  (:require [clojure.string :as s]
            [clojure.set :refer [subset?]]
            [clojure.xml :as x]
            [clojure.zip :as z]
            [clojure.data :as data]
            [cheshire.core :as jsn]
            [protean.utils :as u]
            [protean.api.codex.document :as d]
            [protean.api.codex.placeholder :as ph]
            [protean.api.protocol.http :as h]
            [protean.api.protocol.protean :as pp]
            [protean.api.transformation.coerce :as c]
            [protean.api.transformation.jsonvalidation :as jv]
            [protean.api.transformation.xmlvalidation :as xv])
  (:import java.io.ByteArrayInputStream))

(defn- invalid-values
  [params tree items]
  (let [invalid (fn [value pattern]
                  (when (and pattern (empty? (filter #(re-matches pattern %) value)))
                    (str " value: '" (s/join "," value) "' does not match: " pattern)))
        patterns (into {} (for [[k v] params] {k (ph/regex-pattern tree (first v))}))
        select-items (into {} (for [[k v] (select-keys items (keys patterns))]
                                (if (.contains (get params k) :multiple)
                                  {k (s/split v #",")}
                                  {k [v]})))
        select-patterns (select-keys patterns (keys items))
        errors (merge-with invalid select-items select-patterns)]
    (u/remove-nils errors)))

(defn- validate
  [vtype tree params received-items]
  (let [expected (set (keys (remove #(.contains (second %) :optional) params)))
        received (set (keys received-items))
        invalids (invalid-values params tree received-items)]
    (cond
      (not (subset? expected received)) (str "expected " vtype ": " (s/join ", " expected) " (was " (s/join "," received) ")")
      (not (empty? invalids))           (str "invalid "  vtype ": " (s/join ", " (map (fn [[k v]] (str k v)) invalids)))
      :else nil)))

(defn validate-status [expected-status payload]
  (when-not (= (str (:status payload)) expected-status)
    (str "expected status " expected-status " (was " (:status payload) ")")))

(defn validate-headers
  ([{:keys [headers tree] :as payload}]
    (validate-headers (d/req-hdrs tree) payload))
  ([expected-hdrs {:keys [headers tree]}]
    (defn fix [v] (when v (s/replace v #"; charset=.*" "")))
    (validate "headers" tree (-> (u/update-keys expected-hdrs s/lower-case)
                                 (update "content-type" #(when % (update % 0 fix)))
                                 u/remove-nils)
                             (-> (u/update-keys headers s/lower-case)
                                 (update "content-type" fix)
                                 u/remove-nils))))

(defn validate-query-params
  [{:keys [query-params tree]}]
  (validate "query params" tree (d/qps tree) query-params))

(defn validate-form-params
  [{:keys [form-params tree]}]
  (validate "form params" tree (d/fps tree) form-params))

(defn validate-matrix-params
  [{:keys [matrix-params path-params tree] :as request}]
  (validate "matrix params" tree (d/mps tree (keys path-params)) matrix-params))

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

(defn validate-body [payload schema codex-body]
  (if payload
    (let [ctype (pp/ctype payload)]
      (cond
        (h/jsn? ctype) (validate-jsn-body payload schema codex-body)
        (h/xml? ctype) (validate-xml-body payload schema codex-body)
        :else          nil))
    (str "expected body but was empty")))
