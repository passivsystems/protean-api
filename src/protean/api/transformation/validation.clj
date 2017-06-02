(ns protean.api.transformation.validation
  (:require [clojure.string :as s]
            [clojure.set :refer [subset?]]
            [clojure.xml :as x]
            [clojure.zip :as z]
            [clojure.data :as data]
            [cheshire.core :as jsn]
            [protean.api.codex.document :as d]
            [protean.api.protocol.http :as h]
            [protean.api.protocol.protean :as pp]
            [protean.api.transformation.coerce :as c]
            [protean.api.transformation.jsonvalidation :as jv]
            [protean.api.transformation.xmlvalidation :as xv])
  (:import java.io.ByteArrayInputStream))

(defn validate-status [expected-status payload errors]
  (if (not (= (str (:status payload)) expected-status))
    (conj errors (str "expected status " expected-status " (was " (:status payload) ")"))
    errors))

(defn validate-headers [expected-headers payload errors]
  (if expected-headers
    (let [expected (set (map #(s/lower-case %) (keys expected-headers)))
          received (set (map #(s/lower-case %) (keys (:headers payload))))]
      (if (subset? expected received)
        errors
        (conj errors
          (str "expected headers " (s/join "," expected) " (was " (s/join "," received) ")"))))
    errors))

(defn validate-query-params [request tree errors]
  (if-let [rpms (d/qps tree false)]
    (let [expected-qps (keys rpms)
          received-qps (map name (keys (:query-params request)))]
      (if (every? (set received-qps) expected-qps)
        errors
        (conj errors
          (str "expected query params " expected-qps " (was " (s/join "," received-qps) ")"))))
    errors))

(defn validate-form-params [request tree errors]
  (if-let [f-keys (d/fps tree false)]
    (let [expected-form (keys f-keys)
          received-form (keys (:form-params request))]
      (if (every? (set received-form) expected-form)
        errors
        (conj errors
          (str "expected form params " expected-form " (was " received-form ")"))))
    errors))

(defn- zip-str [s] (z/xml-zip (x/parse (ByteArrayInputStream. (.getBytes s)))))

(defn- map-vals [m k] (set (keep k (tree-seq #(or (map? %) (vector? %)) identity m))))

(defn- validate-xml-body [payload schema codex-body errors]
  (println "xml schema : " schema)
  (if schema
    (let [body (:body payload)
          validation (xv/validate schema body)]
      (if (:success validation)
        errors
        (conj errors
          (str "Xml body: " body "\ndid not conform to xml schema " schema " : " (:message validation)))))
    (if codex-body
      (let [tags-in-str (fn [s] (map-vals (zip-str s) :tag))
            expected-tags (tags-in-str (c/pretty-xml codex-body))
            received-tags (tags-in-str (:body payload))]
        (if (= received-tags expected-tags)
          errors
          (conj errors
            (str "Xml body not valid - expected " expected-tags " but received " received-tags))))
      errors)))

(defn- validate-jsn-body [payload schema codex-body errors]
  (if schema
    (let [body (:body payload)
          validation (jv/validate schema body)]
      (if (:success validation)
        errors
        (conj errors
          (str "Json body: " body "\ndid not conform to json schema " schema " : " (:message validation)))))
    (if codex-body
      (try
        (let [body-jsn (jsn/parse-string (:body payload))]
          (if (map? codex-body)
            (let [expected-keys (set (keys codex-body))
                  received-keys (set (keys body-jsn))]
              (if (= received-keys expected-keys)
                errors
                (conj errors
                  (str "Json body not valid - expected " expected-keys " but received " received-keys))))
            (contains? codex-body body-jsn)))
        (catch Exception e (conj errors (str "Could not parse json:" (:body payload) "\n" (.getMessage e)))))
      errors)))

(defn parse-hdr [hdr]
  (let [parse-qlf (fn [qlf]
         (if qlf
           (let [[k v _] (map s/trim (s/split qlf #"="))]
              {k v})))
        [value rest] (s/split hdr #";")
        qlfs (into {} (parse-qlf rest))]
      [(s/trim value) qlfs]))

(defn- validate-ctype [expected-ctype actual-ctype]
  (when expected-ctype
    (let [[expected expected-qlfs](parse-hdr expected-ctype)
          [actual actual-qlfs] (parse-hdr actual-ctype)
          fix (fn [q] (assoc q "charset" (s/upper-case (str (get q "charset")))))]
      (or
        (not (= expected actual))
        (first (data/diff (fix expected-qlfs) (fix actual-qlfs)))))))

(defn validate-body [payload expected-ctype schema codex-body errors]
  (if payload
    (let [ctype (pp/ctype payload)]
      (cond
        (validate-ctype expected-ctype ctype)
          (conj errors (str "expected content-type " expected-ctype " (was " ctype ")"))
        (h/xml? ctype)
          (validate-xml-body payload schema codex-body errors)
        (h/txt? ctype)
          errors
        ; TODO should we validate that the ctype is set as json?
        :else
          (validate-jsn-body payload schema codex-body errors)))
  (conj errors (str "expected body but was empty"))))
