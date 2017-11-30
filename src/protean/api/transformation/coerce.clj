(ns protean.api.transformation.coerce
  "General purpose coercion."
  (:refer-clojure :exclude [int long])
  (:require [clojure.string :as s]
            [clojure.data.xml :as xml]
            [cheshire.core :as jsn]
            [yaclot.core :refer [convert to-type]])
  (:use [clojure.pprint :only [pprint]]))

; Note must preserve nil since nil comparisons are used throughout protean core
(defn- rich-generate-string [d keys]
  (let [s (jsn/generate-string d keys)]
    (if (.contains ["null" "\"\""] s) nil s)))

;; =============================================================================
;; Transformation functions
;; =============================================================================

(defn int [s] (convert s (to-type Integer)))

(defn long [s] (convert s (to-type Long)))

(defn jsn [d] (rich-generate-string d {:pretty false}))

(defn pretty-jsn [d] (rich-generate-string d {:pretty true}))

(defn clj
  ([d] (if (map? d) d (jsn/parse-string d)))
  ([d k] (if (map? d) d (jsn/parse-string d (or k false)))))

(defn pretty-clj [d] (pprint (clj d)))

(defn xml [d] (xml/sexp-as-element d))

(defn str-xml [d] (xml/emit-str (xml d)))

(defn pretty-xml [d] (xml/indent-str (xml d)))

(defn ->matrix-params [m]
  (->> m
    (map (fn [[k v]] (str k "=" v)))
    (s/join ";")
    (str ";")))
