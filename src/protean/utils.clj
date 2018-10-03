(ns protean.utils
  "Common methods used in core and api"
  (:require [io.aviso.ansi :as aa]
            [taoensso.timbre :as log])
  (:refer-clojure :exclude [find]))

(defn stacktrace [e]
  (let [sw (java.io.StringWriter.)]
    (.printStackTrace e (java.io.PrintWriter. sw))
    (.toString sw)))

(defn print-error [e] (log/error (aa/red (str "caught exception: " (stacktrace e)))))

(defn find [pred-fn xs] (first (filter pred-fn xs)))

(defn update-keys [m f & args] (into {} (for [[k v] m] {(apply f k args) v})))

(defn update-vals [m f & args] (into {} (for [[k v] m] {k (apply f v args)})))

(defn remove-vals [m f & args] (into {} (remove (fn [[_ v]] (apply f v args)) m)))
