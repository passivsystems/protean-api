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
