(ns protean.utils
  "Machinery provided for running sims and powering sim extensions."
  (:require [io.aviso.ansi :as aa])
  (:use [taoensso.timbre :as timbre :only (error) :rename {error log-error}]))

(defn stacktrace [e]
  (let [sw (java.io.StringWriter.)]
    (.printStackTrace e (java.io.PrintWriter. sw))
    (.toString sw)))

(defn print-error [e] (log-error (aa/red (str "caught exception: " (stacktrace e)))))
