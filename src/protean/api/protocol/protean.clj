(ns protean.api.protocol.protean
  "Our own concise derived protocol for req/rsp."
  (:require [clojure.string :as s]
            [protean.api.protocol.http :as h]))

;; =============================================================================
;; Lense functions
;; =============================================================================

;; Request
;;;;;;;;;;

(defn ctype [req] (or (get-in req [:headers "content-type"])
                      (get-in req [:headers h/ctype])))

(defn accept [req] (get-in req [:headers "accept"]))

(defn matrix-params [request]
 (if-let [param-pairs (rest (s/split (:uri request) #";"))]
   (set (map #(first (s/split % #"=")) param-pairs))
   #{}))


;; Response
;;;;;;;;;;;

(defn errors
  "Modelled on http://jsonapi.org/format/#errors as a default"
  []
  {:400 {:description :detail :template {:code (:400 h/status-docs) :detail "There is a problem with the request"}}})
