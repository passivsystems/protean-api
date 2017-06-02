(ns protean.api.protocol.protean
  "Our own concise derived protocol for req/rsp."
  (:require [protean.api.protocol.http :as h]))

;; =============================================================================
;; Lense functions
;; =============================================================================

;; Request
;;;;;;;;;;

(defn ctype [req] (or (get-in req [:headers "content-type"])
                      (get-in req [:headers h/ctype])))

(defn accept [req] (get-in req [:headers "accept"]))


;; Response
;;;;;;;;;;;

(defn errors
  "Modelled on http://jsonapi.org/format/#errors as a default"
  []
  {:400 {:description :detail :template {:code (:400 h/status-docs) :detail "There is a problem with the request"}}})
