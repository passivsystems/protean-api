(ns protean.core
  "Machinery provided for running sims."
  (:require [clojure.string :as s]
            [me.rossputin.diskops :as dk]
            [protean.utils :as utils]
            [protean.api.codex.document :as d]
            [protean.api.codex.placeholder :as ph]
            [protean.api.protocol.http :as http]
            [protean.api.transformation.sim :as sim])
  (:use [taoensso.timbre :as timbre
     :only (debug info warn error)
     :rename {info log-info warn log-warn error log-error}]))

(defn- parse-endpoint [requested-endpoint cod-endpoint]
  ; TODO instead of requiring namedparameter to start with ';' when a matrix parameter
  ;      could look up [:var s :type] to see if is a Matrix parameter
  ;      however currently require parse-endpoint before we can get tree
  (let [any (fn [s] (if (.startsWith s ";")
              "(;[^/^?]*)?" ; matrixParam regex
              "([^/^?]+)")) ; pathParam regex
        regex (ph/replace-all-with cod-endpoint any)]
    (re-matches (re-pattern regex) requested-endpoint)))

(defn- to-endpoint [requested-endpoint paths svc]
  (let [endpoints (keys (get-in paths [svc]))
        filtered-ep (filter #(parse-endpoint requested-endpoint %) endpoints)]
    (if (next filtered-ep)
      (or (some #{requested-endpoint} filtered-ep) requested-endpoint nil)
        (first (filter #(parse-endpoint requested-endpoint %) endpoints)))))

(defn- aug-path-params [req-endpoint cod-endpoint request]
  (let [ph-ks (map second (re-seq ph/ph cod-endpoint))
        ph-vs (drop 1 (parse-endpoint req-endpoint cod-endpoint))
        params (into {} (map vector ph-ks ph-vs))]
    (assoc request :path-params params)))

(defn- sim-req
  "Prepare request for sim binding - augment with necessary information.
   Handles converting body from input stream to content for multi access."
  [req ep svc]
  (assoc req :endpoint ep :svc svc :body (or (dk/slurp-pun (:body req)) "")))

(defn- protean-error-400 [errors] {:status 400 :headers {"Protean-error" "Bad Request" "Protean-error-messages"  errors}})

(defn- protean-error-404 [] {:status 404 :headers {"Protean-error" "Not Found"}})

(defn- protean-error-405 [supported-methods]
  {:status 405
   :headers {
     "Protean-error" "Method Not Allowed"
     "Allow" (s/join ", " (map #(s/upper-case (name %)) supported-methods))
   }
  })

(defn- protean-error-500 [] {:status 500 :headers {"Protean-error" "Error in sim"}})

(defn- format-rsp
  [protean-home tree rsp-entry]
  (let [status-code (Integer/parseInt (name (key rsp-entry)))
        rsp (val rsp-entry)
        body-url (first (:body-examples rsp))
        headers (:headers rsp)
        headers_w_ctype (if (and body-url (not (get-in headers [http/ctype])))
                          (assoc headers http/ctype (http/mime body-url))
                          headers)
        raw-body (if body-url (slurp (d/to-path protean-home body-url tree)))
        body (if (http/txt? (get headers_w_ctype http/ctype))
                (s/trim-newline raw-body)
                raw-body)
        response {:status status-code :headers headers_w_ctype :body body}]
    response))

(defn- execute-fn
  "Prepare bindings for use through out sim execution context.
   Handle rules processing.

   rep is the requested endpoint
   ep is the endpoint
   req is the request"
  [protean-home tree rep ep req cfg]
  (fn [rule]
    (let [success-rsp (map #(format-rsp protean-home tree %) (into {} (d/success-status tree)))
          error-rsp (map #(format-rsp protean-home tree %) (into {} (d/error-status tree)))
          aug-req (merge (aug-path-params rep ep req) ;; TODO required?
                         {:tree tree
                          :protean-home protean-home
                          :response {:success success-rsp :error error-rsp}})]
      (try
        (cond
          rule                       (apply rule [aug-req])
          (false? (:validating cfg)) (first success-rsp)
          :else                      (if-let [errors (sim/validate aug-req)]
                                       (protean-error-400 errors)
                                       (first success-rsp)))
        (catch Exception e  (utils/print-error e) (protean-error-500))))))

;; =============================================================================
;; Sim Execution
;; =============================================================================

(defn- http-options [paths svc endpoint sim-cfg]
  (let [e (get-in paths [svc endpoint])
        m (map #(s/upper-case (name %)) (keys e))
        h (keys (into {} (for [[k v] e] (d/req-hdrs v))))
        hdrs {"Content-Type" "text/html"
              "Access-Control-Allow-Methods" (s/join ", "  m)
              "Access-Control-Allow-Headers" (s/join ", "  (conj h "Content-Type"))}
        cors-hdrs (if (false? (:cors sim-cfg)) hdrs (assoc hdrs "Access-Control-Allow-Origin" "*"))]
    [{:rsp {:200 {:headers cors-hdrs}}}]))

(defn sim-rsp [protean-home {:keys [uri] :as req} paths sims]
  (let [svc (second (s/split uri #"/"))
        req-ep-raw (s/split uri (re-pattern (str "/" (name svc) "/")))
        requested-endpoint (if (> (count req-ep-raw) 1) (second req-ep-raw) "/")
        endpoint (to-endpoint requested-endpoint paths svc)
        method (:request-method req)
        sim-cfg (:sim-cfg sims)
        tree (if-let [x (get-in paths [svc endpoint method])]
               x
               (when (= method :options) (http-options paths svc endpoint sim-cfg)))]
    (if (not tree)
      (do
        (log-warn "warning - no endpoint found for" [uri method])
        (if-let [supported-methods (keys (get-in paths [svc endpoint]))]
          (protean-error-405 supported-methods)
          (protean-error-404)))
      (let [rules (get-in sims [svc endpoint method])
            request (sim-req req endpoint svc)
            execute (execute-fn protean-home tree requested-endpoint endpoint request sim-cfg)
            response (when tree (execute rules))]
        (log-info "sim cfg:" sim-cfg)
        (log-info "executed" (if rules "sim extension rules" "default rules") "for uri:" uri "(svc:" svc "endpoint:" endpoint "method:" method ")")
        (log-info "responding with" response)
        (cond
           ;;TODO validate response structure
           (not (map? response))    (do
                                      (log-error "Response:" response "does not match stuture {:status Int :header Vector :body String}")
                                      (protean-error-500))
           (false? (:cors sim-cfg)) response
           :else                    (merge-with merge {:headers {"Access-Control-Allow-Origin" "*"}} response))))))
