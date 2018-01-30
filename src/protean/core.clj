(ns protean.core
  "Machinery provided for running sims."
  (:require [clojure.string :as s]
            [me.rossputin.diskops :as dk]
            [protean.utils :as u]
            [protean.api.codex.document :as d]
            [protean.api.codex.placeholder :as ph]
            [protean.api.protocol.http :as http]
            [protean.api.transformation.sim :as sim]
            [protean.api.transformation.coerce :as coerce]
            [taoensso.timbre :as log]))

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
        (u/find #(parse-endpoint requested-endpoint %) endpoints))))

(defn- aug-path-params [req-endpoint cod-endpoint request]
  (let [ph-ks (map second (re-seq ph/ph cod-endpoint))
        ph-vs (drop 1 (parse-endpoint req-endpoint cod-endpoint))
        path-params (into {} (map vector ph-ks ph-vs))
        matrix-params (->> path-params
                           (filter (fn [[_ v]] (s/starts-with? (str v) ";")))
                           (map (fn [[k v]] (map #(str k "." %) (rest (s/split v #";")))))
                           flatten
                           (map #(if (s/includes? % "=") (s/split % #"=") [% ""]))
                           (into {}))]
    (assoc request :path-params path-params :matrix-params matrix-params)))

(defn- sim-req
  "Prepare request for sim binding - augment with necessary information.
   Handles converting body from input stream to content for multi access."
  [req ep svc]
  (assoc req :endpoint ep :svc svc :body (or (dk/slurp-pun (:body req)) "")))

(defn- protean-error-400 [msg] {:status 400 :headers {"Protean-error" "Bad Request" "Protean-error-messages" msg}})

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
   Handle rules processing."
  [aug-req cfg]
  (fn [rule]
    (let [validate? (not (false? (:validate? cfg)))
          validate-rule (:validate-rule cfg)]
      (try
        (if (and validate? (not (nil? validate-rule)))
          (apply validate-rule [aug-req rule])
          (if-let [errors (and validate? (sim/validate aug-req))]
            (protean-error-400 (s/join ", " (vals errors)))
            (if rule
              (apply rule [aug-req])
              (first (sim/success-responses aug-req)))))
        (catch Exception e  (u/print-error e) (protean-error-500))))))

(defn- transform-cors
  [rsp {:keys [cors]}]
  (if (false? cors)
    rsp
    (merge-with merge {:headers {"Access-Control-Allow-Origin" "*"}} rsp)))

(defn- serialise
  [{:keys [status body] :as rsp} tree]
  (let [ctype (str (or (get-in rsp [:headers "Content-Type"])
                       (d/rsp-ctype (keyword (str status)) tree)))]
    (cond
      (nil? body)                            rsp
      (string? body)                         rsp ;; Assume already coerced
      (s/starts-with? ctype http/jsn-simple) (assoc rsp :body (coerce/jsn body))
      (s/starts-with? ctype http/xml)        (assoc rsp :body (coerce/xml body))
      :else                                  rsp)))


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
        sim-cfg (merge (:sim-cfg sims)
                       (get-in sims [svc :sim-cfg])
                       (get-in sims [svc endpoint :sim-cfg])
                       (get-in sims [svc endpoint method :sim-cfg]))
        tree (if-let [x (get-in paths [svc endpoint method])]
               x
               (when (= method :options) (http-options paths svc endpoint sim-cfg)))]
    (if (not tree)
      (do
        (log/warn "warning - no endpoint found for" [uri method])
        (if-let [supported-methods (keys (get-in paths [svc endpoint]))]
          (protean-error-405 supported-methods)
          (protean-error-404)))
      (let [rules (get-in sims [svc endpoint method])
            request (sim-req req endpoint svc)
            success-rsp (map #(format-rsp protean-home tree %) (into {} (d/success-status tree)))
            error-rsp (map #(format-rsp protean-home tree %) (into {} (d/error-status tree)))
            aug-req (merge (aug-path-params requested-endpoint endpoint request)
                         {:tree tree
                          :protean-home protean-home
                          :response {:success success-rsp :error error-rsp}})
            execute (execute-fn aug-req sim-cfg)
            response (when tree (execute rules))]
        (log/info "executed" (if rules "sim extension rules" "default rules") "for uri:" uri "(svc:" svc "endpoint:" endpoint "method:" method ")")
        (if (map? response)
          (let [r (-> (if (get-in response [:headers "Protean-error"])
                        response
                        (ph/swap response tree (ph/response-bag response aug-req) :gen-all true))
                      (transform-cors sim-cfg)
                      (serialise tree))]
            (log/info "responding with :status" (:status r) ":header" (:headers r) ":body" (:body r))
            r)
          (do
            (u/print-error (Exception. (str "Response: " (or response "nil") " does not match structure {:status Int :headers Vector :body String} - does your response comply with the codex ?")))
            (protean-error-500)))))))
