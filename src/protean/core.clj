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

(defn- parse-endpoint [req-endpoint cod-endpoint]
  (let [any (fn [s] (if (.startsWith s ";")
                          "(;[^/^?]*)?" ; matrix param
                          "([^/^?]+)")) ; path param
        regex (ph/replace-all-with cod-endpoint any)]
    (re-matches (re-pattern regex) req-endpoint)))

(defn- to-endpoint [req-endpoint paths svc]
  (let [endpoints (keys (get-in paths [svc]))
        filtered-ep (filter #(parse-endpoint req-endpoint %) endpoints)]
    (if (next filtered-ep)
      (or (some #{req-endpoint} filtered-ep) req-endpoint nil)
      (u/find #(parse-endpoint req-endpoint %) endpoints))))

(defn- sim-req
  "Prepare request for sim binding - augment with necessary information.
   Handles converting body from input stream to content for multi access."
  [req protean-home tree svc req-endpoint cod-endpoint]
  (let [ph-ks (map second (re-seq ph/ph cod-endpoint))
        ph-vs (drop 1 (parse-endpoint req-endpoint cod-endpoint))
        path-params (into {} (map vector ph-ks ph-vs))
        matrix-params (->> path-params
                           (filter (fn [[_ v]] (s/starts-with? (str v) ";")))
                           (map (fn [[k v]] (map #(str k "." %) (rest (s/split v #";")))))
                           flatten
                           (map #(if (s/includes? % "=") (s/split % #"=") [% ""]))
                           (into {}))]
    (assoc req :protean-home protean-home
               :tree tree
               :endpoint cod-endpoint
               :svc svc
               :body (or (dk/slurp-pun (:body req)) "")
               :path-params path-params
               :matrix-params matrix-params)))

(defn- protean-error-400 [msg]
  {:status 400
   :headers {"Protean-error" "Bad Request" "Protean-error-messages" msg}})

(defn- protean-error-404 []
  {:status 404 :headers {"Protean-error" "Not Found"}})

(defn- protean-error-405 [allowed-methods]
  {:status 405
   :headers {"Protean-error" "Method Not Allowed"
             "Allow" (s/join ", " (map #(s/upper-case (name %)) allowed-methods))}})

(defn- protean-error-500 [ex]
  (u/print-error ex)
  {:status 500 :headers {"Protean-error" "Error in sim"}})

(defn- http-options [paths svc endpoint {:keys [cors]}]
  (let [e (get-in paths [svc endpoint])
        m (map #(s/upper-case (name %)) (keys e))
        h (conj (keys (into {} (for [[_ v] e] (d/req-hdrs v)))) "Content-Type")]
    {:status 200
     :headers (merge {"Content-Type" "text/html"
                      "Access-Control-Allow-Methods" (s/join ", " (distinct m))
                      "Access-Control-Allow-Headers" (s/join ", " (distinct h))})
     :body nil}))

(defn- execute-fn
  "Prepare bindings for use through out sim execution context.
   Handle rules processing."
  [aug-req {:keys [validate? validate-rule]}]
  (fn [rule]
    (let [validate? (not (false? validate?))
          validate-rule? (and validate? (not (nil? validate-rule)))
          errors (and (not validate-rule?) validate? (sim/validate aug-req))]
      (try
        (cond
          validate-rule? (apply validate-rule [aug-req rule])
          errors         (protean-error-400 (s/join ", " (vals errors)))
          rule           (apply rule [aug-req])
          :else          (sim/response aug-req (first (sim/success-codes aug-req))))
        (catch Exception e (protean-error-500 e))))))

(defn- swap-placeholders
  [rsp tree aug-req]
  (if (get-in rsp [:headers "Protean-error"])
    rsp
    (ph/swap rsp tree (ph/response-bag rsp aug-req) :gen-all true)))

(defn- transform-cors
  [rsp {:keys [cors]}]
  (if (false? cors)
    rsp
    (merge-with merge {:headers {"Access-Control-Allow-Origin" "*"}} rsp)))

(defn- serialise
  [{:keys [status body] :as raw-rsp} tree]
  (let [ctype (str (or (get-in raw-rsp [:headers "Content-Type"])
                       (d/rsp-ctype (keyword (str status)) tree)))
        rsp (if (or (empty? ctype) (nil? body))
              raw-rsp
              (assoc-in raw-rsp [:headers "Content-Type"] ctype))]
    (cond
      (nil? body)                            rsp
      (s/starts-with? ctype http/txt)        (assoc rsp :body (s/trim-newline body))
      (string? body)                         rsp ;; Assume already coerced
      (s/starts-with? ctype http/jsn-simple) (assoc rsp :body (coerce/jsn body))
      (s/starts-with? ctype http/xml)        (assoc rsp :body (coerce/xml body))
      :else                                  rsp)))

;; =============================================================================
;; Sim Execution
;; =============================================================================

(defn sim-rsp [protean-home {:keys [uri request-method] :as req} paths sims]
  (let [svc (second (s/split uri #"/"))
        req-ep-raw (s/split uri (re-pattern (str "/" (name svc) "/")))
        req-endpoint (if (> (count req-ep-raw) 1) (second req-ep-raw) "/")
        endpoint (to-endpoint req-endpoint paths svc)
        sim (first (filter #(get-in % [svc endpoint request-method]) sims))
        sim-cfg (merge (:sim-cfg sim)
                       (get-in sim [svc :sim-cfg])
                       (get-in sim [svc endpoint :sim-cfg])
                       (get-in sim [svc endpoint request-method :sim-cfg]))]
    (if-let [tree (get-in paths [svc endpoint request-method])]
      (let [rules (get-in sim [svc endpoint request-method])
            aug-req (sim-req req protean-home tree svc req-endpoint endpoint)
            fn-rsp ((execute-fn aug-req sim-cfg) rules)
            rsp (if (and (map? fn-rsp) (int? (:status fn-rsp)))
                  fn-rsp
                  (protean-error-500 (IllegalArgumentException. (str
                    "Response: " fn-rsp " does not match structure {:status Int}."))))]
        (-> rsp
            (swap-placeholders tree aug-req)
            (transform-cors sim-cfg)
            (serialise tree)))
      (let [rsp (if (= request-method :options)
                  (http-options paths svc endpoint sim-cfg)
                  (if-let [supported-methods (keys (get-in paths [svc endpoint]))]
                    (do (log/warn "method not supported for" [uri request-method])
                        (protean-error-405 supported-methods))
                    (do (log/warn "no endpoint found for" [uri request-method])
                        (protean-error-404))))]
        (transform-cors rsp sim-cfg)))))
