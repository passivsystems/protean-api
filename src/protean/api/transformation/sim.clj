(ns protean.api.transformation.sim
  "API for powering sim extensions."
  (:require [clojure.string :as s]
            [cemerick.pomegranate :as pom]
            [protean.utils :as u]
            [protean.api.protocol.http :as h]
            [protean.api.protocol.protean :as p]
            [protean.api.codex.document :as d]
            [protean.api.transformation.coerce :as c]
            [protean.api.codex.placeholder :as ph]
            [protean.api.transformation.validation :as v]
            [clj-http.client :as clt]
            [overtone.at-at :as at]
            [environ.core :as ec])
  (:use [taoensso.timbre :as timbre
     :only (debug info warn)
     :rename {debug log-debug info log-info warn log-warn}]))

; (defn- pretty-str [s ctype]
;   (cond
;     (h/xml? ctype) (c/pretty-xml s)
;     (h/txt? ctype) s
;     :else (c/pretty-js s)))

(defn dependencies [xs]
  (pom/add-dependencies
    :coordinates xs
    :repositories (merge cemerick.pomegranate.aether/maven-central
                       {"clojars" "http://clojars.org/repo"})))


;; =============================================================================
;; Sim Machinery Access
;; =============================================================================

(defn qslurp
  "Quantum slurp, used to look for sim extension referenced resources in
   multiple places.
   p is a resource path (probably relative)."
  [{:keys [protean-home tree] :as request} p]
  (slurp (d/to-path protean-home p tree)))


;; =============================================================================
;; Scheduling
;; =============================================================================

(def ^:private schedule-pool (at/mk-pool))

(defn- job
  "Creates a job to be scheduled from provided delay - will ensure dynamic bindings are preserved"
  [delayed]
  (fn []
    (log-debug "timeout - executing job")
    (try @delayed
      (catch Exception e (u/print-error e)))))

(defn at-delayed [ms-time delayed]
  (at/at ms-time (job delayed) schedule-pool)
  nil)

(defmacro at
  [ms-time then]
  `(at-delayed ~ms-time (delay ~then)))

(defn after-delayed
  [delay-ms delayed]
  (at/after delay-ms (job delayed) schedule-pool)
  nil)

(defmacro after
  [delay-ms then]
  `(after-delayed ~delay-ms (delay ~then)))

(defmacro in
  [delay-ms then]
  `(after-delayed ~delay-ms (delay ~then)))


;; =============================================================================
;; Requests
;; =============================================================================

(defn body [request] (:body request))

(defn body-clj
  ([request] (c/clj (body request)))
  ([request k] (c/clj (body request) (or k false))))

(defn query-param [request p] (get-in request [:query-params p]))

(defn path-param [request p] (get-in request [:path-params p]))

(defn- flip [f] (fn [x y] (f y x)))

(defn matrix-params
  "Gets matrix params given a matrix param placeholder.
   E.G. /groups${;groupFilter} has a
   matrix param placeholder ';groupFilter' which can be associated with several
   matrix params."
  [request mp-name]
  (when-let [pp (path-param request (str ";" mp-name))]
    (->> pp
        ((flip s/split) #";")
        (filter seq)
        (map #(s/split % #"="))
        (into {}))))

(defn param [request p] (get-in request [:params p]))

(defn form-param [request p] (get-in request [:form-params p]))

(defn body-param
  ([request p] ((body-clj) p))
  ([request p k] (p (body-clj k))))

(defn header [request h] (get-in request [:headers h]))


;; =============================================================================
;; Responses
;; =============================================================================


;
; (defn success
;   "Returns a randomly selected success response as defined for endpoint"
;   []
;   (let [successes (d/success-status *tree*)
;         {:keys [svc request-method uri]} *request*
;         success (format-rsp (rand-nth successes))]
;     (if (empty? successes)
;       (log-warn "warning - no successes found for endpoint" [svc uri request-method])
;       (ph/swap success *tree* {} :gen-all true))))
;
; (defn error
;   "Returns a specific or randomly selected error response for an endpoint"
;   ([x]
;     (let [errors (d/error-status *tree*)
;           {:keys [svc request-method uri]} *request*
;           error (filter #(= (first %) (keyword (str x))) errors)
;           error-rsp (format-rsp (first error))]
;       (when (empty? errors) (log-warn "warning - no errors found for endpoint" [svc uri request-method]))
;       (when (empty? error) (log-warn "warning - sim extension error not described in codex" [svc uri request-method]))
;       (if (seq error) (ph/swap error-rsp *tree* {} :gen-all true) {:status x})))
;   ([] (error (Long. (name (first (rand-nth (d/error-status *tree*))))))))
;
;

(defn respond
  ([request status] (u/find #(= (:status %) status) (d/responses request)))
  ([request status body] (assoc (respond request status) :body body)))

; (defn respond
;   ([status] {:status status})
;   ([status & {:keys [body-url body headers]}]
;     (if body-url
;       (rsp status {h/ctype (h/mime body-url)} (qslurp body-url))
;       (rsp status headers body))))
;
; (defn respond-400
;   "Get a 400 error body template from either defaults.edn or our code default"
;   [detail]
;   (let [desc (or (d/get-in-tree *tree* [:errors :400 :description]) (:description (:400 (p/errors))))
;         tpl (or (d/get-in-tree *tree* [:errors :400 :template]) (:template (:400 (p/errors))))
;         body (assoc tpl desc detail)]
;     (respond 400 :headrs {h/ctype h/jsn} :body (c/js body))))


;; =============================================================================
;; Validation of Request by Codex Specification
;; =============================================================================

(defn- body-errors [request protean-home tree]
  (let [expected-ctype (d/req-ctype tree)
        schema (d/to-path protean-home (d/get-in-tree tree [:req :body-schema]) tree)
        codex-body (d/body-req tree)]
    (v/validate-body request expected-ctype schema codex-body)))

(defn validate
  "Validate request against codex specification"
  [request]
  (let [{:keys [tree protean-home]} request
        errors (seq (remove nil? (conj (v/validate-headers (d/req-hdrs tree) request)
                                       (v/validate-query-params request tree)
                                       (v/validate-form-params request tree)
                                       (v/validate-matrix-params request tree)
                                       (body-errors request protean-home tree))))]
    (when errors (log-warn (s/join "," errors)))
    errors))
