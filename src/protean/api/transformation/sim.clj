(ns protean.api.transformation.sim
  "API for powering sim extensions."
  (:require [clojure.string :as s]
            [cemerick.pomegranate :as pom]
            [protean.utils :as u]
            [protean.api.codex.document :as d]
            [protean.api.transformation.coerce :as c]
            [protean.api.transformation.validation :as v]
            [overtone.at-at :as at])
  (:use [taoensso.timbre :as timbre
     :only (debug info warn)
     :rename {debug log-debug info log-info warn log-warn}]))

(defn dependencies [xs]
  (pom/add-dependencies
    :coordinates xs
    :repositories (merge cemerick.pomegranate.aether/maven-central
                       {"clojars" "http://clojars.org/repo"})))


;; =============================================================================
;; Sim Machinery Access
;; =============================================================================

(defn find-path
  "Quantum path lookup, used to look for sim extension referenced resources in
   multiple places. Resolves relative paths to absolute.
   p is a resource path (probably relative)."
  [{:keys [protean-home tree] :as request} p]
  (d/to-path protean-home p tree))


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

(defn matrix-param [request p] (get-in request [:matrix-params p]))

(defn matrix-params [{:keys [matrix-params]} p]
  (-> (filter (fn [[k _]] (s/starts-with? k p)) matrix-params)
      (u/update-keys #(s/join "." (rest (s/split % #"\."))))))

(defn param [request p] (get-in request [:params p]))

(defn form-param [request p] (get-in request [:form-params p]))

(defn body-param
  ([request p] (get-in (body-clj request) (if (vector? p) p [p])))
  ([request p k] (get-in (body-clj request k) (if (vector? p) p [p]))))

(defn header [request h] (get-in request [:headers h]))


;; =============================================================================
;; Responses
;; =============================================================================

(defn success-responses
  "Note here request is a request with tree and other data blended in"
  [{:keys [response]}] (:success response))

(defn error-responses
  "Note here request is a request with tree and other data blended in"
  [{:keys [response]}] (:error response))

(defn responses [request]
  (concat (success-responses request) (error-responses request)))

(defn respond
  ([request status] (u/find #(= (:status %) status) (responses request)))
  ([request status body] (assoc (respond request status) :body body)))

;; =============================================================================
;; Validation of Request by Codex Specification
;; =============================================================================

(defn- body-errors [request protean-home tree]
  (let [schema (d/to-path protean-home (d/get-in-tree tree [:req :body-schema]) tree)
        codex-body (d/req-body tree)]
    (v/validate-body request schema codex-body)))

(defn validate
  "Validate request against codex specification"
  [request]
  (let [{:keys [tree protean-home]} request
        raw {:header-errors (v/validate-headers request)
             :query-errors (v/validate-query-params request)
             :form-errors (v/validate-form-params request)
             :matrix-errors (v/validate-matrix-params request)
             :body-errors (body-errors request protean-home tree)}
        errors (into {} (remove (fn [[k v]] (nil? v)) raw))]
    (when-not (empty? errors)
      (log-warn (s/join "," errors))
      errors)))
