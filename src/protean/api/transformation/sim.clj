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

(defn header-param [request p] (get-in request [:headers (s/lower-case p)]))

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

(defn success-codes
  [{:keys [tree]}]
  (for [[k _] (d/success-status tree)] (read-string (name k))))

(defn error-codes
  [{:keys [tree]}]
  (for [[k _] (d/error-status tree)] (read-string (name k))))

(defn- build-rsp
  ([{:keys [tree] :as request} status custom-body]
  (when-let [rsp (d/get-in-tree tree [:rsp (keyword (str status))])]
    (let [{:keys [body-examples headers]} rsp
          body (cond
                 (some? custom-body) custom-body
                 (first body-examples) (slurp (find-path request (first body-examples)))
                 :else  nil)]
      {:status status :headers headers :body body}))))

(defn response
  ([request status] (response request status nil))
  ([request status body] (or (build-rsp request status body)
                             (throw (IllegalArgumentException. (str
                               "Status: " status " not found. Codex statuses are: "
                               (s/join ", " (concat (success-codes request)
                                                    (error-codes request)))))))))

;; =============================================================================
;; Validation of Request by Codex Specification
;; =============================================================================

(defn- body-errors
  [{:keys [tree] :as request}]
  (let [schema (find-path request (d/get-in-tree tree [:req :body-schema]))
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
             :body-errors (body-errors request)}
        errors (into {} (remove (fn [[k v]] (nil? v)) raw))]
    (when-not (empty? errors)
      (log-warn (s/join "," errors))
      errors)))
