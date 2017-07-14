(ns protean.test-core
  (:require [clojure.java.io :refer [file]]
            [protean.core :as core]
            [protean.api.protocol.http :as h]
            [protean.api.codex.reader :as r]
            [expectations :refer :all]
            [taoensso.timbre :as l]
            [me.rossputin.diskops :as dsk]))

(def protean-home (dsk/pwd))

(l/set-level! :warn)

(defn- req [m u c b f]
  {
    :ssl-client-cert nil
    :remote-addr "127.0.0.1"
    :params {:* u}
    :route-params {:* u}
    :headers {"user-agent" "curl/7.29.0"
              "content-type" c
              "accept" "*/*"
              "host" "localhost:3000"}
    :server-port 3000
    :content-length nil
    :form-params (or f {})
    :query-params {}
    :content-type nil
    :character-encoding nil
    :uri u
    :server-name "localhost"
    :query-string nil
    :body b
    :scheme :http
    :request-method m
  })

(def body (.getBytes "" "UTF-8"))

;; =============================================================================
;; Simple methods statuses and headers
;; =============================================================================

(def cdx-1 {
  "sample" {
    "simple" {
      :get [{:rsp {:200 {}}}]
      :head [{:rsp {:200 {:headers {"token" "aGVsbG8gc2FpbG9y"}}}}]
      :put [{:rsp {:204 {}}}]
      :post [{:rsp {:201 {:headers {"Location" "over here"}}}}]
      :delete [{:rsp {:204 {}}}]
      :patch [{:rsp {:204 {}}}]
    }
  }
})

(let [rsp-1 (core/sim-rsp protean-home (req :get "/sample/simple" h/txt body nil) cdx-1 {})
      rsp-2 (core/sim-rsp protean-home (req :head "/sample/simple" nil body nil) cdx-1 {})
      rsp-3 (core/sim-rsp protean-home (req :put "/sample/simple" nil body nil) cdx-1 {})
      rsp-4 (core/sim-rsp protean-home (req :post "/sample/simple" nil body nil) cdx-1 {})
      rsp-5 (core/sim-rsp protean-home (req :delete "/sample/simple" nil body nil) cdx-1 {})
      rsp-6 (core/sim-rsp protean-home (req :patch "/sample/simple" nil body nil) cdx-1 {})
      rsp-7 (core/sim-rsp protean-home (req :get "/sample/404" nil body nil) cdx-1 {})
      rsp-8 (core/sim-rsp protean-home (req :muppet "/sample/simple" nil body nil) cdx-1 {})]
  (expect 200 (:status rsp-1))
  (expect 200 (:status rsp-2))
  (expect 2 (count (:headers rsp-2))) ;; account for CORS headers
  (expect 204 (:status rsp-3))
  (expect 201 (:status rsp-4))
  (expect 2 (count (:headers rsp-4))) ;; account for CORS headers
  (expect 204 (:status rsp-5))
  (expect 204 (:status rsp-6))
  (expect 404 (:status rsp-7))
  (expect true (contains? (:headers rsp-7) "Protean-error"))
  (expect 405 (:status rsp-8))
  (expect true (contains? (:headers rsp-8) "Allow")))


;; =============================================================================
;; Parameters
;; =============================================================================

(def cdx-2 {
  "sample" {
    "simple/${thingId}" {
      :get [
        {:rsp {:200 {}}}
        {:get {:rsp {:200 {}}}}
        {
          "simple/${thingId}" {:get {:rsp {:200 {}}}},
          :vars {"thingId" {:doc "Id of thing", :type :Int}}
        }
        {:rsp {:200 {:doc "OK"}}}
        {:get {:rsp {:200 {:doc "OK"}}},
         :default-content-type "application/json; charset=utf-8",
         "sample" {
           "simple/${thingId}" {:get {:rsp {:200 {}}}},
           :vars {"thingId" {:doc "Id of thing", :type :Int}}
         }
        }
      ]
    }
  }
})

(let [rsp-1 (core/sim-rsp protean-home (req :get "/sample/simple/1" h/txt body nil) cdx-2 {})
      rsp-2 (core/sim-rsp protean-home (req :get "/sample/simple" h/txt body nil) cdx-2 {})]
  (expect 200 (:status rsp-1))
  (expect 404 (:status rsp-2))
  (expect true (contains? (:headers rsp-2) "Protean-error")))


;; =============================================================================
;; Validation
;; =============================================================================

(def cdx-3 {
  "sample" {
    "simple" {
      :get [{
        :validating true
        :vars {"rp1" {:type :String :doc "A test request param"}}
        :req {:query-params {"rp1" ["${rp1}" :required]}}
        :rsp {:200 {}}
      }]
    }
  }
  })

(let [rsp-1 (core/sim-rsp protean-home (req :get "/sample/simple" h/txt body nil) cdx-3 {})]
  (expect 400 (:status rsp-1)))


;; =============================================================================
;; Sim extension
;; =============================================================================

(def sims (clojure.main/load-script "test-data/simext-simple-error.sim.edn"))

(def cdx-4 {
  "sample" {
    "simple" {:get [{:rsp {:200 {} :501 {}}}]}
  }
})

;; test we get a protean error 500 if response breaks codex contract
;; in this case we test against a codex which does not contain a 400 response
(let [rsp-1 (core/sim-rsp protean-home (req :get "/sample/simple" h/txt body nil) cdx-4 sims)]
  (expect 500 (:status rsp-1)))


;; validating sim extension

(def sim-2 (clojure.main/load-script "test-data/simext-simple-validate.sim.edn"))

(def cdx-5 {
  "sample" {
    "simple" {
      :get [{
        :vars {"rp1" {:type :String :doc "A test request param"}}
        :req {:query-params {"rp1" ["${rp1}" :required]}}
        :rsp {:200 {} :400 {}}
      }]
    }
    "complex" {
      :get [{
        :vars {"rp1" {:type :String :doc "A test request param"}}
        :req {:query-params {"rp1" ["${rp1}" :required]}}
        :rsp {:200 {} :403 {}}
      }]
    }
  }
  })

(let [rsp-1 (core/sim-rsp protean-home (req :get "/sample/simple" h/txt body nil) cdx-5 sim-2)
      rsp-2 (core/sim-rsp protean-home (req :get "/sample/complex" h/txt body nil) cdx-5 sim-2)]
  (expect 400 (:status rsp-1))
  (expect 403 (:status rsp-2)))


;; matrix-parameter sim extension

(def sim-3 (clojure.main/load-script "test-data/matrix-params.sim.edn"))

(let [cdx (r/read-codex (dsk/pwd) (file "test-data/matrix-params.edn"))
      rsp-1 (core/sim-rsp protean-home (req :get "/gu/groups;groupId=2143759047;city=szcgPg2pm5cmU6Kv8y4kCDVv4CiBVUU" h/txt body nil) cdx sim-3)
      rsp-2 (core/sim-rsp protean-home (req :get "/gu/groups" h/txt body nil) cdx {})]
  (expect #"groupId" (:body rsp-1))
  (expect 400 (:status rsp-2)))
