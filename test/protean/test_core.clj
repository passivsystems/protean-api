(ns protean.test-core
  (:require [clojure.java.io :refer [file]]
            [clojure.string :as s]
            [protean.core :as core]
            [protean.api.protocol.http :as h]
            [protean.api.codex.reader :as r]
            [expectations :refer :all]
            [taoensso.timbre :as l]
            [me.rossputin.diskops :as dsk]))

(defn- sim-rsp [req cdx sim] (core/sim-rsp (dsk/pwd) req cdx sim))

(l/set-level! :info)

(def json-hdrs {"Access-Control-Allow-Origin" "*"
                "Content-Type" "application/json; charset=utf-8"})

(defn- req [m u h b f]
  (let [items (s/split u #"[?&]")
        uri (first items)
        qps (into {} (map #(s/split % #"=") (rest items)))]
    {
      :ssl-client-cert nil
      :remote-addr "127.0.0.1"
      :params {:* uri}
      :route-params {:* uri}
      :headers (merge {"user-agent" "curl/7.29.0"
                       "content-type" h/txt
                       "accept" "*/*"
                       "host" "localhost:3000"}
                      h)
      :server-port 3000
      :content-length nil
      :form-params (or f {})
      :query-params qps
      :content-type nil
      :character-encoding nil
      :uri uri
      :server-name "localhost"
      :query-string nil
      :body b
      :scheme :http
      :request-method m
    }))

(def body (.getBytes "" "UTF-8"))

(def get-sample-simple (req :get "/sample/simple" nil body nil))

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

(expect {:status 200 :headers {"Access-Control-Allow-Origin" "*"} :body nil}
        (sim-rsp get-sample-simple cdx-1 {}))

(expect {:status 200 :headers {"Access-Control-Allow-Origin" "*", "token" "aGVsbG8gc2FpbG9y"} :body nil}
        (sim-rsp (req :head "/sample/simple" nil body nil) cdx-1 {}))

(expect {:status 204 :headers {"Access-Control-Allow-Origin" "*"} :body nil}
        (sim-rsp (req :put "/sample/simple" nil body nil) cdx-1 {}))

(expect {:status 201 :headers {"Access-Control-Allow-Origin" "*", "Location" "over here"} :body nil}
        (sim-rsp (req :post "/sample/simple" nil body nil) cdx-1 {}))

(expect {:status 204 :headers {"Access-Control-Allow-Origin" "*"} :body nil}
        (sim-rsp (req :delete "/sample/simple" nil body nil) cdx-1 {}))

(expect {:status 204 :headers {"Access-Control-Allow-Origin" "*"} :body nil}
        (sim-rsp (req :patch "/sample/simple" nil body nil) cdx-1 {}))

(expect {:status 404 :headers {"Protean-error" "Not Found"}}
        (sim-rsp (req :get "/sample/404" nil body nil) cdx-1 {}))

(expect {:status 405 :headers {"Protean-error" "Method Not Allowed", "Allow" "GET, HEAD, PUT, POST, DELETE, PATCH"}}
        (sim-rsp (req :muppet "/sample/simple" nil body nil) cdx-1 {}))

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

(expect {:status 200 :headers {"Access-Control-Allow-Origin" "*"} :body nil}
        (sim-rsp (req :get "/sample/simple/1" nil body nil) cdx-2 {}))

(expect {:status 404 :headers {"Protean-error" "Not Found"}}
        (sim-rsp get-sample-simple cdx-2 {}))

;; =============================================================================
;; Validation
;; =============================================================================

(def cdx-3 {
  "sample" {
    "simple" {
      :get [{
        :validate? true
        :types {:String "[a-zA-Z0-9]+"}
        :vars {"q1" {:type :String :doc "A test request param"}}
        :req {:query-params {"q1" ["${q1}" :required]}}
        :rsp {:200 {}}
      }]
    }
  }
})

(expect {:status 400
         :headers {"Access-Control-Allow-Origin" "*"
                   "Protean-error" "Bad Request"
                   "Protean-error-messages" "expected query params: q1 (was )"}}
        (sim-rsp get-sample-simple cdx-3 {}))

;; =============================================================================
;; Sim extension
;; =============================================================================

;; defines a 400 response
(def sims (clojure.main/load-script "test/resources/simext-simple.sim.edn"))

;; test we get a protean error 500 if response breaks codex contract
;; in this case we test against a codex which does not contain a 400 response
(let [cdx (r/read-codex (dsk/pwd) (file "test/resources/simext-simple.edn"))]
  (expect {:status 500, :headers {"Protean-error" "Error in sim"}}
          (sim-rsp get-sample-simple cdx sims)))


;; validating sim extension

(def sim-2 (clojure.main/load-script "test/resources/simext-simple-validate.sim.edn"))

(def cdx-5 {
  "sample" {
    "simple" {
      :get [{
        :types {:String "[a-zA-Z0-9]+"}
        :vars {"q1" {:type :String}
               "q2" {:type :Int}
               "q3" {:type :Int}}
        :req {:query-params {"q1" ["${q1}" :required]
                             "q2" ["${q2}" :optional]
                             "q3" ["${q3}" :optional :multiple]}}
        :rsp {:200 {} :400 {:headers {"Content-Type" "application/json; charset=utf-8"}}}
      }]
    }
    "bespoke" {
      :get [{
        :types {:String "[a-zA-Z0-9]+"}
        :vars {"q1" {:type :String :doc "A test request param"}}
        :req {:query-params {"q1" ["${q1}" :required]}}
        :rsp {:200 {} :403 {}}
      }]
    }
    "override" {
      :get [{
        :types {:String "[a-zA-Z0-9]+"}
        :vars {"q1" {:type :String :doc "A test request param"}}
        :req {:query-params {"q1" ["${q1}" :required]}}
        :rsp {:200 {} :403 {}}
      }]
    }
    "auth" {
      :get [{
        :types {:Token "[0-9a-zA-Z0-9]{15}"}
        :vars {"bearerToken" {:type :Token :examples ["08d2301e-ee81-4654-b448-0636f454612a"]}}
        :req {:headers {"Authorization" "Bearer ${bearerToken}"}}
        :rsp {:200 {} :401 {} :403 {}}
      }]
    }
  }
})

;; missing q1
(expect {:status 400 :headers json-hdrs :body "{\"query-errors\":\"expected query params: q1 (was )\"}"}
        (sim-rsp get-sample-simple cdx-5 sim-2))

;; q2 is not an integer
(expect {:status 400 :headers json-hdrs :body "{\"query-errors\":\"invalid query params: q2 value: 'bad' does not match: ^-?\\\\d{1,10}$\"}"}
        (sim-rsp (req :get "/sample/simple?q1=1?q2=bad" nil body nil) cdx-5 sim-2))

;; q3 is not a list of integers
(expect {:status 400 :headers json-hdrs :body "{\"query-errors\":\"invalid query params: q3 value: 'a,b,c' does not match: ^-?\\\\d{1,10}$\"}"}
        (sim-rsp (req :get "/sample/simple?q1=1?q2=1?q3=a,b,c" nil body nil) cdx-5 sim-2))

;; q3 is using an invalid separator
(expect {:status 400 :headers json-hdrs :body "{\"query-errors\":\"invalid query params: q3 value: '1;2;3' does not match: ^-?\\\\d{1,10}$\"}"}
        (sim-rsp (req :get "/sample/simple?q1=1?q2=1?q3=1;2;3" nil body nil) cdx-5 sim-2))

(expect {:status 200 :headers {"Access-Control-Allow-Origin" "*"} :body nil}
        (sim-rsp (req :get "/sample/simple?q1=1?q2=1?q3=1" nil body nil) cdx-5 sim-2))

(expect {:status 200 :headers {"Access-Control-Allow-Origin" "*"} :body nil}
        (sim-rsp (req :get "/sample/simple?q1=1?q2=1?q3=1,2,3" nil body nil) cdx-5 sim-2))

(expect {:status 200 :headers {"Access-Control-Allow-Origin" "*"} :body nil}
        (sim-rsp (req :get "/sample/simple?q1=1" nil body nil) cdx-5 sim-2))

;; missing q1 - sim changes 400 to 403
(expect {:status 403 :headers {"Access-Control-Allow-Origin" "*"}  :body nil}
        (sim-rsp (req :get "/sample/bespoke" nil body nil) cdx-5 sim-2))

(expect {:status 200 :headers {"Access-Control-Allow-Origin" "*"} :body nil}
        (sim-rsp (req :get "/sample/bespoke?q1=1" nil body nil) cdx-5 sim-2))

;; missing q1 - sim changes 400 to 403
(expect {:status 403 :headers {"Access-Control-Allow-Origin" "*"}  :body nil}
        (sim-rsp (req :get "/sample/override" nil body nil) cdx-5 sim-2))

(expect {:status 200 :headers {"Access-Control-Allow-Origin" "*"} :body nil}
        (sim-rsp (req :get "/sample/override?q1=1" nil body nil) cdx-5 sim-2))

;; missing auth header - sim changes 400 to 401 when auth header is missing
(expect {:status 401 :headers {"Access-Control-Allow-Origin" "*"}  :body nil}
        (sim-rsp (req :get "/sample/auth" {} body nil) cdx-5 sim-2))

;; invalid auth header - sim changes 400 to 403 when auth header is invalid
(expect {:status 403 :headers {"Access-Control-Allow-Origin" "*"}  :body nil}
        (sim-rsp (req :get "/sample/auth" {"Authorization" "Bearer xxx"} body nil) cdx-5 sim-2))

(expect {:status 200 :headers {"Access-Control-Allow-Origin" "*"} :body nil}
        (sim-rsp (req :get "/sample/auth" {"Authorization" "Bearer abcdefghicklmno"} body nil) cdx-5 sim-2))

;; matrix-parameter sim extension

(def cdx-6 {
  "gu" {
    "groups${;groupFilter}" {
      :get [{
        :types {
          :String "[a-zA-Z0-9]+"}
        :vars {
          "groupId" {:type :Int, :doc "Group Id"},
          "city" {:type :String, :doc "City"},
          ";groupFilter" {
            :type :MatrixParams,
            :doc "matrix parameters to filter groups. Valid parameters are: groupId (multiple), city (multiple)",
            :struct {
              "groupId" ["${groupId}" :required :multiple],
              "city" ["${city}" :optional :multiple]}}},
        :rsp {
          :200 {:headers {"Content-Type" "application/json; charset=utf-8"}},
          :400 {:headers {"Content-Type" "application/json; charset=utf-8"}}
        }
      }]
    }
  }
})

(def sim-3 (clojure.main/load-script "test/resources/matrix-params.sim.edn"))

(expect {:status 200 :headers json-hdrs :body "{\"groupId\":\"2143759047\",\"city\":\"Glasgow\"}"}
        (sim-rsp (req :get "/gu/groups;groupId=2143759047;city=Glasgow" nil body nil) cdx-6 sim-3))

(expect {:status 400 :headers {"Access-Control-Allow-Origin" "*"
                               "Protean-error" "Bad Request"
                               "Protean-error-messages" "expected matrix params: groupId (was )"}}
        (sim-rsp (req :get "/gu/groups" nil body nil) cdx-6 sim-3))


;; Input as output tests

(def cdx-7 {
  "sample" {
    "inputs${;inputFilter}/${pathPlaceholder}/form" {
      :post [{
        :types {:Token "[a-z]{3}" :String "[a-zA-Z0-9]+"}
        :vars {"pathPlaceholder" {:type :Long}
               "headerPlaceholder" {:type :Token}
               "queryPlaceholder" {:type :String}
               "matrixPlaceholder" {:type :String}
               ";inputFilter" {:type :MatrixParams
                               :struct {"m" ["${matrixPlaceholder}" :required]}}
               "formPlaceholder" {:type :String}}
        :req {:headers {"h" "Bearer ${headerPlaceholder}"}
              :query-params {"q" ["${queryPlaceholder}" :required]}
              :form-params {"f" ["${formPlaceholder}" :required]}}
        :rsp {:200 {:body-examples ["test/resources/responses/output.json"]
                    :headers {"location" "outputs/${pathPlaceholder}/${headerPlaceholder}"}}}
      }]
    }
  }
})

(def json-body "{
  \"pathPlaceholder\": 123,
  \"headerPlaceholder\": \"xxx\",
  \"queryPlaceholder\": \"sweet\",
  \"matrixPlaceholder\": \"juice\",
  \"formPlaceholder\": \"me\"
}\n")

(expect {:status 200 :headers (merge json-hdrs {"location" "outputs/123/xxx"}) :body json-body}
        (sim-rsp (req :post "/sample/inputs;m=juice/123/form?q=sweet" {"h" "Bearer xxx"} nil {"f" "me"}) cdx-7 nil))
