(ns protean.api.transformation.test-paths
  (:require [clojure.java.io :refer [file]]
            [protean.api.codex.reader :as r]
            [protean.api.transformation.paths :refer [services]]
            [expectations :refer :all]
            [me.rossputin.diskops :as dsk]))

(defn read-edn [f] (r/read-codex (dsk/pwd) (file "test/resources/paths" f)))

;; =============================================================================
;; Testing computation of endpoints from codex
;; =============================================================================

(let [services (services (read-edn "get-codex-root.edn") ["sample"])]
  (expect ["sample" "sample"] (map #(:svc %) services))
  (expect ["/" "test"] (map #(:path %) services)))

(let [services (services (read-edn "get-codex-paths.edn") ["sample simple test"])]
  (expect ["sample" "sample"] (map #(:svc %) services))
  (expect ["simple" "test"] (map #(:path %) services)))

(let [services (services (read-edn "get-codex-paths.edn") ["sample"])]
  (expect ["sample" "sample"] (map #(:svc %) services))
  (expect #{"simple" "test"} (set (map #(:path %) services))))

(let [services (services (read-edn "get-codex.edn") ["sample simple test"])
      p (first services)]
  (expect "sample" (:svc p))
  (expect "simple" (:path p)))

(let [services (services (read-edn "get-codex.edn") ["sample"])
      p (first services)]
  (expect "sample" (:svc p))
  (expect "simple" (:path p)))

(let [services (services (read-edn "multi-method.edn") ["sample homes/1"])]
  (expect ["sample" "sample" "sample"] (map #(:svc %) services))
  (expect #{"homes/1"} (set (map #(:path %) services)))
  (expect #{:get :put :delete} (set (map #(:method %) services))))
