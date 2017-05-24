(ns protean.api.codex.test-reader
  (:require [clojure.java.io :refer [file]]
            [expectations :refer :all]
            [protean.api.codex.reader :as r]))

(let [cdx (r/read-codex (file (str "test-data/" "reader.edn")))
      odr (:ordered-resources cdx)]
  (expect 12 (count odr))
  (expect "cpath-1" (first odr))
  (expect "bpath_2" (second odr)))
