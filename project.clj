(defproject protean-api "0.12.0-pre.1"
  :description "Protean API."
  :url "http://github.com/passivsystems/protean-api"
  :license {:name "Apache License v2.0"
            :url "http://www.apache.org/licenses/LICENSE-2.0"}
  :min-lein-version "2.0.0"
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/data.xml "0.0.8"]
                 [cheshire "5.7.1"]
                 [clj-http "3.6.0"]
                 [com.cemerick/pomegranate "0.3.1"]
                 [com.gfredericks/test.chuck "0.2.7"]
                 [com.github.fge/json-schema-validator "2.1.7"]
                 [com.taoensso/timbre "4.10.0"]
                 [environ "1.1.0"]
                 [expectations "2.1.9"]
                 [io.aviso/pretty "0.1.33"]
                 [overtone/at-at "1.2.0"]
                 [yaclot "0.1.5"]
                 [me.rossputin/diskops "0.7.0"]]
  :plugins [[lein-expectations "0.0.8"]
            [lein-codox "0.10.3"]]
  :codox {:namespaces [protean.api.codex.document
                       protean.api.protocol.http
                       protean.api.generation.generate
                       protean.api.generation.json
                       protean.api.transformation.sim
                       protean.api.transformation.coerce]}
  :aot :all)
