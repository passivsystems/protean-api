(defproject protean-api "0.14.0"
  :description "Protean API."
  :url "http://github.com/passivsystems/protean-api"
  :license {:name "Apache License v2.0"
            :url "http://www.apache.org/licenses/LICENSE-2.0"}
  :min-lein-version "2.0.0"
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.clojure/data.xml "0.0.8"]
                 [cheshire "5.10.0"]
                 [clj-http "3.10.1"]
                 [org.tcrawley/dynapath "1.1.0"]
                 [com.cemerick/pomegranate "1.1.0"]
                 [com.gfredericks/test.chuck "0.2.9"]
                 [com.github.java-json-tools/json-schema-validator "2.2.14"]
                 [com.taoensso/timbre "4.10.0"]
                 [environ "1.2.0"]
                 [expectations "2.1.10"]
                 [io.aviso/pretty "0.1.37"]
                 [overtone/at-at "1.2.0"]
                 [yaclot "0.1.5"]
                 [me.rossputin/diskops "0.8.0"]]
  :plugins [[lein-expectations "0.0.8"]
            [lein-codox "0.10.7"]]
  :codox {:namespaces [protean.api.protocol.http
                       protean.api.generation.generate
                       protean.api.generation.json
                       protean.api.transformation.sim
                       protean.api.transformation.coerce]}
  :aot :all)
