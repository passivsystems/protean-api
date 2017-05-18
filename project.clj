(defproject protean-api "0.11.0-alpha.1"
  :description "Protean API."
  :url "http://github.com/passivsystems/protean-api"
  :license {:name "Apache License v2.0"
            :url "http://www.apache.org/licenses/LICENSE-2.0"}
  :min-lein-version "2.0.0"
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/data.xml "0.0.8"]
                 [cheshire "5.6.3"]
                 [clj-http "3.0.1"]
                 [com.cemerick/pomegranate "0.3.0"]
                 [com.gfredericks/test.chuck "0.2.7"]
                 [com.github.fge/json-schema-validator "2.1.7"]
                 [com.taoensso/timbre "4.7.4"]
                 [environ "1.0.0"]
                 [io.aviso/pretty "0.1.30"]
                 [overtone/at-at "1.2.0"]
                 [yaclot "0.1.5"]
                 [me.rossputin/diskops "0.4.1"]]
  :plugins [[lein-expectations "0.0.8"]
            [lein-codox "0.10.3"]]
  :codox {:namespaces [protean.api.codex.document
                       protean.api.protocol.http
                       protean.api.generation.generate
                       protean.api.generation.json
                       protean.api.transformation.sim
                       protean.api.transformation.coerce]}
  :aot :all
  :uberjar-name ~(str (-> "project.clj" slurp read-string (nth 1)) "-" (-> "project.clj" slurp read-string (nth 2)) "-standalone.jar"))
