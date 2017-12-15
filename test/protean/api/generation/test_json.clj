(ns protean.api.generation.test-json
  (:require [cheshire.core :as cc]
            [protean.api.generation.json :refer [gen]]
            [protean.api.transformation.coerce :refer [clj]]
            [expectations :refer :all]))

(let [json (gen "test/resources/schemas/pet.schema.json")
      c (clj json true)]
  (expect (:status c) (in #{"available", "pending", "sold"})))

(let [json (gen "test/resources/schemas/test.schema.json")
      c (clj json true)]
  (expect (:enum c) (in #{"one", "two", "three"})))
