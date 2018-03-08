(ns protean.api.transformation.xmlvalidation
  (:import [javax.xml XMLConstants]
           [org.xml.sax SAXException]
           [javax.xml.validation SchemaFactory]
           [java.io File]
           [java.io StringReader]
           [javax.xml.transform.stream StreamSource]))

(defn- validator [schema]
  (.newValidator
    (.newSchema
      (SchemaFactory/newInstance XMLConstants/W3C_XML_SCHEMA_NS_URI)
      (StreamSource. (File. schema)))))

(defn validate [schema data]
  (try
    (.validate (validator schema)
      (StreamSource. (StringReader. data)))
    {:success true}
    (catch SAXException e {:success false :message (.getMessage e)})))
