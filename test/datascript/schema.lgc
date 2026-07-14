(require [clojure.string :as string])

(module Datascript_schema
  (def schema-keys
    #{:db/ident :db/isComponent :db/noHistory :db/valueType :db/cardinality
      :db/unique :db/index :db.install/_attribute :db/doc :db/tupleType
      :db/tupleTypes :db/tupleAttrs})

  (defonce schema-attr?
    #{:db/id :db/ident :db/isComponent :db/valueType :db/cardinality
      :db/unique :db/index :db/doc :db/tupleAttrs :db/tupleType :db/tupleTypes})

  (def type-keywords
    #{:db.type/number :db.type/instant :db.type/keyword :db.type/ref
      :db.type/string :db.type/uuid :db.type/tuple})

  (defn schema? [^:Datascript_value.value entity]
    (match
      (Datascript_value/map-get
        (Datascript_value/keyword-value ":db/ident") entity)
      None None
      (Some _)
        (Datascript_value/map-get
          (Datascript_value/keyword-value ":db/cardinality") entity)))

  (defn schema-entity? [^:Datascript_value.value entity]
    (some
      (fn [key]
        (some?
          (Datascript_value/map-get
            (Datascript_value/keyword-value key) entity)))
      (list ":db/ident" ":db/isComponent" ":db/noHistory" ":db/valueType"
            ":db/cardinality" ":db/unique" ":db/index"
            ":db.install/_attribute" ":db/doc" ":db/tupleType"
            ":db/tupleTypes" ":db/tupleAttrs")))

  (defn is-system-keyword? [^:Datascript_value.value value]
    (match value
      (Datascript_value.KeywordValue label)
        (if (string/starts-with? label ":db/")
          true
          (string/starts-with? label ":db."))
      (Datascript_value.StringValue label)
        (if (string/starts-with? label "db/")
          true
          (string/starts-with? label "db."))
      _ false))

  (defn type? [^:Datascript_value.value value]
    (match value
      (Datascript_value.KeywordValue label)
        (if (= label ":db.type/number")
          (Some value)
          (if (= label ":db.type/instant")
            (Some value)
            (if (= label ":db.type/keyword")
              (Some value)
              (if (= label ":db.type/ref")
                (Some value)
                (if (= label ":db.type/string")
                  (Some value)
                  (if (= label ":db.type/uuid")
                    (Some value)
                    (if (= label ":db.type/tuple")
                      (Some value)
                      None)))))))
      _ None)))
