(ns datascript.schema
  (:require
   [clojure.string :as string]
   [ocaml.package/datascript.runtime]))

(def schema-keys #{:db/ident :db/isComponent :db/noHistory :db/valueType :db/cardinality :db/unique :db/index :db.install/_attribute :db/doc :db/tupleType :db/tupleTypes :db/tupleAttrs})

(defonce schema-attr?
  #{:db/id :db/ident :db/isComponent :db/valueType :db/cardinality :db/unique :db/index :db/doc :db/tupleAttrs  :db/tupleType :db/tupleTypes})

(defn schema?
  [entity]
  (and
   (contains? entity :db/ident)
   (contains? entity :db/cardinality)))

(defn schema-entity?
  [entity]
  (reduce
   (fn [found key]
     (or found (contains? entity key)))
   false
   schema-keys))

(defn system-label? [label prefix]
  (or
   (string/starts-with? label (str prefix "/"))
   (string/starts-with? label (str prefix "."))))

(defn is-system-keyword?
  [value]
  (if-some [label (Datascript_runtime.Data_value.keyword_value value)]
    (system-label? label ":db")
    (match value
      (Datascript_runtime.Data_value.String label)
      (system-label? label "db")
      _ false)))

(def type?
  #{:db.type/number
    :db.type/instant
    :db.type/keyword
    :db.type/ref
    :db.type/string
    :db.type/uuid
    :db.type/tuple})
