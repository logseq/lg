(ns datascript.test.validation
  (:require
    [clojure.test :refer [deftest is]]
    [datascript.core :as d]
    [datascript.db :as db]))

(defn ^:Datascript_runtime.Data_value.entity_ref entity-id
  [^:int value]
  (Datascript_runtime.Data_value.Entity_id value))

(defn ^:Datascript_runtime.Data_value.t int-value [^:int value]
  (Datascript_runtime.Data_value.Int value))

(defn ^:Datascript_runtime.Data_value.t string-value [^:string value]
  (Datascript_runtime.Data_value.String value))

(defn ^:Datascript_runtime.Data_value.t keyword-value [^:keyword value]
  (Datascript_runtime.Data_value.Keyword (str value)))

(defn ^:map<keyword;Datascript_runtime.Data_value.t> schema-entry
  [^:keyword key ^:keyword value]
  (zipmap [key] [(keyword-value value)]))

(defn ^:map<keyword;Datascript_runtime.Data_value.t> entity-map
  [^:vector<keyword> keys
   ^:vector<Datascript_runtime.Data_value.t> values]
  (zipmap keys values))

(defn ^datascript.db/DB validation-db []
  (d/empty-db
   (zipmap
    [:profile :id]
    [(schema-entry :db/valueType :db.type/ref)
     (schema-entry :db/unique :db.unique/identity)])))

(defn assert-invalid
  [^datascript.db/DB database
   ^:string message
   ^:vector<datascript.db/tx-entry> transaction]
  (is (thrown-msg? message (d/db-with database transaction))))

(defn assert-nil-value
  [^datascript.db/DB database
   ^:vector<datascript.db/tx-entry> transaction]
  (is
   (thrown-msg?
    "Cannot store nil as a value at <value>"
    (d/db-with database transaction))))

(deftest test-with-validation
  (let [database (validation-db)]
    (assert-invalid
     database
     "Expected number, string or lookup ref for :db/id"
     [(db/tx-entity
       (entity-map
        [:db/id :name]
        [(Datascript_runtime.Data_value.Regex "")
         (string-value "Ivan")]))])

    ;; Invalid attributes, operation tags, and top-level transaction shapes
    ;; cannot inhabit the closed tx-entry type and are rejected statically.

    (assert-nil-value
     database
     [(db/tx-add
       (entity-id -1)
       :name
       (Datascript_runtime.Data_value.Nil))])
    (assert-nil-value
     database
     [(db/tx-entity
       (entity-map
        [:db/id :name]
        [(int-value -1) (Datascript_runtime.Data_value.Nil)]))])
    (assert-nil-value
     database
     [(db/tx-add
       (entity-id -1)
       :id
       (Datascript_runtime.Data_value.Nil))])
    (assert-nil-value
     database
     [(db/tx-entity
       (entity-map
        [:db/id :id]
        [(int-value -1) (string-value "A")]))
      (db/tx-entity
       (entity-map
        [:db/id :id]
        [(int-value -1) (Datascript_runtime.Data_value.Nil)]))])

    (assert-invalid
     database
     "Expected number or lookup ref for entity id"
     [(db/tx-add
       (entity-id -1)
       :profile
       (Datascript_runtime.Data_value.Regex "regexp"))])
    (assert-invalid
     database
     "Expected number or lookup ref for entity id"
     [(db/tx-entity
       (entity-map
        [:db/id :profile]
        [(int-value -1)
         (Datascript_runtime.Data_value.Regex "regexp")]))])

    (assert-invalid
     database
     "Tempids are allowed in :db/add only"
     [(db/tx-retract
       (entity-id -1)
       :name
       (string-value "Ivan"))])))

(deftest test-unique
  (let [database
        (d/db-with
         (d/empty-db
          (zipmap
           [:name]
           [(schema-entry :db/unique :db.unique/value)]))
         [(db/tx-add (entity-id 1) :name (string-value "Ivan"))
          (db/tx-add (entity-id 2) :name (string-value "Petr"))])]
    (is
     (thrown-msg?
      "Cannot add #datascript/Datom [3 :name \"Ivan\" 536870914 true] because of unique constraint: (<value>)"
      (d/db-with
       database
       [(db/tx-add
         (entity-id 3)
         :name
         (string-value "Ivan"))])))
    (is
     (thrown-msg?
      "Cannot add #datascript/Datom [3 :name \"Petr\" 536870914 true] because of unique constraint: (<value>)"
      (d/db-with
       database
       [(db/tx-entity
         (entity-map
          [:db/id :name]
          [(int-value 3) (string-value "Petr")]))])))
    (d/db-with
     database
     [(db/tx-add (entity-id 3) :name (string-value "Igor"))])
    (d/db-with
     database
     [(db/tx-add (entity-id 3) :nick (string-value "Ivan"))])))
