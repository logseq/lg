(ns datascript.bench.datascript
  (:require
   [datascript.core :as d]
   [datascript.db :as db]
   [datascript.bench.bench :as bench]
   [datascript.lg.query :as query]
   [datascript.lg.query-types :as query-types]
   [datascript.parser :as parser]
   [datascript.impl.entity :as entity]
   [datascript.pull-api :as pull-api]
   [datascript.pull-parser :as pull-parser]
   [ocaml.Lg_runtime.Runtime_edn :as runtime-edn]))

(def ^:map<keyword;map<keyword;Datascript_runtime.Data_value.t>> schema
  (let [^:map<keyword;Datascript_runtime.Data_value.t> id-entry {}
        ^:map<keyword;Datascript_runtime.Data_value.t> follows-entry {}
        ^:map<keyword;Datascript_runtime.Data_value.t> alias-entry {}
        ^:map<keyword;map<keyword;Datascript_runtime.Data_value.t>> result {}]
    (assoc
     result
     :id
     (assoc
      id-entry
      :db/unique
      (Datascript_runtime.Data_value.Keyword ":db.unique/identity"))
     :follows
     (assoc
      follows-entry
      :db/valueType
      (Datascript_runtime.Data_value.Keyword ":db.type/ref")
      :db/cardinality
      (Datascript_runtime.Data_value.Keyword ":db.cardinality/many"))
     :alias
     (assoc
      alias-entry
      :db/cardinality
      (Datascript_runtime.Data_value.Keyword ":db.cardinality/many")))))

(def empty-db (d/empty-db schema))

(defn ^:vector<datascript.db/tx-entry> people-transactions
  [^:vector<map<keyword;Datascript_runtime.Data_value.t>> people]
  (mapv datascript.db/tx-entity people))

(defn ^:Datascript_runtime.Data_value.t person-value
  [^:map<keyword;Datascript_runtime.Data_value.t> person ^:keyword attr]
  (get person attr (Datascript_runtime.Data_value.Nil)))

(defn ^:Datascript_runtime.Data_value.entity_ref person-ref
  [^:map<keyword;Datascript_runtime.Data_value.t> person]
  (match
   (Datascript_runtime.Data_value.entity_ref_value
    (person-value person :db/id))
    (Some entity-ref) entity-ref
    None (Stdlib.invalid_arg "benchmark person has no entity reference")))

(defn ^:int person-id
  [^:map<keyword;Datascript_runtime.Data_value.t> person]
  (match (person-ref person)
    (Datascript_runtime.Data_value.Temp_id source)
    (Stdlib.int_of_string source)
    _ (Stdlib.invalid_arg "benchmark person id is not temporary")))

(def *db100k
  (delay
    (d/db-with
     (d/empty-db schema)
     (people-transactions (bench/people20k)))))

(defn wide-db [depth width]
  (d/db-with empty-db (bench/wide-db 1 depth width)))

(defn long-db [depth width]
  (d/db-with empty-db (bench/long-db depth width)))

(defn ^datascript.db/DB add-person-attributes
  [^datascript.db/DB database
   ^:map<keyword;Datascript_runtime.Data_value.t> person]
  (-> database
    (d/db-with [(datascript.db/tx-add
                 (person-ref person) :name
                 (person-value person :name))])
    (d/db-with [(datascript.db/tx-add
                 (person-ref person) :last-name
                 (person-value person :last-name))])
    (d/db-with [(datascript.db/tx-add
                 (person-ref person) :sex
                 (person-value person :sex))])
    (d/db-with [(datascript.db/tx-add
                 (person-ref person) :age
                 (person-value person :age))])
    (d/db-with [(datascript.db/tx-add
                 (person-ref person) :salary
                 (person-value person :salary))])))

(defn ^datascript.db/DB add-people-attributes
  [^datascript.db/DB database
   ^:vector<map<keyword;Datascript_runtime.Data_value.t>> people]
  (loop [index 0
         current database]
    (if (< index (count people))
      (recur
       (inc index)
       (add-person-attributes current (nth people index)))
      current)))

(defn ^datascript.bench.bench/benchmark-result bench-add-1 []
  (bench/bench
    (add-people-attributes empty-db (bench/people20k))))

(defn bench-add-5 []
  (bench/bench
    (reduce
      (fn [^datascript.db/DB database
           ^:map<keyword;Datascript_runtime.Data_value.t> person]
        (d/db-with database [(datascript.db/tx-entity person)]))
      empty-db
      (bench/people20k))))

(defn bench-add-all []
  (bench/bench
    (d/db-with
      empty-db
      (people-transactions (bench/people20k)))))

(defn ^:vector<datascript.db/Datom> people-datoms
  [^:vector<map<keyword;Datascript_runtime.Data_value.t>> people]
  (loop [index 0
         datoms []]
    (if (< index (count people))
      (let [person (nth people index)
            id (person-id person)]
        (recur
         (inc index)
         (conj
          datoms
          (d/datom id :name (person-value person :name))
          (d/datom id :last-name (person-value person :last-name))
          (d/datom id :full-name (person-value person :full-name))
          (d/datom id :alias (person-value person :alias))
          (d/datom id :sex (person-value person :sex))
          (d/datom id :age (person-value person :age))
          (d/datom id :salary (person-value person :salary)))))
      datoms)))

(defn bench-init []
  (let [datoms (people-datoms (bench/people20k))]
    (bench/bench
      (d/init-db datoms))))

(defn bench-find-datoms []
  (bench/bench
    (doseq [id (range 9000 11000)]
      (-> (d/datoms
           @*db100k
           :eavt
           (Datascript_runtime.Data_value.Ref_to
            (Datascript_runtime.Data_value.Entity_id id))
           (Datascript_runtime.Data_value.Keyword ":full-name"))
        first
        :v))))

(defn bench-find-datom []
  (bench/bench
    (doseq [id (range 9000 11000)]
      (-> (d/find-datom
           @*db100k
           :eavt
           (Datascript_runtime.Data_value.Ref_to
            (Datascript_runtime.Data_value.Entity_id id))
           (Datascript_runtime.Data_value.Keyword ":full-name"))
        :v))))

(defn ^:map<keyword;map<keyword;Datascript_runtime.Data_value.t>>
  component-schema []
  (let [^:map<keyword;Datascript_runtime.Data_value.t> component {}
        ^:map<keyword;Datascript_runtime.Data_value.t> reference {}
        ^:map<keyword;map<keyword;Datascript_runtime.Data_value.t>> result {}]
    (assoc
     result
     :profile
     (assoc
      component
      :db/valueType
      (Datascript_runtime.Data_value.Keyword ":db.type/ref")
      :db/isComponent
      (Datascript_runtime.Data_value.Bool true))
     :friend
     (assoc
      reference
      :db/valueType
      (Datascript_runtime.Data_value.Keyword ":db.type/ref")))))

(defn ^:Datascript_runtime.Data_value.entity_ref entity-id-ref [^:int eid]
  (Datascript_runtime.Data_value.Entity_id eid))

(defn ^:Datascript_runtime.Data_value.t entity-ref-value [^:int eid]
  (Datascript_runtime.Data_value.Ref_to (entity-id-ref eid)))

(defn ^:int entity-datom-count
  [^datascript.db/DB database ^:int eid]
  (count (d/datoms database :eavt (entity-ref-value eid))))

(defn assert-component-retractions []
  (let [database
        (d/db-with
         (d/empty-db (component-schema))
         [(datascript.db/tx-add
           (entity-id-ref 1) :profile (entity-ref-value 2))
          (datascript.db/tx-add
           (entity-id-ref 2) :name
           (Datascript_runtime.Data_value.String "child"))
          (datascript.db/tx-add
           (entity-id-ref 3) :friend (entity-ref-value 1))])
        retracted-entity
        (d/db-with database [(datascript.db/tx-retract-entity-id 1)])
        retracted-attribute
        (d/db-with
         database
         [(datascript.db/tx-retract-attribute
           (entity-id-ref 1) :profile)])]
    (when-not (= 0 (entity-datom-count retracted-entity 1))
      (Stdlib.failwith
       "retractEntity left datoms on the retracted entity"))
    (when-not (= 0 (entity-datom-count retracted-entity 2))
      (Stdlib.failwith
       "retractEntity did not cascade through a component"))
    (when-not (= 0 (entity-datom-count retracted-entity 3))
      (Stdlib.failwith
       "retractEntity did not remove incoming references"))
    (when-not (= 0 (entity-datom-count retracted-attribute 2))
      (Stdlib.failwith
       "retractAttribute did not cascade through a component"))))

(defn assert-multival-cas []
  (let [entity (entity-id-ref 1)
        database
        (d/db-with
         (d/empty-db schema)
         [(datascript.db/tx-add
           entity :alias
           (Datascript_runtime.Data_value.String "first"))
          (datascript.db/tx-add
           entity :alias
           (Datascript_runtime.Data_value.String "second"))])
        database
        (d/db-with
         database
         [(datascript.db/tx-cas
           entity :alias
           (Some (Datascript_runtime.Data_value.String "second"))
           (Datascript_runtime.Data_value.String "third"))])]
    (when-not
        (= 3
           (count
            (d/datoms
             database :eavt (entity-ref-value 1)
             (Datascript_runtime.Data_value.Keyword ":alias"))))
      (Stdlib.failwith
       "Multival CAS did not match upstream behavior"))))

(defn transaction-rejected?
  [^:fn<unit> transaction]
  (try
    (transaction)
    false
    (catch _
      true)))

(defn assert-conflicting-upsert-rejected []
  (let [database
        (d/db-with
         (d/empty-db schema)
        [(datascript.db/tx-add
           (entity-id-ref 1) :id
           (Datascript_runtime.Data_value.String "first"))
          (datascript.db/tx-add
           (entity-id-ref 2) :id
           (Datascript_runtime.Data_value.String "second"))])
        ^:map<keyword;Datascript_runtime.Data_value.t> entity
        (let [^:map<keyword;Datascript_runtime.Data_value.t> result {}]
          (assoc
           result
           :db/id (entity-ref-value 1)
           :id (Datascript_runtime.Data_value.String "second")))
        ^:map<keyword;Datascript_runtime.Data_value.t> upsert {}
        upsert
        (assoc
         upsert
         :id (Datascript_runtime.Data_value.String "first")
         :name (Datascript_runtime.Data_value.String "updated"))
        upserted
        (d/db-with database [(datascript.db/tx-entity upsert)])
        vector-tempid (Datascript_runtime.Data_value.Temp_id "existing")
        vector-report
        (d/with
         database
         [(datascript.db/tx-add
           vector-tempid :id
           (Datascript_runtime.Data_value.String "first"))])
        vector-tempid-key
        (Datascript_runtime.Data_value.Ref_to vector-tempid)]
    (when-not
        (transaction-rejected?
         (fn []
           (Stdlib.ignore
            (d/db-with
             database
             [(datascript.db/tx-entity entity)]))))
      (Stdlib.failwith
       "Conflicting unique identity upsert was not rejected"))
    (when-not
        (transaction-rejected?
         (fn []
           (Stdlib.ignore
            (d/db-with
             database
             [(datascript.db/tx-add
               (entity-id-ref 1) :id
               (Datascript_runtime.Data_value.String "second"))]))))
      (Stdlib.failwith
       "Explicit entity ID silently upserted to a different entity"))
    (when-not (= 2 (entity-datom-count upserted 1))
      (Stdlib.failwith
       "Unique identity upsert did not target the existing entity"))
    (if-some [resolved
              (get (:tempids vector-report) vector-tempid-key)]
      (when-not (= 1 resolved)
        (Stdlib.failwith
         "Vector unique identity upsert bound its tempid incorrectly"))
      (Stdlib.failwith
       "Vector unique identity upsert did not bind its tempid"))
    (when-not (= 2 (.-max-eid (:db-after vector-report)))
      (Stdlib.failwith
       "Vector unique identity upsert allocated a phantom entity"))))

(defn assert-upsert-retry-merges-entities []
  (let [database
        (d/db-with
         (d/empty-db schema)
         [(datascript.db/tx-add
           (entity-id-ref -1) :age
           (Datascript_runtime.Data_value.Int 42))
          (datascript.db/tx-add
           (entity-id-ref -2) :likes
           (Datascript_runtime.Data_value.String "Pizza"))
          (datascript.db/tx-add
           (entity-id-ref -1) :id
           (Datascript_runtime.Data_value.String "Bob"))
          (datascript.db/tx-add
           (entity-id-ref -2) :id
           (Datascript_runtime.Data_value.String "Bob"))])
        matches
        (d/datoms
         database :avet
         (Datascript_runtime.Data_value.Keyword ":id")
         (Datascript_runtime.Data_value.String "Bob"))]
    (when-not (= 1 (count matches))
      (Stdlib.failwith
       "Unique identity retry left duplicate entities"))
    (if-some [identity (first matches)]
      (when-not (= 3
                   (entity-datom-count database (.-e identity)))
        (Stdlib.failwith
         "Unique identity retry lost earlier transaction attributes"))
      (Stdlib.failwith
       "Unique identity retry lost the identity datom")))
  (let [alice (Datascript_runtime.Data_value.Temp_id "alice")
        bob (Datascript_runtime.Data_value.Temp_id "bob")
        database
        (d/db-with
         (d/empty-db schema)
         [(datascript.db/tx-add
           (entity-id-ref 1) :id
           (Datascript_runtime.Data_value.String "Alice"))
          (datascript.db/tx-add
           (entity-id-ref 2) :id
           (Datascript_runtime.Data_value.String "Bob"))])
        database
        (d/db-with
         database
         [(datascript.db/tx-add
           (entity-id-ref 3) :follows
           (Datascript_runtime.Data_value.Ref_to alice))
          (datascript.db/tx-add
           (entity-id-ref 4) :follows
           (Datascript_runtime.Data_value.Ref_to bob))
          (datascript.db/tx-add
           alice :id
           (Datascript_runtime.Data_value.String "Alice"))
          (datascript.db/tx-add
           bob :id
           (Datascript_runtime.Data_value.String "Bob"))])]
    (when-not
        (= 1
           (count
            (d/datoms
             database :eavt
             (entity-ref-value 3)
             (Datascript_runtime.Data_value.Keyword ":follows")
             (entity-ref-value 1))))
      (Stdlib.failwith
       "First unique identity retry did not preserve a reference"))
    (when-not
        (= 1
           (count
            (d/datoms
             database :eavt
             (entity-ref-value 4)
             (Datascript_runtime.Data_value.Keyword ":follows")
             (entity-ref-value 2))))
      (Stdlib.failwith
       "Second unique identity retry did not preserve a reference"))))

(defn assert-ref-identity-upsert-resolves-tempids []
  (let [^:map<keyword;Datascript_runtime.Data_value.t> owner-entry {}
        owner-entry
        (assoc
         owner-entry
         :db/valueType
         (Datascript_runtime.Data_value.Keyword ":db.type/ref")
         :db/unique
         (Datascript_runtime.Data_value.Keyword ":db.unique/identity")
         :db/cardinality
         (Datascript_runtime.Data_value.Keyword ":db.cardinality/one"))
        ref-schema (assoc schema :owner owner-entry)
        database
        (d/db-with
         (d/empty-db ref-schema)
         [(datascript.db/tx-add
           (entity-id-ref 1) :id
           (Datascript_runtime.Data_value.String "Target"))
          (datascript.db/tx-add
           (entity-id-ref 2) :owner
           (entity-ref-value 1))])
        target-tempid
        (Datascript_runtime.Data_value.Temp_id "target")
        claim-tempid
        (Datascript_runtime.Data_value.Temp_id "claim")
        report
        (d/with
         database
         [(datascript.db/tx-add
           target-tempid :id
           (Datascript_runtime.Data_value.String "Target"))
          (datascript.db/tx-add
           claim-tempid :owner
           (Datascript_runtime.Data_value.Ref_to target-tempid))])
        claim-key
        (Datascript_runtime.Data_value.Ref_to claim-tempid)]
    (if-some [resolved (get (:tempids report) claim-key)]
      (when-not (= 2 resolved)
        (Stdlib.failwith
         "Reference identity upsert bound its tempid incorrectly"))
      (Stdlib.failwith
       "Reference identity upsert did not bind its tempid"))
    (when-not (= 2 (.-max-eid (:db-after report)))
      (Stdlib.failwith
       "Reference identity upsert allocated a phantom entity"))))

(defn assert-nested-entity-map []
  (let [^:map<keyword;Datascript_runtime.Data_value.t> entity {}
        entity
        (assoc
         entity
         :db/id (entity-ref-value 5)
         :profile
         (Datascript_runtime.Data_value.Map
          (list
           (tuple
            (Datascript_runtime.Data_value.Keyword ":name")
            (Datascript_runtime.Data_value.String "nested")))))
        database
        (d/db-with
         (d/empty-db (component-schema))
         [(datascript.db/tx-entity entity)])]
    (match
     (first
      (d/datoms
       database :eavt (entity-ref-value 5)
       (Datascript_runtime.Data_value.Keyword ":profile")))
     None
     (Stdlib.failwith
      "Nested entity map did not create the parent reference")
     (Some datom)
     (if-some [child-eid
               (Datascript_runtime.Data_value.ref_value (.-v datom))]
       (let [child-datom-count
             (entity-datom-count database child-eid)]
         (when-not (= 1 child-datom-count)
           (Stdlib.failwith
            (str
             "Nested entity map did not transact the child entity: eid="
             child-eid
             ", datoms="
             child-datom-count))))
       (Stdlib.failwith
        "Nested entity map parent value is not a resolved ref")))))

(defn assert-schema-transaction-validation []
  (let [^:map<keyword;Datascript_runtime.Data_value.t> incomplete {}
        incomplete
        (assoc
         incomplete
         :db/id (entity-ref-value 1)
         :db/valueType
         (Datascript_runtime.Data_value.Keyword ":db.type/string"))
        ^:map<keyword;Datascript_runtime.Data_value.t> reserved {}
        reserved
        (assoc
         reserved
         :db/id (entity-ref-value 1)
         :db/ident
         (Datascript_runtime.Data_value.Keyword ":db/custom")
         :db/cardinality
         (Datascript_runtime.Data_value.Keyword ":db.cardinality/one"))]
    (when-not
        (transaction-rejected?
         (fn []
           (Stdlib.ignore
            (d/db-with
             (d/empty-db)
             [(datascript.db/tx-entity incomplete)]))))
      (Stdlib.failwith
       "Incomplete schema transaction was not rejected"))
    (when-not
        (transaction-rejected?
         (fn []
           (Stdlib.ignore
            (d/db-with
             (d/empty-db)
             [(datascript.db/tx-entity reserved)]))))
      (Stdlib.failwith
       "Reserved db namespace was accepted for a schema ident"))))

(defn tuple-schema []
  (let [^:map<keyword;Datascript_runtime.Data_value.t> tuple-entry {}
        ^:map<keyword;map<keyword;Datascript_runtime.Data_value.t>> result {}]
    (assoc
     result
     :full-name
     (assoc
      tuple-entry
      :db/valueType
      (Datascript_runtime.Data_value.Keyword ":db.type/tuple")
      :db/tupleAttrs
      (Datascript_runtime.Data_value.Vector
       (list
        (Datascript_runtime.Data_value.Keyword ":first-name")
        (Datascript_runtime.Data_value.Keyword ":last-name")))))))

(defn assert-tuple-transaction-guard []
  (let [entity (entity-id-ref 1)
        expected
        (Datascript_runtime.Data_value.Tuple
         (list
          (Some (Datascript_runtime.Data_value.String "Ada"))
          (Some (Datascript_runtime.Data_value.String "Lovelace"))))
        invalid
        (Datascript_runtime.Data_value.Tuple
         (list
          (Some (Datascript_runtime.Data_value.String "Grace"))
          (Some (Datascript_runtime.Data_value.String "Hopper"))))
        database
        (d/db-with
         (d/empty-db (tuple-schema))
         [(datascript.db/tx-add
           entity :first-name
           (Datascript_runtime.Data_value.String "Ada"))
          (datascript.db/tx-add
           entity :last-name
           (Datascript_runtime.Data_value.String "Lovelace"))])]
    (when-not
        (= 1
           (count
            (d/datoms
             database :eavt
             (entity-ref-value 1)
             (Datascript_runtime.Data_value.Keyword ":full-name")
             expected)))
      (Stdlib.failwith
       "Tuple source updates did not maintain the derived tuple"))
    (when-not
        (transaction-rejected?
         (fn []
           (Stdlib.ignore
            (d/db-with
             database
             [(datascript.db/tx-add
               entity :full-name invalid)]))))
      (Stdlib.failwith
       "Direct tuple modification was not rejected"))
    (Stdlib.ignore
     (d/db-with
     database
      [(datascript.db/tx-add entity :full-name expected)]))))

(defn assert-value-tempid-validation []
  (let [value-tempid (entity-id-ref -1)
        reference
        (Datascript_runtime.Data_value.Ref_to value-tempid)]
    (when-not
        (transaction-rejected?
         (fn []
           (Stdlib.ignore
            (d/db-with
             (d/empty-db schema)
             [(datascript.db/tx-retract-attribute
               value-tempid :name)]))))
      (Stdlib.failwith
       "Tempid was accepted in a non-add operation"))
    (when-not
        (transaction-rejected?
         (fn []
           (Stdlib.ignore
            (d/db-with
             (d/empty-db schema)
             [(datascript.db/tx-add
               (entity-id-ref 10) :follows reference)]))))
      (Stdlib.failwith
       "Tempid used only as a reference value was accepted"))
    (let [database
          (d/db-with
           (d/empty-db schema)
           [(datascript.db/tx-add
             (entity-id-ref 10) :follows reference)
            (datascript.db/tx-add
             value-tempid :name
             (Datascript_runtime.Data_value.String "target"))])]
      (when-not (= 1
                   (count
                    (d/datoms
                     database :eavt
                     (entity-ref-value 10)
                     (Datascript_runtime.Data_value.Keyword ":follows"))))
        (Stdlib.failwith
         "Used reference tempid did not resolve normally")))))

(defn assert-db-equality-compares-datoms []
  (let [first-datom
        (datascript.db/datom
         1 :name
         (Datascript_runtime.Data_value.String "same")
         100 true)
        same-datom
        (datascript.db/datom
         1 :name
         (Datascript_runtime.Data_value.String "same")
         200 false)
        left
        (d/db-with
         (d/empty-db schema)
         [(datascript.db/tx-add
           (entity-id-ref 1) :name
           (Datascript_runtime.Data_value.String "left"))])
        same
        (d/db-with
         (d/empty-db schema)
         [(datascript.db/tx-add
           (entity-id-ref 1) :name
           (Datascript_runtime.Data_value.String "left"))])
        replayed
        (d/init-db
         [(d/datom
           1 :name
           (Datascript_runtime.Data_value.String "left")
           77)]
         schema)
        different
        (d/db-with
         (d/empty-db schema)
         [(datascript.db/tx-add
           (entity-id-ref 1) :name
           (Datascript_runtime.Data_value.String "right"))])
        keep-all
        (fn [^datascript.db/DB _database
             ^datascript.db/Datom _datom]
          true)
        left-view (d/filter left keep-all)
        replayed-view (d/filter replayed keep-all)]
    (when-not (= first-datom same-datom)
      (Stdlib.failwith
       "DataScript datom equality included transaction metadata"))
    (when-not (= (hash first-datom) (hash same-datom))
      (Stdlib.failwith
       "Equivalent DataScript datoms produced different hashes"))
    (when-not (datascript.db/db-equal? left same)
      (Stdlib.failwith "Equal DataScript databases compare unequal"))
    (when-not (= left replayed)
      (Stdlib.failwith
       "DataScript equality did not use upstream datom equivalence"))
    (when-not (= (hash left) (hash replayed))
      (Stdlib.failwith
       "Equal DataScript databases produced different hashes"))
    (when-not (= left-view replayed-view)
      (Stdlib.failwith
       "Equivalent filtered databases compare unequal"))
    (when-not (= (hash left-view) (hash replayed-view))
      (Stdlib.failwith
       "Equivalent filtered databases produced different hashes"))
    (when-not (= 1 (count left-view))
      (Stdlib.failwith
       "Filtered database count ignored visible datoms"))
    (when (datascript.db/db-equal? left different)
      (Stdlib.failwith
       "DataScript database equality ignored datom contents"))))

(defn bench-retract-5 []
  (assert-component-retractions)
  (assert-multival-cas)
  (assert-conflicting-upsert-rejected)
  (assert-upsert-retry-merges-entities)
  (assert-ref-identity-upsert-resolves-tempids)
  (assert-nested-entity-map)
  (assert-schema-transaction-validation)
  (assert-tuple-transaction-guard)
  (assert-value-tempid-validation)
  (assert-db-equality-compares-datoms)
  (let [db   (d/db-with empty-db
                        (people-transactions (bench/people20k)))
        eids (->> (d/datoms
                   db :aevt
                   (Datascript_runtime.Data_value.Keyword ":name"))
                  (mapv
                   (fn [^datascript.db/Datom datom]
                     (.-e datom)))
                  (shuffle))]
    (bench/bench
      (reduce
       (fn [^datascript.db/DB database ^:int entity]
         (d/db-with
          database
          [(datascript.db/tx-retract-entity-id entity)]))
       db
       eids))))

(defn query-inputs [^datascript.db/DB database]
  [(query-types/source-input
    (query-types/database-source
     (db/database-view database)))])

(defn entity-pattern [^:keyword attr ^:string value-variable]
  [(parser/pattern-variable "?e")
   (parser/pattern-attribute attr)
   (parser/pattern-variable value-variable)])

(def name-is-ivan-pattern
  [(parser/pattern-variable "?e")
   (parser/pattern-attribute :name)
   (parser/pattern-constant
    (Datascript_runtime.Data_value.String "Ivan"))])

(def sex-is-male-pattern
  [(parser/pattern-variable "?e")
   (parser/pattern-attribute :sex)
   (parser/pattern-constant
    (Datascript_runtime.Data_value.Keyword ":male"))])

(def q1
  (parser/static-db-query
   (parser/relation-find ["?e"])
   [name-is-ivan-pattern]))

(def q2
  (parser/static-db-query
   (parser/relation-find ["?e" "?a"])
   [name-is-ivan-pattern
    (entity-pattern :age "?a")]))

(def q3
  (parser/static-db-query
   (parser/relation-find ["?e" "?a"])
   [name-is-ivan-pattern
    (entity-pattern :age "?a")
    sex-is-male-pattern]))

(def q4
  (parser/static-db-query
   (parser/relation-find ["?e" "?l" "?a"])
   [name-is-ivan-pattern
    (entity-pattern :last-name "?l")
    (entity-pattern :age "?a")
    sex-is-male-pattern]))

(def q5-shortcircuit
  (parser/static-db-query-with-scalars
   (parser/relation-find ["?e" "?n" "?l" "?a" "?s" "?al"])
   [(entity-pattern :name "?n")
    (entity-pattern :age "?a")
    (entity-pattern :last-name "?l")
    (entity-pattern :sex "?s")
    (entity-pattern :alias "?al")]
   ["?n" "?a"]))

(def qpred1
  (parser/static-db-query-clauses
   (parser/relation-find ["?e" "?s"])
   [(parser/pattern-clause (entity-pattern :salary "?s"))
    (parser/greater-than-clause
     (parser/variable-argument "?s")
     (parser/constant-argument
      (Datascript_runtime.Data_value.Int 50000)))]))

(def qpred2
  (parser/static-db-query-clauses-with-scalars
   (parser/relation-find ["?e" "?s"])
   [(parser/pattern-clause (entity-pattern :salary "?s"))
    (parser/greater-than-clause
     (parser/variable-argument "?s")
     (parser/variable-argument "?min_s"))]
   ["?min_s"]))

(defn ^:int expected-query-count [^boolean male-only?]
  (reduce
   (fn [^:int total
        ^:map<keyword;Datascript_runtime.Data_value.t> person]
     (let [name-matches?
           (= (person-value person :name)
              (Datascript_runtime.Data_value.String "Ivan"))
           sex-matches?
           (= (person-value person :sex)
              (Datascript_runtime.Data_value.Keyword ":male"))]
       (if (and name-matches?
                (or (not male-only?) sex-matches?))
         (inc total)
         total)))
   0
   (bench/people20k)))

(defn assert-query-count
  [^datascript.lg.query-types/output output
   ^:int expected
   ^:string benchmark-name]
  (if-some [rows (query-types/output-relation output)]
    (when-not (= expected (count rows))
      (Stdlib.failwith
       (str benchmark-name " returned " (count rows)
            " rows; expected " expected)))
    (Stdlib.failwith
     (str benchmark-name " did not return a relation"))))

(defn bench-static-query [query ^:int expected ^:string benchmark-name]
  (let [inputs (query-inputs @*db100k)]
    (assert-query-count
     (query/q-closed query inputs)
     expected
     benchmark-name)
    (bench/bench
     (query/q-closed query inputs))))

(defn bench-q1 []
  (bench-static-query q1 (expected-query-count false) "q1"))

(defn bench-q2 []
  (bench-static-query q2 (expected-query-count false) "q2"))

(defn bench-q3 []
  (bench-static-query q3 (expected-query-count true) "q3"))

(defn bench-q4 []
  (bench-static-query q4 (expected-query-count true) "q4"))

(defn bench-q5-shortcircuit []
  (let [inputs
        [(query-types/source-input
          (query-types/database-source
           (db/database-view @*db100k)))
         (query-types/binding-input
          (query-types/scalar-binding
           (query-types/value-result
            (Datascript_runtime.Data_value.String "Anastasia"))))
         (query-types/binding-input
          (query-types/scalar-binding
           (query-types/value-result
            (Datascript_runtime.Data_value.Int 35))))]]
    (bench/bench
     (query/q-closed q5-shortcircuit inputs))))

(defn ^:int expected-high-salary-count []
  (reduce
   (fn [^:int total
        ^:map<keyword;Datascript_runtime.Data_value.t> person]
     (match (person-value person :salary)
       (Datascript_runtime.Data_value.Int salary)
       (if (> salary 50000) (inc total) total)
       _ total))
   0
   (bench/people20k)))

(defn ^:int relation-output-count
  [^datascript.lg.query-types/output output]
  (if-some [rows (query-types/output-relation output)]
    (count rows)
    (Stdlib.invalid_arg "Predicate benchmark expected relation output")))

(defn assert-predicate-output
  [^datascript.lg.query-types/output output]
  (let [expected (expected-high-salary-count)
        actual (relation-output-count output)]
    (when-not (= expected actual)
      (Stdlib.failwith
       (str
        "Predicate query returned " actual
        " rows; expected " expected)))))

(defn qpred2-inputs []
  [(query-types/source-input
    (query-types/database-source
     (db/database-view @*db100k)))
   (query-types/binding-input
    (query-types/scalar-binding
     (query-types/value-result
      (Datascript_runtime.Data_value.Int 50000))))])

(defn bench-qpred1 []
  (let [inputs (query-inputs @*db100k)]
    (assert-predicate-output (query/q-closed qpred1 inputs))
    (bench/bench
     (query/q-closed qpred1 inputs))))

(defn bench-qpred2 []
  (let [inputs (qpred2-inputs)]
    (assert-predicate-output (query/q-closed qpred2 inputs))
    (bench/bench
     (query/q-closed qpred2 inputs))))

(def *pull-db
  (delay
   (wide-db 4 5)))

(defn benchmark-entity-ref []
  (Datascript_runtime.Data_value.Entity_id 1))

(type-variant pull-one-entity-tree
  (PullOneEntityTree
   :option<datascript.impl.entity/EntityValue>
   :vector<pull-one-entity-tree>))

(type-variant pull-many-entity-tree
  (PullManyEntityTree
   :option<datascript.impl.entity/EntityValue>
   :option<datascript.impl.entity/EntityValue>
   :option<datascript.impl.entity/EntityValue>
   :option<datascript.impl.entity/EntityValue>
   :option<datascript.impl.entity/EntityValue>
   :option<datascript.impl.entity/EntityValue>
   :vector<pull-many-entity-tree>))

(defn entity-reference-children
  [source]
  (match (entity/lookup-entity source :follows)
    (Some (entity/EntityReferences targets))
    (reduce
     (fn [children target]
       (match target
         (Some child) (conj children child)
         None children))
     []
     (vec targets))
    _ []))

(defn pull-one-entity-tree
  [source]
  (PullOneEntityTree
   (entity/lookup-entity source :name)
   (mapv pull-one-entity-tree (entity-reference-children source))))

(defn pull-many-entity-tree
  [source]
  (PullManyEntityTree
   (entity/lookup-entity source :db/id)
   (entity/lookup-entity source :last-name)
   (entity/lookup-entity source :alias)
   (entity/lookup-entity source :sex)
   (entity/lookup-entity source :age)
   (entity/lookup-entity source :salary)
   (mapv pull-many-entity-tree (entity-reference-children source))))

(defn pull-benchmark-root-entity []
  (match
   (entity/entity
    (db/database-view @*pull-db)
    (benchmark-entity-ref))
    (Some source) source
    None
    (Stdlib.failwith
     "Entity benchmark did not find the root entity")))

(defn bench-pull-one-entities []
  (bench/bench
   (pull-one-entity-tree
    (pull-benchmark-root-entity))))

(defn pull-one-pattern [^datascript.db/database-view database]
  (pull-parser/recursive-pattern
   database
   [:name]
   :follows
   false))

(defn pull-many-pattern [^datascript.db/database-view database]
  (pull-parser/recursive-pattern
   database
   [:db/id :last-name :alias :sex :age :salary]
   :follows
   false))

(defn pull-wildcard-pattern [^datascript.db/database-view database]
  (pull-parser/recursive-pattern
   database
   []
   :follows
   true))

(defn assert-pull-output
  [^:option<map<Datascript_runtime.Data_value.t;Datascript_runtime.Data_value.t>>
   output]
  (match output
    None
    (Stdlib.failwith
     "Pull benchmark did not return the root entity")
    (Some values)
    (if-some
      [follows
       (get
        values
        (Datascript_runtime.Data_value.Keyword ":follows"))]
      (match
       (Datascript_runtime.Data_value.sequential_items follows)
       None
       (Stdlib.failwith
        "Pull benchmark :follows is not sequential")
       (Some items)
       (when (empty? items)
         (Stdlib.failwith
          "Pull benchmark :follows is empty")))
      (Stdlib.failwith
       "Pull benchmark did not expand :follows"))))

(defn ^:Datascript_runtime.Data_value.t first-pulled-item
  [^:Datascript_runtime.Data_value.t value]
  (match
   (Datascript_runtime.Data_value.sequential_items value)
   None
   (Stdlib.failwith
    "Expected a sequential pull value")
   (Some items)
   (match (first items)
     None
     (Stdlib.failwith
      "Expected a non-empty pull value")
     (Some item) item)))

(defn ^:Datascript_runtime.Data_value.t pulled-field
  [^:Datascript_runtime.Data_value.t entity ^:keyword field]
  (match
   (Datascript_runtime.Data_value.keyword_map_get
    (str field)
    entity)
   None
   (Stdlib.failwith
   (str "Missing nested pull field " field))
   (Some value) value))

(defn ^:Datascript_runtime.Data_value.t root-pulled-field
  [^:map<Datascript_runtime.Data_value.t;Datascript_runtime.Data_value.t>
   entity
   ^:keyword field]
  (if-some
    [value
     (get
      entity
      (Datascript_runtime.Data_value.Keyword (str field)))]
    value
    (Stdlib.failwith
     (str "Missing root pull field " field))))

(defn cycle-pull-db []
  (d/init-db
   [(d/datom
     1 :name
     (Datascript_runtime.Data_value.String "one"))
    (d/datom
     1 :follows
     (Datascript_runtime.Data_value.Ref 2))
    (d/datom
     2 :name
     (Datascript_runtime.Data_value.String "two"))
    (d/datom
     2 :follows
     (Datascript_runtime.Data_value.Ref 1))]
   schema))

(defn assert-cycle-pull []
  (let [database (cycle-pull-db)
        view (db/database-view database)
        pattern
        (pull-parser/recursive-pattern
         view
         [:db/id :name]
         :follows
         false)]
    (match
     (pull-api/pull-parsed
      view pattern
      (Datascript_runtime.Data_value.Entity_id 1))
     None
     (Stdlib.failwith
      "Cycle pull did not return the root entity")
     (Some root)
     (let [child
           (first-pulled-item
            (root-pulled-field root :follows))
           parent
           (first-pulled-item
            (pulled-field child :follows))
           cycle-stub
           (first-pulled-item
            (pulled-field parent :follows))
           cycle-id
           (pulled-field cycle-stub :db/id)]
       (when-not
           (= cycle-id
              (Datascript_runtime.Data_value.Int 2))
         (Stdlib.failwith
          "Recursive pull produced the wrong cycle stub"))
       (match
        (Datascript_runtime.Data_value.keyword_map_get
         ":name" cycle-stub)
        None (Stdlib.ignore 0)
        (Some _)
        (Stdlib.failwith
         "Recursive pull cycle stub expanded extra fields"))))))

(defn assert-reverse-pull []
  (let [database (cycle-pull-db)
        view (db/database-view database)
        reverse-attr
        (pull-parser/attribute view :_follows)
        pattern
        (pull-parser/pattern [reverse-attr reverse-attr] false)]
    (when-not (= 1 (count (:reverse-attrs pattern)))
      (Stdlib.failwith
       "Pull pattern did not replace a duplicate alias"))
    (match
     (pull-api/pull-parsed
      view pattern
      (Datascript_runtime.Data_value.Entity_id 2))
     None
     (Stdlib.failwith
      "Reverse pull did not return the root entity")
     (Some root)
     (let [source
           (first-pulled-item
            (root-pulled-field root :_follows))
           source-id
           (pulled-field source :db/id)]
       (when-not
           (= source-id
              (Datascript_runtime.Data_value.Int 1))
         (Stdlib.failwith
          "Reverse pull returned the wrong source entity"))))))

(defn assert-nested-reverse-pull []
  (let [database (cycle-pull-db)
        view (db/database-view database)
        reverse-attr
        (pull-parser/attribute view :_follows)
        child-pattern
        (pull-parser/pattern
         [(pull-parser/attribute view :db/id)
          reverse-attr]
         false)
        follows-attr
        (pull-parser/with-pattern
         (pull-parser/attribute view :follows)
         child-pattern)
        pattern
        (pull-parser/pattern [follows-attr] false)]
    (match
     (pull-api/pull-parsed
      view pattern
      (Datascript_runtime.Data_value.Entity_id 1))
     None
     (Stdlib.failwith
      "Nested reverse pull did not return the root entity")
     (Some root)
     (let [child
           (first-pulled-item
            (root-pulled-field root :follows))
           source
           (first-pulled-item
            (pulled-field child :_follows))
           source-id
           (pulled-field source :db/id)]
       (when-not
           (= source-id
              (Datascript_runtime.Data_value.Int 1))
         (Stdlib.failwith
          "Nested reverse pull lost its reverse pattern"))))))

(defn assert-pull-options []
  (let [database (cycle-pull-db)
        view (db/database-view database)
        default-attr
        (pull-parser/with-default
         (pull-parser/attribute view :missing)
         (Datascript_runtime.Data_value.String "default"))
        present-xform
        (pull-parser/with-xform
         (pull-parser/attribute view :name)
         (fn
           [^:option<Datascript_runtime.Data_value.t> _value]
           (Some
            (Datascript_runtime.Data_value.String "present"))))
        missing-xform
        (pull-parser/with-xform
         (pull-parser/attribute view :missing-xform)
         (fn
           [^:option<Datascript_runtime.Data_value.t> value]
           (match value
             None
             (Some
              (Datascript_runtime.Data_value.String "missing"))
             (Some value) (Some value))))
        pattern
        (pull-parser/pattern
         [default-attr present-xform missing-xform]
         false)]
    (match
     (pull-api/pull-parsed
      view pattern
      (Datascript_runtime.Data_value.Entity_id 1))
     None
     (Stdlib.failwith
      "Pull options did not return the root entity")
     (Some root)
     (do
       (when-not
           (= (root-pulled-field root :missing)
              (Datascript_runtime.Data_value.String "default"))
         (Stdlib.failwith
          "Pull default did not match upstream"))
       (when-not
           (= (root-pulled-field root :name)
              (Datascript_runtime.Data_value.String "present"))
         (Stdlib.failwith
          "Pull present xform did not match upstream"))
       (when-not
           (= (root-pulled-field root :missing-xform)
              (Datascript_runtime.Data_value.String "missing"))
         (Stdlib.failwith
          "Pull missing xform did not match upstream")))))
  (let [database (cycle-pull-db)
        view (db/database-view database)
        recursive-attr
        (pull-parser/recursive-attribute-with-limit
         view :follows 1)
        pattern
        (pull-parser/pattern
         [(pull-parser/attribute view :name)
          recursive-attr]
         false)]
    (match
     (pull-api/pull-parsed
      view pattern
      (Datascript_runtime.Data_value.Entity_id 1))
     None
     (Stdlib.failwith
      "Limited recursive pull did not return the root entity")
     (Some root)
     (let [child
           (first-pulled-item
            (root-pulled-field root :follows))]
       (match
        (Datascript_runtime.Data_value.keyword_map_get
         ":follows" child)
        None (Stdlib.ignore 0)
        (Some _)
        (Stdlib.failwith
         "Recursive pull exceeded its upstream limit")))))
  (let [database @*pull-db
        view (db/database-view database)
        follows-attr
        (pull-parser/with-limit
         (pull-parser/attribute view :follows)
         (Some 2))
        pattern (pull-parser/pattern [follows-attr] false)]
    (match
     (pull-api/pull-parsed view pattern (benchmark-entity-ref))
     None
     (Stdlib.failwith
      "Limited multival pull did not return the root entity")
     (Some root)
     (match
      (Datascript_runtime.Data_value.sequential_items
       (root-pulled-field root :follows))
      None
      (Stdlib.failwith
       "Limited multival pull did not return a vector")
      (Some values)
      (when-not (= 2 (count values))
        (Stdlib.failwith
         "Multival pull exceeded its upstream limit"))))))

(defn bench-pull-one []
  (let [database @*pull-db
        view (db/database-view database)
        pattern (pull-one-pattern view)
        entity-ref (benchmark-entity-ref)
        output (pull-api/pull-parsed view pattern entity-ref)]
    (assert-pull-output output)
    (assert-cycle-pull)
    (assert-reverse-pull)
    (assert-nested-reverse-pull)
    (assert-pull-options)
    (bench/bench
     (pull-api/pull-parsed view pattern entity-ref))))

(defn bench-pull-many-entities []
  (bench/bench
   (pull-many-entity-tree
    (pull-benchmark-root-entity))))

(defn bench-pull-many []
  (let [database @*pull-db
        view (db/database-view database)
        pattern (pull-many-pattern view)
        entity-ref (benchmark-entity-ref)
        output (pull-api/pull-parsed view pattern entity-ref)]
    (assert-pull-output output)
    (bench/bench
     (pull-api/pull-parsed view pattern entity-ref))))

(defn bench-pull-wildcard []
  (let [database @*pull-db
        view (db/database-view database)
        pattern (pull-wildcard-pattern view)
        entity-ref (benchmark-entity-ref)
        output (pull-api/pull-parsed view pattern entity-ref)]
    (assert-pull-output output)
    (bench/bench
     (pull-api/pull-parsed view pattern entity-ref))))

(def follows-rule-query
  (parser/static-query-clauses-with-inputs
   (parser/relation-find ["?entity" "?target"])
   [(parser/static-rule-clause
     "follows"
     [(parser/pattern-variable "?entity")
      (parser/pattern-variable "?target")])]
   [(parser/make-static-source-input "$")
    (parser/make-static-rules-input)]))

(def follows-rules
  (let [direct
        (parser/pattern-clause
         [(parser/pattern-variable "?source")
          (parser/pattern-attribute :follows)
          (parser/pattern-variable "?target")])
        first-step
        (parser/pattern-clause
         [(parser/pattern-variable "?source")
          (parser/pattern-attribute :follows)
          (parser/pattern-variable "?intermediate")])
        recursive-step
        (parser/static-rule-clause
         "follows"
         [(parser/pattern-variable "?intermediate")
          (parser/pattern-variable "?target")])]
    (parser/static-rules
     [(parser/static-rule-branch
       "follows" ["?source" "?target"] [direct])
      (parser/static-rule-branch
       "follows" ["?source" "?target"]
       [first-step recursive-step])])))

(defn bench-rules
  [database]
  (query/q-closed
   follows-rule-query
   [(query-types/source-input
     (query-types/database-source
      (db/database-view database)))
    (query-types/rules-input follows-rules)]))

(defn bench-rules-wide-3x3 []
  (let [database (wide-db 3 3)]
    (bench/bench (bench-rules database))))

(defn bench-rules-wide-5x3 []
  (let [database (wide-db 5 3)]
    (bench/bench (bench-rules database))))

(defn bench-rules-wide-7x3 []
  (let [database (wide-db 7 3)]
    (bench/bench (bench-rules database))))

(defn bench-rules-wide-4x6 []
  (let [database (wide-db 4 6)]
    (bench/bench (bench-rules database))))

(defn bench-rules-long-10x3 []
  (let [database (long-db 10 3)]
    (bench/bench (bench-rules database))))

(defn bench-rules-long-30x3 []
  (let [database (long-db 30 3)]
    (bench/bench (bench-rules database))))

(defn bench-rules-long-30x5 []
  (let [database (long-db 30 5)]
    (bench/bench (bench-rules database))))

(def serialization-people-count
  (match (Sys.getenv_opt "LG_BENCH_SERIALIZE_PEOPLE")
    (Some value) (Stdlib.int_of_string value)
    None 300000))

(def *serialize-db
  (delay
   (d/db-with
    empty-db
    (mapv
     (fn [person]
       (datascript.db/tx-entity person))
     (bench/people serialization-people-count)))))

(defn benchmark-serialized-db []
  (d/serializable @*serialize-db))

(defn json-write [value]
  (runtime-edn/write-json-string value))

(defn json-read [^:string source]
  (runtime-edn/read-json-source source))

(defn benchmark-frozen-db []
  (json-write (benchmark-serialized-db)))

(defn bench-freeze []
  (bench/bench
   (json-write (benchmark-serialized-db))))

(defn bench-thaw []
  (let [frozen (benchmark-frozen-db)]
    (bench/bench
     (d/from-serializable (json-read frozen)))))

(def ^:map<string;fn<datascript.bench.bench/benchmark-result>> benches
  {"add-1"              bench-add-1
   "add-5"              bench-add-5
   "add-all"            bench-add-all
   "init"               bench-init
   "find-datoms"        bench-find-datoms
   "find-datom"         bench-find-datom
   "retract-5"          bench-retract-5
   "q1"                 bench-q1
   "q2"                 bench-q2
   "q3"                 bench-q3
   "q4"                 bench-q4
   "q5-shortcircuit"    bench-q5-shortcircuit
   "qpred1"             bench-qpred1
   "qpred2"             bench-qpred2
   "pull-one-entities"  bench-pull-one-entities
   "pull-one"           bench-pull-one
   "pull-many-entities" bench-pull-many-entities
   "pull-many"          bench-pull-many
   "pull-wildcard"      bench-pull-wildcard
   "rules-wide-3x3"     bench-rules-wide-3x3
   "rules-wide-5x3"     bench-rules-wide-5x3
   "rules-wide-7x3"     bench-rules-wide-7x3
   "rules-wide-4x6"     bench-rules-wide-4x6
   "rules-long-10x3"    bench-rules-long-10x3
   "rules-long-30x3"    bench-rules-long-30x3
   "rules-long-30x5"    bench-rules-long-30x5
   "freeze"             bench-freeze
   "thaw"               bench-thaw})

(defn ^:float run-benchmark [^:string name]
  (cond
    (= name "prepare-people")
    (let [started (bench/now)
          prepared (count (bench/people20k))]
      (Stdlib.ignore prepared)
      (Float.sub (bench/now) started))

    (= name "prepare-db")
    (let [started (bench/now)
          prepared (count @*db100k)]
      (Stdlib.ignore prepared)
      (Float.sub (bench/now) started))

    (= name "serialization-people-count")
    (double serialization-people-count)

    :else
    (if-some [benchmark-fn (get benches name)]
      (:mean-ms (benchmark-fn))
      -1.0)))
