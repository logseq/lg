(ns datascript.test.serialize
  (:require
    [clojure.edn :as edn]
    [clojure.test :as t :refer [is are deftest testing]]
    [datascript.core :as d]
    [datascript.db :as db]
    [datascript.test.core :as tdc]
    [ocaml.Lg_runtime.Runtime_edn :as runtime-edn]
    [ocaml.melange-edn-native/Melange_edn_native :as native-edn]
    #?(:clj [ocaml.yojson/Yojson.Safe :as yojson])))

(type-alias serialized-value :Lg_edn_backend.t)

(defn ^serialized-value identity-serialized
  [^serialized-value value]
  value)

(defn ^serialized-value serialized-codec
  [^serialized-value value ^:keyword _type]
  value)

(defn database-has-nan? [^datascript.db/DB database]
  (if-some [entity (d/entity database 1)]
    (match (:nan entity)
      (Some
       (datascript.impl.entity/EntityScalar
        (Datascript_runtime.Data_value.Float value)))
      (js/isNaN value)
      _ false)
    false))

#?(:clj
(defn yojson-write [value]
     (yojson/to-string
       (native-edn/to-json
         (native-edn/of-edn-string (runtime-edn/write-string value))))))

#?(:clj
(defn yojson-read [source]
     (runtime-edn/read-string
       (native-edn/to-edn-string
         (native-edn/of-json (yojson/from-string source))))))

#?(:cljs
(defn melange-json-write [value]
  (native-edn/to-json-string
    (native-edn/of-edn-string (runtime-edn/write-string value)))))

#?(:cljs
(defn melange-json-read [source]
  (runtime-edn/read-string
    (native-edn/to-edn-string (native-edn/of-json-string source)))))

(def readers
  {"clojure.edn/read-string" edn/read-string
   "clojure.core/read-string" edn/read-string})

(def datom-readers
  {"clojure.edn/read-string" db/datom-from-edn-string
   "clojure.core/read-string" db/datom-from-edn-string})

(def database-readers
  {"clojure.edn/read-string" db/db-from-edn-string
   "clojure.core/read-string" db/db-from-edn-string})

(deftest test-public-reader-functions
  (let [expected (db/datom 1 :name "Oleg" 17 false)
        reader-value
        (Datascript_runtime.Serialization_value.read_datom
         (pr-str expected))]
    (is (= expected (db/datom-from-reader reader-value)))
    (is (not (db/datom-added
              (db/datom-from-reader reader-value)))))

  (let [expected
        (->
         (d/empty-db {:name {:db/unique :db.unique/identity}})
         (d/db-with [[:db/add 1 :name "Petr"]
                     [:db/add 2 :name "Ivan"]]))
        reader-value
        (Datascript_runtime.Serialization_value.read_database
         (pr-str expected))
        restored (db/db-from-reader reader-value)]
    (is (db/db-equal? expected restored))
    (is (= (:schema expected) (:schema restored)))
    (is
     (db/datom-vectors-equal?
      (vec (d/datoms expected :eavt))
      (vec (d/datoms restored :eavt))))))

(deftest test-pr-read
  (doseq [[r read-fn] datom-readers]
    (testing r
      (let [d (db/datom 1 :name "Oleg" 17 true)]
        (is (= (pr-str d) "#datascript/Datom [1 :name \"Oleg\" 17 true]"))
        (is (= d (read-fn (pr-str d)))))
      
      (let [d (db/datom 1 :name 3)]
        (is (= (pr-str d) "#datascript/Datom [1 :name 3 536870912 true]"))
        (is (= d (read-fn (pr-str d)))))))
  (doseq [[r read-fn] database-readers]
    (testing r
      (let [db (-> (d/empty-db {:name {:db/unique :db.unique/identity}})
                 (d/db-with [[:db/add 1 :name "Petr"]
                             [:db/add 1 :age 44]])
                 (d/db-with [[:db/add 2 :name "Ivan"]]))]
        (is (= (pr-str db)
              (str "#datascript/DB {"
                ":schema {:name {:db/unique :db.unique/identity}}, "
                ":datoms ["
                "[1 :age 44 536870913] "
                "[1 :name \"Petr\" 536870913] "
                "[2 :name \"Ivan\" 536870914]"
                "]}")))
        (is (= db (read-fn (pr-str db))))))))

(def data
  [(d/datom 1 :name "Petr")
   (d/datom 1 :aka "Devil")
   (d/datom 1 :aka "Tupen")
   (d/datom 1 :age 15)
   (d/datom 1 :follows 2)
   (d/datom 1 :email "petr@gmail.com")
   (d/datom 1 :avatar 10)
   (d/datom 10 :url "http://")
   (d/datom
    1
    :attach
    (Datascript_runtime.Data_value.Map
     (list
      (tuple
       (Datascript_runtime.Data_value.Keyword ":some-key")
       (Datascript_runtime.Data_value.Keyword ":some-value")))))
   (d/datom 2 :name "Oleg")
   (d/datom 2 :age 30)
   (d/datom 2 :email "oleg@gmail.com")
   (d/datom
    2
    :attach
    (Datascript_runtime.Data_value.Vector
     (list
      (Datascript_runtime.Data_value.Keyword ":just")
      (Datascript_runtime.Data_value.Keyword ":values"))))
   (d/datom 3 :name "Ivan")
   (d/datom 3 :age 15)
   (d/datom 3 :follows 2)
   (d/datom
    3
    :attach
    (Datascript_runtime.Data_value.Map
     (list
      (tuple
       (Datascript_runtime.Data_value.Keyword ":another")
       (Datascript_runtime.Data_value.Keyword ":map")))))
   (d/datom 3 :avatar 30)
   (d/datom 4 :name "Nick" d/tx0)
   (d/datom
    5 :inf
    (Datascript_runtime.Data_value.Float ##Inf))
   (d/datom
    5 :-inf
    (Datascript_runtime.Data_value.Float ##-Inf))
   ;; check that facts about transactions doesn’t set off max-eid
   (d/datom d/tx0 :txInstant 0xdeadbeef)
   (d/datom 30 :url "https://")])

(def transaction-data
  (mapv db/tx-datom data))

(deftest test-custom-freeze-called-once-per-value
  (let [calls (volatile! 0)
        freeze-fn
        (fn [^:Lg_edn_backend.t value]
          (vswap! calls inc)
          value)
        database
        (d/init-db
         [(d/datom
           1
           :payload
           (Datascript_runtime.Data_value.Map
            (list
             (tuple
              (Datascript_runtime.Data_value.Keyword ":key")
              (Datascript_runtime.Data_value.String "value")))))])]
    (d/serializable database {:freeze-fn freeze-fn})
    (is (= 2 @calls))))

(defn
  ^:map<keyword;Datascript_runtime.Data_value.t>
  serialize-schema-entry
  [^:vector<keyword> keys
   ^:vector<Datascript_runtime.Data_value.t> values]
  (zipmap keys values))

(def
  ^:map<keyword;map<keyword;Datascript_runtime.Data_value.t>>
  schema
  (zipmap
   [:name :aka :age :follows :email :avatar :url :attach]
   [(serialize-schema-entry [] [])
    (serialize-schema-entry
     [:db/cardinality]
     [(Datascript_runtime.Data_value.Keyword
       ":db.cardinality/many")])
    (serialize-schema-entry
     [:db/index]
     [(Datascript_runtime.Data_value.Bool true)])
    (serialize-schema-entry
     [:db/valueType]
     [(Datascript_runtime.Data_value.Keyword ":db.type/ref")])
    (serialize-schema-entry
     [:db/unique]
     [(Datascript_runtime.Data_value.Keyword
       ":db.unique/identity")])
    (serialize-schema-entry
     [:db/valueType :db/isComponent]
     [(Datascript_runtime.Data_value.Keyword ":db.type/ref")
      (Datascript_runtime.Data_value.Bool true)])
    (serialize-schema-entry [] [])
    (serialize-schema-entry [] [])]))

(deftest test-init-db
  (let [db-init     (d/init-db
                      data
                      schema)
        db-transact (d/db-with
                      (d/empty-db schema)
                      transaction-data)]

    (testing "db-init produces the same result as regular transactions"
      (is (= db-init db-transact)))

    (testing "db-init produces the same max-eid as regular transactions"
      (let [assertions
            [(db/tx-add
              (Datascript_runtime.Data_value.Temp_id "-1")
              :name
              (Datascript_runtime.Data_value.String "Lex"))]]
        (is (= (d/db-with db-init assertions)
              (d/db-with db-transact assertions)))))
    
    (testing "Roundtrip"
      (doseq [[r read-fn] database-readers]
        (testing r
          (let [roundtripped (read-fn (pr-str db-init))]
            (is (= db-init roundtripped))))))

    (testing "Reporting"
      (is (thrown-msg? "init-db expects list of Datoms, got [[:add -1 :name \"Ivan\"] {:add -1, :age 35}]"
            (d/init-db [[:add -1 :name "Ivan"] {:add -1 :age 35}] schema))))))

(deftest ^{:doc "issue-463"} test-max-eid-from-refs
  (let [db (-> (d/empty-db {:ref {:db/valueType :db.type/ref}})
             (d/db-with [[:db/add 1 :name "Ivan"]])
             (d/db-with [{:db/id 1 :ref {:name "Oleg"}}]))]
    (is (= 2 (:max-eid db)))
    (doseq [[r read-fn] database-readers]
      (testing r
        (let [db' (read-fn (pr-str db))]
          (is (= 2 (:max-eid db'))))))))

(deftest serialize
  (let [db (d/db-with
             (d/empty-db schema)
             transaction-data)]
    (testing "direct"
      (let [restored (-> db d/serializable d/from-serializable)]
        (is (= db restored))))
    (testing "edn"
      (is (= db (-> db d/serializable pr-str edn/read-string d/from-serializable))))
    (testing "custom EDN"
      (is (= db (-> db (d/serializable {:freeze-fn identity-serialized}) pr-str edn/read-string (d/from-serializable {:thaw-fn identity-serialized})))))
    (doseq [type [:json :json-verbose]]
      (testing type
        (is (= db (-> db d/serializable (serialized-codec type) (serialized-codec type) d/from-serializable)))))
    #?(:clj
       (is (= db (-> db d/serializable yojson-write yojson-read d/from-serializable))))
    #?(:cljs
       (is (= db (-> db d/serializable melange-json-write melange-json-read d/from-serializable))))))

(deftest serialization-preserves-and-overrides-tree-settings
  (let [source-options
        (db/options-with-branching-factor
         (db/options-with-ref-type
          (db/default-options)
          (Lg_runtime.Runtime_ref_type.Strong))
         7)
        database (d/init-db data schema source-options)
        frozen (d/serializable database)
        restored (d/from-serializable frozen)
        branching-override
        (d/from-serializable frozen {:branching-factor 11})
        ref-override
        (d/from-serializable
         frozen
         {:ref-type (Lg_runtime.Runtime_ref_type.Weak)})]
    (is (= 7 (:branching-factor (d/settings restored))))
    (is (= (Lg_runtime.Runtime_ref_type.Strong)
           (:ref-type (d/settings restored))))
    (is (= 11
           (:branching-factor (d/settings branching-override))))
    (is (= (Lg_runtime.Runtime_ref_type.Strong)
           (:ref-type (d/settings branching-override))))
    (is (= 7 (:branching-factor (d/settings ref-override))))
    (is (= (Lg_runtime.Runtime_ref_type.Weak)
           (:ref-type (d/settings ref-override))))))

(deftest serialization-preserves-empty-database-tree-settings
  (let [source-options
        (db/options-with-branching-factor
         (db/default-options)
         9)
        restored
        (->
         (d/empty-db None source-options)
         d/serializable
         d/from-serializable)]
    (is (= 9 (:branching-factor (d/settings restored))))))

(deftest test-nan
  (let [db (d/db-with
             (d/empty-db schema)
             [[:db/add 1 :nan ##NaN]])
        valid? database-has-nan?]
    (is (valid? (-> db d/serializable d/from-serializable)))
    (is (valid? (-> db d/serializable pr-str edn/read-string d/from-serializable)))
    (is (valid? (-> db (d/serializable {:freeze-fn identity-serialized}) pr-str edn/read-string (d/from-serializable {:thaw-fn identity-serialized}))))
    (doseq [type [:json :json-verbose]]
      (testing type
        (is (valid? (-> db d/serializable (serialized-codec type) (serialized-codec type) d/from-serializable)))))
    #?(:clj
       (is (valid? (-> db d/serializable yojson-write yojson-read d/from-serializable))))
    #?(:cljs
       (is (valid? (-> db d/serializable melange-json-write melange-json-read d/from-serializable))))))
