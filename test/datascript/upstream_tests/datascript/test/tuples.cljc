(ns datascript.test.tuples
  (:require
   [clojure.test :refer [deftest is testing]]
   [datascript.conn :as conn]
   [datascript.core :as d]
   [datascript.db :as db]
   [datascript.test.core :as tdc]))

(defn ^:Datascript_runtime.Data_value.t int-value [^:int value]
  (Datascript_runtime.Data_value.Int value))

(defn ^:Datascript_runtime.Data_value.t string-value [^:string value]
  (Datascript_runtime.Data_value.String value))

(defn ^:Datascript_runtime.Data_value.t keyword-value [^:keyword value]
  (Datascript_runtime.Data_value.Keyword (str value)))

(defn ^:Datascript_runtime.Data_value.t nil-value []
  (Datascript_runtime.Data_value.Nil))

(defn ^:Datascript_runtime.Data_value.t tuple-value
  [^:vector<Datascript_runtime.Data_value.t> values]
  (Datascript_runtime.Data_value.vector_of_vector values))

(defn ^:Datascript_runtime.Data_value.t keyword-vector
  [^:vector<keyword> values]
  (tuple-value (mapv keyword-value values)))

(defn ^:Datascript_runtime.Data_value.entity_ref entity-id [^:int value]
  (Datascript_runtime.Data_value.Entity_id value))

(defn ^:Datascript_runtime.Data_value.entity_ref lookup-ref
  [^:keyword attr ^:Datascript_runtime.Data_value.t value]
  (Datascript_runtime.Data_value.Lookup_ref attr value))

(defn ^:Datascript_runtime.Data_value.t entity-ref-value
  [^:Datascript_runtime.Data_value.entity_ref value]
  (Datascript_runtime.Data_value.Ref_to value))

(defn ^:Datascript_runtime.Data_value.t lookup-ref-value
  [^:keyword attr ^:Datascript_runtime.Data_value.t value]
  (tuple-value [(keyword-value attr) value]))

(defn ^:map<keyword;Datascript_runtime.Data_value.t> schema-entry
  [^:vector<keyword> keys
   ^:vector<Datascript_runtime.Data_value.t> values]
  (zipmap keys values))

(defn ^:map<keyword;Datascript_runtime.Data_value.t> tuple-entry
  [^:vector<keyword> attrs]
  (schema-entry [:db/tupleAttrs] [(keyword-vector attrs)]))

(defn ^:map<keyword;Datascript_runtime.Data_value.t> unique-tuple-entry
  [^:vector<keyword> attrs]
  (schema-entry
   [:db/tupleAttrs :db/unique]
   [(keyword-vector attrs) (keyword-value :db.unique/identity)]))

(defn ^:map<keyword;Datascript_runtime.Data_value.t> unique-entry []
  (schema-entry
   [:db/unique]
   [(keyword-value :db.unique/identity)]))

(defn ^:map<keyword;Datascript_runtime.Data_value.t> ref-entry []
  (schema-entry
   [:db/valueType]
   [(keyword-value :db.type/ref)]))

(defn ^:map<keyword;Datascript_runtime.Data_value.t> ref-unique-tuple-entry
  [^:vector<keyword> attrs]
  (schema-entry
   [:db/valueType :db/tupleAttrs :db/unique]
   [(keyword-value :db.type/tuple)
    (keyword-vector attrs)
    (keyword-value :db.unique/identity)]))

(defn ^:map<keyword;map<keyword;Datascript_runtime.Data_value.t>> schema
  [^:vector<keyword> attrs
   ^:vector<map<keyword;Datascript_runtime.Data_value.t>> entries]
  (zipmap attrs entries))

(defn ^:map<keyword;Datascript_runtime.Data_value.t> entity-map
  [^:vector<keyword> attrs
   ^:vector<Datascript_runtime.Data_value.t> values]
  (zipmap attrs values))

(defn ^datascript.db/tx-entry entity-entry
  [^:vector<keyword> attrs
   ^:vector<Datascript_runtime.Data_value.t> values]
  (db/tx-entity (entity-map attrs values)))

(defn ^datascript.db/tx-entry add
  [^:Datascript_runtime.Data_value.entity_ref eid
   ^:keyword attr
   ^:Datascript_runtime.Data_value.t value]
  (db/tx-add eid attr value))

(defn ^datascript.db/tx-entry retract
  [^:Datascript_runtime.Data_value.entity_ref eid
   ^:keyword attr
   ^:Datascript_runtime.Data_value.t value]
  (db/tx-retract eid attr value))

(type-record expected-datom
  (eid :int)
  (attr :keyword)
  (value :Datascript_runtime.Data_value.t))

(defn ^expected-datom expected
  [^:int eid ^:keyword attr ^:Datascript_runtime.Data_value.t value]
  (record expected-datom (eid eid) (attr attr) (value value)))

(defn assert-datoms
  [^datascript.db/DB database ^:vector<expected-datom> expected-datoms]
  (is (= (count expected-datoms) (db/db-count database)))
  (doseq [item expected-datoms]
    (is
     (reduce
      (fn [^boolean found ^datascript.db/Datom datom]
        (or
         found
         (and
          (= (:eid item) (.-e datom))
          (= (:attr item) (db/datom-attr datom))
          (=
           (Datascript_runtime.Data_value.to_edn_string (:value item))
           (Datascript_runtime.Data_value.to_edn_string
            (.-v datom))))))
      false
      (d/datoms database :eavt)))))

(defn ^datascript.db/DB current-db [^datascript.conn/Conn connection]
  (conn/current-db connection))

(defn ^boolean throws? [f]
  (try
    (f)
    false
    (catch _ true)))

(defn ^:option<int> tuple-position
  [^datascript.db/DB database ^:keyword source ^:keyword target]
  (if-some [targets (get (:attr-tuples (:rschema database)) source)]
    (get targets target)
    None))

(deftest test-schema
  (let [database
        (d/empty-db
         (schema
          [:year+session :semester+course+student :session+student]
          [(tuple-entry [:year :session])
           (tuple-entry [:semester :course :student])
           (schema-entry
            [:db/tupleAttrs :db/valueType]
            [(keyword-vector [:session :student])
             (keyword-value :db.type/tuple)])]))]
    (is
     (= #{:year+session :semester+course+student :session+student}
        (:tuple-attrs (:rschema database))))
    (is (= (Some 0) (tuple-position database :year :year+session)))
    (is (= (Some 1) (tuple-position database :session :year+session)))
    (is (= (Some 0) (tuple-position database :session :session+student)))
    (is
     (= (Some 0)
        (tuple-position database :semester :semester+course+student)))
    (is
     (= (Some 1)
        (tuple-position database :course :semester+course+student)))
    (is
     (= (Some 2)
        (tuple-position database :student :semester+course+student)))
    (is (= (Some 1) (tuple-position database :student :session+student))))

  (is
   (thrown-msg?
    ":t2 :db/tupleAttrs can’t depend on another tuple attribute: :t1"
    (d/empty-db
     (schema
      [:t1 :t2]
      [(tuple-entry [:a :b])
       (tuple-entry [:c :d :e :t1])]))))
  (is
   (thrown-msg?
    ":t1 :db/tupleAttrs must be a sequential collection, got: :a"
    (d/empty-db
     (schema
      [:t1]
      [(schema-entry [:db/tupleAttrs] [(keyword-value :a)])]))))
  (is
   (thrown-msg?
    ":t1 :db/tupleAttrs can’t be empty"
    (d/empty-db (schema [:t1] [(tuple-entry [])]))))
  (is
   (thrown-msg?
    ":t1 has :db/tupleAttrs, must be :db.cardinality/one"
    (d/empty-db
     (schema
      [:t1]
      [(schema-entry
        [:db/tupleAttrs :db/cardinality]
        [(keyword-vector [:a :b :c])
         (keyword-value :db.cardinality/many)])]))))
  (is
   (thrown-msg?
    ":t1 :db/tupleAttrs can’t depend on :db.cardinality/many attribute: :a"
    (d/empty-db
     (schema
      [:a :t1]
      [(schema-entry
        [:db/cardinality]
        [(keyword-value :db.cardinality/many)])
       (tuple-entry [:a :b :c])]))))
  (is
   (thrown-msg?
    "Bad attribute specification for :foo+bar: {:db/valueType :db.type/tuple} should also have :db/tupleAttrs"
    (d/empty-db
     (schema
      [:foo+bar]
      [(schema-entry
        [:db/valueType]
        [(keyword-value :db.type/tuple)])])))))

(deftest test-tx
  (let [connection
        (d/create-conn
         (schema
          [:a+b :a+c+d]
          [(tuple-entry [:a :b]) (tuple-entry [:a :c :d])]))]
    (d/transact! connection [(add (entity-id 1) :a (string-value "a"))])
    (assert-datoms
     (current-db connection)
     [(expected 1 :a (string-value "a"))
      (expected
       1 :a+b
       (tuple-value [(string-value "a") (nil-value)]))
      (expected
       1 :a+c+d
       (tuple-value [(string-value "a") (nil-value) (nil-value)]))])

    (d/transact! connection [(add (entity-id 1) :b (string-value "b"))])
    (assert-datoms
     (current-db connection)
     [(expected 1 :a (string-value "a"))
      (expected 1 :b (string-value "b"))
      (expected
       1 :a+b
       (tuple-value [(string-value "a") (string-value "b")]))
      (expected
       1 :a+c+d
       (tuple-value [(string-value "a") (nil-value) (nil-value)]))])

    (d/transact! connection [(add (entity-id 1) :a (string-value "A"))])
    (assert-datoms
     (current-db connection)
     [(expected 1 :a (string-value "A"))
      (expected 1 :b (string-value "b"))
      (expected
       1 :a+b
       (tuple-value [(string-value "A") (string-value "b")]))
      (expected
       1 :a+c+d
       (tuple-value [(string-value "A") (nil-value) (nil-value)]))])

    (d/transact!
     connection
     [(add (entity-id 1) :c (string-value "c"))
      (add (entity-id 1) :d (string-value "d"))])
    (assert-datoms
     (current-db connection)
     [(expected 1 :a (string-value "A"))
      (expected 1 :b (string-value "b"))
      (expected
       1 :a+b
       (tuple-value [(string-value "A") (string-value "b")]))
      (expected 1 :c (string-value "c"))
      (expected 1 :d (string-value "d"))
      (expected
       1 :a+c+d
       (tuple-value
        [(string-value "A") (string-value "c") (string-value "d")]))])

    (d/transact!
     connection
     [(add (entity-id 1) :a (string-value "A"))
      (add (entity-id 1) :b (string-value "B"))
      (add (entity-id 1) :c (string-value "C"))
      (add (entity-id 1) :d (string-value "D"))])
    (d/transact!
     connection
     [(retract (entity-id 1) :a (string-value "A"))
      (retract (entity-id 1) :b (string-value "B"))])
    (assert-datoms
     (current-db connection)
     [(expected 1 :c (string-value "C"))
      (expected 1 :d (string-value "D"))
      (expected
       1 :a+c+d
       (tuple-value [(nil-value) (string-value "C") (string-value "D")]))])

    (is
     (thrown-msg?
      "Can’t modify tuple attrs directly: [:db/add 1 :a+b [\"A\" \"B\"]]"
      (d/transact!
       connection
       [(entity-entry
         [:db/id :a+b]
         [(int-value 1)
          (tuple-value [(string-value "A") (string-value "B")])])])))))

(deftest test-ignore-correct
  (let [connection
        (d/create-conn (schema [:a+b] [(tuple-entry [:a :b])]))
        matching (tuple-value [(string-value "a") (string-value "b")])
        partial (tuple-value [(string-value "a")])
        nil-tail (tuple-value [(string-value "a") (nil-value)])]
    (d/transact!
     connection
     [(entity-entry
       [:db/id :a :b :a+b]
       [(int-value 1)
        (string-value "a")
        (string-value "b")
        matching])])
    (is
     (thrown-msg?
      "Can’t modify tuple attrs directly: [:db/add 2 :a+b [\"a\" \"b\"]]"
      (d/transact!
       connection
       [(entity-entry
         [:db/id :a :b :a+b]
         [(int-value 2)
          (string-value "x")
          (string-value "y")
          matching])])))
    (is
     (thrown-msg?
      "Can’t modify tuple attrs directly: [:db/add 2 :a+b [\"a\"]]"
      (d/transact!
       connection
       [(entity-entry
         [:db/id :a :b :a+b]
         [(int-value 2)
          (string-value "a")
          (string-value "b")
          partial])])))
    (is
     (thrown-msg?
      "Can’t modify tuple attrs directly: [:db/add 2 :a+b [\"a\" nil]]"
      (d/transact!
       connection
       [(entity-entry
         [:db/id :a :b :a+b]
         [(int-value 2)
          (string-value "a")
          (string-value "b")
          nil-tail])])))
    (is
     (thrown-msg?
      "Can’t modify tuple attrs directly: [:db/add 1 :a+b [\"a\" \"B\"]]"
      (d/transact!
       connection
       [(entity-entry
         [:db/id :a+b]
         [(int-value 1)
          (tuple-value [(string-value "a") (string-value "B")])])])))
    (d/transact!
     connection
     [(entity-entry [:db/id :a+b] [(int-value 1) matching])])
    (d/transact!
     connection
     [(entity-entry
       [:db/id :b :a+b]
       [(int-value 1)
        (string-value "B")
        (tuple-value [(string-value "a") (string-value "B")])])])
    (d/transact!
     connection
     [(entity-entry
       [:db/id :a+b :a]
       [(int-value 1)
        (tuple-value [(string-value "A") (string-value "B")])
        (string-value "A")])])
    (assert-datoms
     (current-db connection)
     [(expected 1 :a (string-value "A"))
      (expected 1 :b (string-value "B"))
      (expected
       1 :a+b
       (tuple-value [(string-value "A") (string-value "B")]))])))

(deftest test-unique
  (let [connection
        (d/create-conn
         (schema [:a+b] [(unique-tuple-entry [:a :b])]))]
    (d/transact! connection [(add (entity-id 1) :a (string-value "a"))])
    (d/transact! connection [(add (entity-id 2) :a (string-value "A"))])
    (is
     (throws?
      (fn []
        (d/transact!
         connection
         [(add (entity-id 1) :a (string-value "A"))]))))
    (d/transact!
     connection
     [(add (entity-id 1) :b (string-value "b"))
      (add (entity-id 2) :b (string-value "b"))
      (entity-entry
       [:db/id :a :b]
       [(int-value 3) (string-value "a") (string-value "B")])])
    (assert-datoms
     (current-db connection)
     [(expected 1 :a (string-value "a"))
      (expected 1 :b (string-value "b"))
      (expected
       1 :a+b
       (tuple-value [(string-value "a") (string-value "b")]))
      (expected 2 :a (string-value "A"))
      (expected 2 :b (string-value "b"))
      (expected
       2 :a+b
       (tuple-value [(string-value "A") (string-value "b")]))
      (expected 3 :a (string-value "a"))
      (expected 3 :b (string-value "B"))
      (expected
       3 :a+b
       (tuple-value [(string-value "a") (string-value "B")]))])
    (is
     (throws?
      (fn []
        (d/transact!
         connection
         [(add (entity-id 1) :b (string-value "B"))]))))
    (d/transact!
     connection
     [(entity-entry
       [:db/id :a :b]
       [(int-value 1) (string-value "A") (string-value "B")])])
    (is
     (some?
      (d/find-datom-closed
       (current-db connection)
       :eavt
       (int-value 1)
       (keyword-value :a+b)
       (tuple-value [(string-value "A") (string-value "B")]))))
    (d/transact!
     connection
     [(entity-entry
       [:db/id :a :b]
       [(int-value 4) (string-value "a") (string-value "b")])])
    (is
     (some?
      (d/find-datom-closed
       (current-db connection)
       :eavt
       (int-value 4)
       (keyword-value :a+b)
       (tuple-value [(string-value "a") (string-value "b")]))))))

(deftest test-upsert
  (let [connection
        (d/create-conn
         (schema
          [:a+b :c]
          [(unique-tuple-entry [:a :b]) (unique-entry)]))]
    (d/transact!
     connection
     [(entity-entry
       [:db/id :a :b]
       [(int-value 1) (string-value "A") (string-value "B")])
      (entity-entry
       [:db/id :a :b]
       [(int-value 2) (string-value "a") (string-value "b")])])
    (d/transact!
     connection
     [(entity-entry
       [:a+b :c]
       [(tuple-value [(string-value "A") (string-value "B")])
        (string-value "C")])
      (entity-entry
       [:a+b :c]
       [(tuple-value [(string-value "a") (string-value "b")])
        (string-value "c")])])
    (is
     (thrown-msg?
      "Conflicting upserts: [:a+b [\"A\" \"B\"]] resolves to 1, but [:c \"c\"] resolves to 2"
      (d/transact!
       connection
       [(entity-entry
         [:a+b :c]
         [(tuple-value [(string-value "A") (string-value "B")])
          (string-value "c")])])))
    (d/transact!
     connection
     [(entity-entry
       [:a+b :b :d]
       [(tuple-value [(string-value "A") (string-value "B")])
        (string-value "b")
        (string-value "D")])])
    (assert-datoms
     (current-db connection)
     [(expected 1 :a (string-value "A"))
      (expected 1 :b (string-value "b"))
      (expected
       1 :a+b
       (tuple-value [(string-value "A") (string-value "b")]))
      (expected 1 :c (string-value "C"))
      (expected 1 :d (string-value "D"))
      (expected 2 :a (string-value "a"))
      (expected 2 :b (string-value "b"))
      (expected
       2 :a+b
       (tuple-value [(string-value "a") (string-value "b")]))
      (expected 2 :c (string-value "c"))])))

(deftest test-upsert-by-tuple-components
  (let [database
        (d/empty-db
         (schema [:a+b] [(unique-tuple-entry [:a :b])]))
        database
        (d/db-with
         database
         [(entity-entry
           [:a :b :name]
           [(string-value "A")
            (string-value "B")
            (string-value "Ivan")])])
        expected-datoms
        [(expected 1 :a (string-value "A"))
         (expected 1 :b (string-value "B"))
         (expected
          1 :a+b
          (tuple-value [(string-value "A") (string-value "B")]))
         (expected 1 :name (string-value "Oleg"))]]
    (assert-datoms
     (d/db-with
      database
      [(entity-entry
        [:db/id :a :b :name]
        [(int-value -1)
         (string-value "A")
         (string-value "B")
         (string-value "Oleg")])])
     expected-datoms)
    (assert-datoms
     (d/db-with
      database
      [(entity-entry
        [:a :b :name]
        [(string-value "A")
         (string-value "B")
         (string-value "Oleg")])])
     expected-datoms)
    (assert-datoms
     (d/db-with
      database
      [(add (entity-id -1) :a (string-value "A"))
       (add (entity-id -1) :b (string-value "B"))
       (add (entity-id -1) :name (string-value "Oleg"))])
     expected-datoms)))

(deftest test-lookup-refs
  (let [connection
        (d/create-conn
         (schema
          [:a+b :c]
          [(unique-tuple-entry [:a :b]) (unique-entry)]))
        upper (tuple-value [(string-value "A") (string-value "B")])
        lower (tuple-value [(string-value "a") (string-value "b")])]
    (d/transact!
     connection
     [(entity-entry
       [:db/id :a :b]
       [(int-value 1) (string-value "A") (string-value "B")])
      (entity-entry
       [:db/id :a :b]
       [(int-value 2) (string-value "a") (string-value "b")])])
    (d/transact!
     connection
     [(add (lookup-ref :a+b upper) :c (string-value "C"))
      (entity-entry
       [:db/id :c]
       [(entity-ref-value (lookup-ref :a+b lower))
        (string-value "c")])])
    (is
     (throws?
      (fn []
        (d/transact!
         connection
         [(add (lookup-ref :a+b upper) :c (string-value "c"))]))))
    (is
     (thrown-msg?
      "Conflicting upsert: [:c \"c\"] resolves to 2, but entity already has :db/id 1"
      (d/transact!
       connection
       [(entity-entry
         [:db/id :c]
         [(entity-ref-value (lookup-ref :a+b upper))
          (string-value "c")])])))
    (d/transact!
     connection
     [(entity-entry
       [:db/id :b :d]
       [(entity-ref-value (lookup-ref :a+b upper))
        (string-value "b")
        (string-value "D")])])
    (is
     (some?
      (d/find-datom-closed
       (current-db connection)
       :eavt
       (int-value 1)
       (keyword-value :a+b)
       (tuple-value [(string-value "A") (string-value "b")]))))))

(deftest lookup-refs-in-tuple
  (let [database
        (d/empty-db
         (schema
          [:ref :name :ref+name]
          [(ref-entry)
           (unique-entry)
           (ref-unique-tuple-entry [:ref :name])]))
        database
        (d/db-with
         database
         [(entity-entry
           [:db/id :name]
           [(int-value -1) (string-value "Ivan")])
          (entity-entry
           [:db/id :name]
           [(int-value -2) (string-value "Oleg")])
          (entity-entry
           [:db/id :name :ref]
           [(int-value -3)
            (string-value "Petr")
            (entity-ref-value (entity-id -1))])
          (entity-entry
           [:db/id :name :ref]
           [(int-value -4)
            (string-value "Yuri")
            (entity-ref-value (entity-id -2))])])
        numeric-tuple (tuple-value [(int-value 1) (string-value "Petr")])
        lookup-tuple
        (tuple-value
         [(lookup-ref-value :name (string-value "Ivan"))
          (string-value "Petr")])]
    (let [updated
          (d/db-with
           database
           [(entity-entry
             [:ref+name :age]
             [numeric-tuple (int-value 32)])])]
      (is
       (some?
        (d/find-datom-closed
         updated :eavt
         (int-value 3) (keyword-value :age) (int-value 32)))))
    (let [updated
          (d/db-with
           database
           [(entity-entry
             [:ref+name :age]
             [lookup-tuple (int-value 32)])])]
      (is
       (some?
        (d/find-datom-closed
         updated :eavt
         (int-value 3) (keyword-value :age) (int-value 32)))))
    (let [updated
          (d/db-with
           database
           [(add (entity-id -1) :ref+name numeric-tuple)
            (add (entity-id -1) :age (int-value 32))])]
      (is
       (some?
        (d/find-datom-closed
         updated :eavt
         (int-value 3) (keyword-value :age) (int-value 32)))))
    (let [updated
          (d/db-with
           database
           [(add (entity-id -1) :ref+name lookup-tuple)
            (add (entity-id -1) :age (int-value 32))])]
      (is
       (some?
        (d/find-datom-closed
         updated :eavt
         (int-value 3) (keyword-value :age) (int-value 32)))))
    (is (= (Some 1) (db/entid database (lookup-ref :name (string-value "Ivan")))))
    (is (= (Some 3) (db/entid database (lookup-ref :ref+name numeric-tuple))))
    (is (= (Some 3) (db/entid database (lookup-ref :ref+name lookup-tuple))))))

(deftest test-validation
  (let [database (d/empty-db (schema [:a+b] [(tuple-entry [:a :b])]))
        database-with-a
        (d/db-with
         database
         [(add (entity-id 1) :a (string-value "a"))])
        nils (tuple-value [(nil-value) (nil-value)])
        partial (tuple-value [(string-value "a") (nil-value)])]
    (is
     (thrown-msg?
      "Can’t modify tuple attrs directly: [:db/add 1 :a+b [nil nil]]"
      (d/db-with database [(add (entity-id 1) :a+b nils)])))
    (is
     (thrown-msg?
      "Can’t modify tuple attrs directly: [:db/add 1 :a+b [\"a\" nil]]"
      (d/db-with
       database-with-a
       [(add (entity-id 1) :a+b partial)])))
    (is
     (thrown-msg?
      "Can’t modify tuple attrs directly: [:db/add 1 :a+b [\"a\" nil]]"
      (d/db-with
       database
       [(add (entity-id 1) :a (string-value "a"))
        (add (entity-id 1) :a+b partial)])))
    (is
     (thrown-msg?
      "Can’t modify tuple attrs directly: [:db/retract 1 :a+b [\"a\" nil]]"
      (d/db-with
       database-with-a
       [(retract (entity-id 1) :a+b partial)])))))

(defn ^:vector<int> datom-eids [datoms]
  (mapv (fn [^datascript.db/Datom datom] (.-e datom)) datoms))

(deftest test-indexes
  (let [database
        (d/db-with
         (d/empty-db
          (schema [:a+b+c] [(tuple-entry [:a :b :c])]))
         (mapv
          (fn [^:int eid]
            (let [a (if (even? eid) "A" "a")
                  b (if (or (= eid 3) (= eid 4) (= eid 7) (= eid 8))
                      "B" "b")
                  c (if (> eid 4) "C" "c")]
              (entity-entry
               [:db/id :a :b :c]
               [(int-value eid)
                (string-value a)
                (string-value b)
                (string-value c)])))
          (range 1 9)))
        exact
        (tuple-value
         [(string-value "A") (string-value "b") (string-value "C")])
        missing
        (tuple-value
         [(string-value "A") (string-value "b") (nil-value)])
        from
        (tuple-value
         [(string-value "A") (string-value "B") (string-value "C")])
        to
        (tuple-value
         [(string-value "A") (string-value "b") (string-value "c")])
        nil-from
        (tuple-value
         [(string-value "A") (string-value "B") (nil-value)])
        nil-to
        (tuple-value
         [(string-value "A") (string-value "b") (nil-value)])]
    (is
     (=
      [6]
      (datom-eids
       (d/datoms-closed
        database :avet (keyword-value :a+b+c) exact))))
    (is
     (empty?
      (d/datoms-closed
       database :avet (keyword-value :a+b+c) missing)))
    (is
     (=
      [8 4 6 2]
      (datom-eids
       (d/index-range-closed database :a+b+c from to))))
    (is
     (=
      [8 4]
      (datom-eids
       (d/index-range-closed database :a+b+c nil-from nil-to))))))

(deftest test-queries
  (let [database
        (d/db-with
         (d/empty-db
          (schema [:a+b] [(unique-tuple-entry [:a :b])]))
         [(entity-entry
           [:db/id :a :b]
           [(int-value 1) (string-value "A") (string-value "B")])
          (entity-entry
           [:db/id :a :b]
           [(int-value 2) (string-value "A") (string-value "b")])
          (entity-entry
           [:db/id :a :b]
           [(int-value 3) (string-value "a") (string-value "B")])
          (entity-entry
           [:db/id :a :b]
           [(int-value 4) (string-value "a") (string-value "b")])])]
    (is
     (tdc/query-relation?
      (d/q '[:find ?e
             :where [?e :a+b ["a" "B"]]]
           database)
      [[3]]))
    (is
     (tdc/query-relation?
      (d/q '[:find ?a+b
             :where [[:a+b ["a" "B"]] :a+b ?a+b]]
           database)
      [[["a" "B"]]]))
    (is
     (tdc/query-relation?
      (d/q '[:find ?a+b
             :where [?e :a ?a]
             [?e :b ?b]
             [(tuple ?a ?b) ?a+b]]
           database)
      [[["A" "B"]] [["A" "b"]] [["a" "B"]] [["a" "b"]]]))
    (is
     (tdc/query-relation?
      (d/q '[:find ?a ?b
             :where [?e :a+b ?a+b]
             [(untuple ?a+b) [?a ?b]]]
           database)
      [["A" "B"] ["A" "b"] ["a" "B"] ["a" "b"]]))))
