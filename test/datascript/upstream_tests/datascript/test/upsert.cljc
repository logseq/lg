(ns datascript.test.upsert
  (:require
    [clojure.test :refer [deftest is testing]]
    [datascript.core :as d]
    [datascript.db :as db]))

(defn ^:Datascript_runtime.Data_value.entity_ref entity-id
  [^:int value]
  (Datascript_runtime.Data_value.Entity_id value))

(defn ^:Datascript_runtime.Data_value.entity_ref numeric-tempid
  [^:int value]
  (Datascript_runtime.Data_value.Entity_id value))

(defn ^:Datascript_runtime.Data_value.entity_ref string-tempid
  [^:string value]
  (Datascript_runtime.Data_value.Temp_id value))

(defn ^:Datascript_runtime.Data_value.t int-value [^:int value]
  (Datascript_runtime.Data_value.Int value))

(defn ^:Datascript_runtime.Data_value.t string-value [^:string value]
  (Datascript_runtime.Data_value.String value))

(defn ^:Datascript_runtime.Data_value.t keyword-value [^:keyword value]
  (Datascript_runtime.Data_value.Keyword (str value)))

(defn ^:Datascript_runtime.Data_value.t ref-value [^:int value]
  (Datascript_runtime.Data_value.Ref value))

(defn ^:Datascript_runtime.Data_value.t entity-ref-value
  [^:Datascript_runtime.Data_value.entity_ref value]
  (Datascript_runtime.Data_value.Ref_to value))

(defn ^:Datascript_runtime.Data_value.t lookup-ref-value
  [^:keyword attr ^:Datascript_runtime.Data_value.t value]
  (Datascript_runtime.Data_value.vector_of_vector
   [(keyword-value attr) value]))

(defn ^:Datascript_runtime.Data_value.t vector-value
  [^:vector<Datascript_runtime.Data_value.t> values]
  (Datascript_runtime.Data_value.vector_of_vector values))

(defn ^:map<keyword;Datascript_runtime.Data_value.t> value-map
  [^:vector<keyword> keys
   ^:vector<Datascript_runtime.Data_value.t> values]
  (zipmap keys values))

(defn ^datascript.db/tx-entry entity-entry
  [^:vector<keyword> keys
   ^:vector<Datascript_runtime.Data_value.t> values]
  (db/tx-entity (value-map keys values)))

(defn ^:map<keyword;Datascript_runtime.Data_value.t> schema-entry
  [^:vector<keyword> keys
   ^:vector<Datascript_runtime.Data_value.t> values]
  (value-map keys values))

(defn ^:map<keyword;map<keyword;Datascript_runtime.Data_value.t>>
  upsert-schema []
  (zipmap
   [:name :email :slugs :ref]
   [(schema-entry
     [:db/unique]
     [(keyword-value :db.unique/identity)])
    (schema-entry
     [:db/unique]
     [(keyword-value :db.unique/identity)])
    (schema-entry
     [:db/unique :db/cardinality]
     [(keyword-value :db.unique/identity)
      (keyword-value :db.cardinality/many)])
    (schema-entry
     [:db/unique :db/valueType]
     [(keyword-value :db.unique/identity)
      (keyword-value :db.type/ref)])]))

(defn ^:map<keyword;map<keyword;Datascript_runtime.Data_value.t>>
  identity-schema []
  (zipmap
   [:name]
   [(schema-entry
     [:db/unique]
     [(keyword-value :db.unique/identity)])]))

(defn ^:map<keyword;map<keyword;Datascript_runtime.Data_value.t>>
  identity-ref-schema []
  (zipmap
   [:name :ref]
   [(schema-entry
     [:db/unique]
     [(keyword-value :db.unique/identity)])
    (schema-entry
     [:db/valueType]
     [(keyword-value :db.type/ref)])]))

(defn ^:map<keyword;Datascript_runtime.Data_value.t> entity-values
  [^datascript.db/DB database ^:int eid]
  (reduce
   (fn [^:map<keyword;Datascript_runtime.Data_value.t> result
        ^datascript.db/Datom datom]
     (if (= eid (.-e datom))
       (assoc result (.-a datom) (.-v datom))
       result))
   {}
   (d/datoms database :eavt)))

(defn ^boolean assert-entity-attributes
  [^:map<keyword;Datascript_runtime.Data_value.t> actual
   ^:vector<keyword> attrs
   ^:vector<Datascript_runtime.Data_value.t> values
   ^:int index]
  (if (< index (count attrs))
    (let [attr (nth attrs index)
          expected (nth values index)]
      (if-some [value (get actual attr)]
        (is (Datascript_runtime.Data_value.equal expected value))
        (is false (str "Missing entity attribute " attr)))
      (assert-entity-attributes actual attrs values (inc index)))
    true))

(defn assert-entity
  [^datascript.db/DB database
   ^:int eid
   ^:vector<keyword> attrs
   ^:vector<Datascript_runtime.Data_value.t> values]
  (let [actual (entity-values database eid)]
    (is
     (= (count attrs) (count actual))
     (Datascript_runtime.Data_value.to_edn_string
      (Datascript_runtime.Data_value.map_of_keyword_map actual)))
    (assert-entity-attributes actual attrs values 0)))

(defn assert-attribute-value
  [^datascript.db/DB database
   ^:int eid
   ^:keyword attr
   ^:Datascript_runtime.Data_value.t value]
  (is
   (some?
    (d/find-datom-closed
     database
     :eavt
     (int-value eid)
     (keyword-value attr)
     value))))

(defn ^:map<Datascript_runtime.Data_value.t;int> user-tempids
  [^datascript.db/TxReport report]
  (dissoc
   (.-tempids report)
   (entity-ref-value
    (Datascript_runtime.Data_value.Current_tx))))

(defn ^:map<Datascript_runtime.Data_value.t;int> expected-tempids
  [^:vector<Datascript_runtime.Data_value.t> tempids
   ^:vector<int> eids]
  (zipmap tempids eids))

(defn ^boolean tempids-equal?
  [^:map<Datascript_runtime.Data_value.t;int> expected
   ^:map<Datascript_runtime.Data_value.t;int> actual]
  (and
   (= (count expected) (count actual))
   (reduce-kv
    (fn [^boolean equal?
         ^:Datascript_runtime.Data_value.t tempid
         ^:int eid]
      (and equal? (= (Some eid) (get actual tempid))))
    true
    expected)))

(defn ^datascript.db/DB base-db []
  (d/db-with
   (d/empty-db (upsert-schema))
   [(entity-entry
     [:db/id :name :email]
     [(int-value 1) (string-value "Ivan") (string-value "@1")])
    (entity-entry
     [:db/id :name :email :ref]
     [(int-value 2)
      (string-value "Petr")
      (string-value "@2")
      (int-value 3)])
    (entity-entry
     [:db/id :name :email :ref]
     [(int-value 3)
      (string-value "Dima")
      (string-value "@3")
      (int-value 4)])
    (entity-entry
     [:db/id :name :email :ref]
     [(int-value 4)
      (string-value "Olga")
      (string-value "@4")
      (int-value 1)])]))

(defn assert-ivan-with-age
  [^datascript.db/TxReport report ^:int age]
  (assert-entity
   (.-db-after report)
   1
   [:name :email :age]
   [(string-value "Ivan") (string-value "@1") (int-value age)]))

(deftest test-upsert
  (let [database (base-db)]
    (testing "upsert, no tempid"
      (let [report
            (d/with
             database
             [(entity-entry
               [:name :age]
               [(string-value "Ivan") (int-value 35)])])]
        (assert-ivan-with-age report 35)
        (is (tempids-equal? {} (user-tempids report)))))

    (testing "upsert by two attributes, no tempid"
      (let [report
            (d/with
             database
             [(entity-entry
               [:name :email :age]
               [(string-value "Ivan")
                (string-value "@1")
                (int-value 35)])])]
        (assert-ivan-with-age report 35)
        (is (tempids-equal? {} (user-tempids report)))))

    (testing "upsert with numeric tempid"
      (let [report
            (d/with
             database
             [(entity-entry
               [:db/id :name :age]
               [(int-value -1)
                (string-value "Ivan")
                (int-value 35)])])]
        (assert-ivan-with-age report 35)
        (is
         (tempids-equal?
          (expected-tempids
           [(entity-ref-value (numeric-tempid -1))]
           [1])
          (user-tempids report)))))

    (testing "upsert with string tempids"
      (let [report
            (d/with
             database
             [(entity-entry
               [:db/id :name :age]
               [(string-value "1")
                (string-value "Ivan")
                (int-value 35)])
              (db/tx-add
               (string-tempid "2")
               :name
               (string-value "Oleg"))
              (db/tx-add
               (string-tempid "2")
               :email
               (string-value "@2"))])]
        (assert-ivan-with-age report 35)
        (assert-entity
         (.-db-after report)
         2
         [:name :email :ref]
         [(string-value "Oleg") (string-value "@2") (ref-value 3)])
        (is
         (tempids-equal?
          (expected-tempids
           [(entity-ref-value (string-tempid "1"))
            (entity-ref-value (string-tempid "2"))]
           [1 2])
          (user-tempids report)))))

    (testing "upsert by two attributes with tempid"
      (let [report
            (d/with
             database
             [(entity-entry
               [:db/id :name :email :age]
               [(int-value -1)
                (string-value "Ivan")
                (string-value "@1")
                (int-value 35)])])]
        (assert-ivan-with-age report 35)
        (is
         (tempids-equal?
          (expected-tempids
           [(entity-ref-value (numeric-tempid -1))]
           [1])
          (user-tempids report)))))

    (testing "two entities can resolve to the same tempid"
      (let [report
            (d/with
             database
             [(entity-entry
               [:db/id :name :age]
               [(int-value -1) (string-value "Ivan") (int-value 35)])
              (entity-entry
               [:db/id :name :age]
               [(int-value -1) (string-value "Ivan") (int-value 36)])])]
        (assert-ivan-with-age report 36)
        (is
         (tempids-equal?
          (expected-tempids
           [(entity-ref-value (numeric-tempid -1))]
           [1])
          (user-tempids report)))))

    (testing "two tempids can resolve to one entity"
      (let [report
            (d/with
             database
             [(entity-entry
               [:db/id :name :age]
               [(int-value -1) (string-value "Ivan") (int-value 35)])
              (entity-entry
               [:db/id :name :age]
               [(int-value -2) (string-value "Ivan") (int-value 36)])])]
        (assert-ivan-with-age report 36)
        (is
         (tempids-equal?
          (expected-tempids
           [(entity-ref-value (numeric-tempid -1))
            (entity-ref-value (numeric-tempid -2))]
           [1 1])
          (user-tempids report)))))

    (testing "upsert with existing ids and lookup refs"
      (doseq [id-value
              [(int-value 1)
               (lookup-ref-value :name (string-value "Ivan"))]]
        (let [report
              (d/with
               database
               [(entity-entry
                 [:db/id :name :email :age]
                 [id-value
                  (string-value "Ivan")
                  (string-value "@1")
                  (int-value 35)])])]
          (assert-ivan-with-age report 35)
          (is (tempids-equal? {} (user-tempids report))))))

    (testing "upsert conflicts with explicit ids"
      (is
       (thrown-msg?
        "Conflicting upsert: [:name \"Ivan\"] resolves to 1, but entity already has :db/id 2"
        (d/with
         database
         [(entity-entry
           [:db/id :name :age]
           [(int-value 2) (string-value "Ivan") (int-value 36)])])))
      (is
       (thrown-msg?
        "Conflicting upsert: [:name \"Ivan\"] resolves to 1, but entity already has :db/id 5"
        (d/with
         database
         [(entity-entry
           [:db/id :name :age]
           [(int-value 5) (string-value "Ivan") (int-value 36)])]))))

    (testing "non-existing unique values update the resolved entity"
      (let [report
            (d/with
             database
             [(entity-entry
               [:name :email :age]
               [(string-value "Ivan")
                (string-value "@5")
                (int-value 35)])])]
        (assert-entity
         (.-db-after report)
         1
         [:name :email :age]
         [(string-value "Ivan") (string-value "@5") (int-value 35)])
        (is (tempids-equal? {} (user-tempids report)))))

    (testing "conflicting unique fields"
      (is
       (thrown-msg?
        "Conflicting upserts: [:name \"Ivan\"] resolves to 1, but [:email \"@2\"] resolves to 2"
        (d/with
         database
         [(entity-entry
           [:name :email :age]
           [(string-value "Ivan")
            (string-value "@2")
            (int-value 35)])]))))

    (testing "upsert over the intermediate database"
      (let [report
            (d/with
             database
             [(entity-entry
               [:name :age]
               [(string-value "Igor") (int-value 35)])
              (entity-entry
               [:name :age]
               [(string-value "Igor") (int-value 36)])])]
        (assert-entity
         (.-db-after report)
         5
         [:name :age]
         [(string-value "Igor") (int-value 36)])
        (is (tempids-equal? {} (user-tempids report)))))

    (testing "intermediate upserts preserve tempid bindings"
      (doseq [second-tempid [-1 -2]]
        (let [report
              (d/with
               database
               [(entity-entry
                 [:db/id :name :age]
                 [(int-value -1) (string-value "Igor") (int-value 35)])
                (entity-entry
                 [:db/id :name :age]
                 [(int-value second-tempid)
                  (string-value "Igor")
                  (int-value 36)])])]
          (assert-entity
           (.-db-after report)
           5
           [:name :age]
           [(string-value "Igor") (int-value 36)])
          (is
           (tempids-equal?
            (if (= second-tempid -1)
              (expected-tempids
               [(entity-ref-value (numeric-tempid -1))]
               [5])
              (expected-tempids
               [(entity-ref-value (numeric-tempid -1))
                (entity-ref-value (numeric-tempid -2))]
               [5 5]))
            (user-tempids report))))))

    (testing "upsert conflicts with the current transaction"
      (is
       (thrown-msg?
        "Conflicting upsert: [:name \"Ivan\"] resolves to 1, but entity already has :db/id 536870914"
        (d/with
         database
         [(entity-entry
           [:db/id :name :age]
           [(keyword-value :db/current-tx)
            (string-value "Ivan")
            (int-value 35)])]))))

    (testing "unique cardinality-many values"
      (let [report
            (d/with
             database
             [(entity-entry
               [:name :slugs]
               [(string-value "Ivan") (string-value "ivan1")])
              (entity-entry
               [:name :slugs]
               [(string-value "Petr") (string-value "petr1")])])
            report2
            (d/with
             (.-db-after report)
             [(entity-entry
               [:name :slugs]
               [(string-value "Ivan")
                (vector-value
                 [(string-value "ivan1")
                  (string-value "ivan2")])])])]
        (assert-entity
         (.-db-after report)
         1
         [:name :email :slugs]
         [(string-value "Ivan")
          (string-value "@1")
          (string-value "ivan1")])
        (assert-attribute-value
         (.-db-after report2) 1 :name (string-value "Ivan"))
        (assert-attribute-value
         (.-db-after report2) 1 :email (string-value "@1"))
        (assert-attribute-value
         (.-db-after report2) 1 :slugs (string-value "ivan1"))
        (assert-attribute-value
         (.-db-after report2) 1 :slugs (string-value "ivan2"))
        (is
         (thrown-msg?
          "Conflicting upserts: [:slugs \"ivan1\"] resolves to 1, but [:slugs \"petr1\"] resolves to 2"
          (d/with
           (.-db-after report)
           [(entity-entry
             [:slugs]
             [(vector-value
               [(string-value "ivan1")
                (string-value "petr1")])])])))))

    (testing "upsert by ref and lookup ref"
      (doseq [case
              [(tuple (int-value 3) 2 36)
               (tuple (int-value 4) 3 37)
               (tuple (int-value 1) 4 38)
               (tuple (lookup-ref-value :name (string-value "Dima"))
                      2 36)
               (tuple (lookup-ref-value :name (string-value "Olga"))
                      3 37)
               (tuple (lookup-ref-value :name (string-value "Ivan"))
                      4 38)]]
        (let [ref-input (tuple-get case 0)
              eid (tuple-get case 1)
              age (tuple-get case 2)
              report
              (d/with
               database
               [(entity-entry
                 [:ref :age]
                 [ref-input (int-value age)])])]
          (is
           (=
            (Some (int-value age))
            (if-some
              [datom
               (d/find-datom-closed
                (.-db-after report)
                :eavt
                (int-value eid)
                (keyword-value :age))]
              (Some (.-v datom))
              None))))))

    (testing "reference tempids do not upsert their owners"
      (doseq [string-ids? [false true]]
        (let [first-id
              (if string-ids? (string-value "A") (int-value -1))
              second-id
              (if string-ids? (string-value "B") (int-value -2))
              ref-id
              (if string-ids? (string-value "A") (int-value -1))
              report
              (d/with
               database
               [(entity-entry
                 [:db/id :name]
                 [first-id (string-value "Igor")])
                (entity-entry
                 [:db/id :name :ref]
                 [second-id (string-value "Anna") ref-id])])]
          (assert-entity
           (.-db-after report)
           5
           [:name]
           [(string-value "Igor")])
          (assert-entity
           (.-db-after report)
           6
           [:name :ref]
           [(string-value "Anna") (ref-value 5)]))))))

(deftest test-redefining-ids
  (let [database
        (d/db-with
         (d/empty-db (identity-schema))
         [(entity-entry
           [:db/id :name]
           [(int-value -1) (string-value "Ivan")])])
        report
        (d/with
         database
         [(entity-entry
           [:db/id :age]
           [(int-value -1) (int-value 35)])
          (entity-entry
           [:db/id :name :age]
           [(int-value -1) (string-value "Ivan") (int-value 36)])])]
    (assert-entity
     (.-db-after report)
     1
     [:age :name]
     [(int-value 36) (string-value "Ivan")])
    (is
     (=
      (Some 1)
      (get
       (.-tempids report)
       (entity-ref-value (numeric-tempid -1))))))

  (let [database
        (d/db-with
         (d/empty-db (identity-schema))
         [(entity-entry
           [:db/id :name]
           [(int-value -1) (string-value "Ivan")])
          (entity-entry
           [:db/id :name]
           [(int-value -2) (string-value "Oleg")])])]
    (is
     (thrown-msg?
      "Conflicting upsert: -1 resolves both to 1 and 2"
      (d/with
       database
       [(entity-entry
         [:db/id :name :age]
         [(int-value -1) (string-value "Ivan") (int-value 35)])
        (entity-entry
         [:db/id :name :age]
         [(int-value -1) (string-value "Oleg") (int-value 36)])])))))

(defn assert-retry-order
  [^:int first-name-id ^:int second-name-id ^:int expected-eid]
  (let [database
        (d/db-with
         (d/empty-db (identity-schema))
         [(db/tx-add (numeric-tempid -1) :age (int-value 42))
          (db/tx-add
           (numeric-tempid -2)
           :likes
           (string-value "Pizza"))
          (db/tx-add
           (numeric-tempid first-name-id)
           :name
           (string-value "Bob"))
          (db/tx-add
           (numeric-tempid second-name-id)
           :name
           (string-value "Bob"))])]
    (assert-entity
     database
     expected-eid
     [:name :likes :age]
     [(string-value "Bob") (string-value "Pizza") (int-value 42)])))

(deftest test-retries-order
  (assert-retry-order -1 -2 1)
  (assert-retry-order -2 -1 2))

(defn assert-string-tempid-ref-result [^datascript.db/DB database]
  (assert-entity database 1 [:name] [(string-value "Alice")])
  (assert-entity
   database
   2
   [:age :ref]
   [(int-value 36) (ref-value 1)]))

(deftest test-upsert-string-tempid-ref
  (let [database
        (d/db-with
         (d/empty-db (identity-ref-schema))
         [(entity-entry [:name] [(string-value "Alice")])])]
    (doseq [transaction
            [[(entity-entry
               [:db/id :name]
               [(string-value "user") (string-value "Alice")])
              (entity-entry
               [:age :ref]
               [(int-value 36) (string-value "user")])]
             [(db/tx-add
               (string-tempid "user")
               :name
               (string-value "Alice"))
              (entity-entry
               [:age :ref]
               [(int-value 36) (string-value "user")])]
             [(entity-entry
               [:db/id :name]
               [(int-value -1) (string-value "Alice")])
              (entity-entry
               [:age :ref]
               [(int-value 36) (int-value -1)])]
             [(db/tx-add
               (numeric-tempid -1)
               :name
               (string-value "Alice"))
              (entity-entry
               [:age :ref]
               [(int-value 36) (int-value -1)])]]]
      (assert-string-tempid-ref-result
       (d/db-with database transaction)))))

(deftest test-two-tempids-two-retries
  (let [database
        (d/db-with
         (d/empty-db (identity-ref-schema))
         [(entity-entry [:name] [(string-value "Alice")])
          (entity-entry [:name] [(string-value "Bob")])])
        result
        (d/db-with
         database
         [(entity-entry
           [:db/id :ref]
           [(int-value 3) (string-value "A")])
          (entity-entry
           [:db/id :ref]
           [(int-value 4) (string-value "B")])
          (entity-entry
           [:db/id :name]
           [(string-value "A") (string-value "Alice")])
          (entity-entry
           [:db/id :name]
           [(string-value "B") (string-value "Bob")])])]
    (assert-entity result 1 [:name] [(string-value "Alice")])
    (assert-entity result 2 [:name] [(string-value "Bob")])
    (assert-entity result 3 [:ref] [(ref-value 1)])
    (assert-entity result 4 [:ref] [(ref-value 2)])))

(deftest test-vector-upsert
  (let [database
        (d/db-with
         (d/empty-db (identity-schema))
         [(entity-entry
           [:db/id :name]
           [(int-value -1) (string-value "Ivan")])])]
    (doseq [transaction
            [[(db/tx-add
               (numeric-tempid -1)
               :name
               (string-value "Ivan"))
              (db/tx-add
               (numeric-tempid -1)
               :age
               (int-value 12))]
             [(db/tx-add
               (numeric-tempid -1)
               :age
               (int-value 12))
              (db/tx-add
               (numeric-tempid -1)
               :name
               (string-value "Ivan"))]]]
      (assert-entity
       (d/db-with database transaction)
       1
       [:age :name]
       [(int-value 12) (string-value "Ivan")])))

  (let [database
        (d/db-with
         (d/empty-db (identity-schema))
         [(db/tx-add
           (numeric-tempid -1)
           :name
           (string-value "Ivan"))
          (db/tx-add
           (numeric-tempid -2)
           :name
           (string-value "Oleg"))])]
    (is
     (thrown-msg?
      "Conflicting upsert: -1 resolves both to 1 and 2"
      (d/with
       database
       [(db/tx-add
         (numeric-tempid -1)
         :name
         (string-value "Ivan"))
        (db/tx-add
         (numeric-tempid -1)
         :age
         (int-value 35))
        (db/tx-add
         (numeric-tempid -1)
         :name
         (string-value "Oleg"))
        (db/tx-add
         (numeric-tempid -1)
         :age
         (int-value 36))])))))
