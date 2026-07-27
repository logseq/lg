(ns datascript.test.db
  (:require
   [clojure.test :refer [deftest is]]
   [datascript.core :as d]
   [datascript.db :as db]))

(defrecord HashBeef [^:keyword value]
  IHash
  (-hash [_record] 0xBEEF))

(deftype ProtocolDatom [^int _value]
  db/IDatom
  (datom-tx [_] 42)
  (datom-added [_] false)
  (datom-get-idx [_] 7)
  (datom-set-idx [_ _value]
    (Stdlib.ignore 0)))

(deftest test-idatom-protocol
  (let [datom (ProtocolDatom. 0)]
    (is (= 42 (db/datom-tx datom)))
    (is (= false (db/datom-added datom)))
    (is (= 7 (db/datom-get-idx datom)))))

(deftest test-combine-hashes
  (is (= (hash-combine 12 34) (db/combine-hashes 12 34))))

(deftest test-public-db-transient-roundtrip
  (let [database
        (d/db-with
         (d/empty-db)
         [{:db/id 1 :name "Ivan"}
          {:db/id 2 :name "Oleg"}])
        transient-database (db/db-transient database)
        persistent-database (db/db-persistent! transient-database)]
    (is (db/db? transient-database))
    (is (db/db? persistent-database))
    (is (db/db-equal? database transient-database))
    (is (db/db-equal? database persistent-database))
    (is (= (:identity-hash database)
           (:identity-hash persistent-database)))
    (is
     (db/datom-vectors-equal?
      (vec (:eavt database))
      (vec (:eavt persistent-database))))))

(deftest test-public-db-helpers
  (let [database
        (d/empty-db
         {:age {:db/index true}
          :name {:db/index false}})]
    (is (db/indexing? database :age))
    (is (not (db/indexing? database :name)))
    (is (db/builtin-fn? :db/add))
    (is (not (db/builtin-fn? :db/unknown)))
    (db/check-schema-update
     database
     (zipmap
      [:db/ident]
      [(Datascript_runtime.Data_value.Keyword ":user/name")]))
    (is true)))

(deftest test-public-search-protocol
  (let [database
        (d/db-with
         (d/empty-db)
         [{:db/id 1 :name "Ivan"}
          {:db/id 2 :name "Oleg"}])
        all-datoms
        (vec
         (db/-search
          database
          (db/search-pattern None None None None)))
        first-datom (first all-datoms)]
    (is (= 2 (count all-datoms)))
    (is
     (db/datom-vectors-equal?
      [(d/datom 1 :name "Ivan" (db/datom-tx first-datom))]
      (vec
       (db/-search
        database
        (db/search-pattern
         (Some 1)
         (Some :name)
         (Some (Datascript_runtime.Data_value.String "Ivan"))
         None)))))
    (is
     (=
      2
      (count
       (db/-search
        database
        (db/search-pattern
         None
         None
         None
         (Some (db/datom-tx first-datom)))))))))

(deftest test-public-value-class-helpers
  (let [integer (Datascript_runtime.Data_value.Int 1)
        float-value (Datascript_runtime.Data_value.Float 1.0)
        string-value (Datascript_runtime.Data_value.String "1")
        symbol-value (Datascript_runtime.Data_value.Symbol "1")
        vector-value (Datascript_runtime.Data_value.vector_of_vector [])
        other-vector (Datascript_runtime.Data_value.vector_of_vector [])]
    (is (db/class-identical? integer float-value))
    (is (db/class-identical? vector-value other-vector))
    (is (not (db/class-identical? string-value symbol-value)))
    (is (= 0 (db/class-compare integer float-value)))
    (is
     (=
      (db/class-compare string-value symbol-value)
      (- (db/class-compare symbol-value string-value))))
    (is (= (Datascript_runtime.Data_value.hash vector-value)
           (db/ihash vector-value)))
    (is (= (db/ihash vector-value) (db/ihash other-vector)))))

(deftest test-public-seqable
  (is (db/seqable? (Datascript_runtime.Data_value.Nil)))
  (is (db/seqable? (Datascript_runtime.Data_value.List (list))))
  (is (db/seqable? (Datascript_runtime.Data_value.vector_of_vector [])))
  (is (db/seqable? (Datascript_runtime.Data_value.Map (list))))
  (is (db/seqable? (Datascript_runtime.Data_value.Set (list))))
  (is (not (db/seqable? (Datascript_runtime.Data_value.String "abc"))))
  (is (not (db/seqable? (Datascript_runtime.Data_value.Int 1)))))

(deftest test-public-pr-db
  (let [^datascript.db/DB database
        (d/db-with
         (d/empty-db)
         [{:db/id 1 :name "Ivan"}])
        writer (Buffer.create 128)]
    (db/pr-db database writer ())
    (is (= (str database) (Buffer.contents writer)))))

(deftest test-defrecord-updatable
  (is (= 0xBEEF (hash (HashBeef. :ignored)))))

(deftest test-db-hash-cache
  (let [database (d/empty-db)]
    (is (= 0 @(:hash database)))
    (let [database-hash (hash database)]
      (is (= database-hash @(:hash database))))))

(defn now []
  (current-time-millis))

(deftest test-uuid
  (let [now-ms
        (loop []
          (let [timestamp (now)]
            (if (> (mod timestamp 1000) 900)
              (recur)
              timestamp)))
        now-seconds (int (/ now-ms 1000))]
    (is
     (=
      (* 1000 now-seconds)
      (d/squuid-time-millis (d/squuid))))
    (is (not= (d/squuid) (d/squuid)))
    (is
     (=
      (subs (str (d/squuid)) 0 8)
      (subs (str (d/squuid)) 0 8)))))

(defn option-datoms-equal?
  [^:option<vector<datascript.db/Datom>> actual
   ^:vector<datascript.db/Datom> expected]
  (if-some [actual actual]
    (db/datom-vectors-equal? actual expected)
    false))

(defn database-diff-equal?
  [^datascript.db/database-diff difference
   ^:vector<datascript.db/Datom> only-left
   ^:vector<datascript.db/Datom> only-right
   ^:vector<datascript.db/Datom> both]
  (and
   (option-datoms-equal? (:only-left difference) only-left)
   (option-datoms-equal? (:only-right difference) only-right)
   (option-datoms-equal? (:both difference) both)))

(deftest test-diff
  (let [left
        (->
         (d/empty-db)
         (d/db-with
          [{:a 1 :b 2 :c 4}
           {:a 1}]))
        right
        (->
         (d/empty-db)
         (d/db-with [{:b 3 :d 5}])
         (d/db-with [{:db/id 1 :a 1}]))]
    (is
     (database-diff-equal?
      (db/diff-databases left right)
      [(d/datom 1 :b 2)
       (d/datom 1 :c 4)
       (d/datom 2 :a 1)]
      [(d/datom 1 :b 3)
       (d/datom 1 :d 5)]
      [(d/datom 1 :a 1)]))))
