(ns datascript.test.storage
  (:require
   [clojure.test :refer [deftest is testing]]
   [datascript.conn :as conn]
   [datascript.core :as d]
   [datascript.db :as db]
   [datascript.storage :as storage]
   [datascript.test.storage-support :as support]))

(def strong (Lg_runtime.Runtime_ref_type.Strong))

(deftest test-maybe-adapt-storage
  (testing "options without storage preserve tree settings"
    (let [opts
          (db/options-with-branching-factor
           (db/options-with-ref-type (db/default-options) strong)
           64)
          adapted (storage/maybe-adapt-storage opts)]
      (is (nil? (db/options-storage adapted)))
      (is (= strong (db/options-ref-type adapted)))
      (is (= 64 (db/options-branching-factor adapted)))))

  (testing "options with storage preserve backend and tree settings"
    (let [memory (support/make-storage)
          backend (support/backend memory)
          opts (support/options memory 64 strong)
          adapted (storage/maybe-adapt-storage opts)
          adapted-twice (storage/maybe-adapt-storage adapted)]
      (if-some [adapted-backend (db/options-storage adapted)]
        (is
         (Datascript_runtime.Storage_backend.equal
          backend adapted-backend))
        (is false))
      (if-some [adapted-backend (db/options-storage adapted-twice)]
        (is
         (Datascript_runtime.Storage_backend.equal
          backend adapted-backend))
        (is false))
      (is (= strong (db/options-ref-type adapted)))
      (is (= 64 (db/options-branching-factor adapted)))))

  (testing "adapted options drive the real store and restore path"
    (let [memory (support/make-storage)
          opts
          (storage/maybe-adapt-storage
           (support/options memory 64 strong))
          database (d/empty-db None opts)]
      (is (= 5 (count @(:writes memory))))
      (d/store database)
      (is (= 5 (count @(:writes memory))))
      (let [settings (d/settings (support/restore-database memory))]
        (is (= strong (:ref-type settings)))
        (is (= 64 (:branching-factor settings)))))))

(deftest test-conn-from-datoms-preserves-options
  (let [memory (support/make-storage)
        backend (support/backend memory)
        connection
        (d/conn-from-datoms
         [(db/datom
           1 :name
           (Datascript_runtime.Data_value.String "Ivan"))]
         db/empty-schema
         (support/options memory 64 strong))
        database (conn/current-db connection)]
    (if-some [database-backend (d/storage database)]
      (is
       (Datascript_runtime.Storage_backend.equal
        backend database-backend))
      (is false))
    (is (pos? (count @(:writes memory))))
    (let [settings (d/settings database)]
      (is (= strong (:ref-type settings)))
      (is (= 64 (:branching-factor settings))))
    (is
     (support/has-string?
      (support/restore-database memory)
      1 :name "Ivan"))))

(deftest test-istorage-protocol
  (let [database (d/empty-db)
        memory (support/make-storage)]
    (is (satisfies? storage/IStorage memory))
    (is (nil? (storage/-restore memory 999999)))
    (d/store database memory)
    (is (= 5 (count @(:writes memory))))
    (is (db/db-equal? database (d/restore memory)))
    (if-some [root (storage/-restore memory 0)]
      (do
        #?(:clj
           (storage/-store memory [(tuple 999999 root)])
           :cljs
           (storage/-store memory [(tuple 999999 root)] []))
        (is (some? (storage/-restore memory 999999)))
        #?(:clj
           (storage/-delete memory [999999])
           :cljs
           (storage/-store memory [] [999999]))
        (is (nil? (storage/-restore memory 999999))))
      (is false))))

(deftest test-basics
  (testing "empty db"
    (let [database (d/empty-db)
          memory (support/make-storage)]
      (d/store database (support/backend memory))
      (is (= 5 (count @(:writes memory))))
      (let [restored (support/restore-database memory)]
        (is (= 2 (count @(:reads memory))))
        (is (db/db-equal? database restored))
        (is (= 3 (count @(:reads memory)))))))

  (testing "small db"
    (let [database (support/small-database 32 strong)
          memory (support/make-storage)]
      (testing "store"
        (d/store database (support/backend memory))
        (is (= 0 (count @(:reads memory))))
        (is (= 5 (count @(:writes memory)))))
      (testing "restore"
        (let [restored (support/restore-database memory)]
          (is (= 2 (count @(:reads memory))))
          (is (db/db-equal? database restored))
          (is (= 3 (count @(:reads memory))))
          (vec (d/datoms restored :aevt))
          (is (= 4 (count @(:reads memory))))
          (vec (d/datoms restored :avet))
          (is (= 5 (count @(:reads memory)))))

        (testing "count"
          (support/reset-stats memory)
          (let [restored (support/restore-database memory)]
            (db/db-count restored)
            (is (= 2 (count @(:reads memory))))))

        (testing "settings"
          (let [settings (d/settings (support/restore-database memory))]
            (is (= 32 (:branching-factor settings)))
            (is (= strong (:ref-type settings))))))))

  (testing "large db"
    (let [database (support/large-database 32 strong)
          memory (support/make-storage)]
      (testing "store"
        (d/store database (support/backend memory))
        (is (= 135 (count @(:writes memory))))
        (d/store database)
        (is (= 135 (count @(:writes memory)))))

      (testing "restore"
        (let [restored (support/restore-database memory)]
          (is (= 2 (count @(:reads memory))))
          (let [datom (first (d/datoms restored :eavt))]
            (is (= 1 (.-e datom)))
            (is (= :str (db/datom-attr datom)))
            (is
             (Datascript_runtime.Data_value.equal
              (Datascript_runtime.Data_value.String "1")
              (.-v datom))))
          (is (= 7 (count @(:reads memory))))
          (first (d/datoms restored :eavt))
          (is (= 7 (count @(:reads memory))))
          (vec (d/datoms restored :eavt))
          (is (= 68 (count @(:reads memory))))
          (vec (d/datoms restored :eavt))
          (is (= 68 (count @(:reads memory))))
          (is (db/db-equal? database restored))
          (is
           (db/datom-vectors-equal?
            (vec (:eavt database)) (vec (:eavt restored))))
          (is
           (db/datom-vectors-equal?
            (vec (:aevt database)) (vec (:aevt restored))))
          (is
           (db/datom-vectors-equal?
            (vec (:avet database)) (vec (:avet restored))))))

      (testing "count"
        (support/reset-stats memory)
        (let [restored (support/restore-database memory)]
          (is (= 1000 (db/db-count restored)))
          (is (= 2 (count @(:reads memory))))))

      (testing "incremental store"
        (support/reset-stats memory)
        (let [updated
              (d/db-with database [(support/add-string 1001 :str "1001")])]
          (d/store updated)
          (is (= 8 (count @(:writes memory)))))))))

(defn ^:vector<Datascript_runtime.Data_value.t> attribute-values
  [^datascript.db/DB database ^:keyword attr]
  (mapv
   (fn [^datascript.db/Datom datom] (.-v datom))
   (d/datoms-closed
    database
    :avet
    (Datascript_runtime.Data_value.Keyword (str attr)))))

(deftest test-db-with-tail
  (testing "db-with-tail retracts stale cardinality/one values"
    (let [database
          (d/db-with
           (d/empty-db (support/indexed-schema))
           [(support/add-int 1 :block/updated-at 2)])
          tail
          [[(db/datom
             1 :block/updated-at
             (Datascript_runtime.Data_value.Int 1772979060646)
             536870915 true)]
           [(db/datom
             1 :block/updated-at
             (Datascript_runtime.Data_value.Int 1772979061145)
             536870916 true)]]
          restored (storage/db-with-tail database tail)]
      (is
       (=
        [(Datascript_runtime.Data_value.Int 1772979061145)]
        (attribute-values restored :block/updated-at)))))

  (testing "restore replays stored tail"
    (let [memory (support/make-storage)
          opts (support/options memory 32 strong)
          database
          (d/db-with
           (d/empty-db (Some (support/indexed-schema)) opts)
           [(support/add-int 1 :block/updated-at 2)])
          tail
          [[(db/datom
             1 :block/updated-at
             (Datascript_runtime.Data_value.Int 1772979060646)
             536870915 true)]
           [(db/datom
             1 :block/updated-at
             (Datascript_runtime.Data_value.Int 1772979061145)
             536870916 true)]]]
      (d/store database)
      (storage/store-tail database tail)
      (is
       (=
        [(Datascript_runtime.Data_value.Int 1772979061145)]
        (attribute-values
         (support/restore-database memory)
         :block/updated-at)))))

  (testing "restore drops tail groups rejected by unique constraints"
    (let [memory (support/make-storage)
          opts (support/options memory 32 strong)
          database
          (d/db-with
           (d/empty-db (Some (support/unique-schema)) opts)
           [(support/add-string 1 :block/uuid "u1")])
          tail
          [[(db/datom
             2 :block/uuid
             (Datascript_runtime.Data_value.String "u1")
             536870915 true)
            (db/datom
             2 :block/title
             (Datascript_runtime.Data_value.String "Title")
             536870915 true)]
           [(db/datom
             3 :block/title
             (Datascript_runtime.Data_value.String "Later")
             536870916 true)]]]
      (d/store database)
      (storage/store-tail database tail)
      (let [restored (support/restore-database memory)]
        (is (support/has-string? restored 1 :block/uuid "u1"))
        (is (empty? (d/datoms restored :eavt 2)))
        (is (support/has-string? restored 3 :block/title "Later"))
        (is (= 536870916 (:max-tx restored)))))))

(deftest test-conn
  (let [memory (support/make-storage)
        connection
        (d/create-conn None (support/options memory 32 strong))]
    (is (= 5 (count @(:writes memory))))

    (d/transact! connection [(support/add-string 1 :name "Ivan")])
    (is (= 6 (count @(:writes memory))))
    (is (= 1 (last @(:writes memory))))

    (d/transact! connection [(support/add-string 2 :name "Oleg")])
    (is (= 7 (count @(:writes memory))))
    (is (= 1 (last @(:writes memory))))
    (is (= 2 (support/tail-group-count connection)))
    (is (= 2 (support/tail-datom-count connection)))

    (d/transact!
     connection
     (mapv
      (fn [eid] (support/add-string eid :name (str eid)))
      (range 3 33)))
    (is (= 8 (count @(:writes memory))))
    (is (= 1 (last @(:writes memory))))
    (is (= 3 (support/tail-group-count connection)))
    (is (= 32 (support/tail-datom-count connection)))

    (d/transact! connection [(support/add-string 33 :name "Petr")])
    (is (= 16 (count @(:writes memory))))

    (d/transact! connection [(support/add-string 34 :name "Anna")])
    (is (= 17 (count @(:writes memory))))
    (is (= 1 (last @(:writes memory))))

    (let [restored (support/restore-connection memory)]
      (is
       (db/db-equal?
        (conn/current-db connection)
        (conn/current-db restored)))

      (d/transact! restored [(support/add-string 35 :name "Vera")])
      (is (= 18 (count @(:writes memory))))
      (is (= 1 (last @(:writes memory))))

      (d/transact!
       restored
       (mapv
        (fn [eid] (support/add-string eid :name (str eid)))
        (range 36 80)))
      (is (= 28 (count @(:writes memory))))
      (is (= 1 (last @(:writes memory))))

      (let [restored-again (support/restore-connection memory)]
        (is
         (db/db-equal?
          (conn/current-db restored)
          (conn/current-db restored-again)))
        (d/transact!
         restored-again
         [(support/add-string 80 :name "Ilya")])
        (is (= 29 (count @(:writes memory))))
        (is (= 1 (last @(:writes memory))))
        (let [restored-final (support/restore-connection memory)]
          (is
           (db/db-equal?
            (conn/current-db restored-again)
            (conn/current-db restored-final))))))))
