(ns datascript.test.storage-native
  (:require
   [clojure.test :refer [deftest is testing]]
   [datascript.conn :as conn]
   [datascript.core :as d]
   [datascript.db :as db]
   [datascript.storage :as storage]
   [datascript.storage-file :as storage-file]
   [datascript.test.storage-support :as support]))

(def strong (Lg_runtime.Runtime_ref_type.Strong))
(def weak (Lg_runtime.Runtime_ref_type.Weak))

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
        (storage/-store memory [(tuple 999999 root)])
        (is (some? (storage/-restore memory 999999)))
        (is (= [999999] (storage/collect-garbage memory)))
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
      (d/store database (support/backend memory))
      (is (= 0 (count @(:reads memory))))
      (is (= 5 (count @(:writes memory))))
      (let [restored (support/restore-database memory)]
        (is (= 2 (count @(:reads memory))))
        (is (db/db-equal? database restored))
        (is (= 3 (count @(:reads memory))))
        (vec (d/datoms restored :aevt))
        (is (= 4 (count @(:reads memory))))
        (vec (d/datoms restored :avet))
        (is (= 5 (count @(:reads memory)))))
      (support/reset-stats memory)
      (let [restored (support/restore-database memory)]
        (is (= 3 (db/db-count restored)))
        ;; LG shares the upstream CLJS metadata-count path on both targets.
        (is (= 2 (count @(:reads memory)))))
      (let [settings (d/settings (support/restore-database memory))]
        (is (= 32 (:branching-factor settings)))
        (is (= strong (:ref-type settings))))))

  (testing "large db"
    (let [database (support/large-database 32 strong)
          memory (support/make-storage)]
      (d/store database (support/backend memory))
      (is (= 135 (count @(:writes memory))))
      (d/store database)
      (is (= 135 (count @(:writes memory))))
      (let [restored (support/restore-database memory)]
        (is (= 2 (count @(:reads memory))))
        (let [datom (first (d/datoms restored :eavt))]
          (is (= 1 (.-e datom)))
          (is (= :str (db/datom-attr datom)))
          (is
           (Datascript_runtime.Data_value.equal
            (Datascript_runtime.Data_value.String "1")
            (.-v datom))))
        ;; LG shares the upstream CLJS lazy `till` path on both targets.
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
          (vec (:avet database)) (vec (:avet restored)))))
      (support/reset-stats memory)
      (let [restored (support/restore-database memory)]
        (is (= 1000 (db/db-count restored)))
        (is (= 2 (count @(:reads memory)))))
      (support/reset-stats memory)
      (let [updated
            (d/db-with database [(support/add-string 1001 :str "1001")])]
        (d/store updated)
        (is (= 8 (count @(:writes memory))))))))

(defn ^:string temporary-directory []
  (let [path (Filename.temp_file "lg-datascript-upstream-storage-" "")]
    (Sys.remove path)
    path))

(defn cleanup-directory
  [^Datascript_runtime.Storage_backend.t backend ^:string directory]
  (storage/-delete backend (storage/-list-addresses backend))
  (Unix.rmdir directory))

(defn ^Datascript_runtime.Storage_backend.t default-file-storage
  [^:string directory]
  (storage-file/file-storage directory))

(defn write-value
  [^:out_channel output
   ^:Datascript_runtime.Storage_value.t value]
  :unit
  (Marshal.to_channel output value (list-of :Marshal.extern_flags)))

(defn ^Datascript_runtime.Storage_value.t read-value
  [^:in_channel input]
  (Marshal.from_channel input))

(defn ^:string standard-filename [^:int address]
  (Lg_runtime.Runtime_int.format_hex address 8))

(defn ^:int standard-address [^:string filename]
  (Stdlib.int_of_string (str "0x" filename)))

(defn ^Datascript_runtime.Storage_backend.t custom-file-storage
  [^:string directory]
  (storage-file/file-storage
   directory
   (storage-file/options
    write-value read-value standard-filename standard-address)))

(type-variant storage-format
  StreamingEdn
  InMemoryEdn
  StreamingTransitJson
  InMemoryTransitJson
  StreamingTransitMsgpack)

(defn ^:string format-name [^storage-format format]
  (match format
    StreamingEdn "streaming-edn"
    InMemoryEdn "inmemory-edn"
    StreamingTransitJson "streaming-transit-json"
    InMemoryTransitJson "inmemory-transit-json"
    StreamingTransitMsgpack "streaming-transit-msgpack"))

(defn ^Datascript_runtime.Storage_backend.t make-file-storage
  [^storage-format format ^:string directory]
  (match format
    StreamingEdn (default-file-storage directory)
    _ (custom-file-storage directory)))

(defn ^:vector<storage-format> file-storage-formats []
  [StreamingEdn
   InMemoryEdn
   StreamingTransitJson
   InMemoryTransitJson
   StreamingTransitMsgpack])

(defn ^:vector<tuple<string;datascript.db/DB>> databases
  [^:int branching-factor ^:Lg_runtime.Runtime_ref_type.t ref-type]
  [(tuple "empty" (support/empty-database branching-factor ref-type))
   (tuple "small" (support/small-database branching-factor ref-type))
   (tuple "large" (support/large-database branching-factor ref-type))])

(deftest test-file-storage
  (doseq [format (file-storage-formats)]
    (let [format-name (format-name format)]
      (doseq [ref-type [strong weak]]
        (doseq [branching-factor [32 64 512]]
          (doseq [database-entry
                  (databases branching-factor ref-type)]
            (let [size-name (tuple-get database-entry 0)
                  database (tuple-get database-entry 1)
                  directory (temporary-directory)
                  backend (make-file-storage format directory)
                  _stored (d/store database backend)
                  restored
                  (if-some [database (d/restore backend)]
                    database
                    (Stdlib.invalid_arg "Stored database did not restore"))]
              (testing
               (str format-name "/" branching-factor "/" size-name)
               (is (db/db-equal? database restored))
               (is
                (db/datom-vectors-equal?
                 (vec (:eavt database)) (vec (:eavt restored))))
               (is
                (db/datom-vectors-equal?
                 (vec (:aevt database)) (vec (:aevt restored))))
               (is
                (db/datom-vectors-equal?
                 (vec (:avet database)) (vec (:avet restored))))
               (let [settings (d/settings restored)]
                 (is
                  (= branching-factor
                     (:branching-factor settings)))
                 (is (= ref-type (:ref-type settings)))))
              (cleanup-directory backend directory))))))))

(deftest test-gc
  (let [memory (support/make-storage)
        database
        (support/large-database 32 strong)
        _stored (d/store database (support/backend memory))]
    (is (= 135 (count (d/addresses database))))
    (is
     (= 135
        (count (storage/-list-addresses (support/backend memory)))))
    (is
     (=
      (d/addresses database)
      (set (storage/-list-addresses (support/backend memory)))))

    (let [updated
          (d/db-with database [(support/add-string 1001 :str "1001")])
          _stored-updated (d/store updated)
          _orphan
          (vswap!
           (:disk memory)
           assoc
           999999
           (Datascript_runtime.Storage_value.Stored_tail []))
          deleted (d/collect-garbage (support/backend memory))]
      (is
       (>
        (count (storage/-list-addresses (support/backend memory)))
        (count (d/addresses updated))))
      (is (contains? (set deleted) 999999))
      (is
       (every?
        (fn [address]
          (contains?
           (set (storage/-list-addresses (support/backend memory)))
           address))
        (into (d/addresses database) (d/addresses updated))))
      (is
       (pos?
        (count
         (storage/-list-addresses (support/backend memory))))))))

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
        (let [state @(:atom restored-again)
              last-stored
              (match (:db-last-stored state)
                (Some database) database
                None (Stdlib.invalid_arg "Missing stored database"))]
          (is
           (>
            (count
             (storage/-list-addresses (support/backend memory)))
            (count (d/addresses last-stored))))
          (d/collect-garbage (support/backend memory))
          (is
           (every?
            (fn [address]
              (contains?
               (set
                (storage/-list-addresses
                 (support/backend memory)))
               address))
            (d/addresses last-stored))))
        (let [restored-final (support/restore-connection memory)]
          (is
           (db/db-equal?
            (conn/current-db restored-again)
            (conn/current-db restored-final))))))))
