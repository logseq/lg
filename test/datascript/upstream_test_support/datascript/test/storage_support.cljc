(ns datascript.test.storage-support
  (:require
   [datascript.conn :as conn]
   [datascript.db :as db]
   [datascript.storage :as storage]))

(type-record memory-storage
  (backend :Datascript_runtime.Storage_backend.t)
  (disk :ref<map<int;Datascript_runtime.Storage_value.t>>)
  (reads :ref<vector<int>>)
  (writes :ref<vector<int>>)
  (deletes :ref<vector<int>>))

#?(:clj
   (extend-type memory-storage
     storage/IStorage
     (-store [storage entries]
       (Datascript_runtime.Storage_backend.store
        (.-backend storage) entries []))
     (-restore [storage address]
       (Datascript_runtime.Storage_backend.restore
        (.-backend storage) address))
     (-list-addresses [storage]
       (Datascript_runtime.Storage_backend.list_addresses
        (.-backend storage)))
     (-delete [storage addresses]
       (Datascript_runtime.Storage_backend.delete
        (.-backend storage) addresses)))
   :cljs
   (extend-type memory-storage
     storage/IStorage
     (-store [storage entries delete-addresses]
       (Datascript_runtime.Storage_backend.store
        (.-backend storage) entries delete-addresses))
     (-restore [storage address]
       (Datascript_runtime.Storage_backend.restore
        (.-backend storage) address))))

(defn ^:map<int;Datascript_runtime.Storage_value.t> empty-disk []
  {})

(defn ^memory-storage make-storage []
  (let [disk (volatile! (empty-disk))
        reads (volatile! [])
        writes (volatile! [])
        deletes (volatile! [])
        backend
        (storage/make-backend
         (fn [^:vector<tuple<int;Datascript_runtime.Storage_value.t>> entries
              ^:vector<int> delete-addresses]
           (doseq [entry entries]
             (let [address (tuple-get entry 0)
                   value (tuple-get entry 1)]
               (vreset! disk (Lg_runtime.Lg_map.assoc @disk address value))
               (vswap! writes conj address)))
           (doseq [address delete-addresses]
             (vreset! disk (Lg_runtime.Lg_map.dissoc @disk address))
             (vswap! deletes conj address))
           (Stdlib.ignore 0))
         (fn [^:int address]
           (vswap! reads conj address)
           (Lg_runtime.Lg_map.get_option @disk address))
         (fn [^:unit _ignored]
           (vec (keys @disk)))
         (fn [^:vector<int> addresses]
           (doseq [address addresses]
             (vreset! disk (Lg_runtime.Lg_map.dissoc @disk address))
             (vswap! deletes conj address))
           (Stdlib.ignore 0)))]
    (record memory-storage
            (backend backend)
            (disk disk)
            (reads reads)
            (writes writes)
            (deletes deletes))))

(defn ^Datascript_runtime.Storage_backend.t backend
  [^memory-storage storage]
  (.-backend storage))

(defn reset-stats [^memory-storage storage]
  (vreset! (:reads storage) [])
  (vreset! (:writes storage) [])
  (vreset! (:deletes storage) []))

(defn ^datascript.db/database-options options
  [^memory-storage storage
   ^:int branching-factor
   ^:Lg_runtime.Runtime_ref_type.t ref-type]
  (db/options-with-branching-factor
   (db/options-with-ref-type
    (db/options-with-storage (backend storage))
    ref-type)
   branching-factor))

(defn ^datascript.db/tx-entry add-string
  [^:int eid ^:keyword attr ^:string value]
  (db/tx-add
   (Datascript_runtime.Data_value.Entity_id eid)
   attr
   (Datascript_runtime.Data_value.String value)))

(defn ^datascript.db/tx-entry add-int
  [^:int eid ^:keyword attr ^:int value]
  (db/tx-add
   (Datascript_runtime.Data_value.Entity_id eid)
   attr
   (Datascript_runtime.Data_value.Int value)))

(defn ^datascript.db/DB empty-database
  [^:int branching-factor ^:Lg_runtime.Runtime_ref_type.t ref-type]
  (db/empty-db
   None
   (db/options-with-branching-factor
    (db/options-with-ref-type (db/default-options) ref-type)
    branching-factor)))

(defn ^datascript.db/DB small-database
  [^:int branching-factor ^:Lg_runtime.Runtime_ref_type.t ref-type]
  (conn/db-with
   (empty-database branching-factor ref-type)
   [(add-string 1 :name "Ivan")
    (add-string 2 :name "Oleg")
    (add-string 3 :name "Petr")]))

(defn ^datascript.db/DB large-database
  [^:int branching-factor ^:Lg_runtime.Runtime_ref_type.t ref-type]
  (conn/db-with
   (empty-database branching-factor ref-type)
   (mapv
    (fn [eid] (add-string eid :str (str eid)))
    (range 1 1001))))

(defn ^datascript.db/DB restore-database
  [^memory-storage storage]
  (if-some [database (storage/restore (backend storage))]
    database
    (Stdlib.invalid_arg "Stored database did not restore")))

(defn ^datascript.conn/Conn restore-connection
  [^memory-storage storage]
  (if-some [connection (conn/restore-conn (backend storage))]
    connection
    (Stdlib.invalid_arg "Stored connection did not restore")))

(defn ^boolean has-value?
  [^datascript.db/DB database
   ^:int eid
   ^:keyword attr
   ^:Datascript_runtime.Data_value.t value]
  (if-some [datom (db/search-ea database eid attr)]
    (Datascript_runtime.Data_value.equal (.-v datom) value)
    false))

(defn ^boolean has-string?
  [^datascript.db/DB database
   ^:int eid
   ^:keyword attr
   ^:string value]
  (has-value?
   database eid attr
   (Datascript_runtime.Data_value.String value)))

(defn ^:int tail-group-count [^datascript.conn/Conn connection]
  (let [^datascript.conn/conn-state state @(:atom connection)]
    (count (:tx-tail state))))

(defn ^:int tail-datom-count [^datascript.conn/Conn connection]
  (let [^datascript.conn/conn-state state @(:atom connection)]
    (transduce (map count) + 0 (:tx-tail state))))

(defn ^:map<keyword;map<keyword;Datascript_runtime.Data_value.t>>
  indexed-schema []
  (zipmap
   [:block/updated-at]
   [(zipmap
     [:db/index]
     [(Datascript_runtime.Data_value.Bool true)])]))

(defn ^:map<keyword;Datascript_runtime.Data_value.t>
  empty-schema-entry []
  {})

(defn ^:map<keyword;map<keyword;Datascript_runtime.Data_value.t>>
  unique-schema []
  (zipmap
   [:block/uuid :block/title]
   [(zipmap
     [:db/unique]
     [(Datascript_runtime.Data_value.Keyword ":db.unique/identity")])
    (empty-schema-entry)]))
