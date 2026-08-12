(ns datascript.storage
  (:require
   [datascript.db :as db]
   [me.tonsky.persistent-sorted-set :as set]
   [me.tonsky.persistent-sorted-set.arrays :as arrays]))

(type-alias serialized-datom
  :Datascript_runtime.Storage_value.serialized_datom)
(type-alias serialized-node
  :Datascript_runtime.Storage_value.serialized_node)
(type-alias serialized-index
  :Datascript_runtime.Storage_value.serialized_index)
(type-alias stored-value :Datascript_runtime.Storage_value.t)
(type-alias storage-backend :Datascript_runtime.Storage_backend.t)

(defprotocol ClosedStorage
  (closed-storage [storage] :Datascript_runtime.Storage_backend.t))

(extend-type :Datascript_runtime.Storage_backend.t
  ClosedStorage
  (closed-storage [backend] backend))

#?(:clj
   (defprotocol IStorage
     (-store
       [backend
        ^:vector<tuple<int;stored_value>> address-data]
       :unit)
     (-restore
       [backend ^:int address]
       :option<stored_value>)
     (-list-addresses
       [backend]
       :vector<int>)
     (-delete
       [backend ^:vector<int> addresses]
       :unit))
   :cljs
   (defprotocol IStorage
     (-store
       [backend
        ^:vector<tuple<int;stored_value>> address-data
        ^:vector<int> delete-addresses]
       :unit)
     (-restore
       [backend ^:int address]
       :option<stored-value>)))

#?(:clj
   (extend-type :Datascript_runtime.Storage_backend.t
     IStorage
     (-store [backend address-data]
       (Datascript_runtime.Storage_backend.store
        backend address-data []))
     (-restore [backend address]
       (Datascript_runtime.Storage_backend.restore backend address))
     (-list-addresses [backend]
       (Datascript_runtime.Storage_backend.list_addresses backend))
     (-delete [backend addresses]
       (Datascript_runtime.Storage_backend.delete backend addresses)))
   :cljs
   (extend-type :Datascript_runtime.Storage_backend.t
     IStorage
     (-store [backend address-data delete-addresses]
       (Datascript_runtime.Storage_backend.store
        backend address-data delete-addresses))
     (-restore [backend address]
       (Datascript_runtime.Storage_backend.restore backend address))))

(defn- store-backend
  [backend address-data delete-addresses]
  :unit
  (Datascript_runtime.Storage_backend.store
   backend address-data delete-addresses))

(defn- list-backend-addresses
  [backend]
  :vector<int>
  (Datascript_runtime.Storage_backend.list_addresses backend))

(defn- delete-backend-addresses
  [backend addresses]
  :unit
  (Datascript_runtime.Storage_backend.delete backend addresses))

(defn make-backend
  [store-fn
   restore-fn
   list-addresses-fn
   delete-fn]
  (Datascript_runtime.Storage_backend.create
   store-fn restore-fn list-addresses-fn delete-fn))

(defn- adapt-storage [storage]
  (if (satisfies? ClosedStorage storage)
    (closed-storage storage)
    (Datascript_runtime.Storage_backend.create
     (fn [address-data delete-addresses]
       #?(:clj
          (do
            (-store storage address-data)
            (when (seq delete-addresses)
              (-delete storage delete-addresses))
            (Stdlib.ignore 0))
          :cljs
          (-store storage address-data delete-addresses)))
     (fn [address]
       (-restore storage address))
     (fn [_ignored]
       #?(:clj
          (-list-addresses storage)
          :cljs
          []))
     (fn [addresses]
       #?(:clj
          (-delete storage addresses)
          :cljs
          (Stdlib.ignore addresses))))))

(signature datascript.storage/maybe-adapt-storage
  :fn<datascript.db/database-options;datascript.db/database-options>)
(defn maybe-adapt-storage
  [opts]
  (if-some [storage (db/options-storage opts)]
    (db/options-assoc-storage opts (adapt-storage storage))
    opts))

(defn- same-backend?
  [left right]
  (Datascript_runtime.Storage_backend.equal left right))

(type-record restoration
  (database :datascript.db/DB)
  (stored-database :datascript.db/DB)
  (tail :vector<vector<datascript.db/Datom>>))

(def ^:private root-addr 0)
(def ^:private tail-addr 1)
(defonce ^:private next-address (volatile! 1000000))
;; The mutable module registry has no element evidence at initialization.
(defonce ^:private
  ^:ref<vector<weak<datascript.db/DB>>> stored-databases
  (volatile! []))

(defn- generate-address []
  (vswap! next-address inc))

(defn- remember-database [database]
  (vswap! stored-databases conj (weak-ref database))
  nil)

(signature datascript.storage/serializable-datom
  :fn<datascript.db/Datom;serialized-datom>)

(defn serializable-datom [datom]
  (Datascript_runtime.Storage_value.serialized_keyword_datom
   (.-e datom) (db/datom-attr datom) (.-v datom) (.-tx datom)))

(defn- restore-datom [datom]
  (db/datom
   (Datascript_runtime.Storage_value.datom_e datom)
   (keyword (Datascript_runtime.Storage_value.datom_a datom))
   (Datascript_runtime.Storage_value.datom_v datom)
   (Datascript_runtime.Storage_value.datom_tx datom)))

(defn- serialize-datoms
  [datoms]
  (loop [idx 0
         result []]
    (if (< idx (count datoms))
      (recur
       (inc idx)
       (conj result (serializable-datom (Rrbvec.nth datoms idx))))
      result)))

(defn- serialize-address [address]
  (match address
    (Some value) value
    None (Stdlib.failwith "stored node has no child address")))

(defn- restore-address [address]
  (Some address))

(defn- serialize-node
  [node]
  (let [^:array<datascript.db/Datom> node-keys (set/node-keys node)
        keys
        (loop [idx 0
               result []]
          (if (< idx (arrays/alength node-keys))
            (recur
             (inc idx)
             (conj
              result
              (serializable-datom (arrays/aget node-keys idx))))
            result))]
    (Datascript_runtime.Storage_value.serialized_node
     keys
     (if (= 0 (set/node-child-count node))
       nil
       (mapv serialize-address
             (array-seq (set/node-addresses node)))))))

(defn- restore-node [data address]
  (let [keys
        (arrays/into-array
         (map restore-datom
              (Datascript_runtime.Storage_value.node_keys data)))]
    (if-some [addresses
              (Datascript_runtime.Storage_value.node_addresses data)]
      (Some
       (set/new-restored-node
        keys
        (arrays/into-array (map restore-address addresses))
        address))
      (Some (set/new-restored-leaf keys address)))))

(signature datascript.storage/make-storage-adapter
  :fn<storage-backend;unit;set/storage<datascript.db/Datom;storage-backend;tuple<int;stored_value>>>)

(defn make-storage-adapter [backend _opts]
  (let [pending-deletes (volatile! (arrays/empty-array))
        write-buffer (volatile! [])]
    (set/make-storage-with-owner
     (fn [address]
       (if-some [stored (-restore backend address)]
         (match stored
           (Datascript_runtime.Storage_value.Stored_node data)
           (restore-node data address)
           _ (Stdlib.failwith "storage address does not contain a tree node"))
         nil))
     (fn [_address]
       (Stdlib.ignore _address))
     (fn [node _previous-address]
       (let [address (generate-address)]
         (vswap!
          write-buffer
          conj
          (tuple
           address
           (Datascript_runtime.Storage_value.Stored_node
            (serialize-node node))))
         address))
     (fn [unused-addresses]
       (Stdlib.ignore
        (vreset!
         pending-deletes
         (arrays/aconcat @pending-deletes unused-addresses))))
     backend
     pending-deletes
     (fn [_]
       (let [entries @write-buffer]
         (vreset! write-buffer [])
         entries)))))

(signature datascript.storage/storage-adapter
  :fn<datascript.db/DB;option<storage-backend>>)
(defn storage-adapter [database]
  (when database
    (match (set/set-storage (:eavt database))
      (Some adapter) adapter
      None nil)))

(defn storage [database]
  (when-some [adapter (storage-adapter database)]
    (match (set/storage-owner adapter)
      (Some backend) backend
      None nil)))

(signature datascript.storage/addresses
  :fn<vector<datascript.db/DB>;set<int>>)
(defn addresses [databases]
  (reduce
   (fn [used database]
     (reduce
      (fn [used address]
        (conj used address))
      used
      (concat
       (set/set-addresses (:eavt database))
       (set/set-addresses (:aevt database))
       (set/set-addresses (:avet database)))))
   #{root-addr tail-addr}
   databases))

(defn- alive-databases []
  (let [databases
        (volatile! [])
        references
        (reduce
         (fn [alive reference]
           (if-some [database (weak-deref reference)]
             (do
               (vswap! databases conj database)
               (conj alive reference))
             alive))
         []
         @stored-databases)
        _updated (vreset! stored-databases references)]
    @databases))

(defn- databases-for-storage
  [backend]
  (reduce
   (fn [databases database]
     (if-some [database-backend (storage database)]
       (if (same-backend? backend database-backend)
         (conj databases database)
         databases)
       databases))
   []
   (alive-databases)))

(defn- adapter-backend [adapter]
  (match (set/storage-owner adapter)
    (Some backend) backend
    None (Stdlib.invalid_arg "Storage adapter has no owner")))

(defn- set-metadata [values address]
  (Datascript_runtime.Storage_value.serialized_index
   address (set/set-shift values) (set/set-count values)))

(defn- serialize-schema
  [schema]
  (reduce-kv
   (fn [result attr properties]
     (assoc
      result
      (str attr)
      (reduce-kv
       (fn [serialized property value]
         (assoc serialized (str property) value))
       {}
       properties)))
   {}
   schema))

(signature datascript.storage/restore-schema
  :fn<map<string;map<string;Datascript_runtime.Data_value.t>>;map<keyword;map<keyword;Datascript_runtime.Data_value.t>>>)
(defn- restore-schema
  [schema]
  (reduce-kv
   (fn [result attr properties]
     (assoc
      result
      (keyword attr)
      (reduce-kv
       (fn [restored property value]
         (assoc restored (keyword property) value))
       {}
       properties)))
   {}
   schema))

(signature datascript.storage/store-impl!
  :fn<datascript.db/DB;set/storage<datascript.db/Datom;storage-backend;tuple<int;stored-value>>;bool;datascript.db/DB>)

(defn store-impl! [database adapter force?]
  (let [_remembered (remember-database database)
        eavt-address (set/store (:eavt database) adapter)
        aevt-address (set/store (:aevt database) adapter)
        avet-address (set/store (:avet database) adapter)
        entries
        (set/storage-drain-writes adapter)
        pending-ref (set/storage-pending-deletes adapter)
        settings (set/settings (:eavt database))
        root
        (Datascript_runtime.Storage_value.serialized_root
         (if-some [schema (:schema database)]
           (Some (serialize-schema schema))
           None)
         (:max-eid database)
         (:max-tx database)
         eavt-address
         aevt-address
         avet-address
         (set-metadata (:eavt database) eavt-address)
         (set-metadata (:aevt database) aevt-address)
         (set-metadata (:avet database) avet-address)
         @next-address
         (:branching-factor settings)
         (:ref-type settings))]
    (when (or force?
              (pos? (count entries)))
      (store-backend
       (adapter-backend adapter)
       (conj
        entries
        (tuple
         root-addr
         (Datascript_runtime.Storage_value.Stored_root root))
        (tuple
         tail-addr
         (Datascript_runtime.Storage_value.Stored_tail [])))
       [])
      (vreset! pending-ref (arrays/empty-array))
      nil)
    database))

(defn store
  ([database]
   (if-some [adapter (storage-adapter database)]
     (store-impl! database adapter false)
     (Stdlib.invalid_arg "Database has no associated storage")))
  ([database storage]
   (let [backend (adapt-storage storage)]
     (if-some [adapter (storage-adapter database)]
       (let [current-backend (adapter-backend adapter)]
         (if (same-backend? current-backend backend)
           (store-impl! database adapter false)
           (Stdlib.invalid_arg
            "Database is already stored with another storage backend")))
       (store-impl!
        database
        (make-storage-adapter backend (Stdlib.ignore 0))
        false)))))

(defn store-tail
  [database tail]
  (if-some [backend (storage database)]
    (store-backend
     backend
     [(tuple
       tail-addr
        (Datascript_runtime.Storage_value.Stored_tail
        (mapv
         serialize-datoms
         tail)))]
     [])
    (Stdlib.invalid_arg "Database has no associated storage")))

(defn- restore-index
  [comparator
   metadata
   adapter
   ref-type
   branching-factor]
  (set/restore-by
   comparator
   (Datascript_runtime.Storage_value.index_address metadata)
   adapter
   (Datascript_runtime.Storage_value.index_shift metadata)
   (Datascript_runtime.Storage_value.index_count metadata)
   ref-type
   branching-factor))

(declare db-with-tail)

(defn restore-impl [backend opts]
  (when-some [stored-root (-restore backend root-addr)]
    (match stored-root
      (Datascript_runtime.Storage_value.Stored_root root)
      (let [tail
            (if-some [stored-tail (-restore backend tail-addr)]
              (match stored-tail
                (Datascript_runtime.Storage_value.Stored_tail tail) tail
                _ (Stdlib.failwith
                   "storage tail address has the wrong payload"))
              [])
          _max-address
          (vswap!
           next-address
           max
           (Datascript_runtime.Storage_value.root_max_address root))
          adapter (make-storage-adapter backend (Stdlib.ignore 0))
          ref-type (Datascript_runtime.Storage_value.root_ref_type root)
          branching-factor
          (Datascript_runtime.Storage_value.root_branching_factor root)
          stored-database
          (db/restore-db-from-storage
           (if-some
             [schema
              (Datascript_runtime.Storage_value.root_schema root)]
             (Some (restore-schema schema))
             None)
           (restore-index
            db/cmp-datoms-eavt
            (Datascript_runtime.Storage_value.root_eavt_metadata root)
            adapter
            ref-type
            branching-factor)
           (restore-index
            db/cmp-datoms-aevt
            (Datascript_runtime.Storage_value.root_aevt_metadata root)
            adapter
            ref-type
            branching-factor)
           (restore-index
            db/cmp-datoms-avet
            (Datascript_runtime.Storage_value.root_avet_metadata root)
            adapter
            ref-type
            branching-factor)
           (Datascript_runtime.Storage_value.root_max_eid root)
           (Datascript_runtime.Storage_value.root_max_tx root))
          restored-tail
          (mapv
           (fn [datoms]
             (mapv restore-datom (Rrbvec.to_list datoms)))
           (Rrbvec.to_list tail))
          database (db-with-tail stored-database restored-tail)
          _remembered (remember-database database)]
      (record restoration
              (database database)
              (stored-database stored-database)
              (tail restored-tail)))
      _ (Stdlib.failwith "storage root address has the wrong payload"))))

(defn- db-with-tail-datoms
  [database datoms]
  ;; Replay a tail group through transaction semantics so cardinality and
  ;; uniqueness constraints match the original transaction.
  (try
    (let [tx (.-tx (nth datoms 0))
          database' (assoc database :max-tx (dec tx))]
      (:db-after
       (db/transact-tx-data
        (db/->TxReport database' database' [] {}
                       (Datascript_runtime.Data_value.Map (list))
                       {} {}
                       (db/empty-used-tempid-eids))
        (mapv db/datom->tx-entry datoms))))
    (catch _ (do database))))

(signature datascript.storage/db-with-tail
  :fn<datascript.db/DB;vector<vector<datascript.db/Datom>>;datascript.db/DB>)
(defn db-with-tail
  [database tail]
  (reduce
   (fn [current datoms]
     (if (empty? datoms)
       current
       (assoc
        (db-with-tail-datoms current datoms)
        :max-tx (.-tx (nth datoms 0)))))
   database
   tail))

(defn restore
  ([storage]
   (restore storage {}))
  ([storage opts]
   (if-some [result (restore-impl (adapt-storage storage) opts)]
     (:database result)
     nil)))

(defn- collect-garbage-backend [backend]
  (let [current (restore backend)
        databases
        (if-some [database current]
          (conj (databases-for-storage backend) database)
          (databases-for-storage backend))
        used (addresses databases)
        unused
        (reduce
         (fn [result address]
           (if (contains? used address)
             result
             (conj result address)))
         []
         (list-backend-addresses backend))]
    (delete-backend-addresses backend unused)
    unused))

#?(:clj
   (defn collect-garbage [storage]
     (collect-garbage-backend (adapt-storage storage)))
   :cljs
   (defn collect-garbage [backend]
     (collect-garbage-backend backend)))
