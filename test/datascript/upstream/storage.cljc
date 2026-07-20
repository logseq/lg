(ns datascript.storage
  (:require
   [datascript.db :as db]
   [me.tonsky.persistent-sorted-set :as set]
   [me.tonsky.persistent-sorted-set.arrays :as arrays]))

(defprotocol IStorage
  (-store [^:dynamic backend address-data delete-addresses])
  (-restore [^:dynamic backend address])
  (-list-addresses [^:dynamic backend])
  (-delete [^:dynamic backend addresses]))

(type-record restoration
  (database :datascript.db/DB)
  (stored-database :datascript.db/DB)
  (tail :vector<vector<datascript.db/Datom>>))

(def ^:private root-address 0)
(def ^:private tail-address 1)
(defonce ^:private next-address (volatile! 1000000))
(defonce ^:private stored-databases (volatile! []))

(defn- generate-address []
  (vswap! next-address inc))

(defn- remember-database [^datascript.db/DB database]
  (vswap! stored-databases conj (weak-ref database))
  nil)

(defn serializable-datom [^datascript.db/Datom datom]
  [(.-e datom) (.-a datom) (.-v datom) (.-tx datom)])

(defn- restore-datom [[e a v tx]]
  (db/datom (int e) a v (int tx)))

(defn- serialize-address [address]
  (match address
    (Some value) (Int64.to_int value)
    None (Stdlib.failwith "stored node has no child address")))

(defn- restore-address [^:int address]
  (Some (Int64.of_int address)))

(defn- serialize-node [node]
  (let [keys
        (mapv serializable-datom
              (array-seq (set/node-keys node)))]
    {:keys keys
     :addresses
     (if (= 0 (set/node-child-count node))
       nil
       (mapv serialize-address
             (array-seq (set/node-addresses node))))}))

(defn make-storage-adapter [^:dynamic backend _opts]
  (let [pending-deletes (volatile! (arrays/empty-array))
        write-buffer (volatile! [])]
    (set/make-storage-with-owner
     (fn [address]
       (if-some [data (-restore backend (Int64.to_int address))]
         (let [keys
               (arrays/into-array
                (map restore-datom (:keys data)))]
           (if-some [addresses (:addresses data)]
             (Some
              (set/new-restored-node
               keys
               (arrays/into-array (map restore-address addresses))
               address))
             (Some (set/new-restored-leaf keys address))))
         nil))
     (fn [_address]
       (Stdlib.ignore _address))
     (fn [node _previous-address]
       (let [address (generate-address)]
         (vswap! write-buffer conj [address (serialize-node node)])
         (Int64.of_int address)))
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

(defn storage-adapter [^datascript.db/DB database]
  (when database
    (match (set/set-storage (:eavt database))
      (Some adapter) adapter
      None nil)))

(defn storage [^datascript.db/DB database]
  (when-some [adapter (storage-adapter database)]
    (match (set/storage-owner adapter)
      (Some backend) backend
      None nil)))

(defn addresses [^:vector<datascript.db/DB> databases]
  (reduce
   (fn [^:set<int> used ^datascript.db/DB database]
     (reduce
      (fn [^:set<int> used address]
        (conj used (Int64.to_int address)))
      used
      (concat
       (set/set-addresses (:eavt database))
       (set/set-addresses (:aevt database))
       (set/set-addresses (:avet database)))))
   #{root-address tail-address}
   databases))

(defn- alive-databases []
  (let [databases (volatile! [])
        references
        (reduce
         (fn [^:vector<weak<datascript.db/DB>> alive
              ^:weak<datascript.db/DB> reference]
           (if-some [database (weak-deref reference)]
             (do
               (vswap! databases conj database)
               (conj alive reference))
             alive))
         []
         @stored-databases)
        _updated (vreset! stored-databases references)]
    @databases))

(defn- databases-for-storage [^:dynamic backend]
  (reduce
   (fn [databases ^datascript.db/DB database]
     (if (identical? backend (storage database))
       (conj databases database)
       databases))
   []
   (alive-databases)))

(defn- adapter-backend [adapter]
  (match (set/storage-owner adapter)
    (Some backend) backend
    None (throw (ex-info "Storage adapter has no owner" {}))))

(defn- set-metadata [values address]
  {:address (Int64.to_int address)
   :shift (set/set-shift values)
   :count (set/set-count values)})

(defn store-impl! [^datascript.db/DB database adapter force?]
  (let [_remembered (remember-database database)
        eavt-address (set/store (:eavt database) adapter)
        aevt-address (set/store (:aevt database) adapter)
        avet-address (set/store (:avet database) adapter)
        entries (set/storage-drain-writes adapter)
        pending-ref (set/storage-pending-deletes adapter)
        root
        (merge
         {:schema (:schema database)
          :max-eid (:max-eid database)
          :max-tx (:max-tx database)
          :eavt (Int64.to_int eavt-address)
          :aevt (Int64.to_int aevt-address)
          :avet (Int64.to_int avet-address)
          :eavt-metadata (set-metadata (:eavt database) eavt-address)
          :aevt-metadata (set-metadata (:aevt database) aevt-address)
          :avet-metadata (set-metadata (:avet database) avet-address)
          :max-address @next-address}
         (set/settings (:eavt database)))]
    (when (or force?
              (pos? (count entries)))
      (-store
       (adapter-backend adapter)
       (conj entries [root-address root] [tail-address []])
       [])
      (vreset! pending-ref (arrays/empty-array))
      nil)
    database))

(defn store
  ([^datascript.db/DB database]
   (if-some [adapter (storage-adapter database)]
     (store-impl! database adapter false)
     (throw (ex-info "Database has no associated storage" {}))))
  ([^datascript.db/DB database backend]
   (if-some [adapter (storage-adapter database)]
     (let [current-backend (adapter-backend adapter)]
       (if (identical? current-backend backend)
         (store-impl! database adapter false)
         (throw
          (ex-info
           "Database is already stored with another IStorage"
           {:storage current-backend}))))
     (store-impl!
      database
      (make-storage-adapter backend (set/settings (:eavt database)))
      false))))

(defn store-tail [^datascript.db/DB database tail]
  (if-some [backend (storage database)]
    (-store
     backend
     [[tail-address
       (mapv
        (fn [datoms]
          (mapv serializable-datom datoms))
        tail)]]
     [])
    (throw (ex-info "Database has no associated storage" {}))))

(defn- serialized-value [^:dynamic value key]
  (get value key))

(defn- serialized-int [^:dynamic value key]
  (int (get value key)))

(defn- restore-index
  [comparator ^:int address ^:int shift ^:int count adapter]
  (set/restore-by
   comparator
   (Int64.of_int address)
   adapter
   shift
   count))

(declare db-with-tail)

(defn restore-impl [^:dynamic backend opts]
  (when-some [root (-restore backend root-address)]
    (let [tail (or (-restore backend tail-address) [])
          _max-address
          (vswap! next-address max (serialized-int root :max-address))
          adapter (make-storage-adapter backend (merge root opts))
          stored-database
          (db/restore-db
           {:schema (serialized-value root :schema)
            :eavt
            (restore-index
             db/cmp-datoms-eavt
             (serialized-int
              (serialized-value root :eavt-metadata) :address)
             (serialized-int
              (serialized-value root :eavt-metadata) :shift)
             (serialized-int
              (serialized-value root :eavt-metadata) :count)
             adapter)
            :aevt
            (restore-index
             db/cmp-datoms-aevt
             (serialized-int
              (serialized-value root :aevt-metadata) :address)
             (serialized-int
              (serialized-value root :aevt-metadata) :shift)
             (serialized-int
              (serialized-value root :aevt-metadata) :count)
             adapter)
            :avet
            (restore-index
             db/cmp-datoms-avet
             (serialized-int
              (serialized-value root :avet-metadata) :address)
             (serialized-int
              (serialized-value root :avet-metadata) :shift)
             (serialized-int
              (serialized-value root :avet-metadata) :count)
             adapter)
            :max-eid (serialized-int root :max-eid)
            :max-tx (serialized-int root :max-tx)})
          restored-tail
          (mapv
           (fn [datoms]
             (mapv restore-datom datoms))
           tail)
          database (db-with-tail stored-database restored-tail)
          _remembered (remember-database database)]
      (record restoration
              (database database)
              (stored-database stored-database)
              (tail restored-tail)))))

(defn db-with-tail
  [^datascript.db/DB database
   ^:vector<vector<datascript.db/Datom>> tail]
  (reduce
   (fn [current ^datascript.db/Datom datom]
     (assoc
      (db/with-datom current datom)
      :max-tx (:tx datom)))
   database
   (mapcat (fn [datoms] datoms) tail)))

(defn restore
  ([^:dynamic backend]
   (restore backend {}))
  ([^:dynamic backend opts]
   (if-some [result (restore-impl backend opts)]
     (:database result)
     nil)))

(defn collect-garbage [^:dynamic backend]
  (let [current (restore backend)
        databases
        (if-some [database current]
          (conj (databases-for-storage backend) database)
          (databases-for-storage backend))
        used (addresses databases)
        unused
        (reduce
         (fn [^:vector<int> result ^:int address]
           (if (contains? used address)
             result
             (conj result address)))
         []
         (-list-addresses backend))]
    (-delete backend unused)
    unused))

(defn ^:dynamic maybe-adapt-storage [^:dynamic opts]
  (if-some [backend (:storage opts)]
    (update opts :storage make-storage-adapter opts)
    opts))
