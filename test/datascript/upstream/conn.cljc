(ns datascript.conn
  (:require
    [datascript.db :as db]
    [datascript.storage :as storage]
    [me.tonsky.persistent-sorted-set :as set]))

(type-record conn-state
  (db :datascript.db/DB)
  (tx-tail :vector<vector<datascript.db/Datom>>)
  (db-last-stored :option<datascript.db/DB>)
  (listeners :dynamic)
  (skip-store? :bool))

(type-record Conn
  (state-ref :ref<conn-state>))

(defn- ^Conn make-conn
  [^datascript.db/DB database
   ^:vector<vector<datascript.db/Datom>> tx-tail
   ^:option<datascript.db/DB> db-last-stored]
  (record Conn
          (state-ref
           (atom
           (record conn-state
                   (db database)
                   (tx-tail tx-tail)
                   (db-last-stored db-last-stored)
                   (listeners {})
                   (skip-store? false))))))

(defn- ^datascript.db/DB state-db [^conn-state state]
  (:db state))

(defn- ^:vector<vector<datascript.db/Datom>> state-tx-tail
  [^conn-state state]
  (:tx-tail state))

(defn- ^:option<datascript.db/DB> state-db-last-stored [^conn-state state]
  (:db-last-stored state))

(defn- ^:dynamic state-listeners [^conn-state state]
  (:listeners state))

(defn- ^boolean state-skip-store? [^conn-state state]
  (:skip-store? state))

(defn ^datascript.db/DB current-db [^Conn conn]
  (state-db @(:state-ref conn)))

(defn- state-with-db [^conn-state state ^datascript.db/DB database]
  (record conn-state
          (db database)
          (tx-tail (state-tx-tail state))
          (db-last-stored (state-db-last-stored state))
          (listeners (state-listeners state))
          (skip-store? (state-skip-store? state))))

(defn- state-with-storage
  [^conn-state state
   ^datascript.db/DB database
   ^:vector<vector<datascript.db/Datom>> tx-tail
   ^:option<datascript.db/DB> db-last-stored]
  (record conn-state
          (db database)
          (tx-tail tx-tail)
          (db-last-stored db-last-stored)
          (listeners (state-listeners state))
          (skip-store? (state-skip-store? state))))

(defn- state-with-tail
  [^conn-state state ^:vector<vector<datascript.db/Datom>> tx-tail]
  (record conn-state
          (db (state-db state))
          (tx-tail tx-tail)
          (db-last-stored (state-db-last-stored state))
          (listeners (state-listeners state))
          (skip-store? (state-skip-store? state))))

(defn- state-with-listeners [^conn-state state ^:dynamic listeners]
  (record conn-state
          (db (state-db state))
          (tx-tail (state-tx-tail state))
          (db-last-stored (state-db-last-stored state))
          (listeners listeners)
          (skip-store? (state-skip-store? state))))

(defn- swap-db!
  ([^Conn conn f]
   (let [state-atom (:state-ref conn)
         state @state-atom
         database (f (state-db state))]
     (reset! state-atom (state-with-db state database))
     database))
  ([^Conn conn f arg]
   (let [state-atom (:state-ref conn)
         state @state-atom
         database (f (state-db state) arg)]
     (reset! state-atom (state-with-db state database))
     database)))

(defn- reset-db! [^Conn conn database]
  (let [state-atom (:state-ref conn)
        state @state-atom]
    (reset! state-atom (state-with-db state database))
    database))

(defn ^datascript.db/TxReport with
  ([^datascript.db/DB database ^:dynamic tx-data]
   (with database tx-data nil))
  ([^datascript.db/DB database ^:dynamic tx-data tx-meta]
   {:pre [(db/db? database)]}
   (if (instance? db/FilteredDB database)
     (throw
       (ex-info
         "Filtered DB cannot be modified"
         {:error :transaction/filtered}))
     (db/transact-tx-data
       (db/->TxReport database database [] {} tx-meta)
       tx-data))))

(defn ^datascript.db/DB db-with [^datascript.db/DB database ^:dynamic tx-data]
  {:pre [(db/db? database)]}
  (:db-after (with database tx-data)))

(defn conn? [^Conn conn]
  (if-some [database (current-db conn)]
    (db/db? database)
    true))

(defn conn-from-db [database]
  {:pre [(db/db? database)]}
  (if-some [database-storage (storage/storage database)]
    (do
      (storage/store database)
      (make-conn database [] (Some database)))
    (make-conn database [] nil)))

(defn conn-from-datoms
  ([datoms]
   (conn-from-db (db/init-db datoms nil {})))
  ([datoms schema]
   (conn-from-db (db/init-db datoms schema {})))
  ([datoms schema opts]
   (conn-from-db
     (db/init-db datoms schema (storage/maybe-adapt-storage opts)))))

(defn create-conn
  ([]
   (conn-from-db (db/empty-db nil {})))
  ([schema]
   (conn-from-db (db/empty-db schema {})))
  ([schema opts]
   (conn-from-db
     (db/empty-db schema (storage/maybe-adapt-storage opts)))))

(defn restore-conn
  ([database-storage]
   (restore-conn database-storage {}))
  ([database-storage opts]
   (when-some [restored
               (storage/restore-impl database-storage opts)]
     (let [database (:database restored)
           stored-database (:stored-database restored)
           tail (:tail restored)]
     (make-conn database tail (Some stored-database))))))

(defn store-after-transact! [^Conn conn tx-report]
  (when-not (state-skip-store? @(:state-ref conn))
    (when-some [adapter (storage/storage-adapter (current-db conn))]
      (let [{database :db-after
             tx-data :tx-data} tx-report
            datoms (mapv (fn [^datascript.db/Datom datom] datom) tx-data)
            settings (set/settings (:eavt database))
            state-atom (:state-ref conn)
            state @state-atom
            tx-tail (conj (state-tx-tail state) datoms)
            _state (reset! state-atom (state-with-tail state tx-tail))]
        (when-not (get-in tx-report [:tx-meta :skip-store?])
          (if (> (transduce (map count) + 0 tx-tail)
                 (:branching-factor settings))
            (do
              (storage/store-impl!
                database
                adapter
                false)
              (let [state @state-atom]
                (reset! state-atom
                        (state-with-storage state database [] (Some database)))))
            (storage/store-tail database tx-tail)))))))

(defn -transact! [^Conn conn ^:dynamic tx-data tx-meta]
  {:pre [(conn? conn)]}
  (let [report-ref (volatile! nil)
        tx-meta (dissoc tx-meta :skip-store?)]
    (swap-db!
      conn
      (fn [^datascript.db/DB database]
        (if-some [report (with database tx-data tx-meta)]
          (do
            (vreset! report-ref report)
            (:db-after report))
          database)))
    (let [report @report-ref]
      (store-after-transact! conn report)
      report)))

(defn run-callbacks [^Conn conn report]
  (let [state @(:state-ref conn)]
    (doseq [[_ callback] (state-listeners state)]
      (callback report))))

(defn transact!
  ([^Conn conn ^:dynamic tx-data]
   (transact! conn tx-data nil))
  ([^Conn conn ^:dynamic tx-data tx-meta]
   {:pre [(conn? conn)]}
   (let [report (-transact! conn tx-data tx-meta)]
     (run-callbacks conn report)
     report)))

(defn reset-conn!
  ([^Conn conn database]
   (reset-conn! conn database nil))
  ([^Conn conn database tx-meta]
   {:pre [(conn? conn)
          (db/db? database)]}
   (let [db-before (current-db conn)
         report (db/map->TxReport
                  {:db-before db-before
                   :db-after database
                   :tx-data
                   (concat
                     (when db-before
                       (map
                         #(assoc % :added false)
                         (db/-datoms db-before :eavt nil nil nil nil)))
                     (db/-datoms database :eavt nil nil nil nil))
                   :tx-meta tx-meta})]
     (if-some [database-storage (storage/storage db-before)]
       (do
         (storage/store database)
         (let [state-atom (:state-ref conn)
               state @state-atom]
           (reset! state-atom
                   (state-with-storage state database [] (Some database)))))
       (reset-db! conn database))
     (run-callbacks conn report)
     database)))

(defn reset-schema! [^Conn conn schema]
  {:pre [(conn? conn)]}
  (let [database (swap-db! conn db/with-schema schema)]
    (when-some [adapter (storage/storage-adapter (current-db conn))]
      (storage/store-impl!
        database
        adapter
        true)
      (let [state-atom (:state-ref conn)
            state @state-atom]
        (reset! state-atom
                (state-with-storage state database [] (Some database)))))
    database))

(defn listen!
  ([^Conn conn callback]
   (listen! conn (rand) callback))
  ([^Conn conn key callback]
   {:pre [(conn? conn)]}
   (let [state-atom (:state-ref conn)
         state @state-atom
         listeners (assoc (state-listeners state) key callback)]
     (reset! state-atom (state-with-listeners state listeners)))
   key))

(defn unlisten! [^Conn conn key]
  {:pre [(conn? conn)]}
  (let [state-atom (:state-ref conn)
        state @state-atom
        listeners (dissoc (state-listeners state) key)]
    (reset! state-atom (state-with-listeners state listeners))))
