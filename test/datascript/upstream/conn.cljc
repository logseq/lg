(ns datascript.conn
  (:require
    [datascript.db :as db]
    [datascript.storage :as storage]
    [me.tonsky.persistent-sorted-set :as set]))

(type-record conn-state
  (db :datascript.db/DB)
  (tx-tail :vector<vector<datascript.db/Datom>>)
  (db-last-stored :option<datascript.db/DB>)
  (listeners :map<string;fn<datascript.db/TxReport;unit>>)
  (skip-store? :bool))

(type-record Conn
  (state-ref :ref<conn-state>)
  (atom :ref<conn-state>))

(defn- ^Conn make-conn
  [^datascript.db/DB database
   ^:vector<vector<datascript.db/Datom>> tx-tail
   ^:option<datascript.db/DB> db-last-stored]
  (let [state-reference
        (atom
         (record conn-state
                 (db database)
                 (tx-tail tx-tail)
                 (db-last-stored db-last-stored)
                 (listeners {})
                 (skip-store? false)))]
    (record Conn
            (state-ref state-reference)
            (atom state-reference))))

(defn- ^datascript.db/DB state-db [^conn-state state]
  (:db state))

(defn- ^:vector<vector<datascript.db/Datom>> state-tx-tail
  [^conn-state state]
  (:tx-tail state))

(defn- ^:option<datascript.db/DB> state-db-last-stored [^conn-state state]
  (:db-last-stored state))

(defn- ^:map<string;fn<datascript.db/TxReport;unit>> state-listeners
  [^conn-state state]
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

(defn- state-with-listeners
  [^conn-state state
   ^:map<string;fn<datascript.db/TxReport;unit>> listeners]
  (record conn-state
          (db (state-db state))
          (tx-tail (state-tx-tail state))
          (db-last-stored (state-db-last-stored state))
          (listeners listeners)
          (skip-store? (state-skip-store? state))))

(defn- ^datascript.db/DB swap-db!
  [^Conn conn ^:fn<datascript.db/DB;datascript.db/DB> f]
  (let [state-atom (:state-ref conn)
        state @state-atom
        database (f (state-db state))]
    (reset! state-atom (state-with-db state database))
    database))

(defn- ^datascript.db/DB reset-db!
  [^Conn conn ^datascript.db/DB database]
  (let [state-atom (:state-ref conn)
        state @state-atom]
    (reset! state-atom (state-with-db state database))
    database))

(defn ^datascript.db/TxReport with
  ([^datascript.db/DB database ^:vector<datascript.db/tx-entry> tx-data]
   (with database tx-data {}))
  ([^datascript.db/DB database
    ^:vector<datascript.db/tx-entry> tx-data
   ^:map<keyword;Datascript_runtime.Data_value.t> tx-meta]
   {:pre [(db/db? database)]}
   (if (instance? db/FilteredDB database)
     (Stdlib.invalid_arg "Filtered DB cannot be modified")
     (db/transact-tx-data
       (db/->TxReport database database [] {} tx-meta {} {})
       tx-data))))

(defn ^datascript.db/DB db-with
  [^datascript.db/DB database
   ^:vector<datascript.db/tx-entry> tx-data]
  {:pre [(db/db? database)]}
  (:db-after (with database tx-data)))

(defn conn? [^Conn conn]
  (if-some [database (current-db conn)]
    (db/db? database)
    true))

(defn ^Conn conn-from-db [^datascript.db/DB database]
  {:pre [(db/db? database)]}
  (if-some [database-storage (storage/storage database)]
    (do
      (storage/store database)
      (make-conn database [] (Some database)))
    (make-conn database [] nil)))

(defn ^Conn conn-from-datoms
  ([datoms]
   (conn-from-db (db/init-db (to-array datoms) db/empty-schema)))
  ([datoms schema]
   (conn-from-db (db/init-db (to-array datoms) schema)))
  ([datoms schema opts]
   (conn-from-db
     (db/init-db (to-array datoms) schema))))

(defn ^Conn create-conn
  ([]
   (conn-from-db (db/empty-db None (db/default-options))))
  ([^:map<keyword;map<keyword;Datascript_runtime.Data_value.t>> schema]
   (conn-from-db (db/empty-db (Some schema) (db/default-options))))
  ([^:option<map<keyword;map<keyword;Datascript_runtime.Data_value.t>>> schema
    ^datascript.db/database-options opts]
   (let [database (db/empty-db schema (db/default-options))
         _stored
         (when-some [backend (db/options-storage opts)]
           (Stdlib.ignore (storage/store database backend)))]
     (conn-from-db database))))

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

(defn store-after-transact!
  [^Conn conn ^datascript.db/TxReport tx-report]
  :unit
  (Stdlib.ignore
   (when-not (state-skip-store? @(:state-ref conn))
     (when-some [adapter (storage/storage-adapter (current-db conn))]
       (let [database (.-db-after tx-report)
             datoms (.-tx-data tx-report)
             settings (set/settings (:eavt database))
             state-atom (:state-ref conn)
             state @state-atom
             tx-tail (conj (state-tx-tail state) datoms)
             _state (reset! state-atom (state-with-tail state tx-tail))]
         (when-not (get-in tx-report [:tx-meta :skip-store?])
           (if (> (transduce (map count) + 0 tx-tail)
                  (:branching-factor settings))
             (do
               (Stdlib.ignore
                (storage/store-impl!
                 database
                 adapter
                 false))
               (Stdlib.ignore
                (let [state @state-atom]
                  (reset! state-atom
                          (state-with-storage
                           state database [] (Some database))))))
             (storage/store-tail database tx-tail))))))))

(defn -transact!
  [^Conn conn
   ^:vector<datascript.db/tx-entry> tx-data
   ^:map<keyword;Datascript_runtime.Data_value.t> tx-meta]
  {:pre [(conn? conn)]}
  (let [^:ref<option<datascript.db/TxReport>> report-ref (volatile! nil)
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

(defn run-callbacks [^Conn conn ^datascript.db/TxReport report]
  (let [state @(:state-ref conn)]
    (doseq [[_ callback] (state-listeners state)]
      (callback report))))

(defn transact!
  ([^Conn conn ^:vector<datascript.db/tx-entry> tx-data]
   (transact! conn tx-data {}))
  ([^Conn conn
    ^:vector<datascript.db/tx-entry> tx-data
    ^:map<keyword;Datascript_runtime.Data_value.t> tx-meta]
   {:pre [(conn? conn)]}
   (let [report (-transact! conn tx-data tx-meta)]
     (run-callbacks conn report)
     report)))

(defn reset-conn!
  ([^Conn conn ^datascript.db/DB database]
   (reset-conn! conn database {}))
  ([^Conn conn
    ^datascript.db/DB database
    ^:map<keyword;Datascript_runtime.Data_value.t> tx-meta]
   {:pre [(conn? conn)
          (db/db? database)]}
   (let [db-before (current-db conn)
         report
         (db/->TxReport
          db-before
          database
          (vec
           (concat
            (map
             (fn [^datascript.db/Datom datom]
               (db/datom
                (.-e datom)
                (db/datom-attr datom)
                (.-v datom)
                (db/datom-tx datom)
                false))
             (db/-datoms db-before :eavt nil nil nil nil))
            (db/-datoms database :eavt nil nil nil nil)))
          {}
          tx-meta
          {}
          {})]
     (if-some [database-storage (storage/storage db-before)]
       (do
         (Stdlib.ignore (storage/store database))
         (Stdlib.ignore
          (let [state-atom (:state-ref conn)
                state @state-atom]
            (reset! state-atom
                    (state-with-storage
                     state database [] (Some database))))))
       (Stdlib.ignore (reset-db! conn database)))
     (run-callbacks conn report)
     database)))

(defn reset-schema!
  [^Conn conn
   ^:map<keyword;map<keyword;Datascript_runtime.Data_value.t>> schema]
  {:pre [(conn? conn)]}
  (let [database
        (swap-db!
         conn
         (fn [^datascript.db/DB database]
           (db/with-schema database schema)))]
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
  ([^Conn conn ^:fn<datascript.db/TxReport;unit> callback]
   (listen! conn (Float.to_string (rand)) callback))
  ([^Conn conn ^:string key ^:fn<datascript.db/TxReport;unit> callback]
   {:pre [(conn? conn)]}
   (let [state-atom (:state-ref conn)
         state @state-atom
         listeners (assoc (state-listeners state) key callback)]
     (reset! state-atom (state-with-listeners state listeners)))
   key))

(defn unlisten! [^Conn conn ^:string key]
  {:pre [(conn? conn)]}
  (let [state-atom (:state-ref conn)
        state @state-atom
        listeners (dissoc (state-listeners state) key)]
    (reset! state-atom (state-with-listeners state listeners))))
