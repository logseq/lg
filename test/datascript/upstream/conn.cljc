(ns datascript.conn
  (:require
    [datascript.db :as db]
    [datascript.storage :as storage]
    [me.tonsky.persistent-sorted-set :as set]))

(type-record conn-state
  (db :datascript.db/DB)
  (tx-tail :vector<vector<datascript.db/Datom>>)
  (db-last-stored :option<datascript.db/DB>)
  (listeners :map<Datascript_runtime.Data_value.t;fn<datascript.db/TxReport;unit>>)
  (identity-hash :int)
  (skip-store? :bool))

(def ^:private next-conn-identity (atom 0))

(defn- fresh-conn-identity []
  (swap! next-conn-identity
         (fn [current]
           (inc current))))

(defn- state-identity-hash [state]
  (:identity-hash state))

(defrecord Conn
  [^:ref<conn-state> atom]
  IEquiv
  (-equiv [connection other]
    (identical? connection other))
  IHash
  (-hash [connection]
    (state-identity-hash @(:atom connection)))
  IDeref
  (-deref [connection]
    (:db @(:atom connection))))

(defn- make-conn
  [database tx-tail db-last-stored]
  (let [state-atom
        (atom
         (record conn-state
                 (db database)
                 (tx-tail tx-tail)
                 (db-last-stored db-last-stored)
                 (listeners {})
                 (identity-hash (fresh-conn-identity))
                 (skip-store? false)))]
    (record Conn
            (atom state-atom))))

(signature datascript.conn/state-db
  :fn<datascript.conn/conn-state;datascript.db/DB>)
(defn- state-db [state]
  (:db state))

(defn- state-tx-tail
  [state]
  (:tx-tail state))

(defn- state-db-last-stored [state]
  (:db-last-stored state))

(defn- state-listeners
  [state]
  (:listeners state))

(defn- state-skip-store? [state]
  (:skip-store? state))

(defn- tx-meta-value
  [tx-meta]
  (if-some [metadata tx-meta]
    (Datascript_runtime.Data_value.map_of_keyword_map metadata)
    (Datascript_runtime.Data_value.Nil)))

(defn current-db [conn]
  (state-db @(:atom conn)))

(defn- state-with-db [state database]
  (record conn-state
          (db database)
          (tx-tail (state-tx-tail state))
          (db-last-stored (state-db-last-stored state))
          (listeners (state-listeners state))
          (identity-hash (state-identity-hash state))
          (skip-store? (state-skip-store? state))))

(extend-type Conn
  IAtom
  (-compare-and-set!
   [connection old-database new-database]
   (let [state-atom (:atom connection)
         state @state-atom]
     (if (identical? (state-db state) old-database)
       (compare-and-set!
        state-atom
        state
        (state-with-db state new-database))
       false))))

(defn- state-with-storage
  [state database tx-tail db-last-stored]
  (record conn-state
          (db database)
          (tx-tail tx-tail)
          (db-last-stored db-last-stored)
          (listeners (state-listeners state))
          (identity-hash (state-identity-hash state))
          (skip-store? (state-skip-store? state))))

(defn- state-with-tail
  [state tx-tail]
  (record conn-state
          (db (state-db state))
          (tx-tail tx-tail)
          (db-last-stored (state-db-last-stored state))
          (listeners (state-listeners state))
          (identity-hash (state-identity-hash state))
          (skip-store? (state-skip-store? state))))

(defn- state-with-listeners
  [state listeners]
  (record conn-state
          (db (state-db state))
          (tx-tail (state-tx-tail state))
          (db-last-stored (state-db-last-stored state))
          (listeners listeners)
          (identity-hash (state-identity-hash state))
          (skip-store? (state-skip-store? state))))

(defn- swap-db!
  [conn f]
  (let [state-atom (:atom conn)
        state @state-atom
        database (f (state-db state))]
    (reset! state-atom (state-with-db state database))
    database))

(defn- reset-db!
  [conn database]
  (let [state-atom (:atom conn)
        state @state-atom]
    (reset! state-atom (state-with-db state database))
    database))

(extend-type Conn
  IReset
  (-reset!
   [connection database]
   (reset-db! connection database))
  ISwap
  (-swap!
   [connection update-database]
   (swap-db! connection update-database)))

(defn with-closed
  ([database tx-data]
   (with-closed database tx-data None))
  ([^datascript.db/DB database tx-data tx-meta]
   {:pre [(db/db? database)]}
   (if (instance? db/FilteredDB database)
     (Stdlib.invalid_arg "Filtered DB cannot be modified")
     (db/transact-tx-data
       (db/->TxReport database database [] {}
                      (tx-meta-value tx-meta)
                      {} {} (db/empty-used-tempid-eids))
       tx-data))))

(defn with
  {:inline
   (fn [database tx-data & tx-meta]
     (cons
      'datascript.conn/with-closed
      (cons
       database
       (cons
        (list 'datascript.db/tx-data tx-data)
        tx-meta))))}
  ([database tx-data]
   (with-closed database tx-data))
  ([database tx-data tx-meta]
   (with-closed database tx-data tx-meta)))

(defn db-with-closed
  [database tx-data]
  {:pre [(db/db? database)]}
  (:db-after (with-closed database tx-data)))

(defn db-with
  {:inline
   (fn [database tx-data]
     (list
      'datascript.conn/db-with-closed
      database
      (list 'datascript.db/tx-data tx-data)))}
  [database tx-data]
  (db-with-closed database tx-data))

(defn conn? [conn]
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
   (conn-from-db
    (db/init-db-with-schema-option (to-array datoms) None)))
  ([datoms schema]
   (conn-from-db (db/init-db (to-array datoms) schema)))
  ([datoms schema opts]
   (let [opts (storage/maybe-adapt-storage opts)
         database (db/init-db (to-array datoms) schema opts)
         _stored
         (when-some [backend (db/options-storage opts)]
           (Stdlib.ignore
            (storage/store database backend)))]
     (conn-from-db database))))

(defn create-conn-closed
  ([]
   (conn-from-db (db/empty-db None (db/default-options))))
  ([schema]
   (conn-from-db (db/empty-db (Some schema) (db/default-options))))
  ([schema opts]
   (let [opts (storage/maybe-adapt-storage opts)
         database (db/empty-db schema opts)
         _stored
         (when-some [backend (db/options-storage opts)]
           (Stdlib.ignore
            (storage/store database backend)))]
     (conn-from-db database))))

(defn create-conn
  {:inline
   (fn [& arguments]
     (case (count arguments)
       0 (list 'datascript.conn/create-conn-closed)
       1 (list
          'datascript.conn/create-conn-closed
          (list 'datascript.db/schema-map (first arguments)))
       2 (list
          'datascript.conn/create-conn-closed
          (if (map? (first arguments))
            (list
             'Some
             (list 'datascript.db/schema-map (first arguments)))
            (first arguments))
          (second arguments))))}
  ([]
   (create-conn-closed))
  ([schema]
   (create-conn-closed schema))
  ([schema opts]
   (create-conn-closed schema opts)))

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
  [conn tx-report]
  :unit
  (Stdlib.ignore
   (when-not (state-skip-store? @(:atom conn))
     (when-some [adapter (storage/storage-adapter (current-db conn))]
       (let [database (.-db-after tx-report)
             datoms (.-tx-data tx-report)
             settings (set/settings (:eavt database))
             state-atom (:atom conn)
             state @state-atom
             tx-tail (conj (state-tx-tail state) datoms)
             _state (reset! state-atom (state-with-tail state tx-tail))]
         (when-not
          (if-some
            [skip-store
             (Datascript_runtime.Data_value.keyword_map_get
              ":skip-store?"
              (.-tx-meta tx-report))]
            (if-some
              [enabled
               (Datascript_runtime.Data_value.bool_value skip-store)]
              enabled
              false)
            false)
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
  [conn
   tx-data
   ^:option<map<keyword;Datascript_runtime.Data_value.t>> tx-meta]
  {:pre [(conn? conn)]}
  (let [report-ref (volatile! nil)
        tx-meta
        (if-some [metadata tx-meta]
          (Some (dissoc metadata :skip-store?))
          None)]
    (swap-db!
      conn
      (fn [database]
        (if-some [report (with-closed database tx-data tx-meta)]
          (do
            (vreset! report-ref report)
            (:db-after report))
          database)))
    (if-some [report @report-ref]
      (do
        (store-after-transact! conn report)
        report)
      (Stdlib.invalid_arg "Transaction did not produce a report"))))

(signature datascript.conn/run-callbacks
  :fn<datascript.conn/Conn;datascript.db/TxReport;unit>)
(defn run-callbacks [conn report]
  (let [state @(:atom conn)]
    (doseq [[_ callback] (state-listeners state)]
      (callback report))))

(defn transact!
  ([conn tx-data]
   (transact! conn tx-data None))
  ([conn tx-data tx-meta]
   {:pre [(conn? conn)]}
   (let [report (-transact! conn tx-data tx-meta)]
     (run-callbacks conn report)
     report)))

(defn reset-conn!
  ([conn database]
   (reset-conn!
    conn database (Datascript_runtime.Data_value.Nil)))
  ([conn database ^:Datascript_runtime.Data_value.t tx-meta]
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
             (fn [datom]
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
          {}
          (db/empty-used-tempid-eids))]
     (if-some [database-storage (storage/storage db-before)]
       (do
         (Stdlib.ignore (storage/store database))
         (Stdlib.ignore
          (let [state-atom (:atom conn)
                state @state-atom]
            (reset! state-atom
                    (state-with-storage
                     state database [] (Some database))))))
       (Stdlib.ignore (reset-db! conn database)))
     (run-callbacks conn report)
     database)))

(defn reset-schema!
  [conn schema]
  {:pre [(conn? conn)]}
  (let [database
        (swap-db!
         conn
         (fn [database]
           (db/with-schema database schema)))]
    (when-some [adapter (storage/storage-adapter (current-db conn))]
      (storage/store-impl!
        database
        adapter
        true)
      (let [state-atom (:atom conn)
            state @state-atom]
        (reset! state-atom
                (state-with-storage state database [] (Some database)))))
    database))

(defn listen!-closed
  ([conn callback]
   (listen!-closed
    conn
    (Datascript_runtime.Data_value.Float (rand))
    callback))
  ([conn key callback]
   {:pre [(conn? conn)]}
   (let [state-atom (:atom conn)
         state @state-atom
         listener (fn [report] (Stdlib.ignore (callback report)))
         listeners (assoc (state-listeners state) key listener)]
     (reset! state-atom (state-with-listeners state listeners)))
   key))

(defn listen!
  {:inline
   (fn [connection & args]
     (let [key-form
           (fn [key]
             (if (keyword? key)
               (list 'Datascript_runtime.Data_value.Keyword (str key))
               (if (string? key)
                 (list 'Datascript_runtime.Data_value.String key)
                 (if (= key true)
                   (list 'Datascript_runtime.Data_value.Bool true)
                   (if (= key false)
                     (list 'Datascript_runtime.Data_value.Bool false)
                     (if (nil? key)
                       (list 'Datascript_runtime.Data_value.Nil)
                       (if (or (symbol? key) (seq? key))
                         key
                         (list
                          'Datascript_runtime.Data_value.Int
                          key))))))))]
       (if (empty? (next args))
         (list
          'datascript.conn/listen!-closed
          connection
          (first args))
         (list
          'datascript.conn/listen!-closed
          connection
          (key-form (first args))
          (first (next args))))))}
  ([conn callback]
   (listen!-closed conn callback))
  ([conn key callback]
   (listen!-closed conn key callback)))

(defn unlisten!-closed
  [conn key]
  {:pre [(conn? conn)]}
  (let [state-atom (:atom conn)
        state @state-atom
        listeners (dissoc (state-listeners state) key)]
    (reset! state-atom (state-with-listeners state listeners))))

(defn unlisten!
  {:inline
   (fn [connection key]
     (let [key-form
           (if (keyword? key)
             (list 'Datascript_runtime.Data_value.Keyword (str key))
             (if (string? key)
               (list 'Datascript_runtime.Data_value.String key)
               (if (= key true)
                 (list 'Datascript_runtime.Data_value.Bool true)
                 (if (= key false)
                   (list 'Datascript_runtime.Data_value.Bool false)
                   (if (nil? key)
                     (list 'Datascript_runtime.Data_value.Nil)
                     (if (or (symbol? key) (seq? key))
                       key
                       (list
                        'Datascript_runtime.Data_value.Int
                        key)))))))]
       (list 'datascript.conn/unlisten!-closed connection key-form)))}
  [conn key]
  (unlisten!-closed conn key))
