(ns datascript.conn
  (:require
    [datascript.db :as db]
    [datascript.storage :as storage]
    [me.tonsky.persistent-sorted-set :as set]))

(defrecord Conn [atom])

(defn- make-conn [opts]
  (->Conn (atom opts)))

(defn current-db [conn]
  (:db @(:atom conn)))

(defn- swap-db!
  ([conn f]
   (:db (swap! (:atom conn) update :db f)))
  ([conn f arg]
   (:db (swap! (:atom conn) update :db f arg))))

(defn- reset-db! [conn database]
  (:db (swap! (:atom conn) assoc :db database)))

(defn with
  ([database tx-data]
   (with database tx-data nil))
  ([database tx-data tx-meta]
   {:pre [(db/db? database)]}
   (if (instance? db/FilteredDB database)
     (throw
       (ex-info
         "Filtered DB cannot be modified"
         {:error :transaction/filtered}))
     (db/transact-tx-data
       (db/->TxReport database database [] {} tx-meta)
       tx-data))))

(defn db-with [database tx-data]
  {:pre [(db/db? database)]}
  (:db-after (with database tx-data)))

(defn conn? [conn]
  (and
    (instance? Conn conn)
    (if-some [database (current-db conn)]
      (db/db? database)
      true)))

(defn conn-from-db [database]
  {:pre [(db/db? database)]}
  (if-some [database-storage (storage/storage database)]
    (do
      (storage/store database)
      (make-conn
        {:db database
         :tx-tail []
         :db-last-stored database}))
    (make-conn {:db database})))

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
   (when-some [[database tail]
               (storage/restore-impl database-storage opts)]
     (make-conn
       {:db (storage/db-with-tail database tail)
        :tx-tail tail
        :db-last-stored database}))))

(defn store-after-transact! [conn tx-report]
  (when-not (:skip-store? @(:atom conn))
    (when-some [database-storage (storage/storage (current-db conn))]
      (let [{database :db-after
             datoms :tx-data} tx-report
            settings (set/settings (:eavt database))
            state-atom (:atom conn)
            tx-tail (:tx-tail
                      (swap! state-atom update :tx-tail conj datoms))]
        (when-not (get-in tx-report [:tx-meta :skip-store?])
          (if (> (transduce (map count) + 0 tx-tail)
                 (:branching-factor settings))
            (do
              (storage/store-impl!
                database
                (storage/storage-adapter database)
                false)
              (swap! state-atom assoc
                :tx-tail []
                :db-last-stored database))
            (storage/store-tail database tx-tail)))))))

(defn -transact! [conn tx-data tx-meta]
  {:pre [(conn? conn)]}
  (let [report-ref (volatile! nil)
        tx-meta (dissoc tx-meta :skip-store?)]
    (swap-db!
      conn
      (fn [database]
        (let [report (with database tx-data tx-meta)]
          (vreset! report-ref report)
          (:db-after report))))
    (let [report @report-ref]
      (store-after-transact! conn report)
      report)))

(defn run-callbacks [conn report]
  (doseq [[_ callback] (:listeners @(:atom conn))]
    (callback report)))

(defn transact!
  ([conn tx-data]
   (transact! conn tx-data nil))
  ([conn tx-data tx-meta]
   {:pre [(conn? conn)]}
   (let [report (-transact! conn tx-data tx-meta)]
     (run-callbacks conn report)
     report)))

(defn reset-conn!
  ([conn database]
   (reset-conn! conn database nil))
  ([conn database tx-meta]
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
         (swap! (:atom conn) assoc
           :db database
           :tx-tail []
           :db-last-stored database))
       (reset-db! conn database))
     (run-callbacks conn report)
     database)))

(defn reset-schema! [conn schema]
  {:pre [(conn? conn)]}
  (let [database (swap-db! conn db/with-schema schema)]
    (when-some [database-storage (storage/storage (current-db conn))]
      (storage/store-impl!
        database
        (storage/storage-adapter database)
        true)
      (swap! (:atom conn) assoc
        :tx-tail []
        :db-last-stored database))
    database))

(defn listen!
  ([conn callback]
   (listen! conn (rand) callback))
  ([conn key callback]
   {:pre [(conn? conn)]}
   (swap! (:atom conn) update :listeners assoc key callback)
   key))

(defn unlisten! [conn key]
  {:pre [(conn? conn)]}
  (swap! (:atom conn) update :listeners dissoc key))
