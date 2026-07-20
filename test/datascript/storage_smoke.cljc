(ns datascript.test.storage-smoke
  (:require
   [datascript.db :as db]
   [datascript.storage :as storage]
   [datascript.conn :as conn]
   [me.tonsky.persistent-sorted-set :as set]))

(def environment #?(:native "native" :melange "melange"))

(defrecord MemoryStorage [disk reads writes deletes]
  storage/IStorage
  (-store [_ entries delete-addresses]
    (doseq [[address data] entries]
      (vswap! disk assoc address data)
      (vswap! writes conj address))
    (doseq [address delete-addresses]
      (vswap! disk dissoc address)
      (vswap! deletes conj address)))
  (-restore [_ address]
    (vswap! reads conj address)
    (get @disk address))
  (-list-addresses [_]
    (vec (keys @disk)))
  (-delete [_ addresses]
    (doseq [address addresses]
      (vswap! disk dissoc address)
      (vswap! deletes conj address))))

(defn make-memory-storage []
  (MemoryStorage. (volatile! {})
                  (volatile! [])
                  (volatile! [])
                  (volatile! [])))

(defn throws? [f]
  (try
    (f)
    false
    (catch _
      true)))

(defn reset-stats [backend]
  (vreset! (:reads backend) [])
  (vreset! (:writes backend) [])
  (vreset! (:deletes backend) []))

(defn small-database []
  (conn/db-with
   (db/empty-db nil {:branching-factor 32 :ref-type :strong})
   [[:db/add 1 :name "Ivan"]
    [:db/add 2 :name "Oleg"]
    [:db/add 3 :name "Petr"]]))

(defn large-database []
  (conn/db-with
   (db/empty-db nil {:branching-factor 32 :ref-type :strong})
   (map
    (fn [entity]
      [:db/add entity :str (str entity)])
    (range 1 1001))))

(defn tail-datom-count [^datascript.conn/Conn connection]
  (let [^datascript.conn/conn-state state @(:state-ref connection)
        ^:vector<vector<datascript.db/Datom>> tail (:tx-tail state)]
    (transduce (map count) + 0 tail)))

(defn tail-group-count [^datascript.conn/Conn connection]
  (let [^datascript.conn/conn-state state @(:state-ref connection)
        ^:vector<vector<datascript.db/Datom>> tail (:tx-tail state)]
    (count tail)))

(defn ^datascript.conn/Conn restore-connection! [^:dynamic backend]
  (if-some [connection (conn/restore-conn backend)]
    connection
    (throw (ex-info "Stored connection did not restore" {}))))

(defn ^datascript.db/DB restore-database! [^:dynamic backend]
  (if-some [database (storage/restore backend)]
    database
    (throw (ex-info "Stored database did not restore" {}))))

(let [database (db/empty-db nil {})]
  (println
   (str environment ":no-storage:"
        (nil? (storage/storage database)) ":"
        (throws? (fn [] (storage/store database))))))

(let [missing-storage (make-memory-storage)]
  (println
   (str environment ":missing-root:"
        (nil? (storage/restore missing-storage)))))

(let [backend (make-memory-storage)
      database
      (db/init-db
       [(db/datom 1 :name "Ivan")
        (db/datom 2 :name "Oleg")]
       {}
      (storage/maybe-adapt-storage {:storage backend}))
      _stored (storage/store database)
      writes-after-first (count @(:writes backend))
      _stored-again (storage/store database)
      writes-after-second (count @(:writes backend))
      restored (or (storage/restore backend)
                   (throw (ex-info "Stored database did not restore" {})))
      used-addresses (storage/addresses [database])
      tail [[(db/datom 3 :name "Petr" 536870913)]]
      _tail-stored (storage/store-tail database tail)
      restored-with-tail (storage/restore backend)]
  (println
   (str environment ":store-restore:"
        (some? (storage/storage database)) ":"
        (= "Ivan" (:v (first (db/-search restored [1 :name])))) ":"
        (= 2 (count (db/-datoms restored :eavt nil nil nil nil))) ":"
        (= writes-after-first writes-after-second) ":"
        (and (contains? used-addresses 0)
             (contains? used-addresses 1)
             (> (count used-addresses) 2)) ":"
        (some? (first (db/-search restored-with-tail [3 :name]))))))

(let [backend (make-memory-storage)
      database (db/empty-db nil {})
      _stored (storage/store database backend)
      writes-ok (= 5 (count @(:writes backend)))
      restored (restore-database! backend)
      lazy-root-ok (= 2 (count @(:reads backend)))
      restored-db-ok (db/db? restored)
      schema-ok (= (:schema database) (:schema restored))
      datoms-ok
      (= (vec (db/-datoms database :eavt nil nil nil nil))
         (vec (db/-datoms restored :eavt nil nil nil nil)))
      direct-equality-ok (-equiv database restored)
      equality-ok (= database restored)
      lazy-index-ok (= 3 (count @(:reads backend)))]
  (println
   (str environment ":storage-basics-empty:"
        writes-ok ":"
        lazy-root-ok ":"
        restored-db-ok ":"
        schema-ok ":"
        datoms-ok ":"
        direct-equality-ok ":"
        equality-ok ":"
        lazy-index-ok)))

(let [backend (make-memory-storage)
      database (small-database)
      _stored (storage/store database backend)
      store-ok
      (and (= 0 (count @(:reads backend)))
           (= 5 (count @(:writes backend))))
      restored (restore-database! backend)
      restore-root-ok (= 2 (count @(:reads backend)))
      equality-ok (= database restored)
      restore-eavt-ok (= 3 (count @(:reads backend)))
      _aevt (vec (db/-datoms restored :aevt nil nil nil nil))
      restore-aevt-ok (= 4 (count @(:reads backend)))
      _avet (vec (db/-datoms restored :avet nil nil nil nil))
      restore-avet-ok (= 5 (count @(:reads backend)))
      _reset (reset-stats backend)
      restored-for-count (restore-database! backend)
      count-ok (= 3 (count restored-for-count))
      count-read-count (count @(:reads backend))
      count-read-ok (<= count-read-count 2)
      settings-ok
      (= {:branching-factor 32 :ref-type :strong}
         (set/settings (:eavt (restore-database! backend))))]
  (println
   (str environment ":storage-basics-small:"
        store-ok ":"
        restore-root-ok ":"
        equality-ok ":"
        restore-eavt-ok ":"
        restore-aevt-ok ":"
        restore-avet-ok ":"
        count-ok ":"
        count-read-ok ":"
        settings-ok)))

(let [backend (make-memory-storage)
      database (large-database)
      _stored (storage/store database backend)
      first-store-ok (= 135 (count @(:writes backend)))
      _stored-again (storage/store database)
      unchanged-store-ok (= 135 (count @(:writes backend)))
      restored (restore-database! backend)
      root-read-ok (= 2 (count @(:reads backend)))
      first-datom (first (db/-datoms restored :eavt nil nil nil nil))
      first-datom-ok
      (= [1 :str "1"] [(:e first-datom) (:a first-datom) (:v first-datom)])
      first-read-count (count @(:reads backend))
      first-read-ok (<= first-read-count 7)
      _first-again (first (db/-datoms restored :eavt nil nil nil nil))
      cached-first-read-count (count @(:reads backend))
      cached-first-ok (= first-read-count cached-first-read-count)
      _all (vec (db/-datoms restored :eavt nil nil nil nil))
      all-read-count (count @(:reads backend))
      all-read-ok (<= all-read-count 68)
      _all-again (vec (db/-datoms restored :eavt nil nil nil nil))
      cached-all-read-count (count @(:reads backend))
      cached-all-ok (= all-read-count cached-all-read-count)
      database-equality-ok (= database restored)
      eavt-equality-ok (= (:eavt database) (:eavt restored))
      aevt-equality-ok (= (:aevt database) (:aevt restored))
      avet-equality-ok (= (:avet database) (:avet restored))
      _reset (reset-stats backend)
      restored-for-count (restore-database! backend)
      count-ok (= 1000 (count restored-for-count))
      count-read-count (count @(:reads backend))
      count-read-ok (<= count-read-count 2)
      _reset-again (reset-stats backend)
      updated
      (conn/db-with database [[:db/add 1001 :str "1001"]])
      _stored-updated (storage/store updated)
      incremental-ok (= 8 (count @(:writes backend)))]
  (println
   (str environment ":storage-basics-large:"
        first-store-ok ":"
        unchanged-store-ok ":"
        root-read-ok ":"
        first-datom-ok ":"
        first-read-ok ":"
        cached-first-ok ":"
        all-read-ok ":"
        cached-all-ok ":"
        database-equality-ok ":"
        eavt-equality-ok ":"
        aevt-equality-ok ":"
        avet-equality-ok ":"
        count-ok ":"
        count-read-ok ":"
        incremental-ok)))

(let [backend (make-memory-storage)
      database
      (db/init-db
       [(db/datom 1 :name "Ivan")]
       {}
      (storage/maybe-adapt-storage {:storage backend}))
      _stored (storage/store database)
      updated (db/with-datom database (db/datom 2 :name "Oleg"))
      _stored-updated (storage/store updated)
      live-databases [database updated]
      _orphaned (vswap! (:disk backend) assoc 999999 {:orphan true})
      deleted (storage/collect-garbage backend)]
  (println
   (str environment ":storage-gc:"
        (contains? (set deleted) 999999) ":"
        (not (contains? @(:disk backend) 999999)) ":"
        (every? (fn [address] (contains? @(:disk backend) address))
                (storage/addresses live-databases)) ":"
        (contains? @(:disk backend) 0) ":"
        (contains? @(:disk backend) 1))))

(let [backend (make-memory-storage)
      connection (conn/create-conn nil {:storage backend})
      callback-count (atom 0)
      listener-key
      (conn/listen! connection :listener
                    (fn [_report]
                      (swap! callback-count inc)))
      report (conn/transact! connection [[:db/add 1 :name "Ivan"]])
      current (conn/current-db connection)
      restored-connection (restore-connection! backend)
      restored (conn/current-db restored-connection)
      _unlistened (conn/unlisten! connection listener-key)
      _second-report
      (conn/transact! connection [[:db/add 2 :name "Oleg"]])]
  (println
   (str environment ":conn:"
        (conn/conn? connection) ":"
        (= 1 @callback-count) ":"
        (= "Ivan" (:v (first (db/-search current [1 :name])))) ":"
        (= "Ivan" (:v (first (db/-search restored [1 :name])))) ":"
        (= 1 (count (:tx-data report))))))

(let [backend (make-memory-storage)
      connection
      (conn/create-conn
       nil
       {:storage backend :branching-factor 32 :ref-type :strong})
      initial-store-ok (= 5 (count @(:writes backend)))
      _first (conn/transact! connection [[:db/add 1 :name "Ivan"]])
      first-tail-ok
      (and (= 6 (count @(:writes backend)))
           (= 1 (last @(:writes backend))))
      _second (conn/transact! connection [[:db/add 2 :name "Oleg"]])
      second-tail-ok
      (and (= 7 (count @(:writes backend)))
           (= 1 (last @(:writes backend)))
           (= 2 (tail-group-count connection))
           (= 2 (tail-datom-count connection)))
      _large-tail
      (conn/transact!
       connection
       (mapv
        (fn [entity] [:db/add entity :name (str entity)])
        (range 3 33)))
      full-tail-ok
      (and (= 8 (count @(:writes backend)))
           (= 1 (last @(:writes backend)))
           (= 3 (tail-group-count connection))
           (= 32 (tail-datom-count connection)))
      _overflow (conn/transact! connection [[:db/add 33 :name "Petr"]])
      overflow-ok (= 16 (count @(:writes backend)))
      _restart (conn/transact! connection [[:db/add 34 :name "Anna"]])
      restart-ok
      (and (= 17 (count @(:writes backend)))
           (= 1 (last @(:writes backend))))
      restored-connection (restore-connection! backend)
      restore-tail-ok
      (and (= (conn/current-db connection)
              (conn/current-db restored-connection))
           (= (:max-eid (conn/current-db connection))
              (:max-eid (conn/current-db restored-connection)))
           (= (:max-tx (conn/current-db connection))
              (:max-tx (conn/current-db restored-connection))))
      _restored-tx
      (conn/transact! restored-connection [[:db/add 35 :name "Vera"]])
      restored-tail-write-ok
      (and (= 18 (count @(:writes backend)))
           (= 1 (last @(:writes backend))))
      _restored-overflow
      (conn/transact!
       restored-connection
       (mapv
        (fn [entity] [:db/add entity :name (str entity)])
        (range 36 80)))
      restored-overflow-ok
      (and (= 28 (count @(:writes backend)))
           (= 1 (last @(:writes backend))))
      restored-without-tail (restore-connection! backend)
      restore-without-tail-ok
      (= (conn/current-db restored-connection)
         (conn/current-db restored-without-tail))
      _final-tx
      (conn/transact! restored-without-tail [[:db/add 80 :name "Ilya"]])
      final-tail-ok
      (and (= 29 (count @(:writes backend)))
           (= 1 (last @(:writes backend))))
      state @(:state-ref restored-without-tail)
      last-stored
      (match (:db-last-stored state)
        (Some database) database
        None (throw (ex-info "Missing last stored database" {})))
      gc-needed-ok
      (> (count (storage/-list-addresses backend))
         (count (storage/addresses [last-stored])))
      disk-address-count-before-gc (count (storage/-list-addresses backend))
      _collected (storage/collect-garbage backend)
      gc-ok
      (and
       (<= (count (storage/-list-addresses backend))
           disk-address-count-before-gc)
       (every?
        (fn [address]
          (contains? (set (storage/-list-addresses backend)) address))
        (storage/addresses [last-stored])))
      restored-after-gc (restore-connection! backend)
      restore-after-gc-ok
      (= (conn/current-db restored-without-tail)
         (conn/current-db restored-after-gc))]
  (println
   (str environment ":conn-tail:"
        initial-store-ok ":"
        first-tail-ok ":"
        second-tail-ok ":"
        full-tail-ok ":"
        overflow-ok ":"
        restart-ok ":"
        restore-tail-ok ":"
        restored-tail-write-ok ":"
        restored-overflow-ok ":"
        restore-without-tail-ok ":"
        final-tail-ok ":"
        gc-needed-ok ":"
        gc-ok ":"
        restore-after-gc-ok)))
