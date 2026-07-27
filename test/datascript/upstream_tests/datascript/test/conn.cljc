(ns datascript.test.conn
  (:require
    [clojure.test :as t :refer [is are deftest testing]]
    [datascript.core :as d]
    [datascript.db :as db]
    [datascript.js :as js]
    [datascript.test.core :as tdc]))

(def ^:map<keyword;map<keyword;Datascript_runtime.Data_value.t>> schema
  (zipmap
   [:aka]
   [(zipmap
     [:db/cardinality]
     [(Datascript_runtime.Data_value.Keyword ":db.cardinality/many")])]))

(def datoms
  #{(d/datom 1 :age  17)
    (d/datom 1 :name "Ivan")})

(deftest test-ways-to-create-conn
  (let [conn (d/create-conn)]
    (is (= #{} (set (d/datoms @conn :eavt))))
    (is (= nil (:schema @conn))))
  
  (let [conn (d/create-conn schema)]
    (is (= #{} (set (d/datoms @conn :eavt))))
    (is (= (Some schema) (:schema @conn))))
  
  (let [conn (d/conn-from-datoms datoms)]
    (is (= datoms (set (d/datoms @conn :eavt))))
    (is (= nil (:schema @conn))))
  
  (let [conn (d/conn-from-datoms datoms schema)]
    (is (= datoms (set (d/datoms @conn :eavt))))
    (is (= (Some schema) (:schema @conn))))
  
  (let [conn (d/conn-from-db (d/init-db datoms))]
    (is (= datoms (set (d/datoms @conn :eavt))))
    (is (= nil (:schema @conn))))
  
  (let [conn (d/conn-from-db (d/init-db datoms schema))]
    (is (= datoms (set (d/datoms @conn :eavt))))
    (is (= (Some schema) (:schema @conn)))))

(deftest test-reset-conn!
  (let [conn    (d/conn-from-datoms datoms schema)
        report  (atom nil)
        _       (d/listen! conn #(reset! report %))
        datoms' #{(d/datom 1 :age 20)
                  (d/datom 1 :sex :male)}
        schema'
        (zipmap
         [:email]
         [(zipmap
           [:db/unique]
           [(Datascript_runtime.Data_value.Keyword ":db.unique/identity")])])
        db'     (d/init-db datoms' schema')]
    (d/reset-conn! conn db' :meta)
    (is (= datoms' (set (d/datoms @conn :eavt))))
    (is (= (Some schema') (:schema @conn)))
    
    (if-some [report @report]
      (let [^datascript.db/DB db-before (.-db-before report)
            ^datascript.db/DB db-after  (.-db-after report)
            ^:vector<datascript.db/Datom> tx-data (.-tx-data report)
            ^:Datascript_runtime.Data_value.t tx-meta (.-tx-meta report)]
        (is (= datoms (set (d/datoms db-before :eavt))))
        (is (= (Some schema) (d/schema db-before)))
        (is (= datoms' (set (d/datoms db-after :eavt))))
        (is (= (Some schema') (d/schema db-after)))
        (is
         (Datascript_runtime.Data_value.equal
          tx-meta
          (Datascript_runtime.Data_value.Keyword ":meta")))
        (is (=
             [(tuple 1 :age (Datascript_runtime.Data_value.Int 17) false)
              (tuple 1 :name
                     (Datascript_runtime.Data_value.String "Ivan") false)
              (tuple 1 :age (Datascript_runtime.Data_value.Int 20) true)
              (tuple 1 :sex
                     (Datascript_runtime.Data_value.Keyword ":male") true)]
             (map
              (fn [^datascript.db/Datom datom]
                (tuple (.-e datom)
                       (db/datom-attr datom)
                       (.-v datom)
                       (db/datom-added datom)))
              tx-data))))
      (is false))))

(deftest test-js-static-adapter-basics
  (let [database (d/init-db datoms)
        conn (js/conn_from_db database)
        filtered
        (js/filter
         database
         (fn [_ datom]
           (= :age (db/datom-attr datom))))
        restored
        (js/from_serializable
         (js/serializable database))
        listener-calls (atom 0)
        listener-key
        (js/listen
         conn
         :js-adapter
         (fn [_report]
           (swap! listener-calls inc)))
        uuid (js/squuid)
        uuid-time (js/squuid_time_millis uuid)]
    (is (= database (js/db conn)))
    (is (js/is_filtered filtered))
    (is (= 1 (count (d/datoms filtered :eavt))))
    (is (= database restored))
    (if-some [entity (d/entity database 1)]
      (do
        (is
         (=
          database
          (db/database-view-unfiltered-db
           (js/entity_db entity))))
        (is (= (Some entity) (js/touch (Some entity)))))
      (is false))
    (is (= 36 (count uuid)))
    #?(:native
       (is (= 0 (mod uuid-time 1000)))
       :melange
       (is (> uuid-time 0.0)))
    (d/transact! conn [[:db/add 2 :name "Petr"]])
    (is (= 1 @listener-calls))
    (js/unlisten conn listener-key)
    (d/transact! conn [[:db/add 3 :name "Oleg"]])
    (is (= 1 @listener-calls))))

(deftest test-js-static-database-constructors
  (let [empty-database (js/empty_db)
        empty-database-with-schema (js/empty_db schema)
        initialized (js/init_db datoms)
        initialized-with-schema (js/init_db datoms schema)
        empty-conn (js/create_conn)
        empty-conn-with-schema (js/create_conn schema)
        datom-conn (js/conn_from_datoms datoms)
        datom-conn-with-schema
        (js/conn_from_datoms datoms schema)]
    (is (= #{} (set (d/datoms empty-database :eavt))))
    (is (= nil (:schema empty-database)))
    (is (= (Some schema) (:schema empty-database-with-schema)))
    (is (= datoms (set (d/datoms initialized :eavt))))
    (is (= nil (:schema initialized)))
    (is (= datoms (set (d/datoms initialized-with-schema :eavt))))
    (is (= (Some schema) (:schema initialized-with-schema)))
    (is (= #{} (set (d/datoms (js/db empty-conn) :eavt))))
    (is (= nil (:schema (js/db empty-conn))))
    (is (= (Some schema) (:schema (js/db empty-conn-with-schema))))
    (is (= datoms (set (d/datoms (js/db datom-conn) :eavt))))
    (is (= nil (:schema (js/db datom-conn))))
    (is
     (=
      datoms
      (set (d/datoms (js/db datom-conn-with-schema) :eavt))))
    (is
     (=
      (Some schema)
      (:schema (js/db datom-conn-with-schema))))))

(deftest test-js-static-database-operations
  (let [database
        (d/init-db
         [(d/datom 1 :age 17)
          (d/datom 2 :age 20)
          (d/datom 3 :age 30)]
         {:age {:db/index true}})
        updated
        (js/db_with
         database
         [[:db/add 4 :age 25]])
        ranged (js/index_range updated :age 20 25)
        tempid
        (Datascript_runtime.Data_value.String "temp")
        tempids (zipmap [tempid] [4])]
    (if-some [datom (d/find-datom updated :eavt 4 :age)]
      (is
       (Datascript_runtime.Data_value.equal
        (Datascript_runtime.Data_value.Int 25)
        (.-v datom)))
      (is false))
    (if-some [entity (js/entity updated 4)]
      (is (= 4 (.-eid entity)))
      (is false))
    (is (= nil (js/entity updated 999)))
    (is
     (=
      [20 25]
      (mapv
       (fn [^datascript.db/Datom datom]
         (match (.-v datom)
           (Datascript_runtime.Data_value.Int value) value
           _ -1))
       ranged)))
    (is (= (Some 4) (js/resolve_tempid tempids tempid)))
    (is
     (=
      nil
      (js/resolve_tempid
       tempids
       (Datascript_runtime.Data_value.String "missing"))))))

(deftest test-js-static-index-wrappers
  (let [database
        (d/init-db
         [(d/datom 1 :age 17)
          (d/datom 2 :age 20)
          (d/datom 3 :age 30)])
        all-datoms (js/datoms database :eavt)
        entity-datoms (js/datoms database ":eavt" 2)
        seeked (js/seek_datoms database ":eavt" 2)]
    (is (= [1 2 3] (mapv (fn [datom] (.-e datom)) all-datoms)))
    (is (= [2] (mapv (fn [datom] (.-e datom)) entity-datoms)))
    (is (= [2 3] (mapv (fn [datom] (.-e datom)) seeked)))))
