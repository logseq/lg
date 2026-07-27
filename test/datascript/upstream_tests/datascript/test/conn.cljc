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

(deftest test-js-static-datom-conversion
  (let [minimal (js/js->Datom [1 :name "Ada"])
        complete (js/js->Datom [2 :age 42 77 false "ignored"])
        runtime-vector
        (js/js->Datom
         (Datascript_runtime.Serialization_value.data_value_of_edn_string
          "[3 :name \"Runtime\" nil nil]"))
        runtime-map
        (js/js->Datom
         (Datascript_runtime.Serialization_value.data_value_of_edn_string
          "{:e 4 :a :name :v \"Map\" :tx 88 :added false}"))]
    (is
     (=
      [(tuple
        1 :name
        (Datascript_runtime.Data_value.String "Ada")
        db/tx0 true)
       (tuple
        2 :age
        (Datascript_runtime.Data_value.Int 42)
        77 true)
       (tuple
        3 :name
        (Datascript_runtime.Data_value.String "Runtime")
        db/tx0 true)
       (tuple
        4 :name
        (Datascript_runtime.Data_value.String "Map")
        88 true)]
      (mapv
       (fn [datom]
         (tuple
          (.-e datom)
          (db/datom-attr datom)
          (.-v datom)
          (db/datom-tx datom)
          (db/datom-added datom)))
       [minimal complete runtime-vector runtime-map])))))

(deftest test-js-static-pull-wrappers
  (let [pull-schema
        (zipmap
         [:friend :email :tags]
         [(zipmap
           [:db/valueType]
           [(Datascript_runtime.Data_value.Keyword ":db.type/ref")])
          (zipmap
           [:db/unique]
           [(Datascript_runtime.Data_value.Keyword
             ":db.unique/identity")])
          (zipmap
           [:db/cardinality]
           [(Datascript_runtime.Data_value.Keyword
             ":db.cardinality/many")])])
        database
        (d/init-db
         [(d/datom 1 :name "Ada")
          (d/datom 1 :email "ada@example")
          (d/datom 1 :age 41)
          (d/datom 1 :tags "first")
          (d/datom 1 :tags "second")
          (d/datom 1 :friend 2)
          (d/datom 2 :name "Bob")
          (d/datom 2 :friend 3)
          (d/datom 3 :name "Cara")]
         pull-schema)
        basic (js/pull database "[:db/id :name *]" 1)
        nested (js/pull database "[:name {:friend [:name]}]" 1)
        recursive (js/pull database "[:name {:friend ...}]" 1)
        options
        (js/pull
         database
         "[[:name :as :display] [:missing :default \"fallback\"] [:tags :limit 1] [:age :xform inc]]"
         1)
        lookup
        (js/pull database "[:name]" [:email "ada@example"])
        many (js/pull_many database "[:name]" [1 999 2])]
    (if-some [result basic]
      (do
        (is
         (=
          (Some (Datascript_runtime.Data_value.String "Ada"))
          (get
           result
           (Datascript_runtime.Data_value.Keyword ":name"))))
        (is
         (=
          (Some (Datascript_runtime.Data_value.Int 1))
          (get
           result
           (Datascript_runtime.Data_value.Keyword ":db/id")))))
      (is false))
    (if-some [result nested]
      (if-some
        [friend
         (get
          result
          (Datascript_runtime.Data_value.Keyword ":friend"))]
        (is
         (=
          (Some (Datascript_runtime.Data_value.String "Bob"))
          (Datascript_runtime.Data_value.keyword_map_get
           ":name"
           friend)))
        (is false))
      (is false))
    (if-some [result recursive]
      (if-some
        [friend
         (get
          result
          (Datascript_runtime.Data_value.Keyword ":friend"))]
        (if-some
          [friend-of-friend
           (Datascript_runtime.Data_value.keyword_map_get
            ":friend"
            friend)]
          (is
           (=
            (Some (Datascript_runtime.Data_value.String "Cara"))
            (Datascript_runtime.Data_value.keyword_map_get
             ":name"
             friend-of-friend)))
          (is false))
        (is false))
      (is false))
    (if-some [result options]
      (do
        (is
         (=
          (Some (Datascript_runtime.Data_value.String "Ada"))
          (get
           result
           (Datascript_runtime.Data_value.Keyword ":display"))))
        (is
         (=
          (Some (Datascript_runtime.Data_value.String "fallback"))
          (get
           result
           (Datascript_runtime.Data_value.Keyword ":missing"))))
        (if-some
          [tags
           (get
            result
            (Datascript_runtime.Data_value.Keyword ":tags"))]
          (is
           (=
            (Some 1)
            (Datascript_runtime.Data_value.count_value tags)))
          (is false))
        (is
         (=
          (Some (Datascript_runtime.Data_value.Int 42))
          (get
           result
           (Datascript_runtime.Data_value.Keyword ":age")))))
      (is false))
    (if-some [result lookup]
      (is
       (=
        (Some (Datascript_runtime.Data_value.String "Ada"))
        (get
         result
         (Datascript_runtime.Data_value.Keyword ":name"))))
      (is false))
    (is (= 3 (count many)))
    (if-some [result (nth many 0)]
      (is
       (=
        (Some (Datascript_runtime.Data_value.String "Ada"))
        (get
         result
         (Datascript_runtime.Data_value.Keyword ":name"))))
      (is false))
    (is (= nil (nth many 1)))
    (if-some [result (nth many 2)]
      (is
       (=
        (Some (Datascript_runtime.Data_value.String "Bob"))
        (get
         result
         (Datascript_runtime.Data_value.Keyword ":name"))))
      (is false))
    (is (= nil (js/pull database "[:name]" 999)))
    (is
     (thrown-msg?
      "Expected pattern to be sequential?, got: 42"
      (js/pull database "42" 1)))))

(deftest test-js-static-transact-wrapper
  (let [connection (d/create-conn)
        ^:ref<option<datascript.db/TxReport>> observed-report (atom nil)
        listener-calls (atom 0)
        _listener-key
        (d/listen!
         connection
         :js-transact
         (fn [report]
           (swap! listener-calls inc)
           (reset! observed-report (Some report))))
        report
        (js/transact
         connection
         [[:db/add -1 :name "Ada"]]
         {:source "js"})]
    (is (= 1 @listener-calls))
    (if-some [observed @observed-report]
      (is (= report observed))
      (is false))
    (is (= (js/db connection) (.-db-after report)))
    (is (= 0 (count (d/datoms (.-db-before report) :eavt))))
    (is
     (=
      (Some (Datascript_runtime.Data_value.String "js"))
      (Datascript_runtime.Data_value.keyword_map_get
       ":source"
       (.-tx-meta report))))
    (if-some [datom (d/find-datom (js/db connection) :eavt 1 :name)]
      (is
       (Datascript_runtime.Data_value.equal
        (Datascript_runtime.Data_value.String "Ada")
        (.-v datom)))
      (is false))
    (let [report-without-meta
          (js/transact
           connection
           [[:db/add 1 :age 42]])]
      (is (= 2 @listener-calls))
      (is
       (Datascript_runtime.Data_value.equal
        (Datascript_runtime.Data_value.Nil)
        (.-tx-meta report-without-meta))))
    (is
     (thrown-msg?
      "Can’t find entity for transaction fn :unknown-fn"
      (js/transact connection [[:unknown-fn]])))))

(deftest test-js-static-reset-conn-wrapper
  (let [database-before
        (d/init-db [(d/datom 1 :name "Before")])
        database-after
        (d/init-db [(d/datom 2 :name "After")])
        connection (d/conn-from-db database-before)
        ^:ref<option<datascript.db/TxReport>> observed-report (atom nil)
        listener-calls (atom 0)
        _listener-key
        (d/listen!
         connection
         :js-reset
         (fn [report]
           (swap! listener-calls inc)
           (reset! observed-report (Some report))))
        returned
        (js/reset_conn connection database-after :reset)]
    (is (= database-after returned))
    (is (= database-after (js/db connection)))
    (is (= 1 @listener-calls))
    (if-some [report @observed-report]
      (do
        (is (= database-before (.-db-before report)))
        (is (= database-after (.-db-after report)))
        (is
         (Datascript_runtime.Data_value.equal
          (Datascript_runtime.Data_value.Keyword ":reset")
          (.-tx-meta report)))
        (is
         (=
          [(tuple 1 :name false)
           (tuple 2 :name true)]
          (mapv
           (fn [datom]
             (tuple
              (.-e datom)
              (db/datom-attr datom)
              (db/datom-added datom)))
           (.-tx-data report)))))
      (is false))
    (let [reset-without-meta
          (js/reset_conn connection database-before)]
      (is (= database-before reset-without-meta))
      (is (= 2 @listener-calls))
      (if-some [report-without-meta @observed-report]
        (is
         (Datascript_runtime.Data_value.equal
          (Datascript_runtime.Data_value.Nil)
          (.-tx-meta report-without-meta)))
        (is false)))))
