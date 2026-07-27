(ns datascript.test.conn
  (:require
    [clojure.test :as t :refer [is are deftest testing]]
    [datascript.core :as d]
    [datascript.db :as db]
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
