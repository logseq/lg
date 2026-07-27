(ns datascript.test.issues
  (:require
   [clojure.test :refer [deftest is]]
   [datascript.core :as ds]
   [datascript.db :as db]
   [datascript.test.core :as tdc]))

(defn keyword-value [^:keyword value]
  :Datascript_runtime.Data_value.t
  (Datascript_runtime.Data_value.Keyword (str value)))

(defn empty-metadata-map []
  :map<keyword;Datascript_runtime.Data_value.t>
  {})

(defn metadata-map
  [^:keyword key ^:keyword value]
  :map<keyword;Datascript_runtime.Data_value.t>
  (assoc (empty-metadata-map) key (keyword-value value)))

(defn keep-all-datoms?
  [^datascript.db/DB _database ^datascript.db/Datom _datom]
  true)

(defn datom-vector-equal?
  [^:option<vector<datascript.db/Datom>> actual
   ^:vector<datascript.db/Datom> expected]
  (if-some [actual actual]
    (db/datom-vectors-equal? actual expected)
    false))

(defn database-diff-equal?
  [^datascript.db/database-diff difference
   ^:vector<datascript.db/Datom> only-left
   ^:vector<datascript.db/Datom> only-right]
  (and
   (datom-vector-equal? (:only-left difference) only-left)
   (datom-vector-equal? (:only-right difference) only-right)
   (nil? (:both difference))))

(deftest
  ^{:doc "CLJS `apply` + `vector` will hold onto mutable array of arguments directly"}
  issue-262
  (let [database
        (ds/db-with
         (ds/empty-db)
         [{:attr "A"} {:attr "B"}])]
    (is
     (tdc/query-relation?
      (ds/q
       '[:find ?a ?b
         :where
         [_ :attr ?a]
         [(vector ?a) ?b]]
       database)
      [["A" ["A"]]
       ["B" ["B"]]]))))

(deftest
  ^{:doc "`empty` should preserve meta of db"}
  issue-331
  (let [metadata (metadata-map :foo :bar)
        database
        (->
         (ds/empty-db)
         (db/with-db-metadata metadata)
         (empty))]
    (is (= metadata (db/db-metadata database)))))

#?(:native
   (deftest
     ^{:doc "Can't pprint filtered db"}
     issue-330
     (let [base
           (->
            (ds/empty-db
             {:aka {:db/cardinality :db.cardinality/many}})
            (ds/db-with
             [{:db/id -1
               :name "Maksim"
               :age 45
               :aka ["Max Otto von Stierlitz" "Jack Ryan"]}]))
           filtered (ds/filter base keep-all-datoms?)]
       (is (= (pr-str base) (pr-str filtered))))))

(deftest
  ^{:doc "Can't diff databases with different types of the same attribute"}
  issue-369
  (let [database-1
        (->
         (ds/empty-db)
         (ds/db-with [[:db/add 1 :attr :aa]]))
        database-2
        (->
         (ds/empty-db)
         (ds/db-with [[:db/add 1 :attr "aa"]]))
        difference (db/diff-databases database-1 database-2)]
    (is
     (database-diff-equal?
      difference
      [(ds/datom 1 :attr :aa)]
      [(ds/datom 1 :attr "aa")]))))

(deftest
  ^{:doc "Expose a schema as a part of the public API."}
  issue-381
  (let [schema
        (db/schema-map
         {:aka {:db/cardinality :db.cardinality/many}})
        database (ds/empty-db schema)]
    (is (= (Some schema) (ds/schema database)))))
