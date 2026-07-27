(ns datascript.test.transact-retract-static
  (:require
   [clojure.test :refer [deftest is testing]]
   [datascript.core :as d]
   [datascript.db :as db]))

(defn ^:vector<datascript.db/tx-entry> installed-set-name
  [^datascript.db/DB _database
   ^:Datascript_runtime.Data_value.t argument]
  (match argument
    (Datascript_runtime.Data_value.String name)
    [(db/tx-add
      (Datascript_runtime.Data_value.Entity_id 1)
      :name
      (Datascript_runtime.Data_value.String name))]
    _
    (Stdlib.invalid_arg
     "The installed set-name transaction function expects a string")))

(deftest test-retract-without-value
  (let [database
        (-> (d/empty-db)
            (d/db-with
             [[:db/add 1 :name "Ivan"]
              [:db/add 1 :age 15]
              [:db/add 2 :employed? true]
              [:db/add 2 :married? false]]))
        retracted
        (d/db-with
         database
         [[:db/retract 1 :name]
          [:db/retract 2 :employed?]
          [:db/retract 2 :married?]])]
    (is (= 1 (count (d/datoms retracted :eavt))))
    (is (some? (d/find-datom retracted :eavt 1 :age)))
    (is (nil? (d/find-datom retracted :eavt 1 :name)))
    (is (nil? (d/find-datom retracted :eavt 2 :employed?)))
    (is (nil? (d/find-datom retracted :eavt 2 :married?)))))

(deftest test-retract-aliases-and-lookup-refs
  (let [database
        (-> (d/empty-db
             (zipmap
              [:name]
              [(zipmap
                [:db/unique]
                [(Datascript_runtime.Data_value.Keyword
                  ":db.unique/identity")])]))
            (d/db-with [[:db/add 1 :name "Ivan"]]))]
    (testing "missing lookup refs preserve the database"
      (is
       (= 1
          (count
           (d/datoms
            (d/db-with
             database
             [[:db/retract [:name "Petr"] :name "Petr"]])
            :eavt))))
      (is
       (= 1
          (count
           (d/datoms
            (d/db-with
             database
             [[:db.fn/retractAttribute [:name "Petr"] :name]])
            :eavt))))
      (is
       (= 1
          (count
           (d/datoms
            (d/db-with
             database
             [[:db/retractEntity [:name "Petr"]]])
            :eavt)))))

    (testing "found lookup refs support both retract aliases"
      (is
       (= 0
          (count
           (d/datoms
            (d/db-with
             database
             [[:db.fn/retractEntity [:name "Ivan"]]])
            :eavt))))
      (is
       (= 0
          (count
           (d/datoms
            (d/db-with
             database
             [[:db/retractEntity [:name "Ivan"]]])
            :eavt)))))))

(deftest test-retract-entity-removes-incoming-refs
  (let [database
        (-> (d/empty-db
             (zipmap
              [:friend]
              [(zipmap
                [:db/valueType]
                [(Datascript_runtime.Data_value.Keyword
                  ":db.type/ref")])]))
            (d/db-with
             [[:db/add 1 :name "Ivan"]
              [:db/add 1 :friend 2]
              [:db/add 2 :name "Petr"]]))
        retracted
        (d/db-with database [[:db.fn/retractEntity 2]])]
    (is (= 1 (count (d/datoms retracted :eavt))))
    (is (nil? (d/find-datom retracted :eavt 1 :friend)))
    (is (nil? (d/find-datom retracted :eavt 2 :name)))))

(deftest test-installed-transaction-function
  (let [installed
        (d/db-with
         (d/empty-db)
         [{:db/id 1
           :name "Ivan"
           :db/ident :person}
          {:db/ident :set-name
           :db/fn installed-set-name}])
        invoked
        (d/db-with installed [[:set-name "Petr"]])]
    (is
     (if-some [datom (d/find-datom invoked :eavt 1 :name)]
       (Datascript_runtime.Data_value.equal
        (.-v datom)
        (Datascript_runtime.Data_value.String "Petr"))
       false))))

(deftest test-public-schema-literals
  (let [database
        (d/empty-db
         {:tags {:db/cardinality :db.cardinality/many}
          :friend {:db/valueType :db.type/ref
                   :db/index true}})
        transacted
        (d/db-with
         database
         [[:db/add 1 :tags "one"]
          [:db/add 1 :tags "two"]
          [:db/add 1 :friend 2]])]
    (is (= 3 (count (d/datoms transacted :eavt)))))

  (let [database (d/empty-db {})
        conn (d/create-conn {})]
    (d/transact! conn [[:db/add 1 :name "Ivan"]])
    (is (= 0 (count (d/datoms database :eavt))))
    (is (= 1 (count (d/datoms @conn :eavt))))))

(deftest test-nil-transaction-entries
  (let [database
        (d/db-with
         (d/empty-db)
         [nil
          nil
          [:db/add 1 :first "one"]
          nil
          [:db/add 1 :second "two"]
          nil
          nil])
        datoms (vec (d/datoms database :eavt))]
    (is (= 2 (count datoms)))
    (is (= :first (db/datom-attr (nth datoms 0))))
    (is (= :second (db/datom-attr (nth datoms 1))))))

(deftest test-datom-transaction-entries-preserve-tx
  (let [database
        (d/db-with
         (d/empty-db)
         [(d/datom 1 :name "Oleg")
          (d/datom 1 :age 17 (+ d/tx0 1))
          [:db/add 1 :aka "x" (+ d/tx0 2)]])
        age (d/find-datom database :eavt 1 :age)
        aka (d/find-datom database :eavt 1 :aka)
        name (d/find-datom database :eavt 1 :name)]
    (is
     (if-some [datom age]
       (= (+ d/tx0 1) (db/datom-tx datom))
       false))
    (is
     (if-some [datom aka]
       (= (+ d/tx0 2) (db/datom-tx datom))
       false))
    (is
     (if-some [datom name]
       (= d/tx0 (db/datom-tx datom))
       false)))

  (let [database
        (d/db-with
         (d/empty-db)
         [(d/datom 1 :name "Oleg")
          (d/datom 1 :age 17)
          (d/datom 1 :name "Oleg" d/tx0 false)])]
    (is (nil? (d/find-datom database :eavt 1 :name)))
    (is (some? (d/find-datom database :eavt 1 :age)))))
