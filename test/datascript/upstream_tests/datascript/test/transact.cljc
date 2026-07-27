(ns datascript.test.transact
  (:require
    [clojure.test :as t :refer [is are deftest testing]]
    [datascript.core :as d]
    [datascript.db :as db]
    [datascript.impl.entity :as entity]
    [datascript.lg.query-types :as query-types]
    [datascript.test.core :as tdc]))

(defn query-rows-equal?
  [^:array<datascript.lg.query-types/result> left
   ^:array<datascript.lg.query-types/result> right]
  (if (= (alength left) (alength right))
    (loop [index 0]
      (if (< index (alength left))
        (if
         (Datascript_runtime.Query_value.equal_result
          (aget left index)
          (aget right index))
          (recur (inc index))
          false)
        true))
    false))

(defn query-relation-contains?
  [^:vector<array<datascript.lg.query-types/result>> rows
   ^:array<datascript.lg.query-types/result> expected]
  (loop [remaining rows]
    (if-some [row (first remaining)]
      (if (query-rows-equal? row expected)
        true
        (recur (subvec remaining 1)))
      false)))

(defn query-relation-equal?
  [^datascript.lg.query-types/output output
   ^:vector<array<datascript.lg.query-types/result>> expected]
  (if-some [rows (query-types/output-relation output)]
    (and
     (= (count rows) (count expected))
     (every?
      (fn [^:array<datascript.lg.query-types/result> row]
        (query-relation-contains? rows row))
      expected))
    false))

(defn query-attr-value-row
  [^:keyword attr ^:Datascript_runtime.Data_value.t value]
  (array
   (query-types/attr-result attr)
   (query-types/value-result value)))

(defn query-value-row
  [^:Datascript_runtime.Data_value.t value]
  (array (query-types/value-result value)))

(defn query-ref-row [^:int entity]
  (array
   (query-types/value-result
    (Datascript_runtime.Data_value.Ref entity))))

(defn query-eav-row
  [^:int entity
   ^:keyword attr
   ^:Datascript_runtime.Data_value.t value]
  (array
   (query-types/entity-result entity)
   (query-types/attr-result attr)
   (query-types/value-result value)))

(defn entity-scalar-equal?
  [^:option<datascript.impl.entity/Entity> maybe-entity
   ^:keyword attr
   ^:Datascript_runtime.Data_value.t expected]
  (if-some [entity maybe-entity]
    (match (entity/lookup-entity entity attr)
      (Some (datascript.impl.entity/EntityScalar actual))
      (Datascript_runtime.Data_value.equal actual expected)
      _ false)
    false))

(defn map-int-value [^:int value]
  (Datascript_runtime.Data_value.map_of_keyword_map
   (zipmap
    [:map]
    [(Datascript_runtime.Data_value.Int value)])))

(defn datom-triples-equal?
  [^:tuple<int;keyword;Datascript_runtime.Data_value.t> left
   ^:tuple<int;keyword;Datascript_runtime.Data_value.t> right]
  (and
   (= (tuple-get left 0) (tuple-get right 0))
   (= (tuple-get left 1) (tuple-get right 1))
   (Datascript_runtime.Data_value.equal
    (tuple-get left 2)
    (tuple-get right 2))))

(defn datom-triples-contain?
  [^:vector<tuple<int;keyword;Datascript_runtime.Data_value.t>> triples
   ^:tuple<int;keyword;Datascript_runtime.Data_value.t> expected]
  (loop [remaining triples]
    (if-some [triple (first remaining)]
      (if (datom-triples-equal? triple expected)
        true
        (recur (next remaining)))
      false)))

(defn datom-triples-equal-set?
  [^:vector<tuple<int;keyword;Datascript_runtime.Data_value.t>> actual
   ^:vector<tuple<int;keyword;Datascript_runtime.Data_value.t>> expected]
  (and
   (= (count actual) (count expected))
   (every?
    (fn [^:tuple<int;keyword;Datascript_runtime.Data_value.t> triple]
      (datom-triples-contain? actual triple))
    expected)))

(defn datom-triples-equal-vector?
  [^:vector<tuple<int;keyword;Datascript_runtime.Data_value.t>> actual
   ^:vector<tuple<int;keyword;Datascript_runtime.Data_value.t>> expected]
  (and
   (= (count actual) (count expected))
   (loop [index 0]
     (if (< index (count actual))
       (if
        (datom-triples-equal?
         (nth actual index)
         (nth expected index))
         (recur (inc index))
         false)
       true))))

(defn all-datom-triples [^datascript.db/DB database]
  (mapv
   (fn [^datascript.db/Datom datom]
     (tuple
      (.-e datom)
      (db/datom-attr datom)
      (.-v datom)))
   (d/datoms database :eavt)))

(defn find-entity-age
  [^datascript.db/DB database ^:string name]
  (if-some
    [name-datom
     (first
      (filter
       (fn [^datascript.db/Datom datom]
         (and
          (= :name (db/datom-attr datom))
          (Datascript_runtime.Data_value.equal
           (.-v datom)
           (Datascript_runtime.Data_value.String name))))
       (d/datoms database :eavt)))]
    (if-some [age-datom (db/search-ea database (.-e name-datom) :age)]
      (match (.-v age-datom)
        (Datascript_runtime.Data_value.Int age)
        (Some (tuple (.-e name-datom) age))
        _ None)
      None)
    None))

(defn ^:vector<datascript.db/tx-entry> increment-age-tx
  [^datascript.db/DB database ^:string name]
  (if-some [entity-age (find-entity-age database name)]
    (let [entity (tuple-get entity-age 0)
          age (tuple-get entity-age 1)
          entity-ref
          (Datascript_runtime.Data_value.Entity_id entity)]
      [(db/tx-add
        entity-ref
        :age
        (Datascript_runtime.Data_value.Int (inc age)))
       (db/tx-add
        entity-ref
        :had-birthday
       (Datascript_runtime.Data_value.Bool true))])
    (Stdlib.invalid_arg (str "No entity with name: " name))))

(defn ^:vector<datascript.db/tx-entry> installed-increment-age-tx
  [^datascript.db/DB database
   ^:Datascript_runtime.Data_value.t argument]
  (match argument
    (Datascript_runtime.Data_value.String name)
    (increment-age-tx database name)
    _
    (Stdlib.invalid_arg
     "The installed increment-age transaction function expects a string")))

(defn ^datascript.db/TxReport tempid-check-report
  [^datascript.db/DB database
   ^:map<int;Datascript_runtime.Data_value.t> value-tempids
   ^:map<int;bool> used-tempid-eids]
  (db/->TxReport
   database
   database
   []
   {}
   (Datascript_runtime.Data_value.Nil)
   {}
   value-tempids
   used-tempid-eids))

(deftest test-public-auto-tempid-api
  (let [before @db/*last-auto-tempid
        first-tempid (db/auto-tempid)
        second-tempid (db/auto-tempid)]
    (is (db/auto-tempid? first-tempid))
    (is (db/auto-tempid? second-tempid))
    (is
     (not
      (db/auto-tempid?
       (Datascript_runtime.Data_value.Entity_id 1))))
    (match first-tempid
      (Datascript_runtime.Data_value.Auto_tempid first-id)
      (match second-tempid
        (Datascript_runtime.Data_value.Auto_tempid second-id)
        (do
          (is (= (inc before) first-id))
          (is (= (inc first-id) second-id)))
        _ (is false))
      _ (is false))))

(deftest test-assoc-auto-tempids-recurses-before-transaction
  (let [database
        (d/empty-db
         {:profile {:db/valueType :db.type/ref}
          :children {:db/valueType :db.type/ref
                     :db/cardinality :db.cardinality/many}})
        before @db/*last-auto-tempid
        entries
        (db/tx-data
         [{:name "parent"
           :profile {:name "profile"}
           :children [{:name "first-child"}
                      {:name "second-child"}]}
          {:name "second-root"}])
        after-construction @db/*last-auto-tempid
        prepared (db/assoc-auto-tempids database entries)
        after-association @db/*last-auto-tempid
        prepared-again (db/assoc-auto-tempids database prepared)
        report
        (db/transact-tx-data-impl
         (tempid-check-report database {} {})
         prepared-again)]
    (is (= before after-construction))
    (is (= (+ before 5) after-association))
    (is (= after-association @db/*last-auto-tempid))
    (is
     (datom-triples-equal-set?
      (all-datom-triples (:db-after report))
      [(tuple
        1 :name
        (Datascript_runtime.Data_value.String "parent"))
       (tuple 1 :profile (Datascript_runtime.Data_value.Ref 2))
       (tuple 1 :children (Datascript_runtime.Data_value.Ref 3))
       (tuple 1 :children (Datascript_runtime.Data_value.Ref 4))
       (tuple
        2 :name
        (Datascript_runtime.Data_value.String "profile"))
       (tuple
        3 :name
        (Datascript_runtime.Data_value.String "first-child"))
       (tuple
        4 :name
        (Datascript_runtime.Data_value.String "second-child"))
       (tuple
        5 :name
        (Datascript_runtime.Data_value.String "second-root"))]))))

(deftest test-assoc-auto-tempids-preserves-explicit-and-non-ref-values
  (let [database (d/empty-db)
        before @db/*last-auto-tempid
        entries
        (db/tx-data
         [{:db/id 10
           :payload {:map 1}}])
        prepared (db/assoc-auto-tempids database entries)
        report
        (db/transact-tx-data-impl
         (tempid-check-report database {} {})
         prepared)]
    (is (= before @db/*last-auto-tempid))
    (is
     (datom-triples-equal-set?
      (all-datom-triples (:db-after report))
      [(tuple 10 :payload (map-int-value 1))]))))

(deftest test-public-check-value-tempids
  (let [database (d/empty-db)
        tempid (db/auto-tempid)
        tempid-value
        (Datascript_runtime.Data_value.Ref_to tempid)
        valid
        (tempid-check-report database {7 tempid-value} {7 true})
        invalid
        (tempid-check-report database {7 tempid-value} {})]
    (is (= valid (db/check-value-tempids valid)))
    (match tempid
      (Datascript_runtime.Data_value.Auto_tempid tempid-id)
      (is
       (thrown-msg?
        (str
         "Tempids used only as value in transaction: "
         "(#datascript/AutoTempid ["
         (Stdlib.string_of_int tempid-id)
         "])")
        (db/check-value-tempids invalid)))
      _ (is false))))

(deftest test-with
  (let [db  (-> (d/empty-db {:aka {:db/cardinality :db.cardinality/many}})
              (d/db-with [[:db/add 1 :name "Ivan"]])
              (d/db-with [[:db/add 1 :name "Petr"]])
              (d/db-with [[:db/add 1 :aka  "Devil"]])
              (d/db-with [[:db/add 1 :aka  "Tupen"]]))]

    (is
     (query-relation-equal?
      (d/q '[:find ?v
             :where [1 :name ?v]] db)
      [(query-value-row
        (Datascript_runtime.Data_value.String "Petr"))]))
    (is
     (query-relation-equal?
      (d/q '[:find ?v
             :where [1 :aka ?v]] db)
      [(query-value-row
        (Datascript_runtime.Data_value.String "Devil"))
       (query-value-row
        (Datascript_runtime.Data_value.String "Tupen"))]))
    
    (testing "Retract"
      (let [db  (-> db
                  (d/db-with [[:db/retract 1 :name "Petr"]])
                  (d/db-with [[:db/retract 1 :aka  "Devil"]]))]

        (is
         (query-relation-equal?
          (d/q '[:find ?v
                 :where [1 :name ?v]] db)
          []))
        (is
         (query-relation-equal?
          (d/q '[:find ?v
                 :where [1 :aka ?v]] db)
          [(query-value-row
            (Datascript_runtime.Data_value.String "Tupen"))]))

        (is
         (if-some [entity (d/entity db 1)]
           (and
            (= 1 (count entity))
            (entity-scalar-equal?
             (Some entity)
             :aka
             (Datascript_runtime.Data_value.Set
              (list
               (Datascript_runtime.Data_value.String "Tupen")))))
           false))))

    (testing "Cannot retract what's not there"
      (let [db  (-> db
                  (d/db-with [[:db/retract 1 :name "Ivan"]]))]
        (is
         (query-relation-equal?
          (d/q '[:find ?v
                 :where [1 :name ?v]] db)
          [(query-value-row
            (Datascript_runtime.Data_value.String "Petr"))])))))
  
  (testing "Skipping nils in tx"
    (let [db (-> (d/empty-db)
               (d/db-with [[:db/add 1 :attr 2]
                           nil
                           [:db/add 3 :attr 4]]))]
      (is (=
           [(tuple 1 :attr (Datascript_runtime.Data_value.Int 2))
            (tuple 3 :attr (Datascript_runtime.Data_value.Int 4))]
           (map
            (fn [^datascript.db/Datom datom]
              (tuple
               (.-e datom)
               (db/datom-attr datom)
               (.-v datom)))
            (d/datoms db :eavt)))))))

(deftest test-with-datoms
  (testing "keeps tx number"
    (let [db (-> (d/empty-db)
               (d/db-with [(d/datom 1 :name "Oleg")
                           (d/datom 1 :age  17 (+ 1 d/tx0))
                           [:db/add 1 :aka  "x" (+ 2 d/tx0)]]))]
      (is (=
           [(tuple 1 :age
                   (Datascript_runtime.Data_value.Int 17)
                   (+ 1 d/tx0))
            (tuple 1 :aka
                   (Datascript_runtime.Data_value.String "x")
                   (+ 2 d/tx0))
            (tuple 1 :name
                   (Datascript_runtime.Data_value.String "Oleg")
                   d/tx0)]
           (map
            (fn [^datascript.db/Datom datom]
              (tuple
               (.-e datom)
               (db/datom-attr datom)
               (.-v datom)
               (db/datom-tx datom)))
            (d/datoms db :eavt))))))
  
  (testing "retraction"
    (let [db (-> (d/empty-db)
               (d/db-with [(d/datom 1 :name "Oleg")
                           (d/datom 1 :age  17)
                           (d/datom 1 :name "Oleg" d/tx0 false)]))]
      (is (=
           [(tuple 1 :age
                   (Datascript_runtime.Data_value.Int 17)
                   d/tx0)]
           (map
            (fn [^datascript.db/Datom datom]
              (tuple
               (.-e datom)
               (db/datom-attr datom)
               (.-v datom)
               (db/datom-tx datom)))
            (d/datoms db :eavt)))))))

(deftest test-retract-fns
  (let [db (-> (d/empty-db {:aka    {:db/cardinality :db.cardinality/many}
                            :friend {:db/valueType :db.type/ref}})
             (d/db-with [{:db/id 1, :name  "Ivan", :age 15, :aka ["X" "Y" "Z"], :friend 2}
                         {:db/id 2, :name  "Petr", :age 37}]))]
    (let [db (d/db-with db [[:db.fn/retractEntity 1]])]
      (is
       (query-relation-equal?
        (d/q '[:find ?a ?v
               :where [1 ?a ?v]] db)
        []))
      (is
       (query-relation-equal?
        (d/q '[:find ?a ?v
               :where [2 ?a ?v]] db)
        [(query-attr-value-row
          :name
          (Datascript_runtime.Data_value.String "Petr"))
         (query-attr-value-row
          :age
          (Datascript_runtime.Data_value.Int 37))])))

    (is (= (d/db-with db [[:db.fn/retractEntity 1]])
          (d/db-with db [[:db/retractEntity 1]])))

    (testing "Retract entitiy with incoming refs"
      (is
       (query-relation-equal?
        (d/q '[:find ?e :where [1 :friend ?e]] db)
        [(query-ref-row 2)]))
      
      (let [db (d/db-with db [[:db.fn/retractEntity 2]])]
        (is
         (query-relation-equal?
          (d/q '[:find ?e :where [1 :friend ?e]] db)
          []))))
    
    (let [db (d/db-with db [[:db.fn/retractAttribute 1 :name]])]
      (is
       (query-relation-equal?
        (d/q '[:find ?a ?v
               :where [1 ?a ?v]] db)
        [(query-attr-value-row
          :age
          (Datascript_runtime.Data_value.Int 15))
         (query-attr-value-row
          :aka
          (Datascript_runtime.Data_value.String "X"))
         (query-attr-value-row
          :aka
          (Datascript_runtime.Data_value.String "Y"))
         (query-attr-value-row
          :aka
          (Datascript_runtime.Data_value.String "Z"))
         (query-attr-value-row
          :friend
          (Datascript_runtime.Data_value.Ref 2))]))
      (is
       (query-relation-equal?
        (d/q '[:find ?a ?v
               :where [2 ?a ?v]] db)
        [(query-attr-value-row
          :name
          (Datascript_runtime.Data_value.String "Petr"))
         (query-attr-value-row
          :age
          (Datascript_runtime.Data_value.Int 37))])))

    (let [db (d/db-with db [[:db.fn/retractAttribute 1 :aka]])]
      (is
       (query-relation-equal?
        (d/q '[:find ?a ?v
               :where [1 ?a ?v]] db)
        [(query-attr-value-row
          :name
          (Datascript_runtime.Data_value.String "Ivan"))
         (query-attr-value-row
          :age
          (Datascript_runtime.Data_value.Int 15))
         (query-attr-value-row
          :friend
          (Datascript_runtime.Data_value.Ref 2))]))
      (is
       (query-relation-equal?
        (d/q '[:find ?a ?v
               :where [2 ?a ?v]] db)
        [(query-attr-value-row
          :name
          (Datascript_runtime.Data_value.String "Petr"))
         (query-attr-value-row
          :age
          (Datascript_runtime.Data_value.Int 37))])))))

(deftest test-retract-without-value-issue-339
  (let [db (-> (d/empty-db {:aka    {:db/cardinality :db.cardinality/many}
                            :friend {:db/valueType :db.type/ref}})
             (d/db-with [{:db/id 1, :name  "Ivan", :age 15, :aka ["X" "Y" "Z"], :friend 2}
                         {:db/id 2, :name  "Petr", :age 37, :employed? true, :married? false}]))]
    (let [db' (d/db-with db [[:db/retract 1 :name]
                             [:db/retract 1 :aka]
                             [:db/retract 2 :employed?]
                             [:db/retract 2 :married?]])]
      (is
       (datom-triples-equal-set?
        (all-datom-triples db')
        [(tuple
          1 :age
          (Datascript_runtime.Data_value.Int 15))
         (tuple
          1 :friend
          (Datascript_runtime.Data_value.Ref 2))
         (tuple
          2 :name
          (Datascript_runtime.Data_value.String "Petr"))
         (tuple
          2 :age
          (Datascript_runtime.Data_value.Int 37))])))
    (let [db' (d/db-with db [[:db/retract 2 :employed? false]])]
      (let [datoms (vec (d/datoms db' :eavt 2 :employed?))]
        (is (= 1 (count datoms)))
        (is
         (datom-triples-equal?
          (let [datom (nth datoms 0)]
            (tuple
             (.-e datom)
             (db/datom-attr datom)
             (.-v datom)))
          (tuple
           2
           :employed?
           (Datascript_runtime.Data_value.Bool true))))))))
  
(deftest test-retract-fns-not-found
  (let [db  (-> (d/empty-db {:name {:db/unique :db.unique/identity}})
              (d/db-with  [[:db/add 1 :name "Ivan"]]))
        all all-datom-triples]
    (are [op]
      (datom-triples-equal-set?
       (all (d/db-with db [op]))
       [(tuple
         1
         :name
         (Datascript_runtime.Data_value.String "Ivan"))])
      (nth (db/tx-data [[:db/retract             2 :name "Petr"]]) 0)
      (nth (db/tx-data [[:db.fn/retractAttribute 2 :name]]) 0)
      (nth (db/tx-data [[:db.fn/retractEntity    2]]) 0)
      (nth (db/tx-data [[:db/retractEntity       2]]) 0)
      (nth (db/tx-data [[:db/retract
                         [:name "Petr"] :name "Petr"]]) 0)
      (nth (db/tx-data [[:db.fn/retractAttribute
                         [:name "Petr"] :name]]) 0)
      (nth (db/tx-data [[:db.fn/retractEntity
                         [:name "Petr"]]]) 0))
         
    (are [op] (= [[] []] 
                [(all (d/db-with db [op]))
                 (all (d/db-with db [op op]))]) ;; idempotency
      (nth (db/tx-data [[:db/retract             1 :name "Ivan"]]) 0)
      (nth (db/tx-data [[:db.fn/retractAttribute 1 :name]]) 0)
      (nth (db/tx-data [[:db.fn/retractEntity    1]]) 0)
      (nth (db/tx-data [[:db/retractEntity       1]]) 0)
      (nth (db/tx-data [[:db/retract
                         [:name "Ivan"] :name "Ivan"]]) 0)
      (nth (db/tx-data [[:db.fn/retractAttribute
                         [:name "Ivan"] :name]]) 0)
      (nth (db/tx-data [[:db.fn/retractEntity
                         [:name "Ivan"]]]) 0))))

(deftest test-transact!
  (let [conn (d/create-conn {:aka {:db/cardinality :db.cardinality/many}})]
    (d/transact! conn [[:db/add 1 :name "Ivan"]])
    (d/transact! conn [[:db/add 1 :name "Petr"]])
    (d/transact! conn [[:db/add 1 :aka  "Devil"]])
    (d/transact! conn [[:db/add 1 :aka  "Tupen"]])

    (is
     (query-relation-equal?
      (d/q '[:find ?v
             :where [1 :name ?v]] @conn)
      [(query-value-row
        (Datascript_runtime.Data_value.String "Petr"))]))
    (is
     (query-relation-equal?
      (d/q '[:find ?v
             :where [1 :aka ?v]] @conn)
      [(query-value-row
        (Datascript_runtime.Data_value.String "Devil"))
       (query-value-row
        (Datascript_runtime.Data_value.String "Tupen"))]))))

(deftest test-db-fn-cas
  (let [conn (d/create-conn)]
    (d/transact! conn [[:db/add 1 :weight 200]])
    (d/transact! conn [[:db.fn/cas 1 :weight 200 300]])
    (is
     (match (:weight (d/entity @conn 1))
       (Some
        (datascript.impl.entity/EntityScalar
         (Datascript_runtime.Data_value.Int value)))
       (= value 300)
       _ false))
    (d/transact! conn [[:db/cas 1 :weight 300 400]])
    (is
     (match (:weight (d/entity @conn 1))
       (Some
        (datascript.impl.entity/EntityScalar
         (Datascript_runtime.Data_value.Int value)))
       (= value 400)
       _ false))
    (is (thrown-msg? ":db.fn/cas failed on datom [1 :weight 400], expected 200"
          (d/transact! conn [[:db.fn/cas 1 :weight 200 210]]))))
  
  (let [conn (d/create-conn {:label {:db/cardinality :db.cardinality/many}})]
    (d/transact! conn [[:db/add 1 :label :x]])
    (d/transact! conn [[:db/add 1 :label :y]])
    (d/transact! conn [[:db.fn/cas 1 :label :y :z]])
    (is
     (match (:label (d/entity @conn 1))
       (Some
        (datascript.impl.entity/EntityScalar value))
       (Datascript_runtime.Data_value.equal
        value
        (Datascript_runtime.Data_value.Set
         (list
          (Datascript_runtime.Data_value.Keyword ":x")
          (Datascript_runtime.Data_value.Keyword ":y")
          (Datascript_runtime.Data_value.Keyword ":z"))))
       _ false))
    (is (thrown-msg? ":db.fn/cas failed on datom [1 :label (:x :y :z)], expected :s"
          (d/transact! conn [[:db.fn/cas 1 :label :s :t]]))))

  (let [conn (d/create-conn)]
    (d/transact! conn [[:db/add 1 :name "Ivan"]])
    (d/transact! conn [[:db.fn/cas 1 :age nil 42]])
    (is
     (match (:age (d/entity @conn 1))
       (Some
        (datascript.impl.entity/EntityScalar
         (Datascript_runtime.Data_value.Int value)))
       (= value 42)
       _ false))
    (is (thrown-msg? ":db.fn/cas failed on datom [1 :age 42], expected nil"
          (d/transact! conn [[:db.fn/cas 1 :age nil 4711]]))))

  (let [conn (d/create-conn)]
    (is (thrown-msg? "Can't use tempid in '[:db.fn/cas -1 :attr nil :val]'. Tempids are allowed in :db/add only"
          (d/transact! conn [[:db/add    -1 :name "Ivan"]
                             [:db.fn/cas -1 :attr nil :val]])))))

(deftest test-db-fn
  (let [conn (d/create-conn {:aka {:db/cardinality :db.cardinality/many}})]
    (d/transact! conn [{:db/id 1 :name "Ivan" :age 31}])
    (d/transact! conn [[:db/add 1 :name "Petr"]])
    (d/transact! conn [[:db/add 1 :aka  "Devil"]])
    (d/transact! conn [[:db/add 1 :aka  "Tupen"]])
    (is
     (query-relation-equal?
      (d/q '[:find ?v ?a
             :where [?e :name ?v]
             [?e :age ?a]] @conn)
      [(array
        (query-types/value-result
         (Datascript_runtime.Data_value.String "Petr"))
        (query-types/value-result
         (Datascript_runtime.Data_value.Int 31)))]))
    (is
     (query-relation-equal?
      (d/q '[:find ?v
             :where [?e :aka ?v]] @conn)
      [(query-value-row
        (Datascript_runtime.Data_value.String "Devil"))
       (query-value-row
        (Datascript_runtime.Data_value.String "Tupen"))]))
    (is (thrown-msg? "No entity with name: Bob"
          (d/transact! conn [[:db.fn/call increment-age-tx "Bob"]])))
    (let [^datascript.db/TxReport report
          (d/transact! conn [[:db.fn/call increment-age-tx "Petr"]])
          db-after (.-db-after report)
          e (d/entity db-after 1)]
      (is
       (entity-scalar-equal?
        e :age (Datascript_runtime.Data_value.Int 32)))
      (is
       (entity-scalar-equal?
        e :had-birthday (Datascript_runtime.Data_value.Bool true))))
    
    (let [^datascript.db/TxReport report
          (d/transact! conn
                       [[:db.fn/call
                         (fn [^datascript.db/DB _]
                           [(db/tx-add
                             (Datascript_runtime.Data_value.Entity_id -1)
                             :name
                             (Datascript_runtime.Data_value.String
                              "Oleg"))])]])
          db-after (.-db-after report)
          e (d/entity db-after 2)]
      (is
       (entity-scalar-equal?
        e :name (Datascript_runtime.Data_value.String "Oleg"))))
    
    (let [^datascript.db/TxReport report
          (d/transact! conn
                       [[:db.fn/call
                         (fn [^datascript.db/DB _]
                           [(db/tx-add
                             (Datascript_runtime.Data_value.Entity_id -1)
                             :name
                             (Datascript_runtime.Data_value.String
                              "Vera"))])]])
          db-after (.-db-after report)
          tempids (.-tempids report)
          entity-id
          (get
           tempids
           (Datascript_runtime.Data_value.Ref_to
            (Datascript_runtime.Data_value.Entity_id -1)))
          e (if-some [entity-id entity-id]
              (d/entity
               db-after
               (Datascript_runtime.Data_value.Entity_id entity-id))
              None)]
      (is
       (entity-scalar-equal?
        e :name (Datascript_runtime.Data_value.String "Vera"))))))

(deftest test-db-ident-fn
  (let [conn (d/create-conn {:name {:db/unique :db.unique/identity}})]
    (d/transact! conn [{:db/id    1
                        :name     "Petr"
                        :age      31
                        :db/ident :Petr}
                       {:db/ident :inc-age
                        :db/fn    installed-increment-age-tx}])
    (is (thrown-msg? "Can’t find entity for transaction fn :unknown-fn"
          (d/transact! conn [[:unknown-fn]])))
    (is (thrown-msg? "Entity :Petr expected to have :db/fn attribute with fn? value"
          (d/transact! conn [[:Petr]])))
    (is (thrown-msg? "No entity with name: Bob"
          (d/transact! conn [[:inc-age "Bob"]])))
    (d/transact! conn [[:inc-age "Petr"]])
    (let [e (d/entity @conn 1)]
      (is
       (entity-scalar-equal?
        e :age (Datascript_runtime.Data_value.Int 32)))
      (is
       (entity-scalar-equal?
        e :had-birthday (Datascript_runtime.Data_value.Bool true))))))

(deftest test-resolve-eid
  (let [db (d/empty-db {:name {:db/unique :db.unique/identity}
                        :aka  {:db/unique :db.unique/identity
                               :db/cardinality :db.cardinality/many}
                        :ref  {:db/valueType :db.type/ref}})]
    (let [report (d/with db [[:db/add -1 :name "Ivan"]
                             [:db/add -1 :age 19]
                             [:db/add -2 :name "Petr"]
                             [:db/add -2 :age 22]
                             [:db/add "Serg" :name "Sergey"]
                             [:db/add "Serg" :age 30]])]
      (let [tempids (.-tempids report)]
        (is
         (=
          (Some 1)
          (get
           tempids
           (Datascript_runtime.Data_value.Ref_to
            (Datascript_runtime.Data_value.Entity_id -1)))))
        (is
         (=
          (Some 2)
          (get
           tempids
           (Datascript_runtime.Data_value.Ref_to
            (Datascript_runtime.Data_value.Entity_id -2)))))
        (is
         (=
          (Some 3)
          (get
           tempids
           (Datascript_runtime.Data_value.Ref_to
            (Datascript_runtime.Data_value.Temp_id "Serg")))))
        (is
         (=
          (Some (+ d/tx0 1))
          (get
           tempids
           (Datascript_runtime.Data_value.Ref_to
            (Datascript_runtime.Data_value.Current_tx))))))
      (is
       (datom-triples-equal-set?
        (all-datom-triples (.-db-after report))
        [(tuple
          1 :name
          (Datascript_runtime.Data_value.String "Ivan"))
         (tuple
          1 :age
          (Datascript_runtime.Data_value.Int 19))
         (tuple
          2 :name
          (Datascript_runtime.Data_value.String "Petr"))
         (tuple
          2 :age
          (Datascript_runtime.Data_value.Int 22))
         (tuple
          3 :name
          (Datascript_runtime.Data_value.String "Sergey"))
         (tuple
          3 :age
          (Datascript_runtime.Data_value.Int 30))])))

    (let [db' (d/db-with db [[:db/add -1 :name "Ivan"]
                             [:db/add -2 :ref -1]])]
      (is
       (datom-triples-equal-set?
        (all-datom-triples db')
        [(tuple
          1 :name
          (Datascript_runtime.Data_value.String "Ivan"))
         (tuple
          2 :ref
          (Datascript_runtime.Data_value.Ref 1))])))

    (testing "issue-363"
      (let [db' (-> db
                  (d/db-with [[:db/add -1 :name "Ivan"]])
                  (d/db-with [[:db/add -1 :name "Ivan"]
                              [:db/add -2 :ref -1]]))]
        (is
         (datom-triples-equal-set?
          (all-datom-triples db')
          [(tuple
            1 :name
            (Datascript_runtime.Data_value.String "Ivan"))
           (tuple
            2 :ref
            (Datascript_runtime.Data_value.Ref 1))])))
      (let [db' (-> db
                  (d/db-with [[:db/add -1 :aka "Batman"]])
                  (d/db-with [[:db/add -1 :aka "Batman"]
                              [:db/add -2 :ref -1]]))]
        (is
         (datom-triples-equal-set?
          (all-datom-triples db')
          [(tuple
            1 :aka
            (Datascript_runtime.Data_value.String "Batman"))
           (tuple
            2 :ref
            (Datascript_runtime.Data_value.Ref 1))]))))))

(deftest test-tempid-ref-issue-295
  (let [db (-> (d/empty-db {:ref {:db/unique :db.unique/identity
                                  :db/valueType :db.type/ref}})
             (d/db-with [[:db/add -1 :name "Ivan"]
                         [:db/add -2 :name "Petr"]
                         [:db/add -1 :ref -2]]))]
    (is
     (datom-triples-equal-set?
      (all-datom-triples db)
      [(tuple
        1 :name
        (Datascript_runtime.Data_value.String "Ivan"))
       (tuple
        1 :ref
        (Datascript_runtime.Data_value.Ref 2))
       (tuple
        2 :name
        (Datascript_runtime.Data_value.String "Petr"))]))))

(deftest test-resolve-eid-refs
  (let [conn (d/create-conn {:friend {:db/valueType :db.type/ref
                                      :db/cardinality :db.cardinality/many}})
        tx   (d/transact! conn [{:name "Sergey"
                                 :friend [-1 -2]}
                                [:db/add -1  :name "Ivan"]
                                [:db/add -2  :name "Petr"]
                                [:db/add "B" :name "Boris"]
                                [:db/add "B" :friend -3]
                                [:db/add -3  :name "Oleg"]
                                [:db/add -3  :friend "B"]])]
    (let [tempids (.-tempids tx)]
      (is
       (=
        (Some 2)
        (get
         tempids
         (Datascript_runtime.Data_value.Ref_to
          (Datascript_runtime.Data_value.Entity_id -1)))))
      (is
       (=
        (Some 3)
        (get
         tempids
         (Datascript_runtime.Data_value.Ref_to
          (Datascript_runtime.Data_value.Entity_id -2)))))
      (is
       (=
        (Some 4)
        (get
         tempids
         (Datascript_runtime.Data_value.Ref_to
          (Datascript_runtime.Data_value.Temp_id "B")))))
      (is
       (=
        (Some 5)
        (get
         tempids
         (Datascript_runtime.Data_value.Ref_to
          (Datascript_runtime.Data_value.Entity_id -3)))))
      (is
       (=
        (Some (+ d/tx0 1))
        (get
         tempids
         (Datascript_runtime.Data_value.Ref_to
          (Datascript_runtime.Data_value.Current_tx))))))
    (is
     (query-relation-equal?
      (d/q '[:find ?fn
             :in $ ?n
             :where [?e :name ?n]
             [?e :friend ?fe]
             [?fe :name ?fn]]
           @conn "Sergey")
      [(query-value-row
        (Datascript_runtime.Data_value.String "Ivan"))
       (query-value-row
        (Datascript_runtime.Data_value.String "Petr"))]))
    (is
     (query-relation-equal?
      (d/q '[:find ?fn
             :in $ ?n
             :where [?e :name ?n]
             [?e :friend ?fe]
             [?fe :name ?fn]]
           @conn "Boris")
      [(query-value-row
        (Datascript_runtime.Data_value.String "Oleg"))]))
    (is
     (query-relation-equal?
      (d/q '[:find ?fn
             :in $ ?n
             :where [?e :name ?n]
             [?e :friend ?fe]
             [?fe :name ?fn]]
           @conn "Oleg")
      [(query-value-row
        (Datascript_runtime.Data_value.String "Boris"))]))

    (let [db (d/empty-db {:friend {:db/valueType :db.type/ref}
                          :comp   {:db/valueType :db.type/ref, :db/isComponent true}
                          :multi  {:db/cardinality :db.cardinality/many}})]
      (testing "Unused tempid" ;; issue-304
        (is (thrown-msg? "Tempids used only as value in transaction: (-2)"
              (d/db-with db [[:db/add -1 :friend -2]])))
        (is (thrown-msg? "Tempids used only as value in transaction: (-2)"
              (d/db-with db [{:db/id -1 :friend -2}])))
        (is (thrown-msg? "Tempids used only as value in transaction: (-1)"
              (d/db-with db [{:db/id -1}
                             [:db/add -2 :friend -1]])))
        ; Needs issue-357
        ; (is (thrown-msg? "Tempids used only as value in transaction: (-1)"
        ;       (d/db-with db [{:db/id -1 :comp {}}
        ;                      [:db/add -2 :friend -1]])))
        (is (thrown-msg? "Tempids used only as value in transaction: (-1)"
              (d/db-with db [{:db/id -1 :multi []}
                             [:db/add -2 :friend -1]])))))))

(deftest test-resolve-current-tx
  (doseq
    [tx-tempid
     [(Datascript_runtime.Data_value.Current_tx)
      (Datascript_runtime.Data_value.Temp_id "datomic.tx")
      (Datascript_runtime.Data_value.Temp_id "datascript.tx")]]
    (testing tx-tempid
      (let [conn (d/create-conn {:created-at {:db/valueType :db.type/ref}})
            tx1  (d/transact! conn [{:name "X"
                                     :created-at
                                     (Datascript_runtime.Data_value.Ref_to
                                      tx-tempid)}
                                    {:db/id tx-tempid, :prop1 "prop1"}
                                    [:db/add tx-tempid :prop2 "prop2"]
                                    [:db/add -1 :name "Y"]
                                    [:db/add
                                     -1
                                     :created-at
                                     (Datascript_runtime.Data_value.Ref_to
                                      tx-tempid)]])]
        (is
         (query-relation-equal?
          (d/q '[:find ?e ?a ?v :where [?e ?a ?v]] @conn)
          [(query-eav-row
            1 :name
            (Datascript_runtime.Data_value.String "X"))
           (query-eav-row
            1 :created-at
            (Datascript_runtime.Data_value.Ref (+ d/tx0 1)))
           (query-eav-row
            (+ d/tx0 1) :prop1
            (Datascript_runtime.Data_value.String "prop1"))
           (query-eav-row
            (+ d/tx0 1) :prop2
            (Datascript_runtime.Data_value.String "prop2"))
           (query-eav-row
            2 :name
            (Datascript_runtime.Data_value.String "Y"))
           (query-eav-row
            2 :created-at
            (Datascript_runtime.Data_value.Ref (+ d/tx0 1)))]))
        (let [tempids (.-tempids tx1)]
          (is
           (=
            (Some 2)
            (get
             tempids
             (Datascript_runtime.Data_value.Ref_to
              (Datascript_runtime.Data_value.Entity_id -1)))))
          (is
           (=
            (Some (+ d/tx0 1))
            (get
             tempids
             (Datascript_runtime.Data_value.Ref_to
              (Datascript_runtime.Data_value.Current_tx)))))
          (is
           (=
            (Some (+ d/tx0 1))
            (get
             tempids
             (Datascript_runtime.Data_value.Ref_to tx-tempid)))))
        (let [tx2   (d/transact! conn [[:db/add tx-tempid :prop3 "prop3"]])
              tx-id
              (get
               (.-tempids tx2)
               (Datascript_runtime.Data_value.Ref_to tx-tempid))]
          (is (= tx-id (Some (+ d/tx0 2))))
          (is
           (if-some [tx-id tx-id]
             (if-some
               [datom
                (d/find-datom
                 @conn
                 :eavt
                 (Datascript_runtime.Data_value.Int tx-id)
                 :prop3)]
               (Datascript_runtime.Data_value.equal
                (.-v datom)
                (Datascript_runtime.Data_value.String "prop3"))
               false)
             false)))
        (let [tx3   (d/transact! conn [{:db/id tx-tempid, :prop4 "prop4"}])
              tx-id
              (get
               (.-tempids tx3)
               (Datascript_runtime.Data_value.Ref_to tx-tempid))]
          (is (= tx-id (Some (+ d/tx0 3))))
          (is
           (if-some [tx-id tx-id]
             (if-some
               [datom
                (d/find-datom
                 @conn
                 :eavt
                 (Datascript_runtime.Data_value.Int tx-id)
                 :prop4)]
               (Datascript_runtime.Data_value.equal
                (.-v datom)
                (Datascript_runtime.Data_value.String "prop4"))
               false)
             false)))))))

(deftest test-transient-issue-294
  "db.fn/retractEntity retracts attributes of adjacent entities issue-294"
  (let [db (reduce #(d/db-with
                     %1
                     [{:db/id
                       (Datascript_runtime.Data_value.Entity_id %2)
                       :a1 1
                       :a2 2
                       :a3 3}])
             (d/empty-db)
             (range 1 10))
        report (d/with db [[:db.fn/retractEntity 1]
                           [:db.fn/retractEntity 2]])]
    (is
     (datom-triples-equal-vector?
      (mapv
       (fn [^datascript.db/Datom datom]
         (tuple
          (.-e datom)
          (db/datom-attr datom)
          (.-v datom)))
       (:tx-data report))
      [(tuple 1 :a1 (Datascript_runtime.Data_value.Int 1))
       (tuple 1 :a2 (Datascript_runtime.Data_value.Int 2))
       (tuple 1 :a3 (Datascript_runtime.Data_value.Int 3))
       (tuple 2 :a1 (Datascript_runtime.Data_value.Int 1))
       (tuple 2 :a2 (Datascript_runtime.Data_value.Int 2))
       (tuple 2 :a3 (Datascript_runtime.Data_value.Int 3))]))))

(deftest test-large-ids-issue-292
  (let [db (d/empty-db {:ref {:db/valueType :db.type/ref}})]
    (is (thrown-msg? "Highest supported entity id is 2147483647, got 285873023227265"
          (d/with db [[:db/add 285873023227265 :name "Valerii"]])))
    (is (thrown-msg? "Highest supported entity id is 2147483647, got 285873023227265"
          (d/with db [{:db/id 285873023227265 :name "Valerii"}])))
    (is (thrown-msg? "Highest supported entity id is 2147483647, got 285873023227265"
          (d/with db [{:db/id 1 :ref 285873023227265}])))
    #?(:cljs
       (is (thrown-msg? "Highest supported entity id is 2147483647, got 285873023227265"
             (d/with db [(db/datom 285873023227265 :name 1)]))))
    #?(:cljs
       (is (thrown-msg? "Highest supported entity id is 2147483647, got 285873023227265"
             (d/with db [(db/datom 1 :ref 285873023227265)]))))))

(deftest test-uncomparable-issue-356
  (let [db (d/empty-db {:multi {:db/cardinality :db.cardinality/many}
                        :index {:db/index true}})]

    (let [db' (-> db
                (d/db-with [[:db/add     1 :single {:map 1}]])
                (d/db-with [[:db/retract 1 :single {:map 1}]])
                (d/db-with [[:db/add     1 :single {:map 2}]])
                (d/db-with [[:db/add     1 :single {:map 3}]]))]
      (is
       (datom-triples-equal-set?
        (all-datom-triples db')
        [(tuple 1 :single (map-int-value 3))]))
      (is
       (some?
        (d/find-datom
         db' :eavt
         (Datascript_runtime.Data_value.Int 1)
         :single
         (map-int-value 3))))
      (is
       (some?
        (d/find-datom
         db' :aevt
         :single
         (Datascript_runtime.Data_value.Int 1)
         (map-int-value 3)))))

    (let [db' (-> db
                (d/db-with [[:db/add 1 :multi {:map 1}]])
                (d/db-with [[:db/add 1 :multi {:map 1}]])
                (d/db-with [[:db/add 1 :multi {:map 2}]]))]
      (is
       (datom-triples-equal-set?
        (all-datom-triples db')
        [(tuple 1 :multi (map-int-value 1))
         (tuple 1 :multi (map-int-value 2))]))
      (is
       (some?
        (d/find-datom
         db' :eavt
         (Datascript_runtime.Data_value.Int 1)
         :multi
         (map-int-value 2))))
      (is
       (some?
        (d/find-datom
         db' :aevt
         :multi
         (Datascript_runtime.Data_value.Int 1)
         (map-int-value 2)))))

    (let [db' (-> db
                (d/db-with [[:db/add     1 :index {:map 1}]])
                (d/db-with [[:db/retract 1 :single {:map 1}]])
                (d/db-with [[:db/add     1 :index {:map 2}]])
                (d/db-with [[:db/add     1 :index {:map 3}]]))]
      (is
       (datom-triples-equal-set?
        (all-datom-triples db')
        [(tuple 1 :index (map-int-value 3))]))
      (is
       (some?
        (d/find-datom
         db' :eavt
         (Datascript_runtime.Data_value.Int 1)
         :index
         (map-int-value 3))))
      (is
       (some?
        (d/find-datom
         db' :aevt
         :index
         (Datascript_runtime.Data_value.Int 1)
         (map-int-value 3))))
      (is
       (some?
        (d/find-datom
         db' :avet
         :index
         (map-int-value 3)
         (Datascript_runtime.Data_value.Int 1)))))))

(deftest test-compare-numbers-js-issue-404
  (let [db  (d/db-with (d/empty-db) [{:num 42.5}])
        db' (d/db-with db [[:db/retract 1 :num 42]])]
    (is
     (datom-triples-equal-set?
      (all-datom-triples db')
      [(tuple
        1 :num
        (Datascript_runtime.Data_value.Float 42.5))]))))

(deftest test-transitive-type-compare-issue-386
  (let [txs    [(db/tx-data [{:block/uid "2LB4tlJGy"}])
                (db/tx-data [{:block/uid "2ON453J0Z"}])
                (db/tx-data [{:block/uid "2KqLLNbPg"}])
                (db/tx-data [{:block/uid "2L0dcD7yy"}])
                (db/tx-data [{:block/uid "2KqFNrhTZ"}])
                (db/tx-data [{:block/uid "2KdQmItUD"}])
                (db/tx-data [{:block/uid "2O8BcBfIL"}])
                (db/tx-data [{:block/uid "2L4ZbI7nK"}])
                (db/tx-data [{:block/uid "2KotiW36Z"}])
                (db/tx-data [{:block/uid "2O4o-y5J8"}])
                (db/tx-data [{:block/uid "2KimvuGko"}])
                (db/tx-data [{:block/uid "dTR20ficj"}])
                (db/tx-data [{:block/uid "wRmp6bXAx"}])
                (db/tx-data [{:block/uid "rfL-iQOZm"}])
                (db/tx-data [{:block/uid "tya6s422-"}])
                (db/tx-data [{:block/uid 45619}])]
        conn   (d/create-conn
                {:block/uid {:db/unique :db.unique/identity}})
        _      (doseq [tx txs] (d/transact! conn tx))
        db     @conn]
    (is (empty? (->> (d/datoms db :eavt)
                  (map
                   (fn [^datascript.db/Datom datom]
                     (Datascript_runtime.Data_value.Lookup_ref
                      (db/datom-attr datom)
                      (.-v datom))))
                  (remove
                   (fn
                     [^:Datascript_runtime.Data_value.entity_ref
                      entity-ref]
                     (d/entity db entity-ref))))))))

(deftest test-db-fn-returning-entity-without-db-id-issue-474
  (let [conn   (d/create-conn {})
        _      (d/transact!
                conn
                [[:db.fn/call
                  (fn [^datascript.db/DB _]
                    [(db/tx-entity
                      (zipmap
                       [:foo]
                       [(Datascript_runtime.Data_value.String
                         "bar")]))])]])
        db     @conn]
    (is
     (datom-triples-equal-set?
      (all-datom-triples db)
      [(tuple
        1 :foo
        (Datascript_runtime.Data_value.String "bar"))]))))
