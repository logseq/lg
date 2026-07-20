(ns datascript.test.transact
  (:require
   [clojure.test :refer [deftest is]]
   [datascript.core :as d]))

(defn throws? [f]
  (try
    (f)
    false
    (catch _
      true)))

(defn check-with []
  (let [schema {:aka {:db/cardinality :db.cardinality/many}}
        database
        (let [empty (d/empty-db schema)
              named (d/db-with empty [[:db/add 1 :name "Ivan"]])
              renamed (d/db-with named [[:db/add 1 :name "Petr"]])
              aliased (d/db-with renamed [[:db/add 1 :aka "Devil"]])]
          (d/db-with aliased [[:db/add 1 :aka "Tupen"]]))
        retracted
        (d/db-with
         database
         [[:db/retract 1 :name "Petr"]
          [:db/retract 1 :aka "Devil"]])
        absent-retraction
        (d/db-with database [[:db/retract 1 :name "Ivan"]])
        retracted-entity (d/entity retracted 1)
        with-nils
        (d/db-with
         (d/empty-db)
         [[:db/add 1 :attr 2]
          nil
          [:db/add 3 :attr 4]])]
    (and
     (= #{["Petr"]}
        (d/q '[:find ?v :where [1 :name ?v]] database))
     (= #{["Devil"] ["Tupen"]}
        (d/q '[:find ?v :where [1 :aka ?v]] database))
     (= #{} (d/q '[:find ?v :where [1 :name ?v]] retracted))
     (= #{["Tupen"]}
        (d/q '[:find ?v :where [1 :aka ?v]] retracted))
     (= #{"Tupen"} (:aka retracted-entity))
     (nil? (:name retracted-entity))
     (= 1 (count retracted-entity))
     (= #{["Petr"]}
        (d/q '[:find ?v :where [1 :name ?v]] absent-retraction))
     (= [[1 :attr 2] [3 :attr 4]]
        (vec
         (map
          (fn [datom] [(:e datom) (:a datom) (:v datom)])
          (d/datoms with-nils :eavt)))))))

(defn check-with-datoms []
  (let [database
        (d/db-with
         (d/empty-db)
         [(d/datom 1 :name "Oleg")
          (d/datom 1 :age 17 (+ 1 d/tx0))
          [:db/add 1 :aka "x" (+ 2 d/tx0)]])
        retracted
        (d/db-with
         (d/empty-db)
         [(d/datom 1 :name "Oleg")
          (d/datom 1 :age 17)
          (d/datom 1 :name "Oleg" d/tx0 false)])]
    (and
     (= #{[1 :age 17 (+ 1 d/tx0)]
          [1 :aka "x" (+ 2 d/tx0)]
          [1 :name "Oleg" d/tx0]}
        (set
         (map
          (fn [datom]
            [(:e datom) (:a datom) (:v datom) (:tx datom)])
          (d/datoms database :eavt))))
     (= [(d/datom 1 :age 17)]
        (vec (d/datoms retracted :eavt))))))

(defn check-retract-fns []
  (let [schema
        {:aka {:db/cardinality :db.cardinality/many}
         :friend {:db/valueType :db.type/ref}}
        database
        (d/db-with
         (d/empty-db schema)
         [{:db/id 1
           :name "Ivan"
           :age 15
           :aka ["X" "Y" "Z"]
           :friend 2}
          {:db/id 2 :name "Petr" :age 37}])
        without-first-entity
        (d/db-with database [[:db.fn/retractEntity 1]])
        without-attribute
        (d/db-with database [[:db.fn/retractAttribute 1 :aka]])
        without-name
        (d/db-with database [[:db.fn/retractAttribute 1 :name]])
        without-entity
        (d/db-with database [[:db.fn/retractEntity 2]])]
    (and
     (= #{}
        (d/q '[:find ?a ?v :where [1 ?a ?v]] without-first-entity))
     (= #{[:name "Petr"] [:age 37]}
        (d/q '[:find ?a ?v :where [2 ?a ?v]] without-first-entity))
     (= (d/db-with database [[:db.fn/retractEntity 1]])
        (d/db-with database [[:db/retractEntity 1]]))
     (= #{[2]}
        (d/q '[:find ?e :where [1 :friend ?e]] database))
     (= #{[:name "Ivan"] [:age 15] [:friend 2]}
        (d/q '[:find ?a ?v :where [1 ?a ?v]] without-attribute))
     (= #{[:name "Petr"] [:age 37]}
        (d/q '[:find ?a ?v :where [2 ?a ?v]] without-attribute))
     (= #{[:age 15]
          [:aka "X"]
          [:aka "Y"]
          [:aka "Z"]
          [:friend 2]}
        (d/q '[:find ?a ?v :where [1 ?a ?v]] without-name))
     (= #{[:name "Petr"] [:age 37]}
        (d/q '[:find ?a ?v :where [2 ?a ?v]] without-name))
     (nil? (d/entity without-entity 2))
     (nil? (d/find-datom without-entity :eavt 1 :friend))
     (= #{}
        (d/q '[:find ?e :where [1 :friend ?e]] without-entity)))))

(defn datom-triples [database]
  (set
   (map
    (fn [datom] [(:e datom) (:a datom) (:v datom)])
    (d/datoms database :eavt))))

(defn check-retract-without-value-issue-339 []
  (let [database
        (d/db-with
         (d/empty-db
          {:aka {:db/cardinality :db.cardinality/many}
           :friend {:db/valueType :db.type/ref}})
         [{:db/id 1
           :name "Ivan"
           :age 15
           :aka ["X" "Y" "Z"]
           :friend 2}
          {:db/id 2
           :name "Petr"
           :age 37
           :employed? true
           :married? false}])
        retracted
        (d/db-with
         database
         [[:db/retract 1 :name]
          [:db/retract 1 :aka]
          [:db/retract 2 :employed?]
          [:db/retract 2 :married?]])
        false-retraction
        (d/db-with database [[:db/retract 2 :employed? false]])]
    (and
     (= #{[1 :age 15]
          [1 :friend 2]
          [2 :name "Petr"]
          [2 :age 37]}
        (datom-triples retracted))
     (= [(d/datom 2 :employed? true)]
        (vec (d/datoms false-retraction :eavt 2 :employed?))))))

(defn check-retract-fns-not-found []
  (let [database
        (d/db-with
         (d/empty-db {:name {:db/unique :db.unique/identity}})
         [[:db/add 1 :name "Ivan"]])
        all-datoms (fn [db] (vec (d/datoms db :eavt)))
        missing-ops
        [[:db/retract 2 :name "Petr"]
         [:db.fn/retractAttribute 2 :name]
         [:db.fn/retractEntity 2]
         [:db/retractEntity 2]
         [:db/retract [:name "Petr"] :name "Petr"]
         [:db.fn/retractAttribute [:name "Petr"] :name]
         [:db.fn/retractEntity [:name "Petr"]]]
        existing-ops
        [[:db/retract 1 :name "Ivan"]
         [:db.fn/retractAttribute 1 :name]
         [:db.fn/retractEntity 1]
         [:db/retractEntity 1]
         [:db/retract [:name "Ivan"] :name "Ivan"]
         [:db.fn/retractAttribute [:name "Ivan"] :name]
         [:db.fn/retractEntity [:name "Ivan"]]]]
    (and
     (every?
      (fn [op]
        (= [(d/datom 1 :name "Ivan")]
           (all-datoms (d/db-with database [op]))))
      missing-ops)
     (every?
      (fn [op]
        (and
         (= [] (all-datoms (d/db-with database [op])))
         (= [] (all-datoms (d/db-with database [op op])))))
      existing-ops))))

(defn check-transact! []
  (let [connection
        (d/create-conn {:aka {:db/cardinality :db.cardinality/many}})
        _first (d/transact! connection [[:db/add 1 :name "Ivan"]])
        _second (d/transact! connection [[:db/add 1 :name "Petr"]])
        _third (d/transact! connection [[:db/add 1 :aka "Devil"]])
        _fourth (d/transact! connection [[:db/add 1 :aka "Tupen"]])]
    (and
     (= #{["Petr"]}
        (d/q '[:find ?v :where [1 :name ?v]] (d/db connection)))
     (= #{["Devil"] ["Tupen"]}
        (d/q '[:find ?v :where [1 :aka ?v]] (d/db connection))))))

(defn check-db-fn-cas []
  (let [connection (d/create-conn)
        _initial
        (d/transact! connection [[:db/add 1 :weight 200]])
        _updated
        (d/transact! connection [[:db.fn/cas 1 :weight 200 300]])
        failed
        (throws?
         (fn []
           (d/transact! connection [[:db.fn/cas 1 :weight 200 400]])))
        many-connection
        (d/create-conn
         {:label {:db/cardinality :db.cardinality/many}})
        _many-x
        (d/transact! many-connection [[:db/add 1 :label :x]])
        _many-y
        (d/transact! many-connection [[:db/add 1 :label :y]])
        _many-z
        (d/transact! many-connection [[:db.fn/cas 1 :label :y :z]])
        many-failed
        (throws?
         (fn []
           (d/transact! many-connection [[:db.fn/cas 1 :label :s :t]])))
        nil-connection (d/create-conn)
        _nil-name
        (d/transact! nil-connection [[:db/add 1 :name "Ivan"]])
        _nil-age
        (d/transact! nil-connection [[:db.fn/cas 1 :age nil 42]])
        nil-failed
        (throws?
         (fn []
           (d/transact! nil-connection [[:db.fn/cas 1 :age nil 4711]])))
        tempid-failed
        (throws?
         (fn []
           (d/transact!
            (d/create-conn)
            [[:db/add -1 :name "Ivan"]
             [:db.fn/cas -1 :attr nil :val]])))]
    (and
     failed
     (= 300
        (:v (d/find-datom (d/db connection) :eavt 1 :weight)))
     many-failed
     (= #{:x :y :z} (:label (d/entity (d/db many-connection) 1)))
     nil-failed
     (= 42 (:age (d/entity (d/db nil-connection) 1)))
     tempid-failed)))

(defn generated-entity-transaction [_database]
  [{:name "Oleg"}])

(defn generated-tempid-transaction [_database]
  [{:db/id -1 :name "Vera"}])

(defn check-db-fn []
  (let [connection
        (d/create-conn {:aka {:db/cardinality :db.cardinality/many}})
        inc-age
        (fn [database name]
          (if-some
           [row
            (first
             (d/q
              '{:find [?e ?age]
                :in [$ ?name]
                :where [[?e :name ?name]
                        [?e :age ?age]]}
              database
              name))]
            (let [entity-id (first row)
                  age (second row)]
              [{:db/id entity-id :age (inc age)}
               [:db/add entity-id :had-birthday true]])
            (throw (ex-info (str "No entity with name: " name) {}))))
        _initial
        (d/transact!
         connection
         [{:db/id 1 :name "Ivan" :age 31}])
        _renamed
        (d/transact! connection [[:db/add 1 :name "Petr"]])
        _alias-devil
        (d/transact! connection [[:db/add 1 :aka "Devil"]])
        _alias-tupen
        (d/transact! connection [[:db/add 1 :aka "Tupen"]])
        initial-name-result
        (d/q
         '[:find ?v ?a
           :where
           [?e :name ?v]
           [?e :age ?a]]
         (d/db connection))
        initial-name-ok
        (= #{["Petr" 31]}
           initial-name-result)
        initial-aliases-ok
        (= #{["Devil"] ["Tupen"]}
           (d/q '[:find ?alias :where [?entity :aka ?alias]]
                (d/db connection)))
        missing-failed
        (throws?
         (fn []
           (d/transact! connection [[:db.fn/call inc-age "Bob"]])))
        report
        (d/transact!
         connection
         [[:db.fn/call inc-age "Petr"]])
        generated-report
        (d/transact!
         connection
         [[:db.fn/call generated-entity-transaction]])
        generated-entity
        (d/entity (:db-after generated-report) 2)
        tempid-report
        (d/transact!
         connection
         [[:db.fn/call generated-tempid-transaction]])
        tempid-entity
        (d/entity
         (:db-after tempid-report)
         (get (:tempids tempid-report) -1))]
    (and
     initial-name-ok
     initial-aliases-ok
     missing-failed
     (= 32
        (:v (d/find-datom (:db-after report) :eavt 1 :age)))
     (= true
        (:v (d/find-datom (:db-after report)
                          :eavt 1 :had-birthday)))
     (= "Oleg" (:name generated-entity))
     (= "Vera" (:name tempid-entity)))))

(defn check-db-ident-fn []
  (let [connection
        (d/create-conn {:name {:db/unique :db.unique/identity}})
        inc-age
        (fn [database name]
          (if-some [entity (d/entity database [:name name])]
            [{:db/id (:db/id entity)
              :age (inc (:age entity))}
             [:db/add (:db/id entity) :had-birthday true]]
            (throw (ex-info (str "No entity with name: " name) {}))))
        _initial
        (d/transact!
         connection
         [{:db/id 1
           :name "Petr"
           :age 31
           :db/ident :Petr}
          {:db/ident :inc-age
           :db/fn inc-age}])
        unknown-failed
        (throws? (fn [] (d/transact! connection [[:unknown-fn]])))
        petr-failed
        (throws? (fn [] (d/transact! connection [[:Petr]])))
        bob-failed
        (throws? (fn [] (d/transact! connection [[:inc-age "Bob"]])))
        _updated (d/transact! connection [[:inc-age "Petr"]])
        entity (d/entity (d/db connection) 1)]
    (and
     unknown-failed
     petr-failed
     bob-failed
     (= 32 (:age entity))
     (= true (:had-birthday entity)))))

(defn check-resolve-eid []
  (let [database
        (d/empty-db
         {:name {:db/unique :db.unique/identity}
          :aka {:db/unique :db.unique/identity
                :db/cardinality :db.cardinality/many}
          :ref {:db/valueType :db.type/ref}})
        report
        (d/with
         database
         [[:db/add -1 :name "Ivan"]
          [:db/add -1 :age 19]
          [:db/add -2 :name "Petr"]
          [:db/add -2 :age 22]
          [:db/add "Serg" :name "Sergey"]
          [:db/add "Serg" :age 30]])
        refs-database
        (d/db-with
         database
         [[:db/add -1 :name "Ivan"]
          [:db/add -2 :ref -1]])
        reused-name-database
        (d/db-with
         (d/db-with database [[:db/add -1 :name "Ivan"]])
         [[:db/add -1 :name "Ivan"]
          [:db/add -2 :ref -1]])
        reused-alias-database
        (d/db-with
         (d/db-with database [[:db/add -1 :aka "Batman"]])
         [[:db/add -1 :aka "Batman"]
          [:db/add -2 :ref -1]])]
    (and
     (= {-1 1
         -2 2
         "Serg" 3
         :db/current-tx (+ d/tx0 1)}
        (:tempids report))
     (= #{[1 :name "Ivan"]
          [1 :age 19]
          [2 :name "Petr"]
          [2 :age 22]
          [3 :name "Sergey"]
          [3 :age 30]}
        (datom-triples (:db-after report)))
     (= #{[1 :name "Ivan"] [2 :ref 1]}
        (datom-triples refs-database))
     (= #{[1 :name "Ivan"] [2 :ref 1]}
        (datom-triples reused-name-database))
     (= #{[1 :aka "Batman"] [2 :ref 1]}
        (datom-triples reused-alias-database)))))

(defn check-tempid-ref-issue-295 []
  (let [database
        (d/db-with
         (d/empty-db
          {:ref {:db/unique :db.unique/identity
                 :db/valueType :db.type/ref}})
         [[:db/add -1 :name "Ivan"]
          [:db/add -2 :name "Petr"]
          [:db/add -1 :ref -2]])]
    (= #{[1 :name "Ivan"]
         [1 :ref 2]
         [2 :name "Petr"]}
       (datom-triples database))))

(defn current-tx-variant-ok? [tx-tempid]
  (let [connection
        (d/create-conn {:created-at {:db/valueType :db.type/ref}})
        tx1
        (d/transact!
         connection
         [{:name "X" :created-at tx-tempid}
          {:db/id tx-tempid :prop1 "prop1"}
          [:db/add tx-tempid :prop2 "prop2"]
          [:db/add -1 :name "Y"]
          [:db/add -1 :created-at tx-tempid]])
        tx1-id (+ d/tx0 1)
        tx1-data
        (d/q '[:find ?e ?a ?v :where [?e ?a ?v]] (d/db connection))
        tx2 (d/transact! connection [[:db/add tx-tempid :prop3 "prop3"]])
        tx2-id (get-in tx2 [:tempids tx-tempid])
        tx3 (d/transact! connection [{:db/id tx-tempid :prop4 "prop4"}])
        tx3-id (get-in tx3 [:tempids tx-tempid])]
    (and
     (= #{[1 :name "X"]
          [1 :created-at tx1-id]
          [tx1-id :prop1 "prop1"]
          [tx1-id :prop2 "prop2"]
          [2 :name "Y"]
          [2 :created-at tx1-id]}
        tx1-data)
     (= (assoc {-1 2 :db/current-tx tx1-id}
               tx-tempid tx1-id)
        (:tempids tx1))
     (= (+ d/tx0 2) tx2-id)
     (= "prop3" (:prop3 (d/entity (d/db connection) tx2-id)))
     (= (+ d/tx0 3) tx3-id)
     (= "prop4" (:prop4 (d/entity (d/db connection) tx3-id))))))

(defn check-resolve-current-tx []
  (every? current-tx-variant-ok?
          [:db/current-tx "datomic.tx" "datascript.tx"]))

(defn check-resolve-eid-refs []
  (let [connection
        (d/create-conn
         {:friend {:db/valueType :db.type/ref
                   :db/cardinality :db.cardinality/many}})
        transaction-data
        [{:name "Sergey" :friend [-1 -2]}
         [:db/add -1 :name "Ivan"]
         [:db/add -2 :name "Petr"]
         [:db/add "B" :name "Boris"]
         [:db/add "B" :friend -3]
         [:db/add -3 :name "Oleg"]
         [:db/add -3 :friend "B"]]
        report (d/transact! connection transaction-data)
        database (d/db connection)
        query
        '[:find ?friend-name
          :in $ ?name
          :where
          [?entity :name ?name]
          [?entity :friend ?friend]
          [?friend :name ?friend-name]]
        tempid-database
        (d/empty-db
         {:friend {:db/valueType :db.type/ref}
          :comp {:db/valueType :db.type/ref
                 :db/isComponent true}
          :multi {:db/cardinality :db.cardinality/many}})
        unused-vector-failed
        (throws?
         (fn []
           (d/db-with tempid-database [[:db/add -1 :friend -2]])))
        unused-map-failed
        (throws?
         (fn []
           (d/db-with tempid-database [{:db/id -1 :friend -2}])))
        empty-map-failed
        (throws?
         (fn []
           (d/db-with
            tempid-database
            [{:db/id -1}
             [:db/add -2 :friend -1]])))
        empty-many-failed
        (throws?
         (fn []
           (d/db-with
            tempid-database
            [{:db/id -1 :multi []}
             [:db/add -2 :friend -1]])))]
    (and
     (= {-1 2
         -2 3
         "B" 4
         -3 5
         :db/current-tx (+ d/tx0 1)}
        (:tempids report))
     (= #{["Ivan"] ["Petr"]} (d/q query database "Sergey"))
     (= #{["Oleg"]} (d/q query database "Boris"))
     (= #{["Boris"]} (d/q query database "Oleg"))
     unused-vector-failed
     unused-map-failed
     empty-map-failed
     empty-many-failed)))

(defn check-transient-issue-294 []
  (let [database
        (reduce
         (fn [db entity-id]
           (d/db-with
            db
            [{:db/id entity-id :a1 1 :a2 2 :a3 3}]))
         (d/empty-db)
         (range 1 10))
        report
        (d/with
         database
         [[:db.fn/retractEntity 1]
          [:db.fn/retractEntity 2]])]
    (= [(d/datom 1 :a1 1)
        (d/datom 1 :a2 2)
        (d/datom 1 :a3 3)
        (d/datom 2 :a1 1)
        (d/datom 2 :a2 2)
        (d/datom 2 :a3 3)]
       (:tx-data report))))

(defn check-large-ids-issue-292 []
  (let [database (d/empty-db {:ref {:db/valueType :db.type/ref}})
        large-id #?(:melange 285873023227265.0
                    :default 285873023227265)]
    (and
     (throws?
      (fn []
        (d/with database [[:db/add large-id :name "Valerii"]])))
     (throws?
      (fn []
        (d/with database [{:db/id large-id :name "Valerii"}])))
     (throws?
      (fn []
        (d/with database [{:db/id 1 :ref large-id}])))
     #?(:melange
        (and
         (throws?
          (fn []
            (d/with database
                    [(d/datom (__lg_dynamic large-id) :name 1)])))
         (throws?
          (fn []
            (d/with database [(d/datom 1 :ref large-id)]))))
        :default true))))

(defn check-uncomparable-issue-356 []
  (let [database
        (d/empty-db
         {:multi {:db/cardinality :db.cardinality/many}
          :index {:db/index true}})
        single-database
        (d/db-with
         (d/db-with
          (d/db-with
           (d/db-with database [[:db/add 1 :single {:map 1}]])
           [[:db/retract 1 :single {:map 1}]])
          [[:db/add 1 :single {:map 2}]])
         [[:db/add 1 :single {:map 3}]])
        multi-database
        (d/db-with
         (d/db-with
          (d/db-with database [[:db/add 1 :multi {:map 1}]])
          [[:db/add 1 :multi {:map 1}]])
         [[:db/add 1 :multi {:map 2}]])
        indexed-database
        (d/db-with
         (d/db-with
          (d/db-with
           (d/db-with database [[:db/add 1 :index {:map 1}]])
           [[:db/retract 1 :single {:map 1}]])
          [[:db/add 1 :index {:map 2}]])
         [[:db/add 1 :index {:map 3}]])]
    (and
     (= #{[1 :single {:map 3}]}
        (datom-triples single-database))
     (= [(d/datom 1 :single {:map 3})]
        (vec (d/datoms single-database :eavt 1 :single {:map 3})))
     (= [(d/datom 1 :single {:map 3})]
        (vec (d/datoms single-database :aevt :single 1 {:map 3})))
     (= #{[1 :multi {:map 1}] [1 :multi {:map 2}]}
        (datom-triples multi-database))
     (= [(d/datom 1 :multi {:map 2})]
        (vec (d/datoms multi-database :eavt 1 :multi {:map 2})))
     (= [(d/datom 1 :multi {:map 2})]
        (vec (d/datoms multi-database :aevt :multi 1 {:map 2})))
     (= #{[1 :index {:map 3}]}
        (datom-triples indexed-database))
     (= [(d/datom 1 :index {:map 3})]
        (vec (d/datoms indexed-database :eavt 1 :index {:map 3})))
     (= [(d/datom 1 :index {:map 3})]
        (vec (d/datoms indexed-database :aevt :index 1 {:map 3})))
     (= [(d/datom 1 :index {:map 3})]
        (vec (d/datoms indexed-database :avet :index {:map 3} 1))))))

(defn check-compare-numbers-js-issue-404 []
  (let [database (d/db-with (d/empty-db) [{:num 42.5}])
        retracted (d/db-with database [[:db/retract 1 :num 42]])]
    (= #{[1 :num 42.5]} (datom-triples retracted))))

(defn check-transitive-type-compare-issue-386 []
  (let [transactions
        [[{:block/uid "2LB4tlJGy"}]
         [{:block/uid "2ON453J0Z"}]
         [{:block/uid "2KqLLNbPg"}]
         [{:block/uid "2L0dcD7yy"}]
         [{:block/uid "2KqFNrhTZ"}]
         [{:block/uid "2KdQmItUD"}]
         [{:block/uid "2O8BcBfIL"}]
         [{:block/uid "2L4ZbI7nK"}]
         [{:block/uid "2KotiW36Z"}]
         [{:block/uid "2O4o-y5J8"}]
         [{:block/uid "2KimvuGko"}]
         [{:block/uid "dTR20ficj"}]
         [{:block/uid "wRmp6bXAx"}]
         [{:block/uid "rfL-iQOZm"}]
         [{:block/uid "tya6s422-"}]
         [{:block/uid 45619}]]
        connection
        (d/create-conn
         {:block/uid {:db/unique :db.unique/identity}})
        _transactions
        (doseq [transaction transactions]
          (d/transact! connection transaction))
        database (d/db connection)]
    (every?
     (fn [datom]
       (some? (d/entity database [(:a datom) (:v datom)])))
     (d/datoms database :eavt))))

(defn generated-entity-without-id [_database]
  [{:foo "bar"}])

(defn check-db-fn-returning-entity-without-db-id-issue-474 []
  (let [connection (d/create-conn {})
        _report
        (d/transact!
         connection
         [[:db.fn/call generated-entity-without-id]])]
    (= #{[1 :foo "bar"]}
       (datom-triples (d/db connection)))))

(deftest test-with (is (check-with)))
(deftest test-with-datoms (is (check-with-datoms)))
(deftest test-retract-fns (is (check-retract-fns)))
(deftest test-retract-without-value-issue-339
  (is (check-retract-without-value-issue-339)))
(deftest test-retract-fns-not-found
  (is (check-retract-fns-not-found)))
(deftest test-transact! (is (check-transact!)))
(deftest test-db-fn-cas (is (check-db-fn-cas)))
(deftest test-db-fn (is (check-db-fn)))
(deftest test-db-ident-fn (is (check-db-ident-fn)))
(deftest test-resolve-eid (is (check-resolve-eid)))
(deftest test-tempid-ref-issue-295
  (is (check-tempid-ref-issue-295)))
(deftest test-resolve-eid-refs (is (check-resolve-eid-refs)))
(deftest test-resolve-current-tx (is (check-resolve-current-tx)))
(deftest test-transient-issue-294
  (is (check-transient-issue-294)))
(deftest test-large-ids-issue-292
  (is (check-large-ids-issue-292)))
(deftest test-uncomparable-issue-356
  (is (check-uncomparable-issue-356)))
(deftest test-compare-numbers-js-issue-404
  (is (check-compare-numbers-js-issue-404)))
(deftest test-transitive-type-compare-issue-386
  (is (check-transitive-type-compare-issue-386)))
(deftest test-db-fn-returning-entity-without-db-id-issue-474
  (is (check-db-fn-returning-entity-without-db-id-issue-474)))
