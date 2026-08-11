(ns datascript.test.lookup-refs
  (:require
    [clojure.test :as t :refer [is are deftest testing]]
    [datascript.core :as d]
    [datascript.db :as db]
    [datascript.impl.entity :as entity]
    [datascript.test.core :as tdc]))

(defn entity-id-map-value
  [^datascript.impl.entity/Entity source]
  :Datascript_runtime.Data_value.t
  (Datascript_runtime.Data_value.map_of_keyword_map
   {:db/id (Datascript_runtime.Data_value.Int (.-eid source))}))

(defn entity-option-id-map-value
  [^:option<datascript.impl.entity/Entity> source]
  :Datascript_runtime.Data_value.t
  (match source
    (Some target) (entity-id-map-value target)
    None (Datascript_runtime.Data_value.Nil)))

(defn entity-value-data
  [^datascript.impl.entity/EntityValue value]
  :Datascript_runtime.Data_value.t
  (match value
    (entity/EntityScalar scalar) scalar
    (entity/EntityReference target) (entity-id-map-value target)
    (entity/EntityReferences targets)
    (Datascript_runtime.Data_value.set_of_vector
     (mapv entity-option-id-map-value (vec targets)))))

(defn empty-entity-data-map []
  :map<keyword;Datascript_runtime.Data_value.t>
  {})

(defn entity-map-value
  [^datascript.impl.entity/Entity source]
  :Datascript_runtime.Data_value.t
  (let [source (entity/touch-entity source)
        values
        (reduce
         (fn
           [^:map<keyword;Datascript_runtime.Data_value.t> values
            ^:keyword attr]
           (if-some [value (entity/lookup-entity source attr)]
             (assoc values attr (entity-value-data value))
             values))
         (assoc
          (empty-entity-data-map)
          :db/id
          (Datascript_runtime.Data_value.Int (.-eid source)))
         (keys @(:cache (.-state source))))]
    (Datascript_runtime.Data_value.map_of_keyword_map values)))

(defn entity-option-map-equal?
  [^:option<datascript.impl.entity/Entity> source
   ^:Datascript_runtime.Data_value.t expected]
  (match source
    (Some entity)
    (Datascript_runtime.Data_value.equal
     (entity-map-value entity)
     expected)
    None
    (Datascript_runtime.Data_value.is_nil expected)))

(defn entity-map?
  {:inline
   (fn [source expected]
     (let [value-form
           (fn value-form [value]
             (cond
               (nil? value)
               (list 'Datascript_runtime.Data_value.Nil)

               (string? value)
               (list 'Datascript_runtime.Data_value.String value)

               (keyword? value)
               (list
                'Datascript_runtime.Data_value.Keyword
                (str value))

               (= value true)
               (list 'Datascript_runtime.Data_value.Bool true)

               (= value false)
               (list 'Datascript_runtime.Data_value.Bool false)

               (vector? value)
               (list
                'Datascript_runtime.Data_value.vector_of_vector
                (vec (map value-form value)))

               (if (seq? value)
                 (or
                  (= (str (first value)) "hash-set")
                  (= (str (first value)) "__lg_hash-set"))
                 false)
               (list
                'Datascript_runtime.Data_value.set_of_vector
                (vec (map value-form (next value))))

               (map? value)
               (list
                'Datascript_runtime.Data_value.map_of_keyword_map
                (list
                 'zipmap
                 (vec (map first value))
                 (vec (map value-form (map second value)))))

               :else
               (list 'Datascript_runtime.Data_value.Int value)))]
       (list
        'datascript.test.lookup-refs/entity-option-map-equal?
        source
        (value-form expected))))}
  [^:option<datascript.impl.entity/Entity> source
   ^:Datascript_runtime.Data_value.t expected]
  (entity-option-map-equal? source expected))

(defn datom-triple
  [^datascript.db/Datom datom]
  :tuple<int;keyword;Datascript_runtime.Data_value.t>
  (tuple (.-e datom) (.-a datom) (.-v datom)))

(defn datom-triple-equal?
  [^:tuple<int;keyword;Datascript_runtime.Data_value.t> left
   ^:tuple<int;keyword;Datascript_runtime.Data_value.t> right]
  (and
   (= (tuple-get left 0) (tuple-get right 0))
   (= (tuple-get left 1) (tuple-get right 1))
   (Datascript_runtime.Data_value.equal
    (tuple-get left 2)
    (tuple-get right 2))))

(defn datom-triples-equal?
  [^:seq<datascript.db/Datom> actual
   ^:vector<tuple<int;keyword;Datascript_runtime.Data_value.t>> expected]
  (let [actual (mapv datom-triple actual)]
    (and
     (= (count actual) (count expected))
     (loop [index 0]
       (if (< index (count actual))
         (if
          (datom-triple-equal?
           (nth actual index)
           (nth expected index))
           (recur (inc index))
           false)
         true)))))

(defn datoms?
  {:inline
   (fn [actual rows]
     (let [value-form
           (fn [value]
             (if (string? value)
               (list 'Datascript_runtime.Data_value.String value)
               (list 'Datascript_runtime.Data_value.Int value)))]
       (list
        'datascript.test.lookup-refs/datom-triples-equal?
        actual
        (cons
         'vector
         (map
          (fn [[eid attr value]]
            (list 'tuple eid attr (value-form value)))
          rows)))))}
  [^:seq<datascript.db/Datom> actual
   ^:vector<tuple<int;keyword;Datascript_runtime.Data_value.t>> expected]
  (datom-triples-equal? actual expected))

(deftest test-lookup-refs
  (let [db (d/db-with (d/empty-db {:name  {:db/unique :db.unique/identity}
                                   :email {:db/unique :db.unique/value}})
             [{:db/id 1 :name "Ivan" :email "@1" :age 35}
              {:db/id 2 :name "Petr" :email "@2" :age 22}])]
    
    (are [eid res] (entity-map? (d/entity db eid) res)
      [:name "Ivan"]   {:db/id 1 :name "Ivan" :email "@1" :age 35}
      [:email "@1"]    {:db/id 1 :name "Ivan" :email "@1" :age 35}
      [:name "Sergey"] nil
      [:name nil]      nil)
    
    (are [eid msg] (thrown-msg? msg (d/entity db eid))
      [:name]     "Lookup ref should contain 2 elements: [:name]"
      [:name 1 2] "Lookup ref should contain 2 elements: [:name 1 2]"
      [:age 10]   "Lookup ref attribute should be marked as :db/unique: [:age 10]")))

(deftest test-lookup-refs-transact
  (let [db (d/db-with (d/empty-db {:name    {:db/unique :db.unique/identity}
                                   :friend  {:db/valueType :db.type/ref}})
             [{:db/id 1 :name "Ivan"}
              {:db/id 2 :name "Petr"}])]
    (are [tx res] (entity-map? (d/entity (d/db-with db tx) 1) res)
      ;; Additions
      [[:db/add [:name "Ivan"] :age 35]]
      {:db/id 1 :name "Ivan" :age 35}
      
      [{:db/id [:name "Ivan"] :age 35}]
      {:db/id 1 :name "Ivan" :age 35}
         
      [[:db/add 1 :friend [:name "Petr"]]]
      {:db/id 1 :name "Ivan" :friend {:db/id 2}}

      [[:db/add 1 :friend [:name "Petr"]]]
      {:db/id 1 :name "Ivan" :friend {:db/id 2}}
         
      [{:db/id 1 :friend [:name "Petr"]}]
      {:db/id 1 :name "Ivan" :friend {:db/id 2}}
      
      [{:db/id 2 :_friend [:name "Ivan"]}]
      {:db/id 1 :name "Ivan" :friend {:db/id 2}}
      
      ;; lookup refs are resolved at intermediate DB value
      [[:db/add 3 :name "Oleg"]
       [:db/add 1 :friend [:name "Oleg"]]]
      {:db/id 1 :name "Ivan" :friend {:db/id 3}}
      
      ;; CAS
      [[:db.fn/cas [:name "Ivan"] :name "Ivan" "Oleg"]]
      {:db/id 1 :name "Oleg"}
      
      [[:db/add 1 :friend 1]
       [:db.fn/cas 1 :friend [:name "Ivan"] 2]]
      {:db/id 1 :name "Ivan" :friend {:db/id 2}}
         
      [[:db/add 1 :friend 1]
       [:db.fn/cas 1 :friend 1 [:name "Petr"]]]
      {:db/id 1 :name "Ivan" :friend {:db/id 2}}
         
      ;; Retractions
      [[:db/add 1 :age 35]
       [:db/retract [:name "Ivan"] :age 35]]
      {:db/id 1 :name "Ivan"}
      
      [[:db/add 1 :friend 2]
       [:db/retract 1 :friend [:name "Petr"]]]
      {:db/id 1 :name "Ivan"}
         
      [[:db/add 1 :age 35]
       [:db.fn/retractAttribute [:name "Ivan"] :age]]
      {:db/id 1 :name "Ivan"}
         
      [[:db.fn/retractEntity [:name "Ivan"]]]
      nil)
    
    (are [tx msg] (thrown-msg? msg (d/db-with db tx))
      [{:db/id [:name "Oleg"], :age 10}]
      "Nothing found for entity id [:name \"Oleg\"]"
         
      [[:db/add [:name "Oleg"] :age 10]]
      "Nothing found for entity id [:name \"Oleg\"]")))

(deftest test-lookup-refs-transact-multi
  (let [db (d/db-with (d/empty-db {:name    {:db/unique :db.unique/identity}
                                   :friends {:db/valueType :db.type/ref
                                             :db/cardinality :db.cardinality/many}})
             [{:db/id 1 :name "Ivan"}
              {:db/id 2 :name "Petr"}
              {:db/id 3 :name "Oleg"}
              {:db/id 4 :name "Sergey"}])]
    (are [tx res] (entity-map? (d/entity (d/db-with db tx) 1) res)
      ;; Additions
      [[:db/add 1 :friends [:name "Petr"]]]
      {:db/id 1 :name "Ivan" :friends #{{:db/id 2}}}

      [[:db/add 1 :friends [:name "Petr"]]
       [:db/add 1 :friends [:name "Oleg"]]]
      {:db/id 1 :name "Ivan" :friends #{{:db/id 2} {:db/id 3}}}
         
      [{:db/id 1 :friends [:name "Petr"]}]
      {:db/id 1 :name "Ivan" :friends #{{:db/id 2}}}

      [{:db/id 1 :friends [[:name "Petr"]]}]
      {:db/id 1 :name "Ivan" :friends #{{:db/id 2}}}
         
      [{:db/id 1 :friends [[:name "Petr"] [:name "Oleg"]]}]
      {:db/id 1 :name "Ivan" :friends #{{:db/id 2} {:db/id 3}}}

      [{:db/id 1 :friends [2 [:name "Oleg"]]}]
      {:db/id 1 :name "Ivan" :friends #{{:db/id 2} {:db/id 3}}}

      [{:db/id 1 :friends [[:name "Petr"] 3]}]
      {:db/id 1 :name "Ivan" :friends #{{:db/id 2} {:db/id 3}}}
         
      ;; reverse refs
      [{:db/id 2 :_friends [:name "Ivan"]}]
      {:db/id 1 :name "Ivan" :friends #{{:db/id 2}}}

      [{:db/id 2 :_friends [[:name "Ivan"]]}]
      {:db/id 1 :name "Ivan" :friends #{{:db/id 2}}}

      [{:db/id 2 :_friends [[:name "Ivan"] [:name "Oleg"]]}]
      {:db/id 1 :name "Ivan" :friends #{{:db/id 2}}})))

(deftest lookup-refs-index-access
  (let [db (d/db-with (d/empty-db {:name    {:db/unique :db.unique/identity}
                                   :friends {:db/valueType :db.type/ref
                                             :db/cardinality :db.cardinality/many}})
             [{:db/id 1 :name "Ivan" :friends [2 3]}
              {:db/id 2 :name "Petr" :friends 3}
              {:db/id 3 :name "Oleg"}])]
    (is
     (datoms?
      (d/datoms db :eavt [:name "Ivan"])
      [[1 :friends 2] [1 :friends 3] [1 :name "Ivan"]]))
    (is
     (datoms?
      (d/datoms db :eavt [:name "Ivan"] :friends)
      [[1 :friends 2] [1 :friends 3]]))
    (is
     (datoms?
      (d/datoms db :eavt [:name "Ivan"] :friends [:name "Petr"])
      [[1 :friends 2]]))
    (is
     (datoms?
      (d/datoms db :aevt :friends [:name "Ivan"])
      [[1 :friends 2] [1 :friends 3]]))
    (is
     (datoms?
      (d/datoms db :aevt :friends [:name "Ivan"] [:name "Petr"])
      [[1 :friends 2]]))
    (is
     (datoms?
      (d/datoms db :avet :friends [:name "Oleg"])
      [[1 :friends 3] [2 :friends 3]]))
    (is
     (datoms?
      (d/datoms db :avet :friends [:name "Oleg"] [:name "Ivan"])
      [[1 :friends 3]]))

    (is
     (db/datom-vectors-equal?
      (vec (d/seek-datoms db :eavt [:name "Ivan"]))
      (vec (d/seek-datoms db :eavt 1))))
    (is
     (db/datom-vectors-equal?
      (vec (d/seek-datoms db :eavt [:name "Ivan"] :name))
      (vec (d/seek-datoms db :eavt 1 :name))))
    (is
     (db/datom-vectors-equal?
      (vec (d/seek-datoms db :eavt [:name "Ivan"] :friends [:name "Oleg"]))
      (vec (d/seek-datoms db :eavt 1 :friends 3))))
    (is
     (db/datom-vectors-equal?
      (vec (d/seek-datoms db :aevt :friends [:name "Petr"]))
      (vec (d/seek-datoms db :aevt :friends 2))))
    (is
     (db/datom-vectors-equal?
      (vec
       (d/seek-datoms db :aevt :friends [:name "Ivan"] [:name "Oleg"]))
      (vec (d/seek-datoms db :aevt :friends 1 3))))
    (is
     (db/datom-vectors-equal?
      (vec (d/seek-datoms db :avet :friends [:name "Oleg"]))
      (vec (d/seek-datoms db :avet :friends 3))))
    (is
     (db/datom-vectors-equal?
      (vec
       (d/seek-datoms db :avet :friends [:name "Oleg"] [:name "Petr"]))
      (vec (d/seek-datoms db :avet :friends 3 2))))

    (is
     (datoms?
      (d/index-range db :friends [:name "Oleg"] [:name "Oleg"])
      [[1 :friends 3] [2 :friends 3]]))
    (is
     (datoms?
      (d/index-range db :friends [:name "Petr"] [:name "Petr"])
      [[1 :friends 2]]))
    (is
     (datoms?
      (d/index-range db :friends [:name "Petr"] [:name "Oleg"])
      [[1 :friends 2] [1 :friends 3] [2 :friends 3]]))))

(deftest test-lookup-refs-query
  (let [schema
        (db/schema-map
         {:name {:db/unique :db.unique/identity}
          :friend {:db/valueType :db.type/ref}})
        db (d/db-with (d/empty-db-closed schema)
             [{:db/id 1 :id 1 :name "Ivan" :age 11 :friend 2}
              {:db/id 2 :id 2 :name "Petr" :age 22 :friend 3}
              {:db/id 3 :id 3 :name "Oleg" :age 33}])]
    (is
     (tdc/query-relation?
      (d/q '[:find ?e ?v
             :in $ ?e
             :where [?e :age ?v]]
           db [:name "Ivan"])
      [[[:name "Ivan"] 11]]))
    
    (is
     (tdc/query-collection?
      (d/q '[:find [?v ...]
             :in $ [?e ...]
             :where [?e :age ?v]]
           db [[:name "Ivan"] [:name "Petr"]])
      [11 22]))
    
    (is
     (tdc/query-collection?
      (d/q '[:find [?e ...]
             :in $ ?v
             :where [?e :friend ?v]]
           db [:name "Petr"])
      [1]))
    
    (is
     (tdc/query-collection?
      (d/q '[:find [?e ...]
             :in $ [?v ...]
             :where [?e :friend ?v]]
           db [[:name "Petr"] [:name "Oleg"]])
      [1 2]))
    
    (is
     (tdc/query-relation?
      (d/q '[:find ?e ?v
             :in $ ?e ?v
             :where [?e :friend ?v]]
           db [:name "Ivan"] [:name "Petr"])
      [[[:name "Ivan"] [:name "Petr"]]]))
    
    (is
     (tdc/query-relation?
      (d/q '[:find ?e ?v
             :in $ [?e ...] [?v ...]
             :where [?e :friend ?v]]
           db
           [[:name "Ivan"] [:name "Petr"] [:name "Oleg"]]
           [[:name "Ivan"] [:name "Petr"] [:name "Oleg"]])
      [[[:name "Ivan"] [:name "Petr"]]
       [[:name "Petr"] [:name "Oleg"]]]))

    ;; issue-214
    (is
     (tdc/query-relation?
      (d/q '[:find ?e
             :in $ [?e ...]
             :where [?e :friend 3]]
           db [1 2 3 "A"])
      [[2]]))
    
    (let [db2 (d/db-with (d/empty-db-closed schema)
                [{:db/id 3 :name "Ivan" :id 3}
                 {:db/id 1 :name "Petr" :id 1}
                 {:db/id 2 :name "Oleg" :id 2}])]
      (is
       (tdc/query-relation?
        (d/q '[:find ?e ?e1 ?e2
               :in $1 $2 [?e ...]
               :where
               [$1 ?e :id ?e1]
               [$2 ?e :id ?e2]]
             db db2 [[:name "Ivan"] [:name "Petr"] [:name "Oleg"]])
        [[[:name "Ivan"] 1 3]
         [[:name "Petr"] 2 1]
         [[:name "Oleg"] 3 2]])))
    
    (testing "inline refs"
      (is
       (tdc/query-relation?
        (d/q '[:find ?v
               :where [[:name "Ivan"] :friend ?v]]
             db)
        [[2]]))
      
      (is
       (tdc/query-relation?
        (d/q '[:find ?e
               :where [?e :friend [:name "Petr"]]]
             db)
        [[1]]))
      
      (is (thrown-msg? "Nothing found for entity id [:name \"Valery\"]"
            (d/q '[:find ?e
                   :where [[:name "Valery"] :friend ?e]]
              db))))))
