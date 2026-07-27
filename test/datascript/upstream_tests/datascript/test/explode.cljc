(ns datascript.test.explode
  (:require
   [clojure.test :refer [deftest is testing]]
   [datascript.core :as d]
   [datascript.test.core :as tdc]))

(defn datoms-equal-closed?
  [^datascript.db/DB database
   ^:vector<vector<datascript.test.core/datom-component>> expected]
  (let [actual (tdc/all-datoms database)]
    (and
     (= (count actual) (count expected))
     (every?
      (fn [^:vector<datascript.test.core/datom-component> row]
        (some
         (fn [^:vector<datascript.test.core/datom-component> item]
           (= row item))
         actual))
      expected))))

(defn datoms?
  {:inline
   (fn [database ref-attr rows]
     (let [value-form
           (fn [attr value]
             (if (string? value)
               (list 'Datascript_runtime.Data_value.String value)
               (if (keyword? value)
                 (list
                  'Datascript_runtime.Data_value.Keyword
                  (str value))
                 (if (= ref-attr attr)
                   (list 'Datascript_runtime.Data_value.Ref value)
                   (list 'Datascript_runtime.Data_value.Int value)))))]
       (list
        'datascript.test.explode/datoms-equal-closed?
        database
        (cons
         'vector
         (map
          (fn [[eid attr value]]
            (list
             'vector
             (list 'datascript.test.core/Entity eid)
             (list 'datascript.test.core/Attribute attr)
             (list
              'datascript.test.core/Value
              (value-form attr value))))
          rows)))))}
  [^datascript.db/DB database
   ^:option<keyword> ref-attr
   ^:vector<vector<datascript.test.core/datom-component>> expected]
  (datoms-equal-closed? database expected))

(defn aka-schema []
  :map<keyword;map<keyword;Datascript_runtime.Data_value.t>>
  (datascript.db/schema-map
   {:aka {:db/cardinality :db.cardinality/many}
    :also {:db/cardinality :db.cardinality/many}}))

(defn string-value [^:string value]
  :Datascript_runtime.Data_value.t
  (Datascript_runtime.Data_value.String value))

(defn string-set-value [^:vector<string> values]
  :Datascript_runtime.Data_value.t
  (Datascript_runtime.Data_value.set_of_vector
   (mapv string-value values)))

(defn string-list-value [^:list<string> values]
  :Datascript_runtime.Data_value.t
  (Datascript_runtime.Data_value.List
   (into (list) (map string-value (reverse values)))))

(defn string-array-value [^:array<string> values]
  :Datascript_runtime.Data_value.t
  (Datascript_runtime.Data_value.vector_of_vector_with
   string-value
   (vec values)))

(defn int-value [^:int value]
  :Datascript_runtime.Data_value.t
  (Datascript_runtime.Data_value.Int value))

(defn int-set-value [^:vector<int> values]
  :Datascript_runtime.Data_value.t
  (Datascript_runtime.Data_value.set_of_vector
   (mapv int-value values)))

(defn int-list-value [^:list<int> values]
  :Datascript_runtime.Data_value.t
  (Datascript_runtime.Data_value.List
   (into (list) (map int-value (reverse values)))))

(defn empty-entity-value-map []
  :map<keyword;Datascript_runtime.Data_value.t>
  {})

(defn email-entity-value [^:string email]
  :Datascript_runtime.Data_value.t
  (Datascript_runtime.Data_value.map_of_keyword_map
   (assoc
    (empty-entity-value-map)
    :email
    (string-value email))))

(defn email-set-value [^:vector<string> emails]
  :Datascript_runtime.Data_value.t
  (Datascript_runtime.Data_value.set_of_vector
   (mapv email-entity-value emails)))

(defn email-list-value [^:list<string> emails]
  :Datascript_runtime.Data_value.t
  (Datascript_runtime.Data_value.List
   (into (list) (map email-entity-value (reverse emails)))))

(deftest test-explode
  (let []
    (is
     (datoms?
      (d/db-with
       (d/empty-db-closed (aka-schema))
       [{:db/id -1
         :name "Ivan"
         :age 16
         :aka ["Devil" "Tupen"]
         :also "ok"}])
      nil
      [[1 :name "Ivan"]
       [1 :age 16]
       [1 :aka "Devil"]
       [1 :aka "Tupen"]
       [1 :also "ok"]]))
    (is
     (datoms?
      (d/db-with
       (d/empty-db-closed (aka-schema))
       [{:db/id -1
         :name "Ivan"
         :age 16
         :aka (string-set-value ["Devil" "Tupen"])
         :also "ok"}])
      nil
      [[1 :name "Ivan"]
       [1 :age 16]
       [1 :aka "Devil"]
       [1 :aka "Tupen"]
       [1 :also "ok"]]))
    (is
     (datoms?
      (d/db-with
       (d/empty-db-closed (aka-schema))
       [{:db/id -1
         :name "Ivan"
         :age 16
         :aka (string-list-value '("Devil" "Tupen"))
         :also "ok"}])
      nil
      [[1 :name "Ivan"]
       [1 :age 16]
       [1 :aka "Devil"]
       [1 :aka "Tupen"]
       [1 :also "ok"]]))
    (is
     (datoms?
      (d/db-with
       (d/empty-db-closed (aka-schema))
       [{:db/id -1
         :name "Ivan"
         :age 16
         :aka (string-array-value
               (to-array ["Devil" "Tupen"]))
         :also "ok"}])
      nil
      [[1 :name "Ivan"]
       [1 :age 16]
       [1 :aka "Devil"]
       [1 :aka "Tupen"]
       [1 :also "ok"]]))))

(deftest test-explode-ref
  (let [database
        (d/empty-db
         {:children
          {:db/valueType :db.type/ref
           :db/cardinality :db.cardinality/many}})
        ]
    (is
     (datoms?
      (d/db-with
       database
       [{:db/id -1 :name "Ivan" :children [-2 -3]}
        {:db/id -2 :name "Petr"}
        {:db/id -3 :name "Evgeny"}])
      :children
      [[1 :name "Ivan"]
       [1 :children 2]
       [1 :children 3]
       [2 :name "Petr"]
       [3 :name "Evgeny"]]))
    (is
     (datoms?
      (d/db-with
       database
       [{:db/id -1
         :name "Ivan"
         :children (int-set-value [-2 -3])}
        {:db/id -2 :name "Petr"}
        {:db/id -3 :name "Evgeny"}])
      :children
      [[1 :name "Ivan"]
       [1 :children 2]
       [1 :children 3]
       [2 :name "Petr"]
       [3 :name "Evgeny"]]))
    (is
     (datoms?
      (d/db-with
       database
       [{:db/id -1
         :name "Ivan"
         :children (int-list-value (list -2 -3))}
        {:db/id -2 :name "Petr"}
        {:db/id -3 :name "Evgeny"}])
      :children
      [[1 :name "Ivan"]
       [1 :children 2]
       [1 :children 3]
       [2 :name "Petr"]
       [3 :name "Evgeny"]]))
    (is
     (datoms?
      (d/db-with
       database
       [{:db/id -1 :name "Ivan"}
        {:db/id -2 :name "Petr" :_children -1}
        {:db/id -3 :name "Evgeny" :_children -1}])
      :children
      [[1 :name "Ivan"]
       [1 :children 2]
       [1 :children 3]
       [2 :name "Petr"]
       [3 :name "Evgeny"]]))
    (is
     (thrown-msg?
      "Bad attribute :_parent: reverse attribute name requires {:db/valueType :db.type/ref} in schema"
      (d/db-with database [{:name "Sergey" :_parent 1}])))))

(deftest test-explode-nested-maps
  (let [database
        (d/empty-db {:profile {:db/valueType :db.type/ref}})]
    (is
     (datoms?
      (d/db-with
       database
       [{:db/id 5
         :name "Ivan"
         :profile {:db/id 7 :email "@2"}}])
      :profile
      [[5 :name "Ivan"] [5 :profile 7] [7 :email "@2"]]))
    (is
     (datoms?
      (d/db-with
       database
       [{:name "Ivan" :profile {:email "@2"}}])
      :profile
      [[1 :name "Ivan"] [1 :profile 2] [2 :email "@2"]]))
    (is
     (datoms?
      (d/db-with database [{:profile {:email "@2"}}])
      :profile
      [[2 :profile 1] [1 :email "@2"]]))
    (is
     (datoms?
      (d/db-with
       database
       [{:email "@2" :_profile {:name "Ivan"}}])
      :profile
      [[1 :email "@2"] [2 :name "Ivan"] [2 :profile 1]])))

  (testing "multi-valued"
    (let [database
          (d/empty-db
           {:profile
            {:db/valueType :db.type/ref
             :db/cardinality :db.cardinality/many}})]
      (is
       (datoms?
        (d/db-with
         database
         [{:db/id 5
           :name "Ivan"
           :profile {:db/id 7 :email "@2"}}])
        :profile
        [[5 :name "Ivan"] [5 :profile 7] [7 :email "@2"]]))
      (is
       (datoms?
        (d/db-with
         database
         [{:db/id 5
           :name "Ivan"
           :profile
           [{:db/id 7 :email "@2"}
            {:db/id 8 :email "@3"}]}])
        :profile
        [[5 :name "Ivan"]
         [5 :profile 7]
         [7 :email "@2"]
         [5 :profile 8]
         [8 :email "@3"]]))
      (is
       (datoms?
        (d/db-with
         database
         [{:name "Ivan" :profile {:email "@2"}}])
        :profile
        [[1 :name "Ivan"] [1 :profile 2] [2 :email "@2"]]))
      (is
       (datoms?
        (d/db-with
         database
         [{:name "Ivan"
           :profile [{:email "@2"} {:email "@3"}]}])
        :profile
        [[1 :name "Ivan"]
         [1 :profile 2]
         [2 :email "@2"]
         [1 :profile 3]
         [3 :email "@3"]]))
      (is
       (datoms?
        (d/db-with
         database
         [{:name "Ivan"
           :profile (email-set-value ["@3" "@2"])}])
        :profile
        [[1 :name "Ivan"]
         [1 :profile 2]
         [2 :email "@3"]
         [1 :profile 3]
         [3 :email "@2"]]))
      (is
       (datoms?
        (d/db-with
         database
         [{:name "Ivan"
           :profile (email-list-value (list "@2" "@3"))}])
        :profile
        [[1 :name "Ivan"]
         [1 :profile 2]
         [2 :email "@2"]
         [1 :profile 3]
         [3 :email "@3"]]))
      (is
       (datoms?
        (d/db-with
         database
         [{:email "@2" :_profile {:name "Ivan"}}])
        :profile
        [[1 :email "@2"] [2 :name "Ivan"] [2 :profile 1]]))
      (is
       (datoms?
        (d/db-with
         database
         [{:email "@2"
           :_profile [{:name "Ivan"} {:name "Petr"}]}])
        :profile
        [[1 :email "@2"]
         [2 :name "Ivan"]
         [2 :profile 1]
         [3 :name "Petr"]
         [3 :profile 1]])))))

(deftest test-closed-nested-entity-map
  (let [^:map<keyword;Datascript_runtime.Data_value.t> child {}
        child
        (assoc
         child
         :name
         (Datascript_runtime.Data_value.String "nested"))
        ^:map<keyword;Datascript_runtime.Data_value.t> parent {}
        parent
        (assoc
         parent
         :db/id
         (Datascript_runtime.Data_value.Ref_to
          (Datascript_runtime.Data_value.Entity_id 5))
         :profile
         (Datascript_runtime.Data_value.map_of_keyword_map child))
        ^:map<keyword;Datascript_runtime.Data_value.t> component {}
        ^:map<keyword;Datascript_runtime.Data_value.t> reference {}
        ^:map<keyword;map<keyword;Datascript_runtime.Data_value.t>> schema {}
        schema
        (assoc
         schema
         :profile
         (assoc
          component
          :db/valueType
          (Datascript_runtime.Data_value.Keyword ":db.type/ref")
          :db/isComponent
          (Datascript_runtime.Data_value.Bool true))
         :friend
         (assoc
          reference
          :db/valueType
          (Datascript_runtime.Data_value.Keyword ":db.type/ref")))
        database
        (d/db-with
         (d/empty-db schema)
         [(datascript.db/tx-entity parent)])]
    (is
     (datoms?
      database
      :profile
      [[1 :name "nested"]
       [5 :profile 1]]))))

(deftest test-circular-refs
  (is
   (datoms?
    (->
     (d/empty-db
      {:comp
       {:db/valueType :db.type/ref
        :db/cardinality :db.cardinality/many
        :db/isComponent true}})
     (d/db-with [{:db/id -1 :name "Name"}])
     (d/db-with [{:db/id 1 :comp [{:name "C"}]}]))
    :comp
    [[1 :comp 2] [1 :name "Name"] [2 :name "C"]]))
  (is
   (datoms?
    (->
     (d/empty-db
      {:comp
       {:db/valueType :db.type/ref
        :db/cardinality :db.cardinality/many}})
     (d/db-with [{:db/id -1 :name "Name"}])
     (d/db-with [{:db/id 1 :comp [{:name "C"}]}]))
    :comp
    [[1 :comp 2] [1 :name "Name"] [2 :name "C"]]))
  (is
   (datoms?
    (->
     (d/empty-db
      {:comp
       {:db/valueType :db.type/ref
        :db/isComponent true}})
     (d/db-with [{:db/id -1 :name "Name"}])
     (d/db-with [{:db/id 1 :comp {:name "C"}}]))
    :comp
    [[1 :comp 2] [1 :name "Name"] [2 :name "C"]]))
  (is
   (datoms?
    (->
     (d/empty-db {:comp {:db/valueType :db.type/ref}})
     (d/db-with [{:db/id -1 :name "Name"}])
     (d/db-with [{:db/id 1 :comp {:name "C"}}]))
    :comp
    [[1 :comp 2] [1 :name "Name"] [2 :name "C"]])))
