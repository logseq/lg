(ns datascript.test.entity
  (:require
   [clojure.test :refer [deftest is testing]]
   [datascript.core :as d]
   [datascript.datafy :as datafy]
   [datascript.db :as db]
   [datascript.impl.entity :as entity]
   [datascript.test.core :as tdc]))

(defn entity-by-ref
  [^datascript.db/DB database
   ^:Datascript_runtime.Data_value.entity_ref entity-ref]
  :option<datascript.impl.entity/Entity>
  (d/entity-closed (db/database-view database) entity-ref))

(defn entity-by-id
  [^datascript.db/DB database ^:int eid]
  :option<datascript.impl.entity/Entity>
  (entity-by-ref
   database
   (Datascript_runtime.Data_value.Entity_id eid)))

(defn entity-scalar-equal?
  [^datascript.impl.entity/Entity source
   ^:keyword attr
   ^:Datascript_runtime.Data_value.t expected]
  (match (entity/lookup-entity source attr)
    (Some (entity/EntityScalar actual))
    (Datascript_runtime.Data_value.equal actual expected)
    _ false))

(defn entity-call-scalar-equal?
  [^datascript.impl.entity/Entity source
   ^:keyword attr
   ^:Datascript_runtime.Data_value.t expected]
  (match (source attr)
    (Some (entity/EntityScalar actual))
    (Datascript_runtime.Data_value.equal actual expected)
    _ false))

(defn entity-scalars-equal?
  [^datascript.impl.entity/Entity source
   ^:map<keyword;Datascript_runtime.Data_value.t> expected]
  (and
   (= (count source) (count expected))
   (every?
    (fn [^:keyword attr]
      (if-some [value (get expected attr)]
        (entity-scalar-equal? source attr value)
        false))
    (keys expected))))

(defn entity-option-id?
  [^:option<datascript.impl.entity/Entity> source ^:int expected]
  (match source
    (Some value) (= expected (.-eid value))
    None false))

(defn entity-touched?
  [^datascript.impl.entity/Entity source]
  @(:touched (.-state source)))

(defn entity-reference
  [^datascript.impl.entity/Entity source ^:keyword attr]
  :option<datascript.impl.entity/Entity>
  (match (entity/lookup-entity source attr)
    (Some (entity/EntityReference target)) (Some target)
    (Some (entity/EntityReferences targets))
    (loop [remaining (vec targets)]
      (if-some [target (first remaining)]
        (if-some [value target]
          (Some value)
          (recur (subvec remaining 1)))
        None))
    _ None))

(defn entity-reference-ids?
  [^datascript.impl.entity/Entity source
   ^:keyword attr
   ^:vector<int> expected]
  (match (entity/lookup-entity source attr)
    (Some (entity/EntityReferences targets))
    (and
     (= (count targets) (count expected))
     (every?
      (fn [^:int eid]
        (some
         (fn [^:option<datascript.impl.entity/Entity> target]
           (entity-option-id? target eid))
         targets))
      expected))
    _ false))

(defn entity-reference-id?
  [^datascript.impl.entity/Entity source
   ^:keyword attr
   ^:int expected]
  (match (entity/lookup-entity source attr)
    (Some (entity/EntityReference target))
    (= expected (.-eid target))
    _ false))

(defn entity-attribute-missing?
  [^:option<datascript.impl.entity/Entity> source ^:keyword attr]
  (match source
    (Some value) (nil? (entity/lookup-entity value attr))
    None true))

(defn entity-print?
  [^:option<datascript.impl.entity/Entity> source ^:string expected]
  (if-some [value source]
    (= expected (pr-str value))
    false))

(defn entity-print-after-lookup?
  [^:option<datascript.impl.entity/Entity> source
   ^:keyword attr
   ^:vector<string> expected]
  (if-some [value source]
    (do
      (entity/lookup-entity value attr)
      (let [actual (pr-str value)]
        (some
         (fn [^:string text]
           (= text actual))
         expected)))
    false))

(defn string-value [^:string value]
  :Datascript_runtime.Data_value.t
  (Datascript_runtime.Data_value.String value))

(defn int-value [^:int value]
  :Datascript_runtime.Data_value.t
  (Datascript_runtime.Data_value.Int value))

(defn bool-value [^boolean value]
  :Datascript_runtime.Data_value.t
  (Datascript_runtime.Data_value.Bool value))

(defn string-set-value [^:vector<string> values]
  :Datascript_runtime.Data_value.t
  (Datascript_runtime.Data_value.set_of_vector
   (mapv string-value values)))

(defn datafy-test-db []
  (->
   (d/empty-db
    {:ref {:db/valueType :db.type/ref}
     :namespace/ref {:db/valueType :db.type/ref}
     :many/ref
     {:db/valueType :db.type/ref
      :db/cardinality :db.cardinality/many}})
   (d/db-with
    [{:db/id 1 :name "Parent1"}
     {:db/id 2 :name "Child1" :ref 1 :namespace/ref 1}
     {:db/id 3 :name "GrandChild1" :ref 2 :namespace/ref 2}
     {:db/id 4 :name "Master" :many/ref [1 2 3]}])))

(defn datafy-navigate
  [^datascript.datafy/navigation-value navigation
   ^:vector<datascript.datafy/navigation-key> path]
  :datascript.datafy/navigation-value
  (if-some [key (first path)]
    (let [datafied (datafy/datafy navigation)
          value (datafy/lookup datafied key)]
      (recur
       (datafy/nav datafied key value)
       (subvec path 1)))
    navigation))

(defn datafy-scalar-equal?
  [^datascript.datafy/navigation-value navigation
   ^Datascript_runtime.Data_value.t expected]
  (match (datafy/navigation-scalar navigation)
    (Some actual)
    (Datascript_runtime.Data_value.equal actual expected)
    None false))

(defn datafy-entity-id-at?
  [^datascript.datafy/navigation-value navigation
   ^:vector<datascript.datafy/navigation-key> path
   ^:int expected]
  (=
   (Some expected)
   (datafy/navigation-entity-id
    (datafy-navigate navigation path))))

(deftest test-navigation
  (let [database (datafy-test-db)]
    (if-some [source (entity-by-id database 3)]
      (let [navigation (datafy/entity-navigation source)]
        (is
         (datafy-entity-id-at?
          navigation
          [(datafy/NavigationAttribute :ref)]
          2))
        (is
         (datafy-entity-id-at?
          navigation
          [(datafy/NavigationAttribute :namespace/ref)]
          2))
        (is
         (datafy-entity-id-at?
          navigation
          [(datafy/NavigationAttribute :ref)
           (datafy/NavigationAttribute :namespace/ref)]
          1))
        (is
         (datafy-entity-id-at?
          navigation
          [(datafy/NavigationAttribute :namespace/ref)
           (datafy/NavigationAttribute :ref)
           (datafy/NavigationAttribute :_ref)
           (datafy/NavigationIndex 0)
           (datafy/NavigationAttribute :namespace/_ref)
           (datafy/NavigationIndex 0)]
          3))
        (is
         (=
          [1 2 3]
          (sort
           (datafy/navigation-entity-ids
            (datafy-navigate
             navigation
             [(datafy/NavigationAttribute :many/_ref)
              (datafy/NavigationIndex 0)
              (datafy/NavigationAttribute :many/ref)])))))
        (is
         (datafy-scalar-equal?
          (datafy-navigate
           navigation
           [(datafy/NavigationAttribute :name)])
          (string-value "GrandChild1")))
        (is
         (nil?
          (datafy/navigation-entity-id
           (datafy-navigate
            navigation
            [(datafy/NavigationAttribute :missing)]))))
        (is
         (nil?
          (datafy/navigation-entity-id
           (datafy-navigate
            navigation
            [(datafy/NavigationAttribute :_ref)
             (datafy/NavigationIndex 99)])))))
      (is false))))

(deftest test-entity
  (let [database
        (->
         (d/empty-db {:aka {:db/cardinality :db.cardinality/many}})
         (d/db-with
          [{:db/id 1 :name "Ivan" :age 19 :aka ["X" "Y"]}
           {:db/id 2 :name "Ivan" :sex "male" :aka ["Z"]}
           [:db/add 3 :huh? false]]))]
    (if-some [source (entity-by-id database 1)]
      (do
        (is (entity-scalar-equal? source :db/id (int-value 1)))
        (is
         (db/database-view-identical?
          (d/entity-db source)
          (db/database-view database)))
        (is (entity-scalar-equal? source :name (string-value "Ivan")))
        (is
         (entity-call-scalar-equal?
          source
          :name
          (string-value "Ivan")))
        (is (entity-scalar-equal? source :age (int-value 19)))
        (is
         (entity-scalar-equal?
          source
          :aka
          (string-set-value ["X" "Y"])))
        (is (= true (contains? source :age)))
        (is (= false (contains? source :not-found)))
        (is
         (entity-scalars-equal?
          source
          {:name (string-value "Ivan")
           :age (int-value 19)
           :aka (string-set-value ["X" "Y"])})))
      (is false))

    (if-some [source (entity-by-id database 1)]
      (is
       (entity-scalars-equal?
        source
        {:name (string-value "Ivan")
         :age (int-value 19)
         :aka (string-set-value ["X" "Y"])}))
      (is false))

    (if-some [source (entity-by-id database 2)]
      (is
       (entity-scalars-equal?
        source
        {:name (string-value "Ivan")
         :sex (string-value "male")
         :aka (string-set-value ["Z"])}))
      (is false))

    (if-some [source (entity-by-id database 3)]
      (do
        (is
         (entity-scalars-equal?
          source
          {:huh? (bool-value false)}))
        (is (entity-scalar-equal? source :huh? (bool-value false))))
      (is false))

    (is (entity-print? (entity-by-id database 1) "{:db/id 1}"))
    (is
     (entity-print-after-lookup?
      (entity-by-id database 1)
      :unknown
      ["{:db/id 1}"]))
    (is
     (entity-print-after-lookup?
      (entity-by-id database 1)
      :name
      ["{:name \"Ivan\", :db/id 1}"
       "{:db/id 1, :name \"Ivan\"}"]))))

(deftest test-entity-refs
  (let [database
        (->
         (d/empty-db
          {:father {:db/valueType :db.type/ref}
           :children
           {:db/valueType :db.type/ref
            :db/cardinality :db.cardinality/many}})
         (d/db-with
          [{:db/id 1 :children [10]}
           {:db/id 10 :father 1 :children [100 101]}
           {:db/id 100 :father 10}
           {:db/id 101 :father 10}]))]
    (if-some [source (entity-by-id database 1)]
      (is (entity-reference-ids? source :children [10]))
      (is false))
    (if-some [source (entity-by-id database 10)]
      (is (entity-reference-ids? source :children [100 101]))
      (is false))

    (testing "empty attribute"
      (if-some [source (entity-by-id database 100)]
        (is (nil? (entity/lookup-entity source :children)))
        (is false)))

    (testing "nested navigation"
      (if-some [source (entity-by-id database 1)]
        (if-some [child (entity-reference source :children)]
          (is (entity-reference-ids? child :children [100 101]))
          (is false))
        (is false))

      (if-some [source (entity-by-id database 10)]
        (if-some [child (entity-reference source :children)]
          (is (entity-reference-id? child :father 10))
          (is false))
        (is false))

      (if-some [source (entity-by-id database 10)]
        (if-some [father (entity-reference source :father)]
          (is (entity-reference-ids? father :children [10]))
          (is false))
        (is false))

      (testing "after touch"
        (if-some [source (entity-by-id database 1)]
          (let [source (entity/touch-entity source)]
            (if-some [child (entity-reference source :children)]
              (is (entity-reference-ids? child :children [100 101]))
              (is false)))
          (is false))

        (if-some [source (entity-by-id database 10)]
          (let [source (entity/touch-entity source)]
            (if-some [child (entity-reference source :children)]
              (is (entity-reference-id? child :father 10))
              (is false)))
          (is false))

        (if-some [source (entity-by-id database 10)]
          (let [source (entity/touch-entity source)]
            (if-some [father (entity-reference source :father)]
              (is (entity-reference-ids? father :children [10]))
              (is false)))
          (is false))))

    (testing "backward navigation"
      (if-some [source (entity-by-id database 1)]
        (is (nil? (entity/lookup-entity source :_children)))
        (is false))

      (if-some [source (entity-by-id database 1)]
        (is (entity-reference-ids? source :_father [10]))
        (is false))

      (if-some [source (entity-by-id database 10)]
        (is (entity-reference-ids? source :_children [1]))
        (is false))

      (if-some [source (entity-by-id database 10)]
        (is (entity-reference-ids? source :_father [100 101]))
        (is false))

      (if-some [source (entity-by-id database 100)]
        (if-some [parent (entity-reference source :_children)]
          (is (entity-reference-ids? parent :_children [1]))
          (is false))
        (is false)))))

(deftest test-touch-components
  (let [database
        (->
         (d/empty-db
          {:child
           {:db/valueType :db.type/ref
            :db/isComponent true}
           :children
           {:db/valueType :db.type/ref
            :db/isComponent true
            :db/cardinality :db.cardinality/many}
           :ref {:db/valueType :db.type/ref}})
         (d/db-with
          [{:db/id 1 :child 2 :children [3] :ref 4}
           {:db/id 2 :name "Child"}
           {:db/id 3 :name "Other child"}
           {:db/id 4 :name "Reference"}]))
        view (db/database-view database)]
    (if-some [child (entity-by-id database 2)]
      (if-some [other-child (entity-by-id database 3)]
        (if-some [reference (entity-by-id database 4)]
          (let [^:map<keyword;datascript.impl.entity/EntityValue> values
                (->
                 (hash-map)
                 (assoc :child (entity/EntityReference child))
                 (assoc
                  :children
                  (entity/EntityReferences
                   (entity/entity-reference-set [(Some other-child)])))
                 (assoc :ref (entity/EntityReference reference)))
                touched (entity/touch-components view values)]
            (is (entity-touched? child))
            (is (entity-touched? other-child))
            (is (not (entity-touched? reference)))
            (match
              (get
               touched
               :child
               (entity/EntityScalar
                (Datascript_runtime.Data_value.Nil)))
              (entity/EntityReference target)
              (is (= 2 (.-eid target)))
              _ (is false))
            (match
              (get
               touched
               :children
               (entity/EntityScalar
                (Datascript_runtime.Data_value.Nil)))
              (entity/EntityReferences targets)
              (is
               (some
                (fn [target] (entity-option-id? target 3))
                targets))
              _ (is false)))
          (is false))
        (is false))
      (is false))))

(deftest test-touch-recurses-through-components
  (let [database
        (->
         (d/empty-db
          {:child
           {:db/valueType :db.type/ref
            :db/isComponent true}
           :grandchild
           {:db/valueType :db.type/ref
            :db/isComponent true}
           :ref {:db/valueType :db.type/ref}})
         (d/db-with
          [{:db/id 1 :child 2 :ref 4}
           {:db/id 2 :grandchild 3}
           {:db/id 3 :name "Grandchild"}
           {:db/id 4 :name "Reference"}]))]
    (if-some [parent (entity-by-id database 1)]
      (let [parent (entity/touch-entity parent)]
        (if-some [child (entity-reference parent :child)]
          (do
            (is (entity-touched? child))
            (if-some [grandchild
                      (entity-reference child :grandchild)]
              (is (entity-touched? grandchild))
              (is false)))
          (is false))
        (if-some [reference (entity-reference parent :ref)]
          (is (not (entity-touched? reference)))
          (is false)))
      (is false))))

(deftest test-missing-refs
  (let [^:map<keyword;map<keyword;Datascript_runtime.Data_value.t>> schema
        (db/schema-map
         {:ref {:db/valueType :db.type/ref}
          :comp
          {:db/valueType :db.type/ref
           :db/isComponent true}
          :multiref
          {:db/valueType :db.type/ref
           :db/cardinality :db.cardinality/many}
          :multicomp
          {:db/valueType :db.type/ref
           :db/isComponent true
           :db/cardinality :db.cardinality/many}})
        database (d/empty-db schema)
        database-with-refs
        (d/db-with
         database
         [[:db/add 1 :ref 2]
          [:db/add 1 :comp 3]
          [:db/add 1 :multiref 4]
          [:db/add 1 :multiref 5]
          [:db/add 1 :multicomp 6]
          [:db/add 1 :multicomp 6]])
        source (entity-by-id database 1)]
    (d/touch source)
    (is (entity-attribute-missing? source :ref))
    (is (entity-attribute-missing? source :comp))
    (is (entity-attribute-missing? source :multiref))
    (is (entity-attribute-missing? source :multicomp))))

(deftest test-entity-misses
  (let [database
        (->
         (d/empty-db {:name {:db/unique :db.unique/identity}})
         (d/db-with
          [{:db/id 1 :name "Ivan"}
           {:db/id 2 :name "Oleg"}]))
        view (db/database-view database)]
    (is
     (nil?
      (d/entity-closed
       view
       (Datascript_runtime.Data_value.Temp_id "abc"))))
    (is
     (nil?
      (d/entity-closed
       view
       (Datascript_runtime.Data_value.Current_tx))))
    (is
     (nil?
      (d/entity-closed
       view
       (Datascript_runtime.Data_value.Ident ":keyword"))))
    (is
     (nil?
      (d/entity-closed
       view
       (Datascript_runtime.Data_value.Lookup_ref
        ":name"
        (string-value "Petr")))))
    (is
     (nil?
      (d/entity-closed
       view
       (Datascript_runtime.Data_value.Entity_id 777))))
    (is
     (thrown-msg?
      "Lookup ref attribute should be marked as :db/unique: [:not-an-attr 777]"
      (d/entity-closed
       view
       (Datascript_runtime.Data_value.Lookup_ref
        ":not-an-attr"
        (int-value 777)))))))

(deftest test-entity-equality
  (let [database-1
        (->
         (d/empty-db {})
         (d/db-with [{:db/id 1 :name "Ivan"}]))
        database-2 (d/db-with database-1 [])
        database-3
        (d/db-with database-2 [{:db/id 2 :name "Oleg"}])]
    (if-some [source (entity-by-id database-1 1)]
      (testing "Two entities are equal if they have the same :db/id"
        (is (= source source))
        (if-some [same (entity-by-id database-1 1)]
          (is (= source same))
          (is false))

        (testing "and refer to the same database"
          (if-some [other (entity-by-id database-2 1)]
            (is (not= source other))
            (is false))
          (if-some [other (entity-by-id database-3 1)]
            (is (not= source other))
            (is false))))
      (is false))))

(deftest test-entity-hash
  (let [database-1
        (->
         (d/empty-db {})
         (d/db-with [{:db/id 1 :name "Ivan"}]))
        database-2 (d/db-with database-1 [])
        database-3
        (d/db-with database-1 [{:db/id 2 :name "Oleg"}])]
    (if-some [source (entity-by-id database-1 1)]
      (testing "Two entities have the same hash if they have the same :db/id"
        (is (= (hash source) (hash source)))
        (if-some [same (entity-by-id database-1 1)]
          (is (= (hash source) (hash same)))
          (is false))

        (testing "and refer to the same database"
          (if-some [other (entity-by-id database-2 1)]
            (is (not= (hash source) (hash other)))
            (is false))
          (if-some [other (entity-by-id database-3 1)]
            (is (not= (hash source) (hash other)))
            (is false))))
      (is false))))
