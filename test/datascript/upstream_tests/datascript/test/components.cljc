(ns datascript.test.components
  (:require
   [clojure.test :refer [deftest is testing]]
   [datascript.core :as d]
   [datascript.db :as db]
   [datascript.impl.entity :as entity]))

(defn ^:option<datascript.impl.entity/Entity> entity-by-id
  [^datascript.db/DB database ^:int eid]
  (d/entity-closed
   (db/database-view database)
   (Datascript_runtime.Data_value.Entity_id eid)))

(defn entity-scalar-equal?
  [^datascript.impl.entity/Entity source
   ^:keyword attr
   ^:Datascript_runtime.Data_value.t expected]
  (match (entity/lookup-entity source attr)
    (Some (entity/EntityScalar actual))
    (Datascript_runtime.Data_value.equal actual expected)
    _ false))

(defn entity-reference-id?
  [^datascript.impl.entity/Entity source ^:keyword attr ^:int expected]
  (match (entity/lookup-entity source attr)
    (Some (entity/EntityReference target))
    (= expected (.-eid target))
    _ false))

(defn entity-reference
  [^datascript.impl.entity/Entity source ^:keyword attr]
  :option<datascript.impl.entity/Entity>
  (match (entity/lookup-entity source attr)
    (Some (entity/EntityReference target)) (Some target)
    _ None))

(defn entity-option-id?
  [^:option<datascript.impl.entity/Entity> source ^:int expected]
  (match source
    (Some target) (= expected (.-eid target))
    None false))

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

(defn no-datoms-for?
  [^datascript.db/DB database ^:vector<int> eids]
  (every?
   (fn [^datascript.db/Datom datom]
     (not
      (some
       (fn [^:int eid]
         (= eid (.-e datom)))
       eids)))
   (d/datoms database :eavt)))

(defn string-value [^:string value]
  :Datascript_runtime.Data_value.t
  (Datascript_runtime.Data_value.String value))

(deftest test-components
  (is
   (thrown-msg?
    "Bad attribute specification for :profile: {:db/isComponent true} should also have {:db/valueType :db.type/ref}"
    (d/empty-db {:profile {:db/isComponent true}})))
  (is
   (thrown-msg?
    "Bad attribute specification for {:profile {:db/isComponent \"aaa\"}}, expected one of #{true false}"
    (d/empty-db
     {:profile
      {:db/isComponent "aaa"
       :db/valueType :db.type/ref}})))

  (let [database
        (d/db-with
         (d/empty-db
          {:profile
           {:db/valueType :db.type/ref
            :db/isComponent true}})
         [{:db/id 1 :name "Ivan" :profile 3}
          {:db/id 3 :email "@3"}
          {:db/id 4 :email "@4"}])]
    (testing "touch"
      (if-some [source (entity-by-id database 1)]
        (let [source (entity/touch-entity source)]
          (is (entity-scalar-equal? source :name (string-value "Ivan")))
          (is (entity-reference-id? source :profile 3))
          (if-some [profile (entity-reference source :profile)]
            (is
             (entity-scalar-equal?
              (entity/touch-entity profile)
              :email
              (string-value "@3")))
            (is false)))
        (is false))
      (let [nested-database
            (d/db-with database [[:db/add 3 :profile 4]])]
        (if-some [source (entity-by-id nested-database 1)]
          (if-some [profile (entity-reference source :profile)]
            (if-some [nested (entity-reference profile :profile)]
              (do
                (is (= 4 (.-eid nested)))
                (is
                 (entity-scalar-equal?
                  nested
                  :email
                  (string-value "@4"))))
              (is false))
            (is false))
          (is false))))

    (testing "retractEntity"
      (let [retracted
            (d/db-with database [[:db.fn/retractEntity 1]])]
        (is (no-datoms-for? retracted [1 3]))))

    (testing "retractAttribute"
      (let [retracted
            (d/db-with
             database
             [[:db.fn/retractAttribute 1 :profile]])]
        (is (no-datoms-for? retracted [3]))))

    (testing "reverse navigation"
      (if-some [profile (entity-by-id database 3)]
        (is (entity-reference-id? profile :_profile 1))
        (is false)))))

(deftest test-components-multival
  (let [database
        (d/db-with
         (d/empty-db
          {:profile
           {:db/valueType :db.type/ref
            :db/cardinality :db.cardinality/many
            :db/isComponent true}})
         [{:db/id 1 :name "Ivan" :profile [3 4]}
          {:db/id 3 :email "@3"}
          {:db/id 4 :email "@4"}])]
    (testing "touch"
      (if-some [source (entity-by-id database 1)]
        (let [source (entity/touch-entity source)]
          (is (entity-reference-ids? source :profile [3 4]))
          (match (entity/lookup-entity source :profile)
            (Some (entity/EntityReferences targets))
            (is
             (every?
              (fn [^:option<datascript.impl.entity/Entity> target]
                (match target
                  (Some value)
                  (let [value (entity/touch-entity value)]
                    (or
                     (entity-scalar-equal?
                      value :email (string-value "@3"))
                     (entity-scalar-equal?
                      value :email (string-value "@4"))))
                  None false))
              targets))
            _ (is false)))
        (is false)))

    (testing "retractEntity"
      (let [retracted
            (d/db-with database [[:db.fn/retractEntity 1]])]
        (is (no-datoms-for? retracted [1 3 4]))))

    (testing "retractAttribute"
      (let [retracted
            (d/db-with
             database
             [[:db.fn/retractAttribute 1 :profile]])]
        (is (no-datoms-for? retracted [3 4]))))

    (testing "reverse navigation"
      (if-some [profile (entity-by-id database 3)]
        (is (entity-reference-id? profile :_profile 1))
        (is false)))))
