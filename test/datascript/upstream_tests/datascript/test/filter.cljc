(ns datascript.test.filter
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

(defn query-string-row [^:string value]
  (array
   (query-types/value-result
    (Datascript_runtime.Data_value.String value))))

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

(defn entity-attr-absent?
  [^:option<datascript.impl.entity/Entity> maybe-entity
   ^:keyword attr]
  (if-some [entity maybe-entity]
    (match (entity/lookup-entity entity attr)
      None true
      _ false)
    true))

(defn data-value-vectors-equal?
  [^:vector<Datascript_runtime.Data_value.t> left
   ^:vector<Datascript_runtime.Data_value.t> right]
  (and
   (= (count left) (count right))
   (loop [index 0]
     (if (< index (count left))
       (if
        (Datascript_runtime.Data_value.equal
         (nth left index)
         (nth right index))
         (recur (inc index))
         false)
       true))))

(defn optional-data-map-equal?
  [^:option<map<Datascript_runtime.Data_value.t;Datascript_runtime.Data_value.t>>
   actual
   ^:option<map<keyword;Datascript_runtime.Data_value.t>> expected]
  (match (tuple actual expected)
    (tuple (Some actual) (Some expected))
    (Datascript_runtime.Data_value.equal
     (Datascript_runtime.Data_value.map_of_data_map actual)
     (Datascript_runtime.Data_value.map_of_keyword_map expected))
    (tuple None None)
    true
    _
    false))

(defn ^:map<keyword;Datascript_runtime.Data_value.t> single-data-map
  [^:keyword key ^:Datascript_runtime.Data_value.t value]
  (assoc {} key value))

(defn datom-values
  [^datascript.db/DB database ^:keyword attr]
  (mapv
   (fn [^datascript.db/Datom datom]
     (.-v datom))
   (d/datoms
    database
    :aevt
    (Datascript_runtime.Data_value.Keyword (str attr)))))

(defn filtered-datom-values
  [^datascript.db/FilteredDB database ^:keyword attr]
  (mapv
   (fn [^datascript.db/Datom datom]
     (.-v datom))
   (d/datoms
    database
    :aevt
    (Datascript_runtime.Data_value.Keyword (str attr)))))

(defn db-filtered-equal?
  [^datascript.db/DB database
   ^datascript.db/FilteredDB filtered]
  (and
   (= (db/-schema database) (db/-schema filtered))
   (db/datom-vectors-equal?
    (vec (db/-datoms database :eavt nil nil nil nil))
    (db/filtered-db-datoms filtered))))

(deftest test-filter-db
  (let [empty-db (d/empty-db {:aka {:db/cardinality :db.cardinality/many}})
        db (-> empty-db
             (d/db-with [{:db/id 1
                          :name  "Petr"
                          :email "petya@spb.ru"
                          :aka   ["I" "Great"]
                          :password "<SECRET>"}
                         {:db/id 2
                          :name  "Ivan"
                          :aka   ["Terrible" "IV"]
                          :password "<PROTECTED>"}
                         {:db/id 3
                          :name  "Nikolai"
                          :aka   ["II"]
                          :password "<UNKWOWN>"}]))
        remove-pass
        (fn [^datascript.db/DB _database
             ^datascript.db/Datom datom]
          (not= :password (db/datom-attr datom)))
        remove-ivan
        (fn [^datascript.db/DB _database
             ^datascript.db/Datom datom]
          (not= 2 (.-e datom)))
        long-akas
        (fn [^datascript.db/DB unfiltered-db
             ^datascript.db/Datom datom]
          (or
           (not= :aka (db/datom-attr datom))
           (<=
            (count
             (db/search-vector
              unfiltered-db
              (Some (.-e datom))
              (Some :aka)
              None
              None))
           1)
           (match (.-v datom)
             (Datascript_runtime.Data_value.String value)
             (>= (String.length value) 4)
             _ false)))]
    
    (are [_db _res]
      (query-relation-equal?
       (d/q '[:find ?v :where [_ :password ?v]] _db)
       _res)
      db
      [(query-string-row "<SECRET>")
       (query-string-row "<PROTECTED>")
       (query-string-row "<UNKWOWN>")]
      (d/filter db remove-pass)
      []
      (d/filter db remove-ivan)
      [(query-string-row "<SECRET>")
       (query-string-row "<UNKWOWN>")]
      (-> db (d/filter remove-ivan) (d/filter remove-pass))
      [])

    (are [_db _res]
      (query-relation-equal?
       (d/q '[:find ?v :where [_ :aka ?v]] _db)
       _res)
      db
      [(query-string-row "I")
       (query-string-row "Great")
       (query-string-row "Terrible")
       (query-string-row "IV")
       (query-string-row "II")]
      (d/filter db remove-pass)
      [(query-string-row "I")
       (query-string-row "Great")
       (query-string-row "Terrible")
       (query-string-row "IV")
       (query-string-row "II")]
      (d/filter db remove-ivan)
      [(query-string-row "I")
       (query-string-row "Great")
       (query-string-row "II")]
      (d/filter db long-akas)
      [(query-string-row "Great")
       (query-string-row "Terrible")
       (query-string-row "II")]
      (-> db (d/filter remove-ivan) (d/filter long-akas))
      [(query-string-row "Great")
       (query-string-row "II")]
      (-> db (d/filter long-akas) (d/filter remove-ivan))
      [(query-string-row "Great")
       (query-string-row "II")])
     
    (testing "Entities"
      (is
       (entity-scalar-equal?
        (d/entity db 1)
        :password
        (Datascript_runtime.Data_value.String "<SECRET>")))
      (is
       (entity-attr-absent?
        (d/entity (d/filter db remove-pass) 1)
        :password))
      (is
       (entity-scalar-equal?
        (d/entity db 2)
        :aka
        (Datascript_runtime.Data_value.Set
         (list
          (Datascript_runtime.Data_value.String "Terrible")
          (Datascript_runtime.Data_value.String "IV")))))
      (is
       (entity-scalar-equal?
        (d/entity (d/filter db long-akas) 2)
        :aka
        (Datascript_runtime.Data_value.Set
         (list
          (Datascript_runtime.Data_value.String "Terrible"))))))

    (testing "Pull"
      (is
       (optional-data-map-equal?
        (d/pull
         (d/filter db remove-pass)
         [:name :password]
        1)
        (Some
         (single-data-map
          :name
          (Datascript_runtime.Data_value.String "Petr")))))
      (is
       (optional-data-map-equal?
        (d/pull
         (d/filter db remove-ivan)
         [:name :password]
         2)
        None))
      (is
       (optional-data-map-equal?
        (d/pull
         (d/filter db long-akas)
         [:aka]
        2)
        (Some
         (single-data-map
          :aka
          (Datascript_runtime.Data_value.Vector
           (list
            (Datascript_runtime.Data_value.String
             "Terrible"))))))))
    
    (testing "Index access"
      (is
       (data-value-vectors-equal?
        (datom-values db :password)
        [(Datascript_runtime.Data_value.String "<SECRET>")
         (Datascript_runtime.Data_value.String "<PROTECTED>")
         (Datascript_runtime.Data_value.String "<UNKWOWN>")]))
      (is
       (data-value-vectors-equal?
        (filtered-datom-values (d/filter db remove-pass) :password)
        [])))
  
    (testing "equiv"
      (is
       (db-filtered-equal?
        (d/db-with db [[:db.fn/retractEntity 2]])
        (d/filter db remove-ivan)))
      (is
       (db-filtered-equal?
        empty-db
        (d/filter
         empty-db
         (fn [^datascript.db/DB _database
              ^datascript.db/Datom _datom]
           true))))
      (is
       (db-filtered-equal?
        empty-db
        (d/filter
         db
         (fn [^datascript.db/DB _database
              ^datascript.db/Datom _datom]
           false)))))
    
    (testing "hash"
      (is
       (=
        (db/db-hash (d/db-with db [[:db.fn/retractEntity 2]]))
        (db/filtered-db-hash (d/filter db remove-ivan))))
      (is
       (=
        (db/db-hash empty-db)
        (db/filtered-db-hash
         (d/filter
          empty-db
          (fn [^datascript.db/DB _database
               ^datascript.db/Datom _datom]
            true)))))
      (is
       (=
        (db/db-hash empty-db)
        (db/filtered-db-hash
         (d/filter
          db
          (fn [^datascript.db/DB _database
               ^datascript.db/Datom _datom]
            false)))))))
  
  (testing "double filtering"
    (let [db       (d/db-with (d/empty-db {})
                     [{:db/id 1, :name "Petr", :age 32}
                      {:db/id 2, :name "Oleg"}
                      {:db/id 3, :name "Ivan", :age 12}])
          has-age?
          (fn [^datascript.db/DB database
               ^datascript.db/Datom datom]
            (some?
             (db/search-ea database (.-e datom) :age)))
          adult?
          (fn [^datascript.db/DB database
               ^datascript.db/Datom datom]
            (if-some [age (db/search-ea database (.-e datom) :age)]
              (match (.-v age)
                (Datascript_runtime.Data_value.Int value)
                (>= value 18)
                _ false)
              false))]
      (is
       (data-value-vectors-equal?
        [(Datascript_runtime.Data_value.String "Petr")
         (Datascript_runtime.Data_value.String "Oleg")
         (Datascript_runtime.Data_value.String "Ivan")]
        (datom-values db :name)))
      (is
       (data-value-vectors-equal?
        [(Datascript_runtime.Data_value.String "Petr")
         (Datascript_runtime.Data_value.String "Ivan")]
        (filtered-datom-values
         (d/filter db has-age?)
         :name)))
      (is
       (data-value-vectors-equal?
        [(Datascript_runtime.Data_value.String "Petr")]
        (filtered-datom-values
         (-> db
             (d/filter has-age?)
             (d/filter adult?))
         :name))))))
