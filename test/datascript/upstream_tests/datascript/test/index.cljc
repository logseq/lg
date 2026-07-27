(ns datascript.test.index
  (:require
   [clojure.test :refer [are deftest is testing]]
   [datascript.core :as d]
   [datascript.db :as db]
   [datascript.test.core :as tdc]))

(defn datom-triple
  [^datascript.db/Datom datom]
  :tuple<int;keyword;Datascript_runtime.Data_value.t>
  (tuple
   (.-e datom)
   (db/datom-attr datom)
   (.-v datom)))

(defn datom-triples
  [^:seq<datascript.db/Datom> datoms]
  :vector<tuple<int;keyword;Datascript_runtime.Data_value.t>>
  (mapv datom-triple datoms))

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
  (let [actual-triples (datom-triples actual)]
    (and
     (= (count actual-triples) (count expected))
     (loop [index 0]
       (if (< index (count actual-triples))
         (if
          (datom-triple-equal?
           (nth actual-triples index)
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
        'datascript.test.index/datom-triples-equal?
        actual
        (cons
         'vector
         (map
          (fn [[eid attr value]]
            (list
             'tuple
             eid
             attr
             (value-form value)))
          rows)))))}
  [^:seq<datascript.db/Datom> actual
   ^:vector<tuple<int;keyword;Datascript_runtime.Data_value.t>> expected]
  (datom-triples-equal? actual expected))

(defn datom-equal?
  [^:option<datascript.db/Datom> actual
   ^:tuple<int;keyword;Datascript_runtime.Data_value.t> expected]
  (if-some [datom actual]
    (datom-triple-equal? (datom-triple datom) expected)
    false))

(defn datom?
  {:inline
   (fn [actual row]
     (let [[eid attr value] row
           value-form
           (if (string? value)
             (list 'Datascript_runtime.Data_value.String value)
             (list 'Datascript_runtime.Data_value.Int value))]
       (list
        'datascript.test.index/datom-equal?
        actual
        (list 'tuple eid attr value-form))))}
  [^:option<datascript.db/Datom> actual
   ^:tuple<int;keyword;Datascript_runtime.Data_value.t> expected]
  (datom-equal? actual expected))

(defn int-vector-value
  [^:vector<int> values]
  :Datascript_runtime.Data_value.t
  (Datascript_runtime.Data_value.vector_of_vector_with
   tdc/int-value
   values))

(defn int-list-value
  [^:list<int> values]
  :Datascript_runtime.Data_value.t
  (Datascript_runtime.Data_value.List
   (into (list) (map tdc/int-value (reverse values)))))

(defn int-sequence-value
  [^:vector<int> values]
  :Datascript_runtime.Data_value.t
  (Datascript_runtime.Data_value.List
   (into (list) (map tdc/int-value (reverse values)))))

(defn datom-eids
  [^:seq<datascript.db/Datom> datoms]
  :vector<int>
  (mapv
   (fn [^datascript.db/Datom datom]
     (.-e datom))
   datoms))

(deftest test-datoms
  (let [database
        (->
         (d/empty-db {:age {:db/index true}})
         (d/db-with
          [[:db/add 1 :name "Petr"]
           [:db/add 1 :age 44]
           [:db/add 2 :name "Ivan"]
           [:db/add 2 :age 25]
           [:db/add 3 :name "Sergey"]
           [:db/add 3 :age 11]]))]
    (testing "Main indexes, sort order"
      (is
       (datoms?
        (d/datoms database :aevt)
        [[1 :age 44]
         [2 :age 25]
         [3 :age 11]
         [1 :name "Petr"]
         [2 :name "Ivan"]
         [3 :name "Sergey"]]))

      (is
       (datoms?
        (d/datoms database :eavt)
        [[1 :age 44]
         [1 :name "Petr"]
         [2 :age 25]
         [2 :name "Ivan"]
         [3 :age 11]
         [3 :name "Sergey"]]))

      (is
       (datoms?
        (d/datoms database :avet)
        [[3 :age 11]
         [2 :age 25]
         [1 :age 44]])))

    (testing "Components filtration"
      (is
       (datoms?
        (d/datoms database :eavt 1)
        [[1 :age 44]
         [1 :name "Petr"]]))

      (is
       (datoms?
        (d/datoms database :eavt 1 :age)
        [[1 :age 44]]))

      (is
       (datoms?
        (d/datoms database :avet :age)
        [[3 :age 11]
         [2 :age 25]
         [1 :age 44]])))

    (testing "Error reporting"
      (d/datoms database :avet)

      (is
       (thrown-msg?
        "Attribute :name should be marked as :db/index true"
        (d/datoms database :avet :name)))

      (is
       (thrown-msg?
        "Attribute :alias should be marked as :db/index true"
        (d/datoms database :avet :alias)))

      (is
       (thrown-msg?
        "Attribute :name should be marked as :db/index true"
        (d/datoms database :avet :name "Ivan")))

      (is
       (thrown-msg?
        "Attribute :name should be marked as :db/index true"
        (d/datoms database :avet :name "Ivan" 1))))

    (testing "Sequence compare issue-470"
      (let [sequence-db
            (->
             (d/empty-db {:path {:db/index true}})
             (d/db-with
              [{:db/id 1 :path [1 2]}
               {:db/id 2 :path [1 2 3]}]))]
        (are [value result]
          (= result (datom-eids (d/datoms sequence-db :avet :path value)))
          (int-vector-value [1])       []
          (int-vector-value [1 1])     []
          (int-vector-value [1 2])     [1]
          (int-list-value (list 1 2))  [1]
          (int-sequence-value (butlast [1 2 3])) [1]
          (int-vector-value [1 3])     []
          (int-vector-value [1 2 2])   []
          (int-vector-value [1 2 3])   [2]
          (int-list-value (list 1 2 3)) [2]
          (int-sequence-value (butlast [1 2 3 4])) [2]
          (int-vector-value [1 2 4])   []
          (int-vector-value [1 2 3 4]) [])))))

(deftest test-datom
  (let [database
        (->
         (d/empty-db {:age {:db/index true}})
         (d/db-with
          [[:db/add 1 :name "Petr"]
           [:db/add 1 :age 44]
           [:db/add 2 :name "Ivan"]
           [:db/add 2 :age 25]
           [:db/add 3 :name "Sergey"]
           [:db/add 3 :age 11]]))]
    (is (datom? (d/find-datom database :eavt) [1 :age 44]))
    (is (datom? (d/find-datom database :eavt 1) [1 :age 44]))
    (is (datom? (d/find-datom database :eavt 1 :age) [1 :age 44]))
    (is
     (datom?
      (d/find-datom database :eavt 1 :name)
      [1 :name "Petr"]))
    (is
     (datom?
      (d/find-datom database :eavt 1 :name "Petr")
      [1 :name "Petr"]))

    (is (datom? (d/find-datom database :eavt 2) [2 :age 25]))
    (is (datom? (d/find-datom database :eavt 2 :age) [2 :age 25]))
    (is
     (datom?
      (d/find-datom database :eavt 2 :name)
      [2 :name "Ivan"]))

    (is (nil? (d/find-datom database :eavt 1 :name "Ivan")))
    (is (nil? (d/find-datom database :eavt 4)))

    (is (nil? (d/find-datom (d/empty-db) :eavt)))
    (is
     (nil?
      (d/find-datom
       (d/empty-db {:age {:db/index true}})
       :eavt)))))

(deftest test-seek-datoms
  (let [database
        (->
         (d/empty-db
          {:name {:db/index true}
           :age {:db/index true}})
         (d/db-with
          [[:db/add 1 :name "Petr"]
           [:db/add 1 :age 44]
           [:db/add 2 :name "Ivan"]
           [:db/add 2 :age 25]
           [:db/add 3 :name "Sergey"]
           [:db/add 3 :age 11]]))]
    (testing "Non-termination"
      (is
       (datoms?
        (d/seek-datoms database :avet :age 10)
        [[3 :age 11]
         [2 :age 25]
         [1 :age 44]
         [2 :name "Ivan"]
         [1 :name "Petr"]
         [3 :name "Sergey"]])))

    (testing "Closest value lookup"
      (is
       (datoms?
        (d/seek-datoms database :avet :name "P")
        [[1 :name "Petr"]
         [3 :name "Sergey"]])))

    (testing "Exact value lookup"
      (is
       (datoms?
        (d/seek-datoms database :avet :name "Petr")
        [[1 :name "Petr"]
         [3 :name "Sergey"]])))

    (is
     (thrown-msg?
      "Attribute :alias should be marked as :db/index true"
      (d/seek-datoms database :avet :alias)))))

(deftest test-rseek-datoms
  (let [database
        (->
         (d/empty-db
          {:name {:db/index true}
           :age {:db/index true}})
         (d/db-with
          [[:db/add 1 :name "Petr"]
           [:db/add 1 :age 44]
           [:db/add 2 :name "Ivan"]
           [:db/add 2 :age 25]
           [:db/add 3 :name "Sergey"]
           [:db/add 3 :age 11]]))]
    (testing "Non-termination"
      (is
       (datoms?
        (d/rseek-datoms database :avet :name "Petr")
        [[1 :name "Petr"]
         [2 :name "Ivan"]
         [1 :age 44]
         [2 :age 25]
         [3 :age 11]])))

    (testing "Closest value lookup"
      (is
       (datoms?
        (d/rseek-datoms database :avet :age 26)
        [[2 :age 25]
         [3 :age 11]])))

    (testing "Exact value lookup"
      (is
       (datoms?
        (d/rseek-datoms database :avet :age 25)
        [[2 :age 25]
         [3 :age 11]])))

    (is
     (thrown-msg?
      "Attribute :alias should be marked as :db/index true"
      (d/rseek-datoms database :avet :alias)))))

(deftest test-index-range
  (let [database
        (d/db-with
         (d/empty-db
          {:name {:db/index true}
           :age {:db/index true}})
         [{:db/id 1 :name "Ivan" :age 15}
          {:db/id 2 :name "Oleg" :age 20}
          {:db/id 3 :name "Sergey" :age 7}
          {:db/id 4 :name "Pavel" :age 45}
          {:db/id 5 :name "Petr" :age 20}])]
    (is
     (datoms?
      (d/index-range database :name "Pe" "S")
      [[5 :name "Petr"]]))
    (is
     (datoms?
      (d/index-range database :name "O" "Sergey")
      [[2 :name "Oleg"]
       [4 :name "Pavel"]
       [5 :name "Petr"]
       [3 :name "Sergey"]]))

    (is
     (datoms?
      (d/index-range database :name nil "P")
      [[1 :name "Ivan"]
       [2 :name "Oleg"]]))
    (is
     (datoms?
      (d/index-range database :name "R" nil)
      [[3 :name "Sergey"]]))
    (is
     (datoms?
      (d/index-range database :name nil nil)
      [[1 :name "Ivan"]
       [2 :name "Oleg"]
       [4 :name "Pavel"]
       [5 :name "Petr"]
       [3 :name "Sergey"]]))

    (is
     (datoms?
      (d/index-range database :age 15 20)
      [[1 :age 15]
       [2 :age 20]
       [5 :age 20]]))
    (is
     (datoms?
      (d/index-range database :age 7 45)
      [[3 :age 7]
       [1 :age 15]
       [2 :age 20]
       [5 :age 20]
       [4 :age 45]]))
    (is
     (datoms?
      (d/index-range database :age 0 100)
      [[3 :age 7]
       [1 :age 15]
       [2 :age 20]
       [5 :age 20]
       [4 :age 45]]))

    (is
     (thrown-msg?
      "Attribute :alias should be marked as :db/index true"
      (d/index-range database :alias "e" "u")))))
