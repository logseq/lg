(ns datascript.test.pull-api
  (:require
    [clojure.test :as t :refer [is are deftest testing]]
    [datascript.core :as d]
    [datascript.db :as db]
    [datascript.pull-api :as pull-api]
    [datascript.pull-parser :as dpp]
    [datascript.test.core :as tdc]))

(defn
  ^:map<keyword;Datascript_runtime.Data_value.t>
  schema-entry
  [^:vector<keyword> keys
   ^:vector<Datascript_runtime.Data_value.t> values]
  (zipmap keys values))

(def
  ^:private
  ^:map<keyword;map<keyword;Datascript_runtime.Data_value.t>>
  test-schema
  (zipmap
   [:name :aka :child :friend :enemy :father :part :spec]
   [(schema-entry
     [:db/unique]
     [(Datascript_runtime.Data_value.Keyword
       ":db.unique/identity")])
    (schema-entry
     [:db/cardinality]
     [(Datascript_runtime.Data_value.Keyword
       ":db.cardinality/many")])
    (schema-entry
     [:db/cardinality :db/valueType]
     [(Datascript_runtime.Data_value.Keyword
       ":db.cardinality/many")
      (Datascript_runtime.Data_value.Keyword
       ":db.type/ref")])
    (schema-entry
     [:db/cardinality :db/valueType]
     [(Datascript_runtime.Data_value.Keyword
       ":db.cardinality/many")
      (Datascript_runtime.Data_value.Keyword
       ":db.type/ref")])
    (schema-entry
     [:db/cardinality :db/valueType]
     [(Datascript_runtime.Data_value.Keyword
       ":db.cardinality/many")
      (Datascript_runtime.Data_value.Keyword
       ":db.type/ref")])
    (schema-entry
     [:db/valueType]
     [(Datascript_runtime.Data_value.Keyword
       ":db.type/ref")])
    (schema-entry
     [:db/valueType :db/isComponent :db/cardinality]
     [(Datascript_runtime.Data_value.Keyword
       ":db.type/ref")
      (Datascript_runtime.Data_value.Bool true)
      (Datascript_runtime.Data_value.Keyword
       ":db.cardinality/many")])
    (schema-entry
     [:db/valueType :db/isComponent :db/cardinality]
     [(Datascript_runtime.Data_value.Keyword
       ":db.type/ref")
      (Datascript_runtime.Data_value.Bool true)
      (Datascript_runtime.Data_value.Keyword
       ":db.cardinality/one")])]))

(def test-datoms
  [(d/datom 1 :name "Petr")
   (d/datom 1 :aka "Devil")
   (d/datom 1 :aka "Tupen")
   (d/datom 2 :name "David")
   (d/datom 3 :name "Thomas")
   (d/datom 4 :name "Lucy")
   (d/datom 5 :name "Elizabeth")
   (d/datom 6 :name "Matthew")
   (d/datom 7 :name "Eunan")
   (d/datom 8 :name "Kerri")
   (d/datom 9 :name "Rebecca")
   (d/datom 1 :child 2)
   (d/datom 1 :child 3)
   (d/datom 2 :father 1)
   (d/datom 3 :father 1)
   (d/datom 6 :father 3)
   (d/datom 10 :name "Part A")
   (d/datom 11 :name "Part A.A")
   (d/datom 10 :part 11)
   (d/datom 12 :name "Part A.A.A")
   (d/datom 11 :part 12)
   (d/datom 13 :name "Part A.A.A.A")
   (d/datom 12 :part 13)
   (d/datom 14 :name "Part A.A.A.B")
   (d/datom 12 :part 14)
   (d/datom 15 :name "Part A.B")
   (d/datom 10 :part 15)
   (d/datom 16 :name "Part A.B.A")
   (d/datom 15 :part 16)
   (d/datom 17 :name "Part A.B.A.A")
   (d/datom 16 :part 17)
   (d/datom 18 :name "Part A.B.A.B")
   (d/datom 16 :part 18)])

(def ^:private *test-db
  (delay
    (d/init-db test-datoms test-schema)))

(deftest test-parse-opts-and-pull-impl
  (let [database (db/database-view @*test-db)
        pattern [(dpp/source-attribute :name)]
        entity-ref (Datascript_runtime.Data_value.Entity_id 1)
        parsed (pull-api/parse-opts database pattern)
        expected (pull-api/pull-source database pattern entity-ref)]
    (is (= expected (pull-api/pull-impl parsed entity-ref)))
    (is
     (=
      None
      (pull-api/pull-impl
       parsed
       (Datascript_runtime.Data_value.Entity_id 999999))))
    (let [visits (volatile! 0)
          options
          (pull-api/pull-options
           (fn [_kind _entity _attr _value]
             (vswap! visits inc)
             (Stdlib.ignore 0)))
          parsed-with-visitor
          (pull-api/parse-opts database pattern options)]
      (is (= expected
             (pull-api/pull-impl parsed-with-visitor entity-ref)))
      (is (pos? @visits)))))

(defn ^:Datascript_runtime.Data_value.t optional-pull-to-data
  [^:option<map<Datascript_runtime.Data_value.t;Datascript_runtime.Data_value.t>>
   result]
  (match result
    None
    (Datascript_runtime.Data_value.Nil)
    (Some values)
    (Datascript_runtime.Data_value.map_of_data_map values)))

(defn pull-data-equal?
  [^:Datascript_runtime.Data_value.t expected
   ^:option<map<Datascript_runtime.Data_value.t;Datascript_runtime.Data_value.t>>
   actual]
  (Datascript_runtime.Data_value.equal
   expected
   (optional-pull-to-data actual)))

(defn ^:Datascript_runtime.Data_value.t optional-pulled-to-data
  [^:option<datascript.pull-api/pulled-value> result]
  (match result
    None
    (Datascript_runtime.Data_value.Nil)
    (Some value)
    (pull-api/pulled-to-data value)))

(deftest test-public-frame-protocol
  (let [database (db/database-view @*test-db)
        name-source [(dpp/source-attribute :name)]
        name-parsed (pull-api/parse-opts database name-source)
        name-context (.-context name-parsed)
        name-pattern (.-pattern name-parsed)
        attrs
        (pull-api/attrs-frame
         name-context (set-of :int) {} name-pattern 1)
        reverse-frame
        (nth (pull-api/-run attrs name-context) 0)
        result-frame
        (nth (pull-api/-run reverse-frame name-context) 0)
        aka-source [(dpp/source-attribute :aka)]
        aka-parsed (pull-api/parse-opts database aka-source)
        aka-context (.-context aka-parsed)
        aka-frames
        (pull-api/-run
         (pull-api/attrs-frame
          aka-context (set-of :int) {} (.-pattern aka-parsed) 1)
         aka-context)
        aka-parent (nth aka-frames 0)
        aka-child (nth aka-frames 1)
        aka-result
        (nth (pull-api/-run aka-child aka-context) 0)
        child-source
        [(dpp/source-nested
          :child
          [(dpp/source-attribute :name)])]
        child-parsed (pull-api/parse-opts database child-source)
        child-context (.-context child-parsed)
        child-frames
        (pull-api/-run
         (pull-api/attrs-frame
          child-context
          (set-of :int)
          {}
          (.-pattern child-parsed)
          1)
         child-context)
        child-ref-frame (nth child-frames 1)]
    (is (satisfies? pull-api/IFrame attrs))
    (is (satisfies? pull-api/IFrame reverse-frame))
    (is (satisfies? pull-api/IFrame result-frame))
    (is (satisfies? pull-api/IFrame aka-child))
    (is (satisfies? pull-api/IFrame child-ref-frame))
    (is (=
         "AttrsFrame<id=1, attr=:name, attrs=>"
         (pull-api/-str attrs)))
    (is (=
         "ReverseAttrsFrame<id=1, attr=, attrs=>"
         (pull-api/-str reverse-frame)))
    (is (=
         "ResultFrame<value={:name \"Petr\"}>"
         (pull-api/-str result-frame)))
    (is (=
         "MultivalAttrFrame<attr=:aka>"
         (pull-api/-str aka-child)))
    (is (=
         "MultivalAttrFrame<attr=:child>"
         (pull-api/-str child-ref-frame)))
    (let [merged (pull-api/-merge aka-parent aka-result)
          actual
          (optional-pulled-to-data
           (pull-api/run-stack aka-context (list merged)))
          expected
          (optional-pull-to-data
           (pull-api/pull-source
            database
            aka-source
            (Datascript_runtime.Data_value.Entity_id 1)))]
      (is (Datascript_runtime.Data_value.equal expected actual)))
    (is
     (thrown-msg?
      "ResultFrame cannot be run"
      (pull-api/-run result-frame name-context)))
    (is
     (thrown-msg?
      "Frame does not accept a child result"
      (pull-api/-merge result-frame result-frame)))))

(deftest test-public-attrs-frame-wildcard-visitor
  (let [database (db/database-view @*test-db)
        visits (volatile! 0)
        source-pattern [dpp/source-wildcard]
        parsed
        (pull-api/parse-opts
         database
         source-pattern
         (pull-api/pull-options
          (fn [kind entity attr value]
            (when
                (and
                 (= kind :db.pull/wildcard)
                 (match entity
                   None false
                   (Some id) (= id 1))
                 (match attr
                   None true
                   (Some _) false)
                 (match value
                   None true
                   (Some _) false))
              (vswap! visits inc))
            (Stdlib.ignore 0))))
        frame
        (pull-api/attrs-frame
         (.-context parsed)
         (set-of :int)
         {}
         (.-pattern parsed)
         1)]
    (is (= 1 @visits))
    (is (satisfies? pull-api/IFrame frame))
    (is
     (Datascript_runtime.Data_value.equal
      (optional-pull-to-data
       (pull-api/pull-source
        database
        source-pattern
        (Datascript_runtime.Data_value.Entity_id 1)))
      (optional-pulled-to-data
       (pull-api/run-stack
        (.-context parsed)
        (list frame)))))))

(defn pull-many-data-equal?
  [^:Datascript_runtime.Data_value.t expected
   ^:vector<option<map<Datascript_runtime.Data_value.t;Datascript_runtime.Data_value.t>>>
   actual]
  (Datascript_runtime.Data_value.equal
   expected
   (Datascript_runtime.Data_value.vector_of_vector
    (mapv
     (fn
       [^:option<map<Datascript_runtime.Data_value.t;Datascript_runtime.Data_value.t>>
        result]
       (optional-pull-to-data result))
     actual))))

(defn ^int pull-vector-count
  [^:option<map<Datascript_runtime.Data_value.t;Datascript_runtime.Data_value.t>>
   result
   ^:keyword attr]
  (match result
    None 0
    (Some values)
    (if-some
      [value
       (get
        values
        (Datascript_runtime.Data_value.Keyword (str attr)))]
      (match value
        (Datascript_runtime.Data_value.Vector items)
        (List.length items)
        _ 0)
      0)))

(defn ^:option<string> deep-pulled-name
  [^:option<map<Datascript_runtime.Data_value.t;Datascript_runtime.Data_value.t>>
   result
   ^int steps]
  (match result
    None None
    (Some values)
    (loop
      [entity
       (Datascript_runtime.Data_value.map_of_data_map values)
       remaining steps]
      (if (= remaining 0)
        (match
          (Datascript_runtime.Data_value.keyword_map_get
           ":name"
           entity)
          (Some
           (Datascript_runtime.Data_value.String name))
          (Some name)
          _ None)
        (if-some
          [friends
           (Datascript_runtime.Data_value.keyword_map_get
            ":friend"
            entity)]
          (if-some
            [items
             (Datascript_runtime.Data_value.sequential_items
              friends)]
            (if-some [friend (first items)]
              (recur friend (dec remaining))
              None)
            None)
          None)))))

(defn
  ^:option<Datascript_runtime.Data_value.t>
  wrap-pull-value
  [^:option<Datascript_runtime.Data_value.t> value]
  (Some
   (Datascript_runtime.Data_value.Vector
    (list
     (match value
       None (Datascript_runtime.Data_value.Nil)
       (Some value) value)))))

(defn
  ^:option<Datascript_runtime.Data_value.t>
  pull-name-value
  [^:option<Datascript_runtime.Data_value.t> value]
  (match value
    None None
    (Some value)
    (Datascript_runtime.Data_value.keyword_map_get
     ":name"
     value)))

(defn
  ^:option<Datascript_runtime.Data_value.t>
  pull-many-name-values
  [^:option<Datascript_runtime.Data_value.t> value]
  (match value
    None None
    (Some value)
    (if-some
      [items
       (Datascript_runtime.Data_value.sequential_items value)]
      (Some
       (Datascript_runtime.Data_value.vector_of_vector
        (mapv
         (fn [^:Datascript_runtime.Data_value.t entity]
           (match
             (Datascript_runtime.Data_value.keyword_map_get
              ":name"
              entity)
             None (Datascript_runtime.Data_value.Nil)
             (Some name) name))
         items)))
      None)))

(defn data-literal
  {:inline
   (fn [source]
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
               '(Datascript_runtime.Data_value.Bool true)
               (= value false)
               '(Datascript_runtime.Data_value.Bool false)
               (vector? value)
               (list
                'Datascript_runtime.Data_value.Vector
                (cons
                 'list
                 (map value-form value)))
               (map? value)
               (list
                'Datascript_runtime.Data_value.Map
                (cons
                 'list
                 (map
                  (fn [entry]
                    (list
                     'tuple
                     (value-form (first entry))
                     (value-form (second entry))))
                  value)))
               (or (symbol? value) (seq? value))
               value
               :else
               (list
                'Datascript_runtime.Data_value.Int value)))]
       (value-form source)))}
  [^:Datascript_runtime.Data_value.t source]
  source)

(defn pull-literal-equal?
  {:inline
   (fn [expected actual]
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
               '(Datascript_runtime.Data_value.Bool true)
               (= value false)
               '(Datascript_runtime.Data_value.Bool false)
               (vector? value)
               (list
                'Datascript_runtime.Data_value.Vector
                (cons
                 'list
                 (map value-form value)))
               (map? value)
               (list
                'Datascript_runtime.Data_value.Map
                (cons
                 'list
                 (map
                  (fn [entry]
                    (list
                     'tuple
                     (value-form (first entry))
                     (value-form (second entry))))
                  value)))
               (or (symbol? value) (seq? value))
               value
               :else
               (list 'Datascript_runtime.Data_value.Int value)))]
       (list
        'datascript.test.pull-api/pull-data-equal?
        (value-form expected)
        actual)))}
  [^:Datascript_runtime.Data_value.t expected
   ^:option<map<Datascript_runtime.Data_value.t;Datascript_runtime.Data_value.t>>
   actual]
  (pull-data-equal? expected actual))

(defn pull-many-literal-equal?
  {:inline
   (fn [expected actual]
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
               '(Datascript_runtime.Data_value.Bool true)
               (= value false)
               '(Datascript_runtime.Data_value.Bool false)
               (vector? value)
               (list
                'Datascript_runtime.Data_value.Vector
                (cons
                 'list
                 (map value-form value)))
               (map? value)
               (list
                'Datascript_runtime.Data_value.Map
                (cons
                 'list
                 (map
                  (fn [entry]
                    (list
                     'tuple
                     (value-form (first entry))
                     (value-form (second entry))))
                  value)))
               (or (symbol? value) (seq? value))
               value
               :else
               (list 'Datascript_runtime.Data_value.Int value)))]
       (list
        'datascript.test.pull-api/pull-many-data-equal?
        (value-form expected)
        actual)))}
  [^:Datascript_runtime.Data_value.t expected
   ^:vector<option<map<Datascript_runtime.Data_value.t;Datascript_runtime.Data_value.t>>>
   actual]
  (pull-many-data-equal? expected actual))

(deftest test-pull-attr-spec
  (is
   (pull-literal-equal?
    {:name "Petr" :aka ["Devil" "Tupen"]}
    (d/pull @*test-db '[:name :aka] 1)))

  (is
   (pull-literal-equal?
    {:name "Matthew" :father {:db/id 3} :db/id 6}
    (d/pull @*test-db '[:name :father :db/id] 6)))

  (is
   (pull-many-literal-equal?
    [{:name "Petr"} {:name "Elizabeth"}
     {:name "Eunan"} {:name "Rebecca"}]
    (d/pull-many @*test-db '[:name] [1 5 7 9]))))

(deftest test-pull-reverse-attr-spec
  (is
   (pull-literal-equal?
    {:name "David" :_child [{:db/id 1}]}
    (d/pull @*test-db '[:name :_child] 2)))

  (is
   (pull-literal-equal?
    {:name "David" :_child [{:name "Petr"}]}
    (d/pull @*test-db '[:name {:_child [:name]}] 2)))

  (testing "Reverse non-component references yield collections"
    (is
     (pull-literal-equal?
      {:name "Thomas" :_father [{:db/id 6}]}
      (d/pull @*test-db '[:name :_father] 3)))

    (is
     (pull-literal-equal?
      {:name "Petr" :_father [{:db/id 2} {:db/id 3}]}
      (d/pull @*test-db '[:name :_father] 1)))

    (is
     (pull-literal-equal?
      {:name "Thomas" :_father [{:name "Matthew"}]}
      (d/pull @*test-db '[:name {:_father [:name]}] 3)))

    (is
     (pull-literal-equal?
      {:name "Petr" :_father [{:name "David"} {:name "Thomas"}]}
      (d/pull @*test-db '[:name {:_father [:name]}] 1))))

  (testing "Multiple reverse refs issue-412"
    (is
     (pull-literal-equal?
      {:name "Petr" :_father [{:db/id 2} {:db/id 3}]}
      (d/pull @*test-db '[:name :_father :_child] 1)))))

(deftest test-pull-component-attr
  (let [recdb (d/init-db
                (concat test-datoms [(d/datom 12 :part 10)])
                test-schema)]
    
    (testing "Component entities are expanded recursively"
      (is
       (pull-literal-equal?
        {:name "Part A"
         :part
         [{:db/id 11
           :name "Part A.A"
           :part
           [{:db/id 12
             :name "Part A.A.A"
             :part
             [{:db/id 13 :name "Part A.A.A.A"}
              {:db/id 14 :name "Part A.A.A.B"}]}]}
          {:db/id 15
           :name "Part A.B"
           :part
           [{:db/id 16
             :name "Part A.B.A"
             :part
             [{:db/id 17 :name "Part A.B.A.A"}
              {:db/id 18 :name "Part A.B.A.B"}]}]}]}
        (d/pull @*test-db '[:name :part] 10))))

    (testing "Reverse component references yield a single result"
      (is
       (pull-literal-equal?
        {:name "Part A.A" :_part {:db/id 10}}
        (d/pull @*test-db [:name :_part] 11)))

      (is
       (pull-literal-equal?
        {:name "Part A.A" :_part {:name "Part A"}}
        (d/pull @*test-db [:name {:_part [:name]}] 11))))

    (testing "Like explicit recursion, expansion will not allow loops"
      (is
       (pull-literal-equal?
        {:name "Part A"
         :part
         [{:db/id 11
           :name "Part A.A"
           :part
           [{:db/id 12
             :name "Part A.A.A"
             :part
             [{:db/id 10
               :name "Part A"
               :part
               [{:db/id 11}
                {:name "Part A.B"
                 :part
                 [{:name "Part A.B.A"
                   :part
                   [{:name "Part A.B.A.A" :db/id 17}
                    {:name "Part A.B.A.B" :db/id 18}]
                   :db/id 16}]
                 :db/id 15}]}
              {:db/id 13 :name "Part A.A.A.A"}
              {:db/id 14 :name "Part A.A.A.B"}]}]}
          {:db/id 15
           :name "Part A.B"
           :part
           [{:db/id 16
             :name "Part A.B.A"
             :part
             [{:db/id 17 :name "Part A.B.A.A"}
              {:db/id 18 :name "Part A.B.A.B"}]}]}]}
        (d/pull recdb '[:name :part] 10))))

    (testing "Reverse recursive component issue-411"
      (is
       (pull-literal-equal?
        {:name "Part A.A.A.B"
         :_part
         {:name "Part A.A.A"
          :_part {:name "Part A.A" :_part {:name "Part A"}}}}
        (d/pull @*test-db '[:name {:_part ...}] 14)))
      (is
       (pull-literal-equal?
        {:name "Part A.A.A.B"
         :_part {:name "Part A.A.A" :_part {:name "Part A.A"}}}
        (d/pull @*test-db '[:name {:_part 2}] 14))))))

(deftest test-pull-wildcard
  (is
   (pull-literal-equal?
    {:db/id 1
     :name "Petr"
     :aka ["Devil" "Tupen"]
     :child [{:db/id 2} {:db/id 3}]}
    (d/pull @*test-db '[*] 1)))

  (is
   (pull-literal-equal?
    {:db/id 2
     :name "David"
     :_child [{:db/id 1}]
     :father {:db/id 1}}
    (d/pull @*test-db '[* :_child] 2)))

  (is
   (pull-literal-equal?
    {:aka ["Devil" "Tupen"]
     :child [{:db/id 2} {:db/id 3}]
     :name "Petr"
     :db/id 1}
    (d/pull @*test-db '[:name *] 1)))

  (is
   (pull-literal-equal?
    {:aka ["Devil" "Tupen"]
     :child [{:db/id 2} {:db/id 3}]
     :name "Petr"
     :db/id 1}
    (d/pull @*test-db '[:aka :name *] 1)))

  (is
   (pull-literal-equal?
    {:aka ["Devil" "Tupen"]
     :child [{:db/id 2} {:db/id 3}]
     :name "Petr"
     :db/id 1}
    (d/pull @*test-db '[:aka :child :name *] 1)))

  (is
   (pull-literal-equal?
    {:alias ["Devil" "Tupen"]
     :child [{:db/id 2} {:db/id 3}]
     :first-name "Petr"
     :db/id 1}
    (d/pull
     @*test-db
     '[[:aka :as :alias] [:name :as :first-name] *]
     1)))

  (is
   (pull-literal-equal?
    {:db/id 1
     :name "Petr"
     :aka ["Devil" "Tupen"]
     :child
     [{:db/id 2
       :father {:db/id 1}
       :name "David"}
      {:db/id 3
       :father {:db/id 1}
       :name "Thomas"}]}
    (d/pull @*test-db '[* {:child ...}] 1))))

(deftest test-pull-limit
  (let [db (d/init-db
             (concat
               test-datoms
               [(d/datom 4 :friend 5)
                (d/datom 4 :friend 6)
                (d/datom 4 :friend 7)
                (d/datom 4 :friend 8)]
               (for [idx (range 2000)]
                 (d/datom
                  8
                  :aka
                  (Datascript_runtime.Data_value.String
                   (str "aka-" idx)))))
             test-schema)]

    (testing "Without an explicit limit, the default is 1000"
      (is (= 1000 (pull-vector-count (d/pull db '[:aka] 8) :aka))))

    (testing "Explicit limit can reduce the default"
      (is (= 500 (pull-vector-count
                  (d/pull db '[(limit :aka 500)] 8)
                  :aka)))
      (is (= 500 (pull-vector-count
                  (d/pull db '[[:aka :limit 500]] 8)
                  :aka))))

    (testing "Explicit limit can increase the default"
      (is (= 1500 (pull-vector-count
                   (d/pull db '[(limit :aka 1500)] 8)
                   :aka))))

    (testing "A nil limit produces unlimited results"
      (is (= 2000 (pull-vector-count
                   (d/pull db '[(limit :aka nil)] 8)
                   :aka))))

    (testing "Limits can be used as map specification keys"
      (is
       (pull-literal-equal?
        {:name "Lucy"
         :friend [{:name "Elizabeth"} {:name "Matthew"}]}
        (d/pull
         db
         '[:name {(limit :friend 2) [:name]}]
         4))))))

(deftest test-pull-default
  (testing "Empty results return nil"
    (is (nil? (d/pull @*test-db '[:foo] 1))))

  (testing "A default can be used to replace nil results"
    (is
     (pull-literal-equal?
      {:foo "bar"}
      (d/pull @*test-db '[(default :foo "bar")] 1)))
    (is
     (pull-literal-equal?
      {:foo "bar"}
      (d/pull @*test-db '[[:foo :default "bar"]] 1)))
    (is
     (pull-literal-equal?
      {:foo false}
      (d/pull @*test-db '[[:foo :default false]] 1)))
    (is
     (pull-literal-equal?
      {:bar false}
      (d/pull
       @*test-db
       '[[:foo :as :bar :default false]]
       1))))

  (testing "default does not override results"
    (is
     (pull-literal-equal?
      {:name "Petr"
       :aka ["Devil" "Tupen"]
       :child
       [{:name "David" :aka "[aka]" :child "[child]"}
        {:name "Thomas" :aka "[aka]" :child "[child]"}]}
      (d/pull @*test-db
        '[[:name :default "[name]"]
          [:aka :default "[aka]"]
          {[:child :default "[child]"] ...}]
        1)))
    (is
     (pull-literal-equal?
      {:name "David" :aka "[aka]" :child "[child]"}
      (d/pull @*test-db
        '[[:name :default "[name]"]
          [:aka :default "[aka]"]
          {[:child :default "[child]"] ...}]
        2))))

  (testing "Ref default"
    (is
     (pull-literal-equal?
      {:child 1 :db/id 2}
      (d/pull @*test-db '[:db/id [:child :default 1]] 2)))
    (is
     (pull-literal-equal?
      {:_child 2 :db/id 1}
      (d/pull
       @*test-db
       '[:db/id [:_child :default 2]]
       1)))))

(deftest test-pull-as
  (is
   (pull-literal-equal?
    {"Name" "Petr" :alias ["Devil" "Tupen"]}
    (d/pull @*test-db '[[:name :as "Name"] [:aka :as :alias]] 1))))

(deftest test-pull-attr-with-opts
  (is
   (pull-literal-equal?
    {"Name" "Nothing"}
    (d/pull @*test-db '[[:x :as "Name" :default "Nothing"]] 1))))

(deftest test-pull-map
  (testing "Single attrs yield a map"
    (is
     (pull-literal-equal?
      {:name "Matthew" :father {:name "Thomas"}}
      (d/pull @*test-db '[:name {:father [:name]}] 6))))

  (testing "Multi attrs yield a collection of maps"
    (is
     (pull-literal-equal?
      {:name "Petr"
       :child [{:name "David"} {:name "Thomas"}]}
      (d/pull @*test-db '[:name {:child [:name]}] 1))))

  (testing "pull-many preserves every entry in a map spec"
    (is
     (pull-many-literal-equal?
      [{:child [{:name "David"} {:name "Thomas"}]}
       {:father {:name "Petr"}}]
      (d/pull-many
       @*test-db
       '[{:child [:name] :father [:name]}]
       [1 2]))))

  (testing "Missing attrs are dropped"
    (is
     (pull-literal-equal?
      {:name "Petr"}
      (d/pull @*test-db '[:name {:father [:name]}] 1))))

  (testing "Non matching results are removed from collections"
    (is
     (pull-literal-equal?
      {:name "Petr"}
      (d/pull @*test-db '[:name {:child [:foo]}] 1))))

  (testing "Map specs can override component expansion"
    (is
     (pull-literal-equal?
      {:name "Part A"
       :part [{:name "Part A.A"} {:name "Part A.B"}]}
      (d/pull @*test-db '[:name {:part [:name]}] 10)))

    (is
     (pull-literal-equal?
      {:name "Part A"
       :part [{:name "Part A.A"} {:name "Part A.B"}]}
      (d/pull @*test-db '[:name {:part 1}] 10)))))

(deftest test-pull-recursion
  (let [db      (-> @*test-db
                  (d/db-with [[:db/add 4 :friend 5]
                              [:db/add 5 :friend 6]
                              [:db/add 6 :friend 7]
                              [:db/add 7 :friend 8]
                              [:db/add 4 :enemy 6]
                              [:db/add 5 :enemy 7]
                              [:db/add 6 :enemy 8]
                              [:db/add 7 :enemy 4]]))
        friends (data-literal
                 {:db/id 4
                 :name "Lucy"
                 :friend
                 [{:db/id 5
                   :name "Elizabeth"
                   :friend
                   [{:db/id 6
                     :name "Matthew"
                     :friend
                     [{:db/id 7
                       :name "Eunan"
                       :friend
                       [{:db/id 8
                         :name "Kerri"}]}]}]}]})
        enemies (data-literal
                 {:db/id 4
                 :name "Lucy"
                 :friend
                 [{:db/id 5
                   :name "Elizabeth"
                   :friend
                   [{:db/id 6
                     :name "Matthew"
                     :enemy [{:db/id 8 :name "Kerri"}]}]
                   :enemy
                   [{:db/id 7
                     :name "Eunan"
                     :friend
                     [{:db/id 8
                       :name "Kerri"}]
                     :enemy
                     [{:db/id 4
                       :name "Lucy"
                       :friend [{:db/id 5}]}]}]}]
                 :enemy
                 [{:db/id 6
                   :name "Matthew"
                   :friend
                   [{:db/id 7
                     :name "Eunan"
                     :friend
                     [{:db/id 8
                       :name "Kerri"}]
                     :enemy [{:db/id 4
                              :name "Lucy"
                              :enemy [{:db/id 6}]
                              :friend [{:db/id 5
                                        :name "Elizabeth"
                                        :enemy [{:db/id 7}],
                                        :friend [{:db/id 6}]}]}]}]
                   :enemy
                   [{:db/id 8
                     :name "Kerri"}]}]})]

    (testing "Infinite recursion"
      (is
       (pull-data-equal?
        friends
        (d/pull db '[:db/id :name {:friend ...}] 4))))

    (testing "Multiple recursion specs in one pattern"
      (is
       (pull-data-equal?
        enemies
        (d/pull
         db
         '[:db/id :name {:friend 2 :enemy 2}]
         4))))

    (testing "Reverse recursion"
      (is
       (pull-literal-equal?
        {:db/id 8
         :_friend
         [{:db/id 7
           :_friend
           [{:db/id 6
             :_friend
             [{:db/id 5 :_friend [{:db/id 4}]}]}]}]}
        (d/pull db '[:db/id {:_friend ...}] 8)))
      (is
       (pull-literal-equal?
        {:db/id 8
         :_friend
         [{:db/id 7 :_friend [{:db/id 6}]}]}
        (d/pull db '[:db/id {:_friend 2}] 8))))

    (testing "Cycles are handled by returning only the :db/id of entities which have been seen before"
      (let [db (d/db-with db [[:db/add 8 :friend 4]])]
        (is
         (pull-literal-equal?
          {:db/id 4
           :name "Lucy"
           :friend
           [{:db/id 5
             :name "Elizabeth"
             :friend
             [{:db/id 6
               :name "Matthew"
               :friend
               [{:db/id 7
                 :name "Eunan"
                 :friend
                 [{:db/id 8
                   :name "Kerri"
                   :friend
                   [{:db/id 4
                     :name "Lucy"
                     :friend [{:db/id 5}]}]}]}]}]}]}
          (d/pull db '[:db/id :name {:friend ...}] 4)))))

    (testing "Seen ids are tracked independently for different branches"
      (let [db (-> (d/empty-db {:friend {:db/valueType :db.type/ref}
                                :enemy  {:db/valueType :db.type/ref}})
                 (d/db-with [{:db/id 1 :name "1" :friend 2 :enemy 2}
                             {:db/id 2 :name "2"}]))]
        (is
         (pull-literal-equal?
          {:name "1"
           :friend {:name "2"}
           :enemy {:name "2"}}
          (d/pull
           db
           '[:name {:friend [:name] :enemy [:name]}]
           1)))))))

(deftest test-dual-recursion
  (let [recursion-db
        (-> (d/empty-db {:friend {:db/valueType :db.type/ref}
                         :enemy {:db/valueType :db.type/ref}})
          (d/db-with [{:db/id 1 :friend 2}
                      {:db/id 2 :enemy 3}
                      {:db/id 3 :friend 4}
                      {:db/id 4 :enemy 5}
                      {:db/id 5 :friend 6}
                      {:db/id 6 :enemy 7}]))]
    (is
     (pull-literal-equal?
      {:db/id 1 :friend {:db/id 2}}
      (d/pull recursion-db '[:db/id {:friend ...}] 1)))
    (is
     (pull-literal-equal?
      {:db/id 1 :friend {:db/id 2 :enemy {:db/id 3}}}
      (d/pull
       recursion-db
       '[:db/id {:friend 1 :enemy 1}]
       1)))
    (is
     (pull-literal-equal?
      {:db/id 1
       :friend
       {:db/id 2 :enemy {:db/id 3 :friend {:db/id 4}}}}
      (d/pull
       recursion-db
       '[:db/id {:friend 2 :enemy 1}]
       1)))
    (is
     (pull-literal-equal?
      {:db/id 1
       :friend
       {:db/id 2
        :enemy
        {:db/id 3
         :friend {:db/id 4 :enemy {:db/id 5}}}}}
      (d/pull
       recursion-db
       '[:db/id {:friend 2 :enemy 2}]
       1))))

  (let [empty (d/empty-db {:part {:db/valueType :db.type/ref}
                           :spec {:db/valueType :db.type/ref}})]
    (let [db (d/db-with empty [[:db/add 1 :part 2]
                               [:db/add 2 :part 3]
                               [:db/add 3 :part 1]
                               [:db/add 1 :spec 2]
                               [:db/add 2 :spec 1]])]
      (is
       (pull-literal-equal?
        {:db/id 1
         :spec
         {:db/id 2
          :spec
          {:db/id 1
           :spec {:db/id 2}
           :part {:db/id 2}}
          :part
          {:db/id 3
           :part
           {:db/id 1
            :spec {:db/id 2}
            :part {:db/id 2}}}}
         :part
         {:db/id 2
          :spec
          {:db/id 1
           :spec {:db/id 2}
           :part {:db/id 2}}
          :part
          {:db/id 3
           :part
           {:db/id 1
            :spec {:db/id 2}
            :part {:db/id 2}}}}}
        (d/pull
         db
         '[:db/id {:part ...} {:spec ...}]
         1))))))

(deftest test-deep-recursion
  (let [start 100
        depth 3000
        txd   (mapcat
                (fn [idx]
                  [(d/datom
                    idx
                    :name
                    (Datascript_runtime.Data_value.String
                     (str "Person-" idx)))
                   (d/datom
                    (dec idx)
                    :friend
                    (Datascript_runtime.Data_value.Int idx))])
                (range (inc start) depth))
        db    (d/init-db (concat
                           test-datoms
                           [(d/datom
                             start
                             :name
                             (Datascript_runtime.Data_value.String
                              (str "Person-" start)))]
                           txd)
                test-schema)
        pulled (d/pull db '[:name {:friend ...}] start)]
    (is
     (=
      (Some (str "Person-" (dec depth)))
      (deep-pulled-name
       pulled
       (dec (- depth start)))))))

; issue-430
(deftest test-component-reverse
  (let [schema
        (zipmap
         [:ref]
         [(schema-entry
           [:db/valueType :db/isComponent]
           [(Datascript_runtime.Data_value.Keyword
             ":db.type/ref")
            (Datascript_runtime.Data_value.Bool true)])])
        db (d/db-with (d/empty-db schema)
             [{:name "1"
               :ref {:name "2"
                     :ref {:name "3"}}}])]
    (is
     (pull-literal-equal?
      {:name "1"
       :ref {:name "2"
             :ref {:name "3" :_ref {:name "2"}}}}
      (d/pull
       db
       [:name
        {:ref
         [:name
          {:ref [:name {:_ref [:name]}]}]}]
       1)))))

(deftest test-lookup-ref-pull
  (is
   (pull-literal-equal?
    {:name "Petr" :aka ["Devil" "Tupen"]}
    (d/pull @*test-db '[:name :aka] [:name "Petr"])))
  (is (= nil
        (d/pull @*test-db '[:name :aka] [:name "NotInDatabase"])))
  (is
   (pull-many-literal-equal?
    [nil {:aka ["Devil" "Tupen"]} nil nil nil]
    (d/pull-many @*test-db
      '[:aka]
      [[:name "Elizabeth"]
       [:name "Petr"]
       [:name "Eunan"]
       [:name "Rebecca"]
       [:name "Unknown"]])))
  (is (nil? (d/pull @*test-db '[*] [:name "No such name"]))))

(deftest test-xform
  (is
   (pull-literal-equal?
    {:db/id [1]
     :name ["Petr"]
     :aka [["Devil" "Tupen"]]
     :child
     [[{:db/id [2]
        :name ["David"]
        :aka [nil]
        :child [nil]}
       {:db/id [3]
        :name ["Thomas"]
        :aka [nil]
        :child [nil]}]]}
    (d/pull @*test-db
      [[:db/id :xform wrap-pull-value]
       [:name :xform wrap-pull-value]
       [:aka :xform wrap-pull-value]
       {[:child :xform wrap-pull-value] '...}]
      1)))
  
  (testing ":xform on cardinality/one ref issue-455"
    (is
     (pull-literal-equal?
      {:name "David" :father "Petr"}
      (d/pull
       @*test-db
       [:name
        {[:father :xform pull-name-value] ['*]}]
       2))))
  
  (testing ":xform on reverse ref"
    (is
     (pull-literal-equal?
      {:name "Petr" :_father ["David" "Thomas"]}
      (d/pull
       @*test-db
       [:name
        {[:_father :xform pull-many-name-values]
         [:name]}]
       1))))

  (testing ":xform on reverse component ref"
    (is
     (pull-literal-equal?
      {:name "Part A.A" :_part "Part A"}
      (d/pull
       @*test-db
       [:name
        {[:_part :xform pull-name-value] [:name]}]
       11))))
  
  (testing "missing attrs are processed by xform"
    (is
     (pull-literal-equal?
      {:normal [nil]
       :aka [nil]
       :child [nil]}
      (d/pull @*test-db
        '[[:normal :xform wrap-pull-value]
          [:aka :xform wrap-pull-value]
          {[:child :xform wrap-pull-value] ...}]
        2))))
  (testing "default takes precedence"
    (is
     (pull-literal-equal?
      {:unknown "[unknown]"}
      (d/pull
       @*test-db
       '[[:unknown
          :default "[unknown]"
          :xform wrap-pull-value]]
       1)))))

(type-record PullTrace
  (kind :keyword)
  (entity :option<int>)
  (attr :option<keyword>)
  (value :option<int>))

(defn ^PullTrace pull-trace
  [^:keyword kind
   ^:option<int> entity
   ^:option<keyword> attr
   ^:option<int> value]
  (record PullTrace
    (kind kind)
    (entity entity)
    (attr attr)
    (value value)))

(defn ^:vector<PullTrace> append-pull-trace
  [^:vector<PullTrace> events ^PullTrace event]
  (conj events event))

(defn ^:vector<PullTrace> empty-pull-traces []
  [])

(type-record PullTraceLog
  (events :ref<vector<PullTrace>>))

(defn ^PullTraceLog new-pull-trace-log []
  (record PullTraceLog
    (events (volatile! (empty-pull-traces)))))

(defn append-pull-trace!
  [^PullTraceLog log ^PullTrace event]
  :unit
  (let [events (:events log)]
    (vswap!
     events
     append-pull-trace
     event))
  (Stdlib.ignore 0))

(defn clear-pull-traces! [^PullTraceLog log]
  :unit
  (let [events (:events log)]
    (vreset! events (empty-pull-traces)))
  (Stdlib.ignore 0))

(defn ^:vector<PullTrace> read-pull-traces
  [^PullTraceLog log]
  (let [events (:events log)]
    @events))

(deftest test-visitor
  (let [trace-log
        (new-pull-trace-log)
        opts
        (pull-api/pull-options
         (fn [k e a v]
           (append-pull-trace!
            trace-log
            (pull-trace k e a v))))
        test-fn
        (fn [^:vector<datascript.pull-parser/pull-source-item>
             pattern
             ^int id]
          (clear-pull-traces! trace-log)
          (pull-api/pull-source-with-options
           (db/database-view @*test-db)
           pattern
           (Datascript_runtime.Data_value.Entity_id id)
           opts)
          (read-pull-traces trace-log))]
    (is
     (=
      [(pull-trace
        :db.pull/attr (Some 1) (Some :name) None)]
      (test-fn [(dpp/source-attribute :name)] 1)))
    
    (testing "multival"
      (is
       (=
        [(pull-trace
          :db.pull/attr (Some 1) (Some :aka) None)
         (pull-trace
          :db.pull/attr (Some 1) (Some :name) None)]
        (test-fn
         [(dpp/source-attribute :name)
          (dpp/source-attribute :aka)]
         1))))
    
    (testing ":db/id is ignored"
      (is
       (=
        (empty-pull-traces)
        (test-fn [(dpp/source-attribute :db/id)] 1)))
      (is
       (=
        [(pull-trace
          :db.pull/attr (Some 1) (Some :name) None)]
        (test-fn
         [(dpp/source-attribute :db/id)
          (dpp/source-attribute :name)]
         1))))

    (testing "wildcard"
      (is
       (=
        [(pull-trace
          :db.pull/wildcard (Some 1) None None)
         (pull-trace
          :db.pull/attr (Some 1) (Some :aka) None)
         (pull-trace
          :db.pull/attr (Some 1) (Some :child) None)
         (pull-trace
          :db.pull/attr (Some 1) (Some :name) None)]
        (test-fn [dpp/source-wildcard] 1))))

    (testing "missing"
      (is
       (=
        [(pull-trace
          :db.pull/attr (Some 1) (Some :missing) None)]
        (test-fn [(dpp/source-attribute :missing)] 1)))
      (is
       (=
        [(pull-trace
          :db.pull/wildcard (Some 1) None None)
         (pull-trace
          :db.pull/attr (Some 1) (Some :aka) None)
         (pull-trace
          :db.pull/attr (Some 1) (Some :child) None)
         (pull-trace
          :db.pull/attr (Some 1) (Some :missing) None)
         (pull-trace
          :db.pull/attr (Some 1) (Some :name) None)]
        (test-fn
         [dpp/source-wildcard
          (dpp/source-attribute :missing)]
         1))))

    (testing "default"
      (is
       (=
        [(pull-trace
          :db.pull/attr (Some 1) (Some :missing) None)]
        (test-fn
         [(dpp/source-default
           :missing
           (Datascript_runtime.Data_value.Int 10))]
         1)))
      (is
       (=
        [(pull-trace
          :db.pull/attr (Some 2) (Some :child) None)]
        (test-fn
         [(dpp/source-default
           :child
           (Datascript_runtime.Data_value.Int 10))]
         2))))

    (testing "recursion"
      (is
       (=
        [(pull-trace
          :db.pull/attr (Some 1) (Some :child) None)]
        (test-fn [(dpp/source-attribute :child)] 1)))
      (is
       (=
        [(pull-trace
          :db.pull/attr (Some 1) (Some :child) None)
         (pull-trace
          :db.pull/attr (Some 2) (Some :name) None)
         (pull-trace
          :db.pull/attr (Some 3) (Some :name) None)]
        (test-fn
         [(dpp/source-nested
           :child
           [(dpp/source-attribute :name)])]
         1)))
      (is
       (=
        [(pull-trace
          :db.pull/attr (Some 1) (Some :child) None)
         (pull-trace
          :db.pull/attr (Some 2) (Some :child) None)
         (pull-trace
          :db.pull/attr (Some 2) (Some :name) None)
         (pull-trace
          :db.pull/attr (Some 3) (Some :child) None)
         (pull-trace
          :db.pull/attr (Some 3) (Some :name) None)
         (pull-trace
          :db.pull/attr (Some 1) (Some :name) None)]
        (test-fn
         [(dpp/source-attribute :name)
          (dpp/source-recursion :child None)]
         1))))

    (testing "reverse"
      (is
       (=
        [(pull-trace
          :db.pull/attr (Some 2) (Some :name) None)
         (pull-trace
          :db.pull/reverse None (Some :child) (Some 2))]
        (test-fn
         [(dpp/source-attribute :name)
          (dpp/source-attribute :_child)]
         2))))))

(deftest test-pull-other-dbs
  (let [db (-> @*test-db
             (d/filter
              (fn [^datascript.db/DB _database
                   ^datascript.db/Datom datom]
                (not
                 (Datascript_runtime.Data_value.equal
                  (Datascript_runtime.Data_value.String "Tupen")
                  (.-v datom))))))]
    (is
     (pull-literal-equal?
      {:name "Petr" :aka ["Devil"]}
      (d/pull db '[:name :aka] 1))))
  (let [db (-> @*test-db d/serializable pr-str clojure.edn/read-string d/from-serializable)]
    (is
     (pull-literal-equal?
      {:name "Petr" :aka ["Devil" "Tupen"]}
      (d/pull db '[:name :aka] 1))))
  (let [db (d/init-db (d/datoms @*test-db :eavt) test-schema)]
    (is
     (pull-literal-equal?
      {:name "Petr" :aka ["Devil" "Tupen"]}
      (d/pull db '[:name :aka] 1)))))
