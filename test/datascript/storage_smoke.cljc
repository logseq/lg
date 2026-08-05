(ns datascript.test.storage-smoke
  (:require
   [datascript.db :as db]
   [datascript.storage :as storage]
   [datascript.conn :as conn]
   [datascript.lg.query :as static-query]
   [datascript.lg.query-types :as query-types]
   [datascript.parser :as parser]
   [me.tonsky.persistent-sorted-set :as set]))

(def environment #?(:native "native" :melange "melange"))

(type-alias smoke-query-result
  :Datascript_runtime.Query_value.result<string>)
(type-alias smoke-query-source
  :Datascript_runtime.Query_value.source<string>)
(type-alias smoke-query-relation
  :Datascript_runtime.Query_value.relation<string>)
(type-alias smoke-query-context
  :Datascript_runtime.Query_value.context<string;vector<string>>)

(signature datascript.test.storage-smoke/query-entity
  :fn<int;smoke-query-result>)
(signature datascript.test.storage-smoke/query-attr
  :fn<keyword;smoke-query-result>)
(signature datascript.test.storage-smoke/query-value
  :fn<Datascript_runtime.Data_value.t;smoke-query-result>)
(signature datascript.test.storage-smoke/query-row-get
  :fn<array<smoke-query-result>;int;option<smoke-query-result>>)
(signature datascript.test.storage-smoke/query-join-rows
  :fn<array<smoke-query-result>;array<int>;array<smoke-query-result>;array<int>;array<smoke-query-result>>)
(signature datascript.test.storage-smoke/relation-query-source
  :fn<vector<array<smoke-query-result>>;smoke-query-source>)
(signature datascript.test.storage-smoke/database-query-source
  :fn<string;smoke-query-source>)
(signature datascript.test.storage-smoke/make-query-relation
  :fn<map<string;int>;vector<array<smoke-query-result>>;map<string;string>;smoke-query-relation>)
(signature datascript.test.storage-smoke/query-relation-result
  :fn<smoke-query-relation;string;array<smoke-query-result>;option<smoke-query-result>>)
(signature datascript.test.storage-smoke/query-relation-lookup-database
  :fn<smoke-query-relation;string;option<string>>)
(signature datascript.test.storage-smoke/make-query-context
  :fn<vector<smoke-query-relation>;map<string;smoke-query-source>;vector<string>;smoke-query-context>)

(defn ^smoke-query-result query-entity [^:int entity]
  (Datascript_runtime.Query_value.entity entity))

(defn ^smoke-query-result query-attr [^:keyword attr]
  (Datascript_runtime.Query_value.attr (str attr)))

(defn ^smoke-query-result query-value
  [^:Datascript_runtime.Data_value.t value]
  (Datascript_runtime.Query_value.value value))

(defn ^:array<smoke-query-result> closed-query-row []
  (array
   (query-entity 42)
   (query-attr :user/name)
   (query-value (Datascript_runtime.Data_value.String "Ada"))))

(defn ^:option<smoke-query-result> query-row-get
  [^:array<smoke-query-result> row ^:int index]
  (Datascript_runtime.Query_value.row_get row index))

(defn ^:array<smoke-query-result> query-join-rows
  [^:array<smoke-query-result> left
   ^:array<int> left-indexes
   ^:array<smoke-query-result> right
   ^:array<int> right-indexes]
  (Datascript_runtime.Query_value.join_rows
   left left-indexes right right-indexes))

(defn ^smoke-query-source relation-query-source
  [^:vector<array<smoke-query-result>> rows]
  (Datascript_runtime.Query_value.relation_source rows))

(defn ^smoke-query-source database-query-source [^:string database]
  (Datascript_runtime.Query_value.database_source database))

(defn ^smoke-query-relation make-query-relation
  [^:map<string;int> attrs
   ^:vector<array<smoke-query-result>> rows
   ^:map<string;string> lookup-databases]
  (Datascript_runtime.Query_value.relation
   attrs rows lookup-databases))

(defn ^:option<smoke-query-result> query-relation-result
  [^smoke-query-relation relation
   ^:string variable
   ^:array<smoke-query-result> row]
  (Datascript_runtime.Query_value.relation_result
   relation variable row))

(defn ^:option<string> query-relation-lookup-database
  [^smoke-query-relation relation ^:string variable]
  (Datascript_runtime.Query_value.relation_lookup_database
   relation variable))

(defn ^smoke-query-context make-query-context
  [^:vector<smoke-query-relation> relations
   ^:map<string;smoke-query-source> sources
   ^:vector<string> rules]
  (Datascript_runtime.Query_value.context relations sources rules))

(defn ^:map<string;datascript.db/database-view> empty-query-databases []
  {})

(defn ^datascript.lg.query-types/source static-database-source
  [^datascript.db/DB database]
  (query-types/database-source
   (db/database-view database)))

(def closed-relation-source
  (relation-query-source [(closed-query-row)]))

(def closed-query-relation
  (make-query-relation
   {"?e" 0 "?name" 1}
   [(closed-query-row)]
   {"?e" "database"}))

(def closed-query-context
  (make-query-context
   [closed-query-relation]
   {"$" (database-query-source "database")}
   ["rule"]))

(def closed-datom-relation
  (query-types/datom-relation
   {"?e" 0 "?v" 1}
   (array 0 2)
   [(db/datom
     42 :user/name
     (Datascript_runtime.Data_value.String "Ada")
     7)]
   (empty-query-databases)))

(def closed-pattern-relation
  (query-types/pattern-relation
   ["?e" "?v"]
   (array 0 2)
   [(db/datom
     42 :user/name
     (Datascript_runtime.Data_value.String "Ada")
     7)]
   (empty-query-databases)))

(def closed-query-binding
  (query-types/collection-binding
   [(query-types/scalar-binding
     (query-types/entity-result 42))
    (query-types/scalar-binding
     (query-types/value-result
      (Datascript_runtime.Data_value.String "Ada")))]))

(def closed-binding-input
  (query-types/binding-input closed-query-binding))

(def closed-rules-input
  (query-types/rules-input []))

(defn ^datascript.parser/pattern-element parse-pattern-element!
  [^:Datascript_runtime.Data_value.t value]
  (if-some [element (parser/parse-pattern-el value)]
    element
    (Stdlib.invalid_arg "Invalid static query pattern element")))

(def static-query-database
  (db/init-db
   (to-array
    [(db/datom
      1 :name (Datascript_runtime.Data_value.String "Ivan"))
     (db/datom
      2 :name (Datascript_runtime.Data_value.String "Oleg"))])
   {}))

(def static-query-view
  (db/database-view static-query-database))

(def static-name-pattern
  [(parse-pattern-element!
    (Datascript_runtime.Data_value.Symbol "?e"))
   (parse-pattern-element!
    (Datascript_runtime.Data_value.Keyword ":name"))
   (parse-pattern-element!
    (Datascript_runtime.Data_value.Symbol "?name"))])

(def static-ivan-pattern
  [(parse-pattern-element!
    (Datascript_runtime.Data_value.Symbol "?e"))
   (parse-pattern-element!
    (Datascript_runtime.Data_value.Keyword ":name"))
   (parse-pattern-element!
    (Datascript_runtime.Data_value.String "Ivan"))])

(def static-missing-pattern
  [(parse-pattern-element!
    (Datascript_runtime.Data_value.Symbol "?e"))
   (parse-pattern-element!
    (Datascript_runtime.Data_value.Keyword ":name"))
   (parse-pattern-element!
    (Datascript_runtime.Data_value.String "Missing"))])

(def static-all-components-pattern
  [(parse-pattern-element!
    (Datascript_runtime.Data_value.Symbol "?e"))
   (parse-pattern-element!
    (Datascript_runtime.Data_value.Symbol "?a"))
   (parse-pattern-element!
    (Datascript_runtime.Data_value.Symbol "?v"))
   (parse-pattern-element!
    (Datascript_runtime.Data_value.Symbol "?tx"))
   (parse-pattern-element!
    (Datascript_runtime.Data_value.Symbol "?added"))])

(def static-retracted-pattern
  [(parse-pattern-element!
    (Datascript_runtime.Data_value.Symbol "?e"))
   (parse-pattern-element!
    (Datascript_runtime.Data_value.Keyword ":name"))
   (parse-pattern-element!
    (Datascript_runtime.Data_value.Symbol "?name"))
   (parse-pattern-element!
    (Datascript_runtime.Data_value.Symbol "?tx"))
   (parse-pattern-element!
    (Datascript_runtime.Data_value.Keyword ":db/retract"))])

(def static-name-relation
  (query-types/lookup-db-pattern
   static-query-view
   static-name-pattern))

(def static-ivan-relation
  (query-types/lookup-db-pattern
   static-query-view
   static-ivan-pattern))

(def static-missing-relation
  (query-types/lookup-db-pattern
   static-query-view
   static-missing-pattern))

(def static-all-components-relation
  (query-types/lookup-db-pattern
   static-query-view
   static-all-components-pattern))

(def static-retracted-relation
  (query-types/lookup-db-pattern
   static-query-view
   static-retracted-pattern))

(def static-age-pattern
  [(parse-pattern-element!
    (Datascript_runtime.Data_value.Symbol "?e"))
   (parse-pattern-element!
    (Datascript_runtime.Data_value.Keyword ":age"))
   (parse-pattern-element!
    (Datascript_runtime.Data_value.Symbol "?age"))])

(def static-join-database
  (db/init-db
   (to-array
    [(db/datom
      1 :name (Datascript_runtime.Data_value.String "Ivan"))
     (db/datom
      1 :age (Datascript_runtime.Data_value.Int 19))
     (db/datom
      2 :name (Datascript_runtime.Data_value.String "Oleg"))
     (db/datom
      2 :age (Datascript_runtime.Data_value.Int 37))])
   {}))

(def static-join-view
  (db/database-view static-join-database))

(def static-joined-relation
  (query-types/lookup-db-patterns
   static-join-view
   [static-name-pattern static-age-pattern]))

(def static-empty-where-relation
  (query-types/lookup-db-patterns
   static-join-view
   []))

(def static-query-patterns
  [static-name-pattern static-age-pattern])

(def static-relation-query
  (parser/static-query
   (parser/relation-find ["?e" "?name" "?age"])
   static-query-patterns))

(def static-collection-query
  (parser/static-query
   (parser/collection-find "?name")
   static-query-patterns))

(def static-scalar-query
  (parser/static-query
   (parser/single-find "?name")
   static-query-patterns))

(def static-tuple-query
  (parser/static-query
   (parser/tuple-find ["?e" "?name"])
   static-query-patterns))

(def static-relation-output
  (query-types/execute-db-query
   static-join-view
   static-relation-query))

(def static-collection-output
  (query-types/execute-db-query
   static-join-view
   static-collection-query))

(def static-scalar-output
  (query-types/execute-db-query
   static-join-view
   static-scalar-query))

(def static-tuple-output
  (query-types/execute-db-query
   static-join-view
   static-tuple-query))

(def static-db-input-query
  (parser/static-db-query
   (parser/relation-find ["?e" "?name" "?age"])
   static-query-patterns))

(def static-db-input-output
  (query-types/execute-query
   static-db-input-query
   [(query-types/source-input
     (static-database-source
      static-join-database))]))

(def static-bound-query
  (parser/static-db-query-with-scalars
   (parser/relation-find ["?e" "?name"])
   [static-name-pattern]
   ["?name"]))

(def static-bound-output
  (query-types/execute-query
   static-bound-query
   [(query-types/source-input
     (static-database-source
      static-join-database))
    (query-types/binding-input
     (query-types/scalar-binding
      (query-types/value-result
       (Datascript_runtime.Data_value.String "Ivan"))))]))

(def static-tuple-input-query
  (parser/static-db-query-with-bindings
   (parser/relation-find ["?x" "?z"])
   []
   [(parser/tuple-input
     [(parser/scalar-input "?x")
      (parser/ignore-input)
      (parser/scalar-input "?z")])]))

(def static-tuple-input-output
  (query-types/execute-query
   static-tuple-input-query
   [(query-types/source-input
     (static-database-source
      static-query-database))
    (query-types/binding-input
     (query-types/collection-binding
      [(query-types/scalar-binding
        (query-types/entity-result 1))
       (query-types/scalar-binding
        (query-types/value-result
         (Datascript_runtime.Data_value.String "ignored")))
       (query-types/scalar-binding
        (query-types/value-result
         (Datascript_runtime.Data_value.String "kept")))]))]))

(def static-collection-input-query
  (parser/static-db-query-with-bindings
   (parser/relation-find ["?e" "?name"])
   [static-name-pattern]
   [(parser/collection-input
     (parser/scalar-input "?name"))]))

(def static-collection-input-output
  (query-types/execute-query
   static-collection-input-query
   [(query-types/source-input
     (static-database-source
      static-query-database))
    (query-types/binding-input
     (query-types/collection-binding
      [(query-types/scalar-binding
        (query-types/value-result
         (Datascript_runtime.Data_value.String "Ivan")))
       (query-types/scalar-binding
        (query-types/value-result
         (Datascript_runtime.Data_value.String "Oleg")))]))]))

(def static-public-query-output
  (static-query/q
   static-collection-input-query
   [(query-types/source-input
     (static-database-source
      static-query-database))
    (query-types/binding-input
     (query-types/collection-binding
      [(query-types/scalar-binding
        (query-types/value-result
         (Datascript_runtime.Data_value.String "Ivan")))
       (query-types/scalar-binding
        (query-types/value-result
         (Datascript_runtime.Data_value.String "Oleg")))]))]))

(defn static-added-result? []
  (if-some [row
            (first
             (query-types/relation-rows
              static-all-components-relation))]
    (if-some [result
              (query-types/relation-result
               static-all-components-relation
               "?added"
               row)]
      (match result
        (Datascript_runtime.Query_value.Added added) added
        _ false)
      false)
    false))

(defn invalid-pattern-relation-rejected? []
  (try
    (Stdlib.ignore
     (query-types/pattern-relation
      ["?e" "?v"]
      (array 0)
      [(db/datom
        42 :user/name
        (Datascript_runtime.Data_value.String "Ada")
        7)]
      (empty-query-databases)))
    false
    (catch _
      true)))

(defn invalid-db-pattern-rejected? []
  (try
    (Stdlib.ignore
     (query-types/lookup-db-pattern
      static-query-view
      (conj
       static-name-pattern
       (parse-pattern-element!
        (Datascript_runtime.Data_value.Symbol "?tx"))
       (parse-pattern-element!
        (Datascript_runtime.Data_value.Symbol "?added"))
       (parse-pattern-element!
        (Datascript_runtime.Data_value.Symbol "?extra")))))
    false
    (catch _
      true)))

(defn unknown-find-variable-rejected? []
  (try
    (Stdlib.ignore
     (query-types/execute-db-query
      static-join-view
      (parser/static-query
       (parser/relation-find ["?missing"])
       static-query-patterns)))
    false
    (catch _
      true)))

(defn missing-query-input-rejected? []
  (try
    (Stdlib.ignore
     (query-types/execute-query
      static-db-input-query
      []))
    false
    (catch _
      true)))

(defn extra-query-input-rejected? []
  (try
    (Stdlib.ignore
     (query-types/execute-query
      static-db-input-query
      [(query-types/source-input
        (static-database-source
         static-join-database))
       (query-types/source-input
        (static-database-source
         static-join-database))]))
    false
    (catch _
      true)))

(defn wrong-query-input-kind-rejected? []
  (try
    (Stdlib.ignore
     (query-types/execute-query
      static-db-input-query
      [(query-types/rules-input [])]))
    false
    (catch _
      true)))

(defn wrong-scalar-binding-shape-rejected? []
  (try
    (Stdlib.ignore
     (query-types/execute-query
      static-bound-query
      [(query-types/source-input
        (static-database-source
         static-join-database))
       (query-types/binding-input
        (query-types/collection-binding
         [(query-types/scalar-binding
           (query-types/value-result
            (Datascript_runtime.Data_value.String "Ivan")))]))]))
    false
    (catch _
      true)))

(defn wrong-tuple-binding-arity-rejected? []
  (try
    (Stdlib.ignore
     (query-types/execute-query
      static-tuple-input-query
      [(query-types/source-input
        (static-database-source
         static-query-database))
       (query-types/binding-input
        (query-types/collection-binding
         [(query-types/scalar-binding
           (query-types/entity-result 1))
          (query-types/scalar-binding
           (query-types/value-result
            (Datascript_runtime.Data_value.String "short")))]))]))
    false
    (catch _
      true)))

(println
 (str environment ":query-sum:"
      (= 3 (count (closed-query-row))) ":"
      (some? (query-row-get (closed-query-row) 1)) ":"
      (= 2
         (count
          (query-join-rows
           (closed-query-row) (array 0)
           (closed-query-row) (array 2)))) ":"
      (= 5
         (alength
          (query-types/datom-row
           (db/datom
            42 :user/name
            (Datascript_runtime.Data_value.String "Ada")
            7)))) ":"
      (= 2
         (alength
          (query-types/project-row
           (query-types/datom-row
            (db/datom
             42 :user/name
             (Datascript_runtime.Data_value.String "Ada")
             7))
           (array 0 2)))) ":"
      (= 1
         (count
          (Datascript_runtime.Query_value.relation_rows
           (query-types/datom-relation
            {"?e" 0 "?v" 1}
            (array 0 2)
            [(db/datom
              42 :user/name
              (Datascript_runtime.Data_value.String "Ada")
              7)]
            (empty-query-databases))))) ":"
      (= 1
         (count
          (Datascript_runtime.Query_value.relation_rows
           (query-types/identity-relation)))) ":"
      (= 0
         (count
          (Datascript_runtime.Query_value.relation_rows
           (query-types/empty-relation
            {}
            (empty-query-databases))))) ":"
      (= 2
         (count
          (query-types/relation-rows
           (query-types/sum-relation
            (query-types/identity-relation)
            (query-types/identity-relation))))) ":"
      (= 2
         (count
          (query-types/product-attrs
           {"?e" 0}
           {"?v" 0}))) ":"
      (= 1
         (count
          (query-types/relation-rows
           (query-types/product-relation
            (query-types/identity-relation)
            (query-types/identity-relation))))) ":"
      (= 1
         (count
          (query-types/relation-rows
           (query-types/hash-join
            closed-datom-relation
            closed-datom-relation)))) ":"
      (= 2
         (count
          (query-types/relation-attrs
           closed-pattern-relation))) ":"
      (= 1
         (count
          (query-types/relation-rows
           closed-pattern-relation))) ":"
      (invalid-pattern-relation-rejected?) ":"
      (= 2
         (if-some [items
                   (query-types/binding-items
                    closed-query-binding)]
           (count items)
           0)) ":"
      (some?
       (query-types/input-binding
        closed-binding-input)) ":"
      (some?
       (query-types/input-rules
        closed-rules-input)) ":"
      (= 2
         (count
          (query-types/relation-attrs
           static-name-relation))) ":"
      (= 2
         (count
          (query-types/relation-rows
           static-name-relation))) ":"
      (= 1
         (count
          (query-types/relation-rows
           static-ivan-relation))) ":"
      (= 0
         (count
          (query-types/relation-rows
           static-missing-relation))) ":"
      (= 5
         (count
          (query-types/relation-attrs
           static-all-components-relation))) ":"
      (= 2
         (count
          (query-types/relation-rows
           static-all-components-relation))) ":"
      (static-added-result?) ":"
      (= 0
         (count
          (query-types/relation-rows
           static-retracted-relation))) ":"
      (invalid-db-pattern-rejected?) ":"
      (= 3
         (count
          (query-types/relation-attrs
           static-joined-relation))) ":"
      (= 2
         (count
          (query-types/relation-rows
           static-joined-relation))) ":"
      (= 1
         (count
          (query-types/relation-rows
           static-empty-where-relation))) ":"
      (= 2
         (if-some [rows
                   (query-types/output-relation
                    static-relation-output)]
           (count rows)
           0)) ":"
      (= 2
         (if-some [values
                   (query-types/output-collection
                    static-collection-output)]
           (count values)
           0)) ":"
      (some?
       (if-some [value
                 (query-types/output-scalar
                  static-scalar-output)]
         value
         None)) ":"
      (= 2
         (if-some [row
                   (query-types/output-tuple
                    static-tuple-output)]
           (if-some [row row]
             (alength row)
             0)
           0)) ":"
      (unknown-find-variable-rejected?) ":"
      (= 2
         (if-some [rows
                   (query-types/output-relation
                    static-db-input-output)]
           (count rows)
           0)) ":"
      (missing-query-input-rejected?) ":"
      (extra-query-input-rejected?) ":"
      (wrong-query-input-kind-rejected?) ":"
      (= 1
         (if-some [rows
                   (query-types/output-relation
                    static-bound-output)]
           (count rows)
           0)) ":"
      (wrong-scalar-binding-shape-rejected?) ":"
      (= 1
         (if-some [rows
                   (query-types/output-relation
                    static-tuple-input-output)]
           (count rows)
           0)) ":"
      (= 2
         (if-some [rows
                   (query-types/output-relation
                    static-collection-input-output)]
           (count rows)
           0)) ":"
      (= 2
         (if-some [rows
                   (query-types/output-relation
                    static-public-query-output)]
           (count rows)
           0)) ":"
      (wrong-tuple-binding-arity-rejected?) ":"
      (some?
       (query-relation-result
        closed-query-relation "?name" (closed-query-row))) ":"
      (some?
       (query-relation-lookup-database
        closed-query-relation "?e")) ":"
      (some?
       (Datascript_runtime.Query_value.source_rows
        closed-relation-source)) ":"
      (= 1
         (count
          (Datascript_runtime.Query_value.relation_rows
           closed-query-relation))) ":"
      (= 1
         (count
          (Datascript_runtime.Query_value.context_relations
           closed-query-context)))))

(type-record memory-storage
  (backend :Datascript_runtime.Storage_backend.t)
  (disk :ref<map<int;Datascript_runtime.Storage_value.t>>)
  (reads :ref<vector<int>>)
  (writes :ref<vector<int>>)
  (deletes :ref<vector<int>>))

(defn ^:map<int;Datascript_runtime.Storage_value.t> empty-memory-disk []
  {})

(defn ^memory-storage make-memory-storage []
  (let [disk (volatile! (empty-memory-disk))
        reads (volatile! [])
        writes (volatile! [])
        deletes (volatile! [])
        backend
        (storage/make-backend
         (fn [^:vector<tuple<int;Datascript_runtime.Storage_value.t>> entries
              ^:vector<int> delete-addresses]
           (doseq [entry entries]
             (let [address (tuple-get entry 0)
                   data (tuple-get entry 1)]
               (vreset!
                disk
                (Lg_runtime.Lg_map.assoc @disk address data))
               (vswap! writes conj address)))
           (doseq [address delete-addresses]
             (vreset!
              disk
              (Lg_runtime.Lg_map.dissoc @disk address))
             (vswap! deletes conj address))
           (Stdlib.ignore 0))
         (fn [^:int address]
           (vswap! reads conj address)
           (Lg_runtime.Lg_map.get_option @disk address))
         (fn [^:unit _ignored]
           (vec (keys @disk)))
         (fn [^:vector<int> addresses]
           (doseq [address addresses]
             (vreset!
              disk
              (Lg_runtime.Lg_map.dissoc @disk address))
             (vswap! deletes conj address))
           (Stdlib.ignore 0)))]
    (record memory-storage
            (backend backend)
            (disk disk)
            (reads reads)
            (writes writes)
            (deletes deletes))))

(defn ^Datascript_runtime.Storage_backend.t memory-backend
  [^memory-storage storage]
  (.-backend storage))

(defn throws? [f]
  (try
    (f)
    false
    (catch _
      true)))

(defn reset-stats [^memory-storage backend]
  (vreset! (:reads backend) [])
  (vreset! (:writes backend) [])
  (vreset! (:deletes backend) []))

(defn ^datascript.db/tx-entry tx-add-string
  [^:int eid ^:keyword attr ^:string value]
  (db/tx-add
   (Datascript_runtime.Data_value.Entity_id eid)
   attr
   (Datascript_runtime.Data_value.String value)))

(defn has-string?
  [^datascript.db/DB database
   ^:int eid
   ^:keyword attr
   ^:string value]
  (if-some [datom (db/search-ea database eid attr)]
    (Datascript_runtime.Data_value.equal
     (.-v datom)
     (Datascript_runtime.Data_value.String value))
    false))

(defn small-database []
  (conn/db-with
   (db/empty-db
    None
    (db/options-with-ref-type
     (db/default-options)
     (Lg_runtime.Runtime_ref_type.Strong)))
   [(tx-add-string 1 :name "Ivan")
    (tx-add-string 2 :name "Oleg")
    (tx-add-string 3 :name "Petr")]))

(defn large-database []
  (conn/db-with
   (db/empty-db
    None
    (db/options-with-ref-type
     (db/default-options)
     (Lg_runtime.Runtime_ref_type.Strong)))
   (mapv
    (fn [entity]
      (tx-add-string entity :str (str entity)))
    (range 1 1001))))

(defn tail-datom-count [^datascript.conn/Conn connection]
  (let [^datascript.conn/conn-state state @(:atom connection)
        ^:vector<vector<datascript.db/Datom>> tail (:tx-tail state)]
    (transduce (map count) + 0 tail)))

(defn tail-group-count [^datascript.conn/Conn connection]
  (let [^datascript.conn/conn-state state @(:atom connection)
        ^:vector<vector<datascript.db/Datom>> tail (:tx-tail state)]
    (count tail)))

(defn ^datascript.conn/Conn restore-connection! [^memory-storage backend]
  (if-some [connection (conn/restore-conn (memory-backend backend))]
    connection
    (Stdlib.invalid_arg "Stored connection did not restore")))

(defn ^datascript.db/DB restore-database! [^memory-storage backend]
  (if-some [database (storage/restore (memory-backend backend))]
    database
    (Stdlib.invalid_arg "Stored database did not restore")))

(let [database (db/empty-db None (db/default-options))]
  (println
   (str environment ":no-storage:"
        (nil? (storage/storage database)) ":"
        (throws? (fn [] (storage/store database))))))

(let [missing-storage (make-memory-storage)]
  (println
   (str environment ":missing-root:"
        (nil? (storage/restore (memory-backend missing-storage))))))

(let [backend (make-memory-storage)
      database
      (db/init-db
       (to-array
        [(db/datom 1 :name (Datascript_runtime.Data_value.String "Ivan"))
         (db/datom 2 :name (Datascript_runtime.Data_value.String "Oleg"))])
       {})
      _stored (storage/store database (memory-backend backend))
      writes-after-first (count @(:writes backend))
      _stored-again (storage/store database)
      writes-after-second (count @(:writes backend))
      restored (or (storage/restore (memory-backend backend))
                   (Stdlib.invalid_arg "Stored database did not restore"))
      used-addresses (storage/addresses [database])
      tail
      [[(db/datom
         3 :name (Datascript_runtime.Data_value.String "Petr") 536870913)]]
      _tail-stored (storage/store-tail database tail)
      restored-with-tail (storage/restore (memory-backend backend))]
  (println
   (str environment ":store-restore:"
        (some? (storage/storage database)) ":"
        (has-string? restored 1 :name "Ivan") ":"
        (= 2 (count (db/-datoms restored :eavt nil nil nil nil))) ":"
        (= writes-after-first writes-after-second) ":"
        (and (contains? used-addresses 0)
             (contains? used-addresses 1)
             (> (count used-addresses) 2)) ":"
        (some? (db/search-ea restored-with-tail 3 :name)))))

(let [backend (make-memory-storage)
      database (db/empty-db None (db/default-options))
      _stored (storage/store database (memory-backend backend))
      writes-ok (= 5 (count @(:writes backend)))
      restored (restore-database! backend)
      lazy-root-ok (= 2 (count @(:reads backend)))
      restored-db-ok (db/db? restored)
      schema-ok (= (:schema database) (:schema restored))
      datoms-ok
      (= 0 (count (db/-datoms restored :eavt nil nil nil nil)))
      direct-equality-ok (and schema-ok datoms-ok)
      equality-ok direct-equality-ok
      lazy-index-ok (= 3 (count @(:reads backend)))]
  (println
   (str environment ":storage-basics-empty:"
        writes-ok ":"
        lazy-root-ok ":"
        restored-db-ok ":"
        schema-ok ":"
        datoms-ok ":"
        direct-equality-ok ":"
        equality-ok ":"
        lazy-index-ok)))

(let [backend (make-memory-storage)
      database (small-database)
      _stored (storage/store database (memory-backend backend))
      store-ok
      (and (= 0 (count @(:reads backend)))
           (= 5 (count @(:writes backend))))
      restored (restore-database! backend)
      restore-root-ok (= 2 (count @(:reads backend)))
      equality-ok (db/db-equal? database restored)
      _eavt (vec (db/-datoms restored :eavt nil nil nil nil))
      restore-eavt-ok (= 3 (count @(:reads backend)))
      _aevt (vec (db/-datoms restored :aevt nil nil nil nil))
      restore-aevt-ok (= 4 (count @(:reads backend)))
      _avet (vec (db/-datoms restored :avet nil nil nil nil))
      restore-avet-ok (= 5 (count @(:reads backend)))
      _reset (reset-stats backend)
      restored-for-count (restore-database! backend)
      count-ok (= 3 (db/db-count restored-for-count))
      count-read-count (count @(:reads backend))
      count-read-ok (<= count-read-count 2)
      settings (set/settings (:eavt (restore-database! backend)))
      settings-ok
      (and (= 32 (:branching-factor settings))
           (= (Lg_runtime.Runtime_ref_type.Strong)
              (:ref-type settings)))]
  (println
   (str environment ":storage-basics-small:"
        store-ok ":"
        restore-root-ok ":"
        equality-ok ":"
        restore-eavt-ok ":"
        restore-aevt-ok ":"
        restore-avet-ok ":"
        count-ok ":"
        count-read-ok ":"
        settings-ok)))

(let [backend (make-memory-storage)
      database (large-database)
      _stored (storage/store database (memory-backend backend))
      first-store-ok (= 135 (count @(:writes backend)))
      _stored-again (storage/store database)
      unchanged-store-ok (= 135 (count @(:writes backend)))
      restored (restore-database! backend)
      root-read-ok (= 2 (count @(:reads backend)))
      first-datom (first (db/-datoms restored :eavt nil nil nil nil))
      first-datom-ok
      (if-some [datom first-datom]
        (and
         (= 1 (.-e datom))
         (= :str (db/datom-attr datom))
         (Datascript_runtime.Data_value.equal
          (Datascript_runtime.Data_value.String "1")
          (.-v datom)))
        false)
      first-read-count (count @(:reads backend))
      first-read-ok (<= first-read-count 7)
      _first-again (first (db/-datoms restored :eavt nil nil nil nil))
      cached-first-read-count (count @(:reads backend))
      cached-first-ok (= first-read-count cached-first-read-count)
      _all (vec (db/-datoms restored :eavt nil nil nil nil))
      all-read-count (count @(:reads backend))
      all-read-ok (<= all-read-count 68)
      _all-again (vec (db/-datoms restored :eavt nil nil nil nil))
      cached-all-read-count (count @(:reads backend))
      cached-all-ok (= all-read-count cached-all-read-count)
      eavt-equality-ok
      (db/datom-vectors-equal?
       (vec (:eavt database))
       (vec (:eavt restored)))
      aevt-equality-ok
      (db/datom-vectors-equal?
       (vec (:aevt database))
       (vec (:aevt restored)))
      avet-equality-ok
      (db/datom-vectors-equal?
       (vec (:avet database))
       (vec (:avet restored)))
      database-equality-ok
      (and eavt-equality-ok aevt-equality-ok avet-equality-ok)
      _reset (reset-stats backend)
      restored-for-count (restore-database! backend)
      count-ok (= 1000 (db/db-count restored-for-count))
      count-read-count (count @(:reads backend))
      count-read-ok (<= count-read-count 2)
      _reset-again (reset-stats backend)
      updated
      (conn/db-with database [(tx-add-string 1001 :str "1001")])
      _stored-updated (storage/store updated)
      incremental-ok (= 8 (count @(:writes backend)))]
  (println
   (str environment ":storage-basics-large:"
        first-store-ok ":"
        unchanged-store-ok ":"
        root-read-ok ":"
        first-datom-ok ":"
        first-read-ok ":"
        cached-first-ok ":"
        all-read-ok ":"
        cached-all-ok ":"
        database-equality-ok ":"
        eavt-equality-ok ":"
        aevt-equality-ok ":"
        avet-equality-ok ":"
        count-ok ":"
        count-read-ok ":"
        incremental-ok)))

(let [backend (make-memory-storage)
      database
      (db/init-db
       (to-array
        [(db/datom 1 :name (Datascript_runtime.Data_value.String "Ivan"))])
       {})
      _stored (storage/store database (memory-backend backend))
      updated
      (db/with-datom
       database
       (db/datom 2 :name (Datascript_runtime.Data_value.String "Oleg")))
      _stored-updated (storage/store updated)
      live-databases [database updated]
      _orphaned
      (vswap!
       (:disk backend)
       assoc
       999999
       (Datascript_runtime.Storage_value.Stored_tail []))
      deleted (storage/collect-garbage (memory-backend backend))]
  (println
   (str environment ":storage-gc:"
        (contains? (set deleted) 999999) ":"
        (not (contains? @(:disk backend) 999999)) ":"
        (every? (fn [address] (contains? @(:disk backend) address))
                (storage/addresses live-databases)) ":"
        (contains? @(:disk backend) 0) ":"
        (contains? @(:disk backend) 1))))

(let [backend (make-memory-storage)
      connection
      (conn/create-conn
       None
       (db/options-with-storage (memory-backend backend)))
      callback-count (atom 0)
      listener-key
      (conn/listen! connection "listener"
                    (fn [_report]
                      (Stdlib.ignore (swap! callback-count inc))))
      report (conn/transact! connection [(tx-add-string 1 :name "Ivan")])
      current (conn/current-db connection)
      restored-connection (restore-connection! backend)
      restored (conn/current-db restored-connection)
      _unlistened (conn/unlisten! connection listener-key)
      _second-report
      (conn/transact! connection [(tx-add-string 2 :name "Oleg")])]
  (println
   (str environment ":conn:"
        (conn/conn? connection) ":"
        (= 1 @callback-count) ":"
        (has-string? current 1 :name "Ivan") ":"
        (has-string? restored 1 :name "Ivan") ":"
        (= 1 (db/tx-data-count report)))))

(let [backend (make-memory-storage)
      connection
      (conn/create-conn
       None
       (db/options-with-storage (memory-backend backend)))
      initial-store-ok (= 5 (count @(:writes backend)))
      _first (conn/transact! connection [(tx-add-string 1 :name "Ivan")])
      first-tail-ok
      (and (= 6 (count @(:writes backend)))
           (= 1 (last @(:writes backend))))
      _second (conn/transact! connection [(tx-add-string 2 :name "Oleg")])
      second-tail-ok
      (and (= 7 (count @(:writes backend)))
           (= 1 (last @(:writes backend)))
           (= 2 (tail-group-count connection))
           (= 2 (tail-datom-count connection)))
      _large-tail
      (conn/transact!
       connection
       (mapv
        (fn [entity] (tx-add-string entity :name (str entity)))
        (range 3 33)))
      full-tail-ok
      (and (= 8 (count @(:writes backend)))
           (= 1 (last @(:writes backend)))
           (= 3 (tail-group-count connection))
           (= 32 (tail-datom-count connection)))
      _overflow
      (conn/transact! connection [(tx-add-string 33 :name "Petr")])
      overflow-ok (= 16 (count @(:writes backend)))
      _restart
      (conn/transact! connection [(tx-add-string 34 :name "Anna")])
      restart-ok
      (and (= 17 (count @(:writes backend)))
           (= 1 (last @(:writes backend))))
      restored-connection (restore-connection! backend)
      restore-tail-ok
      (and (db/db-equal?
            (conn/current-db connection)
            (conn/current-db restored-connection))
           (= (:max-eid (conn/current-db connection))
              (:max-eid (conn/current-db restored-connection)))
           (= (:max-tx (conn/current-db connection))
              (:max-tx (conn/current-db restored-connection))))
      _restored-tx
      (conn/transact!
       restored-connection
       [(tx-add-string 35 :name "Vera")])
      restored-tail-write-ok
      (and (= 18 (count @(:writes backend)))
           (= 1 (last @(:writes backend))))
      _restored-overflow
      (conn/transact!
       restored-connection
       (mapv
        (fn [entity] (tx-add-string entity :name (str entity)))
        (range 36 80)))
      restored-overflow-ok
      (and (= 28 (count @(:writes backend)))
           (= 1 (last @(:writes backend))))
      restored-without-tail (restore-connection! backend)
      restore-without-tail-ok
      (db/db-equal?
       (conn/current-db restored-connection)
       (conn/current-db restored-without-tail))
      _final-tx
      (conn/transact!
       restored-without-tail
       [(tx-add-string 80 :name "Ilya")])
      final-tail-ok
      (and (= 29 (count @(:writes backend)))
           (= 1 (last @(:writes backend))))
      state @(:atom restored-without-tail)
      last-stored
      (match (:db-last-stored state)
        (Some database) database
        None (Stdlib.invalid_arg "Missing last stored database"))
      gc-needed-ok
      (> (count
          (Datascript_runtime.Storage_backend.list_addresses
           (memory-backend backend)))
         (count (storage/addresses [last-stored])))
      disk-address-count-before-gc
      (count
       (Datascript_runtime.Storage_backend.list_addresses
        (memory-backend backend)))
      _collected (storage/collect-garbage (memory-backend backend))
      gc-ok
      (and
       (<= (count
            (Datascript_runtime.Storage_backend.list_addresses
             (memory-backend backend)))
           disk-address-count-before-gc)
       (every?
        (fn [address]
          (contains?
           (set
            (Datascript_runtime.Storage_backend.list_addresses
             (memory-backend backend)))
           address))
        (storage/addresses [last-stored])))
      restored-after-gc (restore-connection! backend)
      restore-after-gc-ok
      (db/db-equal?
       (conn/current-db restored-without-tail)
       (conn/current-db restored-after-gc))]
  (println
   (str environment ":conn-tail:"
        initial-store-ok ":"
        first-tail-ok ":"
        second-tail-ok ":"
        full-tail-ok ":"
        overflow-ok ":"
        restart-ok ":"
        restore-tail-ok ":"
        restored-tail-write-ok ":"
        restored-overflow-ok ":"
        restore-without-tail-ok ":"
        final-tail-ok ":"
        gc-needed-ok ":"
        gc-ok ":"
        restore-after-gc-ok)))
