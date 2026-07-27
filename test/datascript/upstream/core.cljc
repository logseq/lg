(ns datascript.core
  (:refer-clojure :exclude [filter])
  (:require
    [datascript.conn :as conn]
    [datascript.db :as db]
    [datascript.pull-api :as dp]
    [datascript.serialize :as ds]
    [datascript.storage :as storage]
    [#?(:native datascript.storage-file :melange datascript.storage) :as storage-file]
    [datascript.lg.query :as dq]
    [datascript.impl.entity :as de]
    [datascript.util :as util]
    [me.tonsky.persistent-sorted-set :as set]))

(def ^:const ^:no-doc tx0
  db/tx0)

; Entities

(def ^{:tag datascript.impl.entity/Entity
       :arglists '([db eid])
       :doc "Retrieves an entity by its id from database. Entities are lazy map-like structures to navigate DataScript database content.

             For `eid` pass entity id or lookup attr:

                 (entity db 1)
                 (entity db [:unique-attr :value])

             If entity does not exist, `nil` is returned:

                 (entity db 100500) ; => nil

             Creating an entity by id is very cheap, almost no-op, as attr access is on-demand:

                 (entity db 1) ; => {:db/id 1}

             Entity attributes can be lazily accessed through key lookups:

                 (:attr (entity db 1)) ; => :value
                 (get (entity db 1) :attr) ; => :value

             Cardinality many attributes are returned sequences:

                 (:attrs (entity db 1)) ; => [:v1 :v2 :v3]

             Reference attributes are returned as another entities:

                 (:ref (entity db 1)) ; => {:db/id 2}
                 (:ns/ref (entity db 1)) ; => {:db/id 2}

             References can be walked backwards by prepending `_` to name part of an attribute:

                 (:_ref (entity db 2)) ; => [{:db/id 1}]
                 (:ns/_ref (entity db 2)) ; => [{:db/id 1}]

             Reverse reference lookup returns sequence of entities unless attribute is marked as `:db/isComponent`:

                 (:_component-ref (entity db 2)) ; => {:db/id 1}

             Entity gotchas:

             - Entities print as map, but are not exactly maps (they have compatible get interface though).
             - Entities are effectively immutable “views” into a particular version of a database.
             - Entities retain reference to the whole database.
             - You can’t change database through entities, only read.
             - Creating an entity by id is very cheap, almost no-op (attributes are looked up on demand).
             - Comparing entities just compares their ids. Be careful when comparing entities taken from different dbs or from different versions of the same db.
             - Accessed entity attributes are cached on entity itself (except backward references).
             - When printing, only cached attributes (the ones you have accessed before) are printed. See [[touch]]."}
  entity-closed de/entity)

(defn entity
  {:inline
   (fn [database source-ref]
     (let [value-form
           (fn [value]
             (if (nil? value)
               (list 'Datascript_runtime.Data_value.Nil)
               (if (string? value)
               (list
                'Datascript_runtime.Data_value.String
                value)
               (if (keyword? value)
                 (list
                  'Datascript_runtime.Data_value.Keyword
                  (str value))
                 (if (= value true)
                   (list
                    'Datascript_runtime.Data_value.Bool
                    true)
                   (if (= value false)
                     (list
                      'Datascript_runtime.Data_value.Bool
                      false)
                     (if (or (symbol? value) (seq? value))
                       value
                       (list
                        'Datascript_runtime.Data_value.Int
                        value))))))))]
       (list
        'datascript.core/entity-closed
        (list
         'datascript.db/database-view
         database)
        (if (vector? source-ref)
          (if (= 2 (count source-ref))
            (list
             'Datascript_runtime.Data_value.Lookup_ref
             (str (first source-ref))
             (value-form (second source-ref)))
            (list
             'Stdlib.invalid_arg
             (str
              "Lookup ref should contain 2 elements: "
              source-ref)))
          (if (keyword? source-ref)
            (list
             'Datascript_runtime.Data_value.Ident
             (str source-ref))
            (if (or (symbol? source-ref) (seq? source-ref))
              source-ref
              (list
               'Datascript_runtime.Data_value.Entity_id
               source-ref)))))))}
  [^datascript.db/database-view database
   ^:Datascript_runtime.Data_value.entity_ref entity-ref]
  (entity-closed database entity-ref))

(def ^{:arglists '([db eid])
       :doc "Given lookup ref `[unique-attr value]`, returns numberic entity id.

             If entity does not exist, returns `nil`."}
  entid db/entid)

(defn ^datascript.db/database-view entity-db
  "Returns a db that entity was created from."
  [^datascript.impl.entity/Entity entity]
  {:pre [(de/entity? entity)]}
  (de/entity-database-view entity))

(def ^{:tag datascript.impl.entity/Entity
       :arglists '([e])
       :doc "Forces all entity attributes to be eagerly fetched and cached. Only usable for debug output.

             Usage:

             ```
             (entity db 1) ; => {:db/id 1}
             (touch (entity db 1)) ; => {:db/id 1, :dislikes [:pie], :likes [:pizza]}
             ```"}
  touch de/touch)


; Pull

(def ^{:arglists '([db selector eid])
       :doc "Fetches data from database using recursive declarative description. See [docs.datomic.com/on-prem/pull.html](https://docs.datomic.com/on-prem/pull.html).

             Unlike [[entity]], returns plain Clojure map (not lazy).

             Usage:

                 (pull db [:db/id, :name, :likes, {:friends [:db/id :name]}] 1)
                 ; => {:db/id   1,
                 ;     :name    \"Ivan\"
                 ;     :likes   [:pizza]
                 ;     :friends [{:db/id 2, :name \"Oleg\"}]}"}
  pull dp/pull)

(def ^{:arglists '([db selector eids])
       :doc "Same as [[pull]], but accepts sequence of ids and returns sequence of maps.

             Usage:

             ```
             (pull-many db [:db/id :name] [1 2])
             ; => [{:db/id 1, :name \"Ivan\"}
             ;     {:db/id 2, :name \"Oleg\"}]
             ```"}
  pull-many dp/pull-many)

; Query

(def
  ^{:arglists '([query & inputs])
    :doc "Executes a datalog query. See [docs.datomic.com/on-prem/query.html](https://docs.datomic.com/on-prem/query.html).

          Usage:

          ```
          (q '[:find ?value
               :where [_ :likes ?value]]
             db)
          ; => #{[\"fries\"] [\"candy\"] [\"pie\"] [\"pizza\"]}
          ```"}
  q dq/q)


; Creating DB

(defn ^datascript.db/DB empty-db-closed
  "Creates an empty database with an optional schema.

   Usage:

   ```
   (empty-db) ; => #datascript/DB {:schema {}, :datoms []}

   (empty-db {:likes {:db/cardinality :db.cardinality/many}})
   ; => #datascript/DB {:schema {:likes {:db/cardinality :db.cardinality/many}}
   ;                    :datoms []}
   ```

   Options are:

   :branching-factor <int>, default 512. B-tree max node length
   :ref-type         Strong | Weak, default Weak. How nodes that are already stored on disk
                     are referenced. Weak nodes may be unloaded from memory under memory
                     pressure and later fetched from storage again.
  :storage          <IStorage>. Will be used to store this db later with `(d/store db)`"
  ([]
   (db/empty-db None (db/default-options)))
  ([^:map<keyword;map<keyword;Datascript_runtime.Data_value.t>> schema]
   (db/empty-db (Some schema) (db/default-options)))
  ([^:option<map<keyword;map<keyword;Datascript_runtime.Data_value.t>>> schema
    ^datascript.db/database-options opts]
   (let [opts (storage/maybe-adapt-storage opts)
         database (db/empty-db schema opts)]
     (when-some [backend (db/options-storage opts)]
     (Stdlib.ignore (storage/store database backend)))
     database)))

(defn empty-db
  {:inline
   (fn [& arguments]
     (case (count arguments)
       0 (list 'datascript.core/empty-db-closed)
       1 (list
          'datascript.core/empty-db-closed
          (list 'datascript.db/schema-map (first arguments)))
       2 (list
          'datascript.core/empty-db-closed
          (if (map? (first arguments))
            (list
             'Some
             (list 'datascript.db/schema-map (first arguments)))
            (first arguments))
          (second arguments))))}
  ([]
   (empty-db-closed))
  ([^:map<keyword;map<keyword;Datascript_runtime.Data_value.t>> schema]
   (empty-db-closed schema))
  ([^:option<map<keyword;map<keyword;Datascript_runtime.Data_value.t>>> schema
    ^datascript.db/database-options opts]
   (empty-db-closed schema opts)))

(def ^{:arglists '([x])
       :doc "Returns `true` if the given value is an immutable database, `false` otherwise."}
  db? db/db?)

(def ^{:tag datascript.db/Datom
       :arglists '([e a v] [e a v tx] [e a v tx added])
       :doc "Low-level fn to create raw datoms.

             Optionally with transaction id (number) and `added` flag (`true` for addition, `false` for retraction).

             See also [[init-db]]."}
  datom db/datom)

(def ^{:arglists '([x])
       :doc "Returns `true` if the given value is a datom, `false` otherwise."}
  datom? db/datom?)

(defn ^datascript.db/DB init-db-closed
  "Low-level fn for creating database quickly from a trusted sequence of datoms.
   Does no validation on inputs, so `datoms` must be well-formed and match schema.
   Used internally in db (de)serialization. See also [[datom]].
   For options, see [[empty-db]]"
  ([^:seqable<datascript.db/Datom> datoms]
   (db/init-db-with-schema-option (to-array datoms) None))
  ([^:seqable<datascript.db/Datom> datoms
    ^:map<keyword;map<keyword;Datascript_runtime.Data_value.t>> schema]
   (db/init-db (to-array datoms) schema))
  ([^:seqable<datascript.db/Datom> datoms
    ^:map<keyword;map<keyword;Datascript_runtime.Data_value.t>> schema
    ^datascript.db/database-options opts]
   (let [opts (storage/maybe-adapt-storage opts)
         database (db/init-db (to-array datoms) schema opts)]
     (when-some [backend (db/options-storage opts)]
       (Stdlib.ignore (storage/store database backend)))
     database)))

(defn ^datascript.db/DB init-db-invalid [^:string message]
  (Stdlib.invalid_arg message))

(defn init-db
  {:inline
   (fn [datoms & rest]
     (let [invalid-literal?
           (and
            (vector? datoms)
            (if
              (empty?
               (filter
                (fn [item]
                  (or (vector? item) (map? item)))
                datoms))
              false
              true))
           value-form
           (fn [value]
             (if (keyword? value)
               (list
                'Datascript_runtime.Data_value.Keyword
                (str value))
               (if (string? value)
                 (list
                  'Datascript_runtime.Data_value.String
                  value)
                 (if (= value true)
                   (list
                    'Datascript_runtime.Data_value.Bool
                    true)
                   (if (= value false)
                     (list
                      'Datascript_runtime.Data_value.Bool
                      false)
                     (if (or (symbol? value) (seq? value))
                       value
                       (list
                        'Datascript_runtime.Data_value.Int
                        value)))))))
           properties-form
           (fn [properties]
             (list
              'zipmap
              (vec (map first properties))
              (vec
               (map
                (fn [property]
                  (value-form (second property)))
                properties))))
           schema-form
           (fn [schema]
             (if (map? schema)
               (if (empty? schema)
                 schema
                 (list
                  'zipmap
                  (vec (map first schema))
                  (vec
                   (map
                    (fn [entry]
                      (properties-form (second entry)))
                    schema))))
               schema))]
       (if invalid-literal?
         (list
          'datascript.core/init-db-invalid
          (str "init-db expects list of Datoms, got " datoms))
         (if (empty? rest)
           (list 'datascript.core/init-db-closed datoms)
           (cons
            'datascript.core/init-db-closed
            (cons
             datoms
             (cons
              (schema-form (first rest))
              (next rest))))))))}
  ([^:seqable<datascript.db/Datom> datoms]
   (init-db-closed datoms))
  ([^:seqable<datascript.db/Datom> datoms
    ^:map<keyword;map<keyword;Datascript_runtime.Data_value.t>> schema]
   (init-db-closed datoms schema))
  ([^:seqable<datascript.db/Datom> datoms
    ^:map<keyword;map<keyword;Datascript_runtime.Data_value.t>> schema
    ^datascript.db/database-options opts]
   (init-db-closed datoms schema opts)))

(def ^{:arglists '([db] [db opts])
       :doc "Converts db into a data structure (not string!) that can be fed to serializer
             of your choice (e.g. `js/JSON.stringify` in CLJS, `cheshire.core/generate-string`
             or `jsonista.core/write-value-as-string` in CLJ).

             On JVM, `serializable` holds a global lock that prevents any two serializations
             to run in parallel (an implementation constraint, be aware).

             Options:

             `:freeze-fn` Non-primitive values will be serialized using this. Optional.
             `pr-str` by default."}
  serializable ds/serializable)

(def ^{:tag datascript.db/DB
       :arglists '([serializable] [serializable opts])
       :doc "Creates db from a data structure (not string!) produced by serializable.

             Opts:

             `:thaw-fn` Non-primitive values will be deserialized using this.
             Must match :freeze-fn from serializable. Optional. `clojure.edn/read-string`
             by default."}
  from-serializable ds/from-serializable)


; Schema

(defn schema
  "Returns a schema of a database."
  [^datascript.db/DB database]
  (db/-schema database))


; Filtered db

(defn is-filtered
  "Returns true for a database view created by filter."
  [database]
  (db/-filtered? database))

(defn ^datascript.db/FilteredDB filter
  "Returns a view over database that has same interface but only includes datoms for which the `(pred db datom)` is true. Can be applied multiple times.

   Filtered DB gotchas:

   - All operations on filtered database are proxied to original DB, then filter pred is applied.
   - Not cached. You pay filter penalty every time.
   - Supports entities, pull, queries, index access.
   - Does not support [[with]] and [[db-with]]."
  [database
   ^:fn<datascript.db/DB;datascript.db/Datom;bool> pred]
  (db/-filter-view database pred))


; Changing DB

(defn with
  {:inline
   (fn [database tx-data & tx-meta]
     (cons
      'datascript.conn/with
      (cons database (cons tx-data tx-meta))))}
  ([database tx-data]
   (conn/with database tx-data))
  ([database
    tx-data
    ^:option<map<keyword;Datascript_runtime.Data_value.t>> tx-meta]
   (conn/with database tx-data tx-meta)))

(defn db-with
  "Applies transaction to an immutable db value, returning new immutable db value. Same as `(:db-after (with db tx-data))`."
  {:inline
   (fn [database tx-data]
     (list 'datascript.conn/db-with database tx-data))}
  [^datascript.db/DB database
   ^:vector<datascript.db/tx-entry> tx-data]
  (conn/db-with database tx-data))

(defn ^datascript.db/DB with-schema
  "Warning! No validation or conversion. Only change schema in a compatible way"
  [db schema]
  (db/with-schema db schema))

; Index lookups

(defn ^:seq<datascript.db/Datom> datoms-closed
  "Index lookup. Returns a sequence of datoms (lazy iterator over actual DB index) which components (e, a, v) match passed arguments.

   Datoms are sorted in index sort order. Possible `index` values are: `:eavt`, `:aevt`, `:avet`.

   Usage:

       ; find all datoms for entity id == 1 (any attrs and values)
       ; sort by attribute, then value
       (datoms db :eavt 1)
       ; => (#datascript/Datom [1 :friends 2]
       ;     #datascript/Datom [1 :likes \"fries\"]
       ;     #datascript/Datom [1 :likes \"pizza\"]
       ;     #datascript/Datom [1 :name \"Ivan\"])

       ; find all datoms for entity id == 1 and attribute == :likes (any values)
       ; sorted by value
       (datoms db :eavt 1 :likes)
       ; => (#datascript/Datom [1 :likes \"fries\"]
       ;     #datascript/Datom [1 :likes \"pizza\"])

       ; find all datoms for entity id == 1, attribute == :likes and value == \"pizza\"
       (datoms db :eavt 1 :likes \"pizza\")
       ; => (#datascript/Datom [1 :likes \"pizza\"])

       ; find all datoms for attribute == :likes (any entity ids and values)
       ; sorted by entity id, then value
       (datoms db :aevt :likes)
       ; => (#datascript/Datom [1 :likes \"fries\"]
       ;     #datascript/Datom [1 :likes \"pizza\"]
       ;     #datascript/Datom [2 :likes \"candy\"]
       ;     #datascript/Datom [2 :likes \"pie\"]
       ;     #datascript/Datom [2 :likes \"pizza\"])

       ; find all datoms that have attribute == `:likes` and value == `\"pizza\"` (any entity id)
       ; `:likes` must be a unique attr, reference or marked as `:db/index true`
       (datoms db :avet :likes \"pizza\")
       ; => (#datascript/Datom [1 :likes \"pizza\"]
       ;     #datascript/Datom [2 :likes \"pizza\"])

       ; find all datoms sorted by entity id, then attribute, then value
       (datoms db :eavt) ; => (...)

   Useful patterns:

       ; get all values of :db.cardinality/many attribute
       (->> (datoms db :eavt eid attr) (map :v))

       ; lookup entity ids by attribute value
       (->> (datoms db :avet attr value) (map :e))

       ; find all entities with a specific attribute
       (->> (datoms db :aevt attr) (map :e))

       ; find “singleton” entity by its attr
       (->> (datoms db :aevt attr) first :e)

       ; find N entities with lowest attr value (e.g. 10 earliest posts)
       (->> (datoms db :avet attr) (take N))

       ; find N entities with highest attr value (e.g. 10 latest posts)
       (->> (datoms db :avet attr) (reverse) (take N))

   Gotchas:

   - Index lookup is usually more efficient than doing a query with a single clause.
   - Resulting iterator is calculated in constant time and small constant memory overhead.
   - Iterator supports efficient `first`, `next`, `reverse`, `seq` and is itself a sequence.
   - Will not return datoms that are not part of the index (e.g. attributes with no `:db/index` in schema when querying `:avet` index).
     - `:eavt` and `:aevt` contain all datoms.
     - `:avet` only contains datoms for references, `:db/unique` and `:db/index` attributes."
  ([db index]             {:pre [(db/db? db)]} (db/-datoms db index nil nil nil nil))
  ([db index c0]          {:pre [(db/db? db)]} (db/-datoms db index c0  nil nil nil))
  ([db index c0 c1]       {:pre [(db/db? db)]} (db/-datoms db index c0  c1  nil nil))
  ([db index c0 c1 c2]    {:pre [(db/db? db)]} (db/-datoms db index c0  c1  c2  nil))
  ([db index c0 c1 c2 c3] {:pre [(db/db? db)]} (db/-datoms db index c0  c1  c2  c3)))

(defn index-component-value
  {:inline
   (fn [value]
     (let [value-form
           (fn value-form [value]
             (if (nil? value)
               (list 'Datascript_runtime.Data_value.Nil)
               (if (vector? value)
                 (list
                  'Datascript_runtime.Data_value.Ref_to
                  (list
                   'Datascript_runtime.Data_value.Lookup_ref
                   (str (first value))
                   (value-form (second value))))
                 (if (string? value)
                   (list
                    'Datascript_runtime.Data_value.String
                    value)
                   (if (keyword? value)
                     (list
                      'Datascript_runtime.Data_value.Keyword
                      (str value))
                     (if (= value true)
                       (list
                        'Datascript_runtime.Data_value.Bool
                        true)
                       (if (= value false)
                         (list
                          'Datascript_runtime.Data_value.Bool
                          false)
                         (if (or (symbol? value) (seq? value))
                           value
                           (list
                            'Datascript_runtime.Data_value.Int
                            value)))))))))]
       (value-form value)))}
  [^:Datascript_runtime.Data_value.t value]
  value)

(defn datoms
  {:inline
   (fn [database index & components]
     (cons
      'datascript.core/datoms-closed
      (cons
       database
       (cons
        index
        (vec
         (map
          (fn [component]
            (list 'datascript.core/index-component-value component))
          components))))))}
  ([db index] (datoms-closed db index))
  ([db index c0] (datoms-closed db index c0))
  ([db index c0 c1] (datoms-closed db index c0 c1))
  ([db index c0 c1 c2] (datoms-closed db index c0 c1 c2))
  ([db index c0 c1 c2 c3]
   (datoms-closed db index c0 c1 c2 c3)))

(defn ^datascript.db/Datom find-datom-closed
  "Same as [[datoms]], but only returns single datom. Faster than `(first (datoms ...))`"
  ([db index]             {:pre [(db/db? db)]} (db/find-datom db index nil nil nil nil))
  ([db index c0]          {:pre [(db/db? db)]} (db/find-datom db index c0  nil nil nil))
  ([db index c0 c1]       {:pre [(db/db? db)]} (db/find-datom db index c0  c1  nil nil))
  ([db index c0 c1 c2]    {:pre [(db/db? db)]} (db/find-datom db index c0  c1  c2  nil))
  ([db index c0 c1 c2 c3] {:pre [(db/db? db)]} (db/find-datom db index c0  c1  c2  c3)))

(defn find-datom
  {:inline
   (fn [database index & components]
     (cons
      'datascript.core/find-datom-closed
      (cons
       database
       (cons
        index
        (vec
         (map
          (fn [component]
            (list 'datascript.core/index-component-value component))
          components))))))}
  ([db index] (find-datom-closed db index))
  ([db index c0] (find-datom-closed db index c0))
  ([db index c0 c1] (find-datom-closed db index c0 c1))
  ([db index c0 c1 c2] (find-datom-closed db index c0 c1 c2))
  ([db index c0 c1 c2 c3]
   (find-datom-closed db index c0 c1 c2 c3)))

(defn- ^:seq<datascript.db/Datom> seek-datoms*
  [db index c0 c1 c2 c3]
  (db/-seek-datoms db index c0 c1 c2 c3))

(defn seek-datoms-closed
  "Similar to [[datoms]], but will return datoms starting from specified components and including rest of the database until the end of the index.

   If no datom matches passed arguments exactly, iterator will start from first datom that could be considered “greater” in index order.

   Usage:

       (seek-datoms db :eavt 1)
       ; => (#datascript/Datom [1 :friends 2]
       ;     #datascript/Datom [1 :likes \"fries\"]
       ;     #datascript/Datom [1 :likes \"pizza\"]
       ;     #datascript/Datom [1 :name \"Ivan\"]
       ;     #datascript/Datom [2 :likes \"candy\"]
       ;     #datascript/Datom [2 :likes \"pie\"]
       ;     #datascript/Datom [2 :likes \"pizza\"])

       (seek-datoms db :eavt 1 :name)
       ; => (#datascript/Datom [1 :name \"Ivan\"]
       ;     #datascript/Datom [2 :likes \"candy\"]
       ;     #datascript/Datom [2 :likes \"pie\"]
       ;     #datascript/Datom [2 :likes \"pizza\"])

       (seek-datoms db :eavt 2)
       ; => (#datascript/Datom [2 :likes \"candy\"]
       ;     #datascript/Datom [2 :likes \"pie\"]
       ;     #datascript/Datom [2 :likes \"pizza\"])

       ; no datom [2 :likes \"fish\"], so starts with one immediately following such in index
       (seek-datoms db :eavt 2 :likes \"fish\")
       ; => (#datascript/Datom [2 :likes \"pie\"]
       ;     #datascript/Datom [2 :likes \"pizza\"])"
  ([db index]             {:pre [(db/db? db)]} (seek-datoms* db index nil nil nil nil))
  ([db index c0]          {:pre [(db/db? db)]} (seek-datoms* db index c0  nil nil nil))
  ([db index c0 c1]       {:pre [(db/db? db)]} (seek-datoms* db index c0  c1  nil nil))
  ([db index c0 c1 c2]    {:pre [(db/db? db)]} (seek-datoms* db index c0  c1  c2  nil))
  ([db index c0 c1 c2 c3] {:pre [(db/db? db)]} (seek-datoms* db index c0  c1  c2  c3)))

(defn seek-datoms
  {:inline
   (fn [database index & components]
     (cons
      'datascript.core/seek-datoms-closed
      (cons
       database
       (cons
        index
        (vec
         (map
          (fn [component]
            (list 'datascript.core/index-component-value component))
          components))))))}
  ([db index] (seek-datoms-closed db index))
  ([db index c0] (seek-datoms-closed db index c0))
  ([db index c0 c1] (seek-datoms-closed db index c0 c1))
  ([db index c0 c1 c2] (seek-datoms-closed db index c0 c1 c2))
  ([db index c0 c1 c2 c3]
   (seek-datoms-closed db index c0 c1 c2 c3)))

(defn- ^:seq<datascript.db/Datom> rseek-datoms*
  [db index c0 c1 c2 c3]
  (db/-rseek-datoms db index c0 c1 c2 c3))

(defn rseek-datoms-closed
  "Same as [[seek-datoms]], but goes backwards until the beginning of the index."
  ([db index]             {:pre [(db/db? db)]} (rseek-datoms* db index nil nil nil nil))
  ([db index c0]          {:pre [(db/db? db)]} (rseek-datoms* db index c0  nil nil nil))
  ([db index c0 c1]       {:pre [(db/db? db)]} (rseek-datoms* db index c0  c1  nil nil))
  ([db index c0 c1 c2]    {:pre [(db/db? db)]} (rseek-datoms* db index c0  c1  c2  nil))
  ([db index c0 c1 c2 c3] {:pre [(db/db? db)]} (rseek-datoms* db index c0  c1  c2  c3)))

(defn rseek-datoms
  {:inline
   (fn [database index & components]
     (cons
      'datascript.core/rseek-datoms-closed
      (cons
       database
       (cons
        index
        (vec
         (map
          (fn [component]
            (list 'datascript.core/index-component-value component))
          components))))))}
  ([db index] (rseek-datoms-closed db index))
  ([db index c0] (rseek-datoms-closed db index c0))
  ([db index c0 c1] (rseek-datoms-closed db index c0 c1))
  ([db index c0 c1 c2] (rseek-datoms-closed db index c0 c1 c2))
  ([db index c0 c1 c2 c3]
   (rseek-datoms-closed db index c0 c1 c2 c3)))

(defn- ^:seq<datascript.db/Datom> index-range*
  [db attr start end]
  (db/-index-range db attr start end))

(defn index-range-closed
  "Returns part of `:avet` index between `[_ attr start]` and `[_ attr end]` in AVET sort order.

   Same properties as [[datoms]].

   `attr` must be a reference, unique attribute or marked as `:db/index true`.

   Usage:

       (index-range db :likes \"a\" \"zzzzzzzzz\")
       ; => (#datascript/Datom [2 :likes \"candy\"]
       ;     #datascript/Datom [1 :likes \"fries\"]
       ;     #datascript/Datom [2 :likes \"pie\"]
       ;     #datascript/Datom [1 :likes \"pizza\"]
       ;     #datascript/Datom [2 :likes \"pizza\"])

       (index-range db :likes \"egg\" \"pineapple\")
       ; => (#datascript/Datom [1 :likes \"fries\"]
       ;     #datascript/Datom [2 :likes \"pie\"])

   Useful patterns:

       ; find all entities with age in a specific range (inclusive)
       (->> (index-range db :age 18 60) (map :e))"
  [db attr start end]
  {:pre [(db/db? db)]}
  (index-range* db attr start end))

(defn index-range
  {:inline
   (fn [database attr start end]
     (list
      'datascript.core/index-range-closed
      database
      attr
      (list 'datascript.core/index-component-value start)
      (list 'datascript.core/index-component-value end)))}
  [database attr start end]
  (index-range-closed database attr start end))

;; Conn

(def ^{:arglists '([conn])} conn?
  "Returns `true` if this is a connection to a DataScript db, `false` otherwise."
  conn/conn?)

(def ^{:arglists '([db])} conn-from-db
  "Creates a mutable reference to a given immutable database. See [[create-conn]]."
  conn/conn-from-db)

(def ^{:arglists '([datoms] [datoms schema] [datoms schema opts])} conn-from-datoms
  "Creates an empty DB and a mutable reference to it. See [[create-conn]]."
  conn/conn-from-datoms)

(def ^{:arglists '([] [schema] [schema opts])} create-conn
  "Creates a mutable reference (a “connection”) to an empty immutable database.

   Connections are lightweight in-memory structures (~atoms) with direct support of transaction listeners ([[listen!]], [[unlisten!]]) and other handy DataScript APIs ([[transact!]], [[reset-conn!]], [[db]]).

   To access underlying immutable DB value, deref: `@conn`.

   For list of options, see [[empty-db]].

   If you specify `:storage` option, conn will be stored automatically after each transaction"
  conn/create-conn)

(def ^{:arglists '([storage] [storage opts])} restore-conn
  "Lazy-load database from storage and make conn out of it.
   Returns nil if there’s no database yet in storage"
  conn/restore-conn)

(defn transact!
  "Applies transaction the underlying database value and atomically updates connection reference to point to the result of that transaction, new db value.

   Returns transaction report, a map:

       {:db-before ...      ; db value before transaction
        :db-after  ...      ; db value after transaction
        :tx-data   [...]    ; plain datoms that were added/retracted from db-before
        :tempids   {...}    ; map of tempid from tx-data => assigned entid in db-after
        :tx-meta   tx-meta} ; the exact value you passed as `tx-meta`

  Note! `conn` will be updated in-place and is not returned from [[transact!]].

  Usage:

      ; add a single datom to an existing entity (1)
      (transact! conn [[:db/add 1 :name \"Ivan\"]])

      ; retract a single datom
      (transact! conn [[:db/retract 1 :name \"Ivan\"]])

      ; retract single entity attribute
      (transact! conn [[:db.fn/retractAttribute 1 :name]])

      ; ... or equivalently (since Datomic changed its API to support this):
      (transact! conn [[:db/retract 1 :name]])

      ; retract all entity attributes (effectively deletes entity)
      (transact! conn [[:db.fn/retractEntity 1]])

      ; create a new entity (`-1`, as any other negative value, is a tempid
      ; that will be replaced with DataScript to a next unused eid)
      (transact! conn [[:db/add -1 :name \"Ivan\"]])

      ; check assigned id (here `*1` is a result returned from previous `transact!` call)
      (def report *1)
      (:tempids report) ; => {-1 296}

      ; check actual datoms inserted
      (:tx-data report) ; => [#datascript/Datom [296 :name \"Ivan\"]]

      ; tempid can also be a string
      (transact! conn [[:db/add \"ivan\" :name \"Ivan\"]])
      (:tempids *1) ; => {\"ivan\" 297}

      ; reference another entity (must exist)
      (transact! conn [[:db/add -1 :friend 296]])

      ; create an entity and set multiple attributes (in a single transaction
      ; equal tempids will be replaced with the same yet unused entid)
      (transact! conn [[:db/add -1 :name \"Ivan\"]
                       [:db/add -1 :likes \"fries\"]
                       [:db/add -1 :likes \"pizza\"]
                       [:db/add -1 :friend 296]])

      ; create an entity and set multiple attributes (alternative map form)
      (transact! conn [{:db/id  -1
                        :name   \"Ivan\"
                        :likes  [\"fries\" \"pizza\"]
                        :friend 296}])

      ; update an entity (alternative map form). Can’t retract attributes in
      ; map form. For cardinality many attrs, value (fish in this example)
      ; will be added to the list of existing values
      (transact! conn [{:db/id  296
                        :name   \"Oleg\"
                        :likes  [\"fish\"]}])

      ; ref attributes can be specified as nested map, that will create nested entity as well
      (transact! conn [{:db/id  -1
                        :name   \"Oleg\"
                        :friend {:db/id -2
                                 :name \"Sergey\"}}])

      ; reverse attribute name can be used if you want created entity to become
      ; a value in another entity reference
      (transact! conn [{:db/id  -1
                        :name   \"Oleg\"
                        :_friend 296}])
      ; equivalent to
      (transact! conn [{:db/id  -1, :name   \"Oleg\"}
                       {:db/id 296, :friend -1}])
      ; equivalent to
      (transact! conn [[:db/add  -1 :name   \"Oleg\"]
                       [:db/add 296 :friend -1]])"
  {:inline
   (fn [connection tx-data & tx-meta]
     (let [value-form
           (fn [value]
             (if (keyword? value)
               (list 'Datascript_runtime.Data_value.Keyword (str value))
               (if (string? value)
                 (list 'Datascript_runtime.Data_value.String value)
                 (if (= value true)
                   (list 'Datascript_runtime.Data_value.Bool true)
                   (if (= value false)
                     (list 'Datascript_runtime.Data_value.Bool false)
                     (if (nil? value)
                       (list 'Datascript_runtime.Data_value.Nil)
                       (if (or (symbol? value) (seq? value))
                         value
                         (list
                          'Datascript_runtime.Data_value.Int
                          value))))))))
           metadata-form
           (fn [metadata]
             (if (map? metadata)
               (list
                'zipmap
                (vec (map first metadata))
                (vec
                 (map
                  (fn [entry]
                    (value-form (second entry)))
                  metadata)))
               metadata))]
       (cons
        'datascript.conn/transact!
        (cons
         connection
         (cons
          (list 'datascript.db/tx-data tx-data)
          (if (empty? tx-meta)
            tx-meta
            (list (metadata-form (first tx-meta)))))))))}
  ([^datascript.conn/Conn connection
    ^:vector<datascript.db/tx-entry> tx-data]
   (conn/transact! connection tx-data))
  ([^datascript.conn/Conn connection
    ^:vector<datascript.db/tx-entry> tx-data
    ^:map<keyword;Datascript_runtime.Data_value.t> tx-meta]
   (conn/transact! connection tx-data (Some tx-meta))))

(defn reset-conn!
  {:inline
   (fn [connection database & metadata]
     (let [value-form
           (fn [value]
             (if (keyword? value)
               (list 'Datascript_runtime.Data_value.Keyword (str value))
               (if (string? value)
                 (list 'Datascript_runtime.Data_value.String value)
                 (if (= value true)
                   (list 'Datascript_runtime.Data_value.Bool true)
                   (if (= value false)
                     (list 'Datascript_runtime.Data_value.Bool false)
                     (if (nil? value)
                       (list 'Datascript_runtime.Data_value.Nil)
                       (if (or (symbol? value) (seq? value))
                         value
                         (list
                          'Datascript_runtime.Data_value.Int
                          value))))))))
           metadata-form
           (fn [value]
             (if (map? value)
               (list
                'Datascript_runtime.Data_value.map_of_keyword_map
                (list
                 'zipmap
                 (vec (map first value))
                 (vec
                  (map
                   (fn [entry]
                     (value-form (second entry)))
                   value))))
               (if (or (symbol? value) (seq? value))
                 value
                 (value-form value))))]
       (if (empty? metadata)
         (list 'datascript.conn/reset-conn! connection database)
         (list
          'datascript.conn/reset-conn!
          connection
          database
          (metadata-form (first metadata))))))}
  ([^datascript.conn/Conn connection ^datascript.db/DB database]
   (conn/reset-conn! connection database))
  ([^datascript.conn/Conn connection
    ^datascript.db/DB database
    ^:Datascript_runtime.Data_value.t tx-meta]
   (conn/reset-conn! connection database tx-meta)))

(defn ^datascript.db/DB reset-schema!
  [^datascript.conn/Conn connection schema]
  (conn/reset-schema! connection schema))

(def ^{:arglists '([conn callback] [conn key callback])} listen!
  "Listen for changes on the given connection. Whenever a transaction is applied to the database via [[transact!]], the callback is called
   with the transaction report. `key` is any opaque unique value.

   Idempotent. Calling [[listen!]] with the same key twice will override old callback with the new value.

   Returns the key under which this listener is registered. See also [[unlisten!]]."
  conn/listen!)

(def ^{:arglists '([conn key])} unlisten!
  "Removes registered listener from connection. See also [[listen!]]."
  conn/unlisten!)


;; Datomic compatibility layer

(def ^:private last-tempid (atom -1000000))

(defn ^:Datascript_runtime.Data_value.entity_ref tempid
  "Allocates and returns an unique temporary id (a negative integer). Ignores `part`. Returns `x` if it is specified.

   Exists for Datomic API compatibility. Prefer using negative integers directly if possible."
  ([^:keyword part]
   (if (= part :db.part/tx)
     (Datascript_runtime.Data_value.Current_tx)
     (Datascript_runtime.Data_value.Entity_id
      (swap! last-tempid dec))))
  ([^:keyword part ^:Datascript_runtime.Data_value.entity_ref x]
   (if (= part :db.part/tx)
     (Datascript_runtime.Data_value.Current_tx)
     x)))

(defn resolve-tempid
  "Does a lookup in tempids map, returning an entity id that tempid was resolved to.

   Exists for Datomic API compatibility. Prefer using map lookup directly if possible."
  [_db tempids tempid]
  (get tempids tempid))

(defn ^datascript.db/DB db
  "Returns the underlying immutable database value from a connection.

   Exists for Datomic API compatibility. Prefer using `@conn` directly if possible."
  [^datascript.conn/Conn connection]
  {:pre [(conn? connection)]}
  (conn/current-db connection))

(defn transact
  "Same as [[transact!]], but returns an immediately realized future.

   Exists for Datomic API compatibility. Prefer using [[transact!]] if possible."
  ([conn tx-data] (transact conn tx-data nil))
  ([^datascript.conn/Conn conn
    ^:vector<datascript.db/tx-entry> tx-data
    ^:option<map<keyword;Datascript_runtime.Data_value.t>> tx-meta]
   {:pre [(conn? conn)]}
   (let [res (transact! conn tx-data tx-meta)]
     (future-call (fn [] res)))))

(defn transact-async
  "In CLJ, calls [[transact!]] on a future thread pool, returning immediately.

   In CLJS, just calls [[transact!]] and returns a realized future."
  ([conn tx-data] (transact-async conn tx-data nil))
  ([^datascript.conn/Conn conn
    ^:vector<datascript.db/tx-entry> tx-data
    ^:option<map<keyword;Datascript_runtime.Data_value.t>> tx-meta]
   {:pre [(conn? conn)]}
   (future-call #(transact! conn tx-data tx-meta))))


;; squuid

(def ^{:arglists '([] [msec])} squuid
  "Generates a UUID that grow with time. Such UUIDs will always go to the end  of the index and that will minimize insertions in the middle.

   Consist of 64 bits of current UNIX timestamp (in seconds) and 64 random bits (2^64 different unique values per second)."
  util/squuid)

(def ^{:arglists '([uuid])} squuid-time-millis
  "Returns time that was used in [[squuid]] call, in milliseconds, rounded to the closest second."
  util/squuid-time-millis)


;; Storage
(def ^{:arglists '([db])} storage
  "Returns IStorage used by DB instance"
  storage/storage)

(def ^{:arglists '([db] [db storage])} store
  "Stores databases to provided storage. If database was created
      with :storage option or restored from storage, use single-argument version.

      Subsequent stores are incremental, i.e. only newly added nodes will be actually stored.

      Storing already stored dbs into another storage is not supported (may change)."
  storage/store)

(def ^{:arglists '([storage] [storage opts])} restore
  "Lazy-loads database from storage. Ultra-fast, fetches the rest as it’s needed"
  storage/restore)

(defn addresses
  "Returns all addresses in use by current db.
   Anything that is not in the return set is safe to be deleted"
  [& dbs]
  (storage/addresses (vec dbs)))

(defn collect-garbage
  "Deletes all keys from storage that are not referenced by any of the currently alive db refs.
   Has a side-effect of fully loading databases fully into memory, so, can be slow"
  [backend]
  (storage/collect-garbage backend))

#?(:native
   (def ^{:arglists '([dir] [dir opts])} file-storage
     "Default implementation that stores data in files in a dir.

   Options are:

   :freeze-fn :: (data)   -> String. A serialization function
   :thaw-fn   :: (String) -> data. A deserialization function
   :write-fn  :: (OutputStream data) -> void. Implement your own writer to FileOutputStream
   :read-fn   :: (InputStream) -> Object. Implement your own reader from FileInputStream
   :addr->filename-fn :: (UUID) -> String. Construct file name from address
   :filename->addr-fn :: (String) -> UUID. Reconstruct address from file name

   All options are optional."
     storage-file/file-storage))

(defn settings [db]
  (set/settings (:eavt db)))
