(ns ^:no-doc datascript.js
  (:refer-clojure :exclude [filter])
  (:require
   [datascript.conn :as conn]
   [datascript.core :as d]))

(def ^:export serializable d/serializable)
(def ^:export from_serializable d/from-serializable)

(defn- ^:keyword index-string->keyword
  [^string index]
  (if (= ":" (subs index 0 1))
    (keyword (subs index 1))
    (keyword index)))

(signature datascript.js/datoms-from-components
  :fn<datascript.db/DB;keyword;vector<Datascript_runtime.Data_value.t>;seq<datascript.db/Datom>>)

(defn- datoms-from-components
  [database index components]
  (cond
    (= 0 (count components))
    (d/datoms-closed database index)

    (= 1 (count components))
    (d/datoms-closed database index (nth components 0))

    (= 2 (count components))
    (d/datoms-closed
     database index
     (nth components 0)
     (nth components 1))

    (= 3 (count components))
    (d/datoms-closed
     database index
     (nth components 0)
     (nth components 1)
     (nth components 2))

    (= 4 (count components))
    (d/datoms-closed
     database index
     (nth components 0)
     (nth components 1)
     (nth components 2)
     (nth components 3))

    :else
    (Stdlib.invalid_arg "datoms accepts at most four components")))

(defn ^:export datoms
  {:inline
   (fn [database index & components]
     (let [index
           (if (string? index)
             (cond
               (= index ":eavt") :eavt
               (= index ":aevt") :aevt
               (= index ":avet") :avet
               :else index)
             index)]
       (cons
        'datascript.core/datoms
        (cons database (cons index components)))))}
  [^datascript.db/DB database
   ^string index
   & ^:list<Datascript_runtime.Data_value.t> components]
  (datoms-from-components
   database
   (index-string->keyword index)
   (vec components)))

(signature datascript.js/seek-datoms-from-components
  :fn<datascript.db/DB;keyword;vector<Datascript_runtime.Data_value.t>;seq<datascript.db/Datom>>)

(defn- seek-datoms-from-components
  [database index components]
  (cond
    (= 0 (count components))
    (d/seek-datoms-closed database index)

    (= 1 (count components))
    (d/seek-datoms-closed database index (nth components 0))

    (= 2 (count components))
    (d/seek-datoms-closed
     database index
     (nth components 0)
     (nth components 1))

    (= 3 (count components))
    (d/seek-datoms-closed
     database index
     (nth components 0)
     (nth components 1)
     (nth components 2))

    (= 4 (count components))
    (d/seek-datoms-closed
     database index
     (nth components 0)
     (nth components 1)
     (nth components 2)
     (nth components 3))

    :else
    (Stdlib.invalid_arg "seek_datoms accepts at most four components")))

(defn ^:export seek_datoms
  {:inline
   (fn [database index & components]
     (let [index
           (if (string? index)
             (cond
               (= index ":eavt") :eavt
               (= index ":aevt") :aevt
               (= index ":avet") :avet
               :else index)
             index)]
       (cons
        'datascript.core/seek-datoms
        (cons database (cons index components)))))}
  [^datascript.db/DB database
   ^string index
   & ^:list<Datascript_runtime.Data_value.t> components]
  (seek-datoms-from-components
   database
   (index-string->keyword index)
   (vec components)))

(defn ^:export db_with
  {:inline
   (fn [database entities]
     (list 'datascript.core/db-with database entities))}
  [database entities]
  (d/db-with database entities))

(defn ^:export empty_db [& [schema]]
  (if-some [schema schema]
    (d/empty-db schema)
    (d/empty-db)))

(defn ^:export init_db [datoms & [schema]]
  (if-some [schema schema]
    (d/init-db datoms schema)
    (d/init-db datoms)))

(def ^:export touch d/touch)
(def ^:export entity_db d/entity-db)

(defn ^:export entity
  {:inline
   (fn [database entity-ref]
     (list 'datascript.core/entity database entity-ref))}
  [database entity-ref]
  (d/entity database entity-ref))

(def ^:export filter d/filter)
(def ^:export is_filtered d/is-filtered)

(defn ^:export create_conn [& [schema]]
  (if-some [schema schema]
    (d/create-conn schema)
    (d/create-conn)))

(def ^:export conn_from_db d/conn-from-db)

(defn ^:export conn_from_datoms
  ([datoms]
   (conn_from_db (init_db datoms)))
  ([datoms schema]
   (conn_from_db (init_db datoms schema))))

(defn ^:export db
  [connection]
  (conn/current-db connection))

(def ^:export listen d/listen!)
(def ^:export unlisten d/unlisten!)

(defn ^:export resolve_tempid
  [^:map<Datascript_runtime.Data_value.t;int> tempids
   ^:Datascript_runtime.Data_value.t tempid]
  (get tempids tempid))

(defn ^:export index_range
  {:inline
   (fn [database attr start end]
     (list
      'datascript.core/index-range
      database
      attr
      start
      end))}
  [database attr start end]
  (d/index-range database attr start end))

(defn ^:export squuid []
  (str (d/squuid)))

(defn ^:export squuid_time_millis
  [^:string uuid-string]
  #?(:native
     (d/squuid-time-millis (uuid uuid-string))
     :melange
     (*
      (Lg_runtime.Runtime_string.parse_float_radix
       (subs uuid-string 0 8)
       16)
      1000.0)))
