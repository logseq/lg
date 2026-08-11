(ns datascript.test.core
  (:require
   [datascript.core :as d]
   [datascript.lg.query-types :as query-types]))

(def available true)

(defn no-namespace-maps [run]
  (run))

(defn ^:Datascript_runtime.Data_value.t string-value [^:string value]
  (Datascript_runtime.Data_value.String value))

(defn ^:Datascript_runtime.Data_value.t int-value [^:int value]
  (Datascript_runtime.Data_value.Int value))

(defn ^:Datascript_runtime.Data_value.t keyword-value [^:keyword value]
  (Datascript_runtime.Data_value.Keyword (str value)))

(defn ^:Datascript_runtime.Data_value.t entity-id-value [^:int value]
  (Datascript_runtime.Data_value.Ref_to
   (Datascript_runtime.Data_value.Entity_id value)))

(defn ^:Datascript_runtime.Data_value.t query-result-value
  [^datascript.lg.query-types/result result]
  (match result
    (Datascript_runtime.Query_value.Entity value)
    (Datascript_runtime.Data_value.Int value)
    (Datascript_runtime.Query_value.Attr value)
    (Datascript_runtime.Data_value.Keyword value)
    (Datascript_runtime.Query_value.Value value) value
    (Datascript_runtime.Query_value.Metadata value _) value
    (Datascript_runtime.Query_value.Pull value) value
    (Datascript_runtime.Query_value.Added value)
    (Datascript_runtime.Data_value.Bool value)
    (Datascript_runtime.Query_value.Database _)
    (Stdlib.invalid_arg
     "Database query results cannot be compared as EDN values")
    (Datascript_runtime.Query_value.Callable _)
    (Stdlib.invalid_arg
     "Callable query results cannot be compared as EDN values")))

(defn query-result-row-equal?
  [^:array<datascript.lg.query-types/result> actual
   ^:array<Datascript_runtime.Data_value.t> expected]
  (if (= (alength actual) (alength expected))
    (loop [index 0]
      (if (< index (alength actual))
        (if
         (Datascript_runtime.Data_value.equal
          (query-result-value (aget actual index))
          (aget expected index))
          (recur (inc index))
          false)
        true))
    false))

(defn query-result-rows-contain?
  [^:vector<array<datascript.lg.query-types/result>> rows
   ^:array<Datascript_runtime.Data_value.t> expected]
  (if-some [row (first rows)]
    (if (query-result-row-equal? row expected)
      true
      (query-result-rows-contain? (subvec rows 1) expected))
    false))

(defn query-relation-equal-closed?
  [^datascript.lg.query-types/output output
   ^:vector<array<Datascript_runtime.Data_value.t>> expected]
  (if-some [rows (query-types/output-relation output)]
    (and
     (= (count rows) (count expected))
     (every?
      (fn [^:array<Datascript_runtime.Data_value.t> row]
        (query-result-rows-contain? rows row))
      expected))
    false))

(defn query-relation?
  {:inline
   (fn [output rows]
     (let [value-form
           (fn value-form [value]
             (if (nil? value)
               'Datascript_runtime.Data_value.Nil
               (if (keyword? value)
                 (list
                  'Datascript_runtime.Data_value.Keyword
                  (str value))
                 (if (vector? value)
                   (list
                    'Datascript_runtime.Data_value.vector_of_vector
                    (vec (map value-form value)))
                   (if (map? value)
                     (list
                      'Datascript_runtime.Data_value.map_of_keyword_map
                      (reduce
                       (fn [entries entry]
                         (assoc
                          entries
                          (first entry)
                          (value-form (second entry))))
                       {}
                       value))
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
                       (if (float? value)
                         (list
                          'Datascript_runtime.Data_value.Float
                          value)
                         (list
                          'Datascript_runtime.Data_value.Int
                          value))))))))))]
       (list
        'datascript.test.core/query-relation-equal-closed?
        output
        (vec
         (map
          (fn [row]
            (list
             'to-array
             (vec (map value-form row))))
          rows)))))}
  [^datascript.lg.query-types/output output
   ^:vector<array<Datascript_runtime.Data_value.t>> expected]
  (query-relation-equal-closed? output expected))

(defn query-collection-equal-closed?
  [^datascript.lg.query-types/output output
   ^:vector<Datascript_runtime.Data_value.t> expected]
  (if-some [actual (query-types/output-collection output)]
    (and
     (= (count actual) (count expected))
     (every?
      (fn [^:Datascript_runtime.Data_value.t value]
        (some
         (fn [^datascript.lg.query-types/result result]
           (Datascript_runtime.Data_value.equal
            (query-result-value result)
            value))
         actual))
      expected))
    false))

(defn query-collection?
  {:inline
   (fn [output values]
     (let [value-form
           (fn value-form [value]
             (if (nil? value)
               'Datascript_runtime.Data_value.Nil
               (if (keyword? value)
                 (list
                  'Datascript_runtime.Data_value.Keyword
                  (str value))
                 (if (vector? value)
                   (list
                    'Datascript_runtime.Data_value.vector_of_vector
                    (vec (map value-form value)))
                   (if (map? value)
                     (list
                      'Datascript_runtime.Data_value.map_of_keyword_map
                      (reduce
                       (fn [entries entry]
                         (assoc
                          entries
                          (first entry)
                          (value-form (second entry))))
                       {}
                       value))
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
                           (if (float? value)
                             (list
                              'Datascript_runtime.Data_value.Float
                              value)
                             (list
                              'Datascript_runtime.Data_value.Int
                              value))))))))))]
       (list
        'datascript.test.core/query-collection-equal-closed?
        output
        (vec (map value-form values)))))}
  [^datascript.lg.query-types/output output
   ^:vector<Datascript_runtime.Data_value.t> expected]
  (query-collection-equal-closed? output expected))

(type-variant datom-component
  (Entity :int)
  (Attribute :keyword)
  (Value :Datascript_runtime.Data_value.t))

(defn all-datoms [^datascript.db/DB db]
  (into
   #{}
   (map
    (fn [^datascript.db/Datom datom]
      [(Entity (.-e datom))
       (Attribute (.-a datom))
       (Value (.-v datom))]))
   (d/datoms db :eavt)))
