(ns ^:no-doc datascript.pull-parser
  (:require
   [clojure.string :as str]
   [datascript.db :as db]))

(type-alias pull-xform
  :fn<option<Datascript_runtime.Data_value.t>;option<Datascript_runtime.Data_value.t>>)

(type-record PullAttrData
  (alias :Datascript_runtime.Data_value.t)
  (recursion-key :int)
  (default :option<Datascript_runtime.Data_value.t>)
  (limit :option<int>)
  (name :keyword)
  (recursion-limit :option<int>)
  (recursive :bool)
  (reverse :bool)
  (xform :option<pull-xform>)
  (multival :bool)
  (ref :bool)
  (component :bool))

(def attr-key-counter (atom 0))

(defn ^int next-attr-key []
  (swap! attr-key-counter inc))

(type-variant pull-attr
  (PullAttribute :datascript.pull-parser/PullAttrData)
  (PullNested
   :datascript.pull-parser/PullAttrData
   :vector<pull-attr>
   :option<pull-attr>
   :option<pull-attr>
   :vector<pull-attr>
   :bool))

(type-record PullPattern
  (attrs :vector<pull-attr>)
  (first-attr :option<pull-attr>)
  (last-attr :option<pull-attr>)
  (reverse-attrs :vector<pull-attr>)
  (wildcard :bool))

(type-record PullAttr
  (as :Datascript_runtime.Data_value.t)
  (default :option<Datascript_runtime.Data_value.t>)
  (limit :option<int>)
  (name :keyword)
  (pattern :option<PullPattern>)
  (recursion-limit :option<int>)
  (recursive? :option<bool>)
  (reverse? :option<bool>)
  (xform :pull-xform)
  (multival? :option<bool>)
  (ref? :option<bool>)
  (component? :option<bool>))

(type-variant pull-source-option
  (PullOptionAlias :Datascript_runtime.Data_value.t)
  (PullOptionDefault :Datascript_runtime.Data_value.t)
  (PullOptionLimit :option<int>)
  (PullOptionXform :pull-xform))

(type-variant pull-source-item
  (PullSourceAttribute :keyword)
  (PullSourceOptions
   :keyword
   :vector<pull-source-option>)
  (PullSourceNested
   :keyword
   :vector<pull-source-item>)
  (PullSourceNestedOptions
   :keyword
   :vector<pull-source-option>
   :vector<pull-source-item>)
  (PullSourceRecursion
   :keyword
   :option<int>)
  (PullSourceRecursionOptions
   :keyword
   :vector<pull-source-option>
   :option<int>)
  (PullSourceGroup :vector<pull-source-item>)
  (PullSourceInvalid :Datascript_runtime.Data_value.t)
  PullSourceWildcard)

(type-variant pull-attr-spec
  (PullAttrNameSpec :keyword)
  (PullAttrExprSpec
   :pull-attr-spec
   :vector<pull-source-option>)
  (PullLegacyLimitSpec
   :pull-attr-spec
   :option<int>)
  (PullLegacyDefaultSpec
   :pull-attr-spec
   :Datascript_runtime.Data_value.t)
  (PullInvalidAttrSpec
   :Datascript_runtime.Data_value.t))

(type-variant pull-map-value
  (PullMapPatternValue :vector<pull-source-item>)
  (PullMapRecursionValue :option<int>))

(defn attr-name-spec [attr]
  (PullAttrNameSpec attr))

(defn attr-expr-spec
  [attr options]
  (PullAttrExprSpec attr options))

(defn legacy-limit-spec
  [attr limit]
  (PullLegacyLimitSpec attr limit))

(defn legacy-default-spec
  [attr default]
  (PullLegacyDefaultSpec attr default))

(defn invalid-attr-spec
  [fragment]
  (PullInvalidAttrSpec fragment))

(defn map-pattern-value
  [pattern]
  (PullMapPatternValue pattern))

(defn map-recursion-value
  [limit]
  (PullMapRecursionValue limit))

(defn source-attribute [source-attr]
  (PullSourceAttribute source-attr))

(def source-wildcard PullSourceWildcard)

(defn source-default
  [source-attr default]
  (PullSourceOptions
   source-attr
   [(PullOptionDefault default)]))

(defn source-alias-value
  [source-attr alias]
  (PullSourceOptions
   source-attr
   [(PullOptionAlias alias)]))

(defn source-alias
  {:inline
   (fn [source-attr alias]
     (let [alias-form
           (if (nil? alias)
             (list 'Datascript_runtime.Data_value.Nil)
             (if (string? alias)
               (list
                'Datascript_runtime.Data_value.String alias)
               (if (keyword? alias)
                 (list
                  'Datascript_runtime.Data_value.Keyword
                  (str alias))
                 (if (or (symbol? alias) (seq? alias))
                   alias
                   (list
                    'Datascript_runtime.Data_value.Int
                    alias)))))]
       (list
        'datascript.pull-parser/source-alias-value
        source-attr
        alias-form)))}
  [source-attr alias]
  (source-alias-value source-attr alias))

(defn source-limit
  [source-attr limit]
  (PullSourceOptions
   source-attr
   [(PullOptionLimit (Some limit))]))

(defn source-xform
  [source-attr xform]
  (PullSourceOptions
   source-attr
   [(PullOptionXform xform)]))

(defn option-alias-value
  [alias]
  (PullOptionAlias alias))

(defn option-alias
  {:inline
   (fn [alias]
     (let [alias-form
           (if (nil? alias)
             (list 'Datascript_runtime.Data_value.Nil)
             (if (string? alias)
               (list
                'Datascript_runtime.Data_value.String alias)
               (if (keyword? alias)
                 (list
                  'Datascript_runtime.Data_value.Keyword
                  (str alias))
                 (if (or (symbol? alias) (seq? alias))
                   alias
                   (list
                    'Datascript_runtime.Data_value.Int
                    alias)))))]
       (list
        'datascript.pull-parser/option-alias-value
        alias-form)))}
  [alias]
  (option-alias-value alias))

(defn option-default
  [default]
  (PullOptionDefault default))

(defn option-limit [limit]
  (PullOptionLimit (Some limit)))

(defn option-unlimited []
  (PullOptionLimit None))

(defn option-xform [xform]
  (PullOptionXform xform))

(defn source-options
  [source-attr options]
  (PullSourceOptions source-attr options))

(defn source-nested
  [source-attr source-pattern]
  (PullSourceNested source-attr source-pattern))

(defn source-nested-options
  [source-attr options source-pattern]
  (PullSourceNestedOptions source-attr options source-pattern))

(defn source-recursion
  [source-attr limit]
  (PullSourceRecursion source-attr limit))

(defn source-recursion-options
  [source-attr options limit]
  (PullSourceRecursionOptions source-attr options limit))

(defn source-group
  [items]
  (PullSourceGroup items))

(defn source-invalid
  [fragment]
  (PullSourceInvalid fragment))

(def default-db-id-attr
  (PullAttribute
   (record PullAttrData
    (alias
     (Datascript_runtime.Data_value.Keyword ":db/id"))
    (recursion-key 0)
    (default None)
    (limit None)
    (name :db/id)
    (recursion-limit None)
    (recursive false)
    (reverse false)
    (xform None)
    (multival false)
    (ref false)
    (component false))))

(def default-pattern-ref
  (record PullPattern
    (attrs [default-db-id-attr])
    (first-attr None)
    (last-attr None)
    (reverse-attrs [])
    (wildcard false)))

(def default-pattern-component
  (record PullPattern
    (attrs [default-db-id-attr])
    (first-attr None)
    (last-attr None)
    (reverse-attrs [])
    (wildcard true)))

(defn attribute
  [database source-attr]
  (let [reverse (db/reverse-ref? source-attr)
        name (if reverse (db/reverse-ref source-attr) source-attr)
        ref (db/database-view-ref? database name)
        component (db/database-view-component? database name)
        multival (db/database-view-multival? database name)]
    (when (and reverse (not ref))
      (Stdlib.invalid_arg
       "Reverse pull attribute requires :db.type/ref"))
    (let [data
          (record PullAttrData
            (alias
             (Datascript_runtime.Data_value.Keyword
              (str source-attr)))
            (recursion-key (next-attr-key))
            (default None)
            (limit (if multival (Some 1000) None))
            (name name)
            (recursion-limit None)
            (recursive false)
            (reverse reverse)
            (xform None)
            (multival multival)
            (ref ref)
            (component component))]
      (if ref
        (let [default-pattern
              (if (and component (not reverse))
                default-pattern-component
                default-pattern-ref)]
          (PullNested
           data
           (:attrs default-pattern)
           (:first-attr default-pattern)
           (:last-attr default-pattern)
           (:reverse-attrs default-pattern)
           (:wildcard default-pattern)))
        (PullAttribute data)))))

(defn identity-value
  [value]
  value)

(defn attr-data [attr]
  (match attr
    (PullAttribute data) data
    (PullNested data _ _ _ _ _) data))

(defn replace-attr-data
  [attr data]
  (match attr
    (PullAttribute _) (PullAttribute data)
    (PullNested _ attrs first-attr last-attr reverse-attrs wildcard)
    (PullNested
     data attrs first-attr last-attr reverse-attrs wildcard)))

(defn attr-pattern [attr]
  (match attr
    (PullAttribute _) None
    (PullNested _ attrs first-attr last-attr reverse-attrs wildcard)
    (Some
     (record PullPattern
       (attrs attrs)
       (first-attr first-attr)
       (last-attr last-attr)
       (reverse-attrs reverse-attrs)
       (wildcard wildcard)))))

(defn parse-attr-name
  [database source-attr]
  (let [attr (attribute database source-attr)
        data (attr-data attr)]
    (record PullAttr
      (as (.-alias data))
      (default (.-default data))
      (limit (.-limit data))
      (name (.-name data))
      (pattern (attr-pattern attr))
      (recursion-limit (.-recursion-limit data))
      (recursive? (if (.-recursive data) (Some true) None))
      (reverse? (if (.-reverse data) (Some true) None))
      (xform identity-value)
      (multival? (if (.-multival data) (Some true) None))
      (ref? (if (.-ref data) (Some true) None))
      (component? (if (.-component data) (Some true) None)))))

(defn attr-pattern-wildcard [attr]
  (match attr
    (PullAttribute _) false
    (PullNested _ _ _ _ _ wildcard) wildcard))

(defn with-pattern
  [attr pattern]
  (let [data (attr-data attr)]
    (when-not (.-ref data)
      (Stdlib.invalid_arg
       "Nested pull pattern requires :db.type/ref"))
    (PullNested
     data
     (:attrs pattern)
     (:first-attr pattern)
     (:last-attr pattern)
     (:reverse-attrs pattern)
     (:wildcard pattern))))

(defn with-default
  [attr default]
  (replace-attr-data
   attr
   (assoc (attr-data attr) :default (Some default))))

(defn with-alias-value
  [attr alias]
  (replace-attr-data
   attr
   (assoc (attr-data attr) :alias alias)))

(defn with-alias
  {:inline
   (fn [attr alias]
     (let [alias-form
           (if (nil? alias)
             (list 'Datascript_runtime.Data_value.Nil)
             (if (string? alias)
               (list
                'Datascript_runtime.Data_value.String alias)
               (if (keyword? alias)
                 (list
                  'Datascript_runtime.Data_value.Keyword
                  (str alias))
                 (if (or (symbol? alias) (seq? alias))
                   alias
                   (list
                    'Datascript_runtime.Data_value.Int
                    alias)))))]
       (list
        'datascript.pull-parser/with-alias-value
        attr
        alias-form)))}
  [attr alias]
  (with-alias-value attr alias))

(defn with-limit
  [attr limit]
  (let [data (attr-data attr)]
    (when-not (.-multival data)
      (Stdlib.invalid_arg
       "Pull limit requires :db.cardinality/many"))
    (match limit
      (Some limit)
      (when-not (pos? limit)
        (Stdlib.invalid_arg
         "Pull limit must be positive"))
      None (Stdlib.ignore 0))
    (replace-attr-data
     attr
     (assoc data :limit limit))))

(defn with-xform
  [attr xform]
  (replace-attr-data
   attr
   (assoc (attr-data attr) :xform (Some xform))))

(defn apply-source-option
  [attr option]
  (match option
    (PullOptionAlias alias)
    (with-alias attr alias)
    (PullOptionDefault default)
    (with-default attr default)
    (PullOptionLimit limit)
    (with-limit attr limit)
    (PullOptionXform xform)
    (with-xform attr xform)))

(defn with-recursion
  [attr recursion-limit]
  (let [data (attr-data attr)]
    (when-not (.-ref data)
      (Stdlib.invalid_arg
       "Recursive pull attribute requires :db.type/ref"))
    (match recursion-limit
      None (Stdlib.ignore 0)
      (Some limit)
      (when-not (pos? limit)
        (Stdlib.invalid_arg
         "Recursive pull limit must be positive")))
    (PullAttribute
     (record PullAttrData
      (alias (.-alias data))
      (recursion-key (.-recursion-key data))
      (default (.-default data))
      (limit (.-limit data))
      (name (.-name data))
      (recursion-limit recursion-limit)
      (recursive true)
      (reverse (.-reverse data))
      (xform (.-xform data))
      (multival (.-multival data))
      (ref true)
      (component (.-component data))))))

(signature datascript.pull-parser/parse-attr-expr
  :fn<datascript.db/database-view;pull-attr-spec;option<pull-attr>>)
(signature datascript.pull-parser/parse-legacy-limit-expr
  :fn<datascript.db/database-view;pull-attr-spec;option<pull-attr>>)
(signature datascript.pull-parser/parse-legacy-default-expr
  :fn<datascript.db/database-view;pull-attr-spec;option<pull-attr>>)
(signature datascript.pull-parser/parse-attr-spec
  :fn<datascript.db/database-view;pull-attr-spec;option<pull-attr>>)
(signature datascript.pull-parser/parse-map-spec
  :fn<datascript.db/database-view;pull-attr-spec;pull-map-value;pull-attr>)
(signature datascript.pull-parser/parse-pattern-view
  :fn<datascript.db/database-view;vector<pull-source-item>;PullPattern>)

(declare parse-attr-spec)
(declare parse-pattern-view)

(defn ^:option<pull-attr> parse-attr-expr
  [^datascript.db/database-view database
   ^pull-attr-spec attr-spec]
  (match attr-spec
    (PullAttrExprSpec base options)
    (if-some [attr (parse-attr-spec database base)]
      (Some (reduce apply-source-option attr options))
      None)
    _ None))

(defn ^:option<pull-attr> parse-legacy-limit-expr
  [^datascript.db/database-view database
   ^pull-attr-spec attr-spec]
  (match attr-spec
    (PullLegacyLimitSpec base limit)
    (if-some [attr (parse-attr-spec database base)]
      (Some (with-limit attr limit))
      None)
    _ None))

(defn ^:option<pull-attr> parse-legacy-default-expr
  [^datascript.db/database-view database
   ^pull-attr-spec attr-spec]
  (match attr-spec
    (PullLegacyDefaultSpec base default)
    (if-some [attr (parse-attr-spec database base)]
      (Some (with-default attr default))
      None)
    _ None))

(defn ^:option<pull-attr> parse-attr-spec
  [^datascript.db/database-view database
   ^pull-attr-spec attr-spec]
  (match attr-spec
    (PullAttrNameSpec attr)
    (Some (attribute database attr))
    (PullAttrExprSpec _ _)
    (parse-attr-expr database attr-spec)
    (PullLegacyLimitSpec _ _)
    (parse-legacy-limit-expr database attr-spec)
    (PullLegacyDefaultSpec _ _)
    (parse-legacy-default-expr database attr-spec)
    (PullInvalidAttrSpec fragment)
    (if-some
      [_items
       (Datascript_runtime.Data_value.sequential_items fragment)]
      (do
        (check
         false
         "[attr-name attr-option+] | ['limit attr-name (positive-num | nil)] | ['default attr-name any-val]"
         fragment)
        None)
      None)))

(defn ^pull-attr parse-map-spec
  [^datascript.db/database-view database
   ^pull-attr-spec attr-spec
   ^pull-map-value value]
  (let [attr
        (match (parse-attr-spec database attr-spec)
          None
          (Stdlib.invalid_arg "Expected attr-name | attr-expr")
          (Some attr) attr)]
    (match value
      (PullMapPatternValue source)
      (with-pattern attr (parse-pattern-view database source))
      (PullMapRecursionValue limit)
      (with-recursion attr limit))))

(defn- ^pull-attr required-attr-spec
  [^datascript.db/database-view database
   ^pull-attr-spec attr-spec]
  (match (parse-attr-spec database attr-spec)
    None (Stdlib.invalid_arg "Expected pull attribute")
    (Some attr) attr))

(defn ^pull-attr recursive-attribute
  [^datascript.db/database-view database ^:keyword source-attr]
  (with-recursion (attribute database source-attr) None))

(defn ^pull-attr recursive-attribute-with-limit
  [^datascript.db/database-view database
   ^:keyword source-attr
   ^int limit]
  (when-not (pos? limit)
    (Stdlib.invalid_arg
     "Recursive pull limit must be positive"))
  (with-recursion
   (attribute database source-attr)
   (Some limit)))

(defn ^:vector<pull-attr> upsert-attr
  [^:vector<pull-attr> attrs ^pull-attr attr]
  (let [alias (.-alias (attr-data attr))]
    (loop [index 0]
      (if (= index (count attrs))
        (conj attrs attr)
        (if
          (Datascript_runtime.Data_value.equal
           alias
           (.-alias (attr-data (nth attrs index))))
          (assoc attrs index attr)
          (recur (inc index)))))))

(defn ^PullPattern pattern
  [^:vector<pull-attr> attrs ^boolean wildcard]
  (let [attrs (reduce upsert-attr [] attrs)
        attrs
        (if (and
             wildcard
             (not
              (some
               (fn [^pull-attr attr]
                 (= :db/id (.-name (attr-data attr))))
               attrs)))
          (conj attrs default-db-id-attr)
          attrs)
        key-fn
        (fn [^pull-attr attr]
          (.-name (attr-data attr)))
        forward-attrs
        (vec
         (sort-by
          key-fn
          (filter
           (fn [^pull-attr attr]
             (not (.-reverse (attr-data attr))))
           attrs)))
        reverse-attrs
        (vec
         (sort-by
          key-fn
          (filter
           (fn [^pull-attr attr]
             (.-reverse (attr-data attr)))
           attrs)))
        datom-attrs
        (vec
         (filter
          (fn [^pull-attr attr]
            (not (= :db/id (.-name (attr-data attr)))))
          forward-attrs))]
    (record PullPattern
      (attrs forward-attrs)
      (first-attr (first datom-attrs))
      (last-attr (last datom-attrs))
      (reverse-attrs reverse-attrs)
      (wildcard wildcard))))

(defn ^PullPattern nested-pattern
  [^:vector<pull-attr> attrs ^boolean wildcard]
  (pattern attrs wildcard))

(defn ^PullPattern recursive-pattern
  [^datascript.db/database-view database
   ^:vector<keyword> attrs
   ^:keyword recursive-attr
   ^boolean wildcard]
  (pattern
   (conj
    (mapv
     (fn [^:keyword attr]
       (attribute database attr))
     attrs)
    (recursive-attribute database recursive-attr))
   wildcard))

(defn ^:string source-fragment-string
  [^:Datascript_runtime.Data_value.t fragment]
  (Datascript_runtime.Data_value.to_edn_string fragment))

(defn check
  [^boolean condition
   ^:string expected
   ^:Datascript_runtime.Data_value.t fragment]
  :unit
  (when-not condition
    (Stdlib.invalid_arg
     (str
      "Expected "
      expected
      ", got: "
      (source-fragment-string fragment))))
  (Stdlib.ignore 0))

(type-variant source-operation-kind
  SourceLimit
  SourceDefault
  SourceOther)

(defn ^source-operation-kind source-operation
  [^:Datascript_runtime.Data_value.t fragment]
  (match fragment
    (Datascript_runtime.Data_value.Symbol value)
    (cond
      (= value 'limit) SourceLimit
      (= value 'default) SourceDefault
      :else SourceOther)
    (Datascript_runtime.Data_value.String value)
    (cond
      (= value "limit") SourceLimit
      (= value "default") SourceDefault
      :else SourceOther)
    _ SourceOther))

(defn ^:option<keyword> source-attr-name
  [^:Datascript_runtime.Data_value.t fragment]
  (match fragment
    (Datascript_runtime.Data_value.Keyword value)
    (Some value)
    _
    (if-some
      [items
       (Datascript_runtime.Data_value.sequential_items fragment)]
      (if (empty? items)
        None
        (let [first-item (nth items 0)]
          (match (source-operation first-item)
            SourceLimit
            (if (< (count items) 2)
              None
              (source-attr-name (nth items 1)))
            SourceDefault
            (if (< (count items) 2)
              None
              (source-attr-name (nth items 1)))
            SourceOther
            (source-attr-name first-item))))
      None)))

(defn validate-source-limit
  [^datascript.db/database-view database
   ^:keyword attr
   ^:Datascript_runtime.Data_value.t limit]
  :unit
  (let [valid-limit
        (match limit
          (Datascript_runtime.Data_value.Nil) true
          (Datascript_runtime.Data_value.Int value) (pos? value)
          _ false)]
    (when-not valid-limit
      (Stdlib.invalid_arg
       (str
        "Expected (positive-number | nil), got: "
        (source-fragment-string limit))))
    (when-not (db/database-view-multival? database attr)
      (Stdlib.invalid_arg
       (str
        "Expected limit attribute having :db.cardinality/many, got: "
        attr))))
  (Stdlib.ignore 0))

(defn validate-invalid-attr-options
  [^datascript.db/database-view database
   ^:keyword attr
   ^:vector<Datascript_runtime.Data_value.t> items
   ^:Datascript_runtime.Data_value.t fragment]
  :unit
  (let [option-count (dec (count items))]
    (when (odd? option-count)
      (Stdlib.invalid_arg
       (str
        "Expected even number of opts, got: "
        (source-fragment-string fragment))))
    (loop [index 1]
      (if (< index (count items))
        (let [option (nth items index)
              value (nth items (inc index))]
          (match option
            (Datascript_runtime.Data_value.Keyword option-name)
            (cond
              (= option-name :limit)
              (validate-source-limit database attr value)
              (= option-name :xform)
              (match value
                (Datascript_runtime.Data_value.Symbol symbol)
                (when (= symbol 'unknown)
                  (Stdlib.invalid_arg
                   "Can't resolve symbol unknown"))
                _ (Stdlib.ignore 0))
              :else
              (Stdlib.ignore 0))
            _ (Stdlib.ignore 0))
          (recur (+ index 2)))
        (Stdlib.ignore 0))))
  (Stdlib.ignore 0))

(defn raise-invalid-source
  [^datascript.db/database-view database
   ^:Datascript_runtime.Data_value.t fragment]
  :unit
  (if-some [items
            (Datascript_runtime.Data_value.sequential_items fragment)]
    (if (empty? items)
      (Stdlib.invalid_arg
       (str
        "Expected attr-name, got: "
        (source-fragment-string fragment)))
      (let [operation (source-operation (nth items 0))]
        (match operation
          SourceLimit
          (if-not (= 3 (count items))
            (Stdlib.invalid_arg
             (str
              "Expected ['limit attr-name (positive-number | nil)], got: "
              (source-fragment-string fragment)))
            (match (source-attr-name (nth items 1))
              None
              (Stdlib.invalid_arg
               (str
                "Expected attr-name, got: "
                (source-fragment-string (nth items 1))))
              (Some attr)
              (validate-source-limit database attr (nth items 2))))

          SourceDefault
          (when-not (= 3 (count items))
            (Stdlib.invalid_arg
             (str
              "Expected ['default attr-name any-value], got: "
              (source-fragment-string fragment))))

          SourceOther
          (match (source-attr-name (nth items 0))
            None
            (Stdlib.invalid_arg
             (str
              "Expected attr-name, got: "
              (source-fragment-string (nth items 0))))
            (Some attr)
            (validate-invalid-attr-options
             database attr items fragment)))))
    (match fragment
      (Datascript_runtime.Data_value.Keyword value)
      (let [attr value]
        (when (and
               (db/reverse-ref? attr)
               (not
                (db/database-view-ref?
                 database
                 (db/reverse-ref attr))))
          (Stdlib.invalid_arg
           (str
            "Expected reverse attribute having :db.type/ref, got: "
            attr))))

      (Datascript_runtime.Data_value.Map _)
      (let [entries
            (match
              (Datascript_runtime.Data_value.map_entries fragment)
              None
              (Stdlib.invalid_arg
               "Expected serialized pull map entries")
              (Some entries) entries)
            entry (nth entries 0)
            attr-form (tuple-get entry 0)
            nested (tuple-get entry 1)]
        (match (source-attr-name attr-form)
          None
          (Stdlib.invalid_arg
           (str
            "Expected attr-name | attr-expr, got: "
            (source-fragment-string attr-form)))
          (Some attr)
          (do
            (if-some
              [attr-items
               (Datascript_runtime.Data_value.sequential_items attr-form)]
              (validate-invalid-attr-options
               database attr attr-items attr-form)
              (Stdlib.ignore 0))
            (when-not (db/database-view-ref? database attr)
              (Stdlib.invalid_arg
               (str
                "Expected attribute having :db.type/ref, got: "
                (source-fragment-string attr-form))))
            (when-not
              (or
               (if-some
                 [_items
                  (Datascript_runtime.Data_value.sequential_items nested)]
                 true
                 false)
               (match nested
                 (Datascript_runtime.Data_value.Int value)
                 (pos? value)
                 (Datascript_runtime.Data_value.Symbol value)
                 (= value '...)
                 (Datascript_runtime.Data_value.String value)
                 (= value "...")
                 _ false))
              (Stdlib.invalid_arg
               (str
                "Expected pattern to be sequential?, got: "
                (source-fragment-string nested)))))))

      _
      (Stdlib.invalid_arg
       (str
        "Expected pull pattern fragment, got: "
        (source-fragment-string fragment)))))
  (Stdlib.ignore 0))

(defn ^PullPattern parse-pattern-items
  [^datascript.db/database-view database
   ^:vector<pull-source-item> items
   ^int index
   ^:vector<pull-attr> attrs
   ^boolean wildcard]
  (if (= index (count items))
    (pattern attrs wildcard)
    (match (nth items index)
      PullSourceWildcard
      (parse-pattern-items database items (inc index) attrs true)

      (PullSourceGroup group-items)
      (parse-pattern-items
       database
       (vec
        (concat
         group-items
         (subvec items (inc index))))
       0
       attrs
       wildcard)

      (PullSourceInvalid fragment)
      (do
        (raise-invalid-source database fragment)
        (pattern attrs wildcard))

      (PullSourceAttribute source-attr)
      (parse-pattern-items
       database
       items
       (inc index)
       (conj
        attrs
        (required-attr-spec
         database
         (attr-name-spec source-attr)))
       wildcard)

      (PullSourceOptions source-attr options)
      (parse-pattern-items
       database
       items
       (inc index)
       (conj
        attrs
        (required-attr-spec
         database
         (attr-expr-spec
          (attr-name-spec source-attr)
          options)))
       wildcard)

      (PullSourceNested source-attr source-pattern)
      (parse-pattern-items
       database
       items
       (inc index)
       (conj
        attrs
        (parse-map-spec
         database
         (attr-name-spec source-attr)
         (map-pattern-value source-pattern)))
       wildcard)

      (PullSourceNestedOptions source-attr options source-pattern)
      (parse-pattern-items
       database
       items
       (inc index)
       (conj
        attrs
        (parse-map-spec
         database
         (attr-expr-spec
          (attr-name-spec source-attr)
          options)
         (map-pattern-value source-pattern)))
       wildcard)

      (PullSourceRecursion source-attr limit)
      (parse-pattern-items
       database
       items
       (inc index)
       (conj
        attrs
        (parse-map-spec
         database
         (attr-name-spec source-attr)
         (map-recursion-value limit)))
       wildcard)

      (PullSourceRecursionOptions source-attr options limit)
      (parse-pattern-items
       database
       items
       (inc index)
       (conj
        attrs
        (parse-map-spec
         database
         (attr-expr-spec
          (attr-name-spec source-attr)
          options)
         (map-recursion-value limit)))
       wildcard))))

(defn ^PullPattern parse-pattern-view
  [^datascript.db/database-view database
   ^:vector<pull-source-item> source]
  (parse-pattern-items database source 0 [] false))

(defn parse-pattern
  {:inline
   (fn [database source]
     (list
      'datascript.pull-parser/parse-pattern-view
      (list
       'datascript.db/database-view
       database)
      source))}
  [^datascript.db/database-view database
   ^:vector<pull-source-item> source]
  (parse-pattern-view database source))
