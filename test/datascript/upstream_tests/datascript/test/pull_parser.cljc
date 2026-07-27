(ns datascript.test.pull-parser
  (:require
    [clojure.test :as t :refer [is are deftest testing]]
    [datascript.core :as d]
    [datascript.db :as db]
    [datascript.pull-parser :as dpp]
    [datascript.test.core :as tdc]))

(def *db
  (delay
    (d/empty-db
      {:ref            {:db/valueType :db.type/ref}
       :ref2           {:db/valueType :db.type/ref}
       :ref3           {:db/valueType :db.type/ref}
       :ns/ref         {:db/valueType :db.type/ref}
       :multival       {:db/cardinality :db.cardinality/many}
       :multiref       {:db/valueType :db.type/ref
                        :db/cardinality :db.cardinality/many}
       :component      {:db/valueType :db.type/ref
                        :db/isComponent true}
       :multicomponent {:db/valueType :db.type/ref
                        :db/isComponent true
                        :db/cardinality :db.cardinality/many}})))

(defn increment-pull-value
  [^:option<Datascript_runtime.Data_value.t> value]
  value)

(defn datom-pull-value?
  [^:option<Datascript_runtime.Data_value.t> _value]
  (Some (Datascript_runtime.Data_value.Bool false)))

(defmacro pattern [& key-values]
  (let [lookup
        (fn [key]
          (loop [remaining key-values]
            (if (empty? remaining)
              nil
              (if (= key (first remaining))
                (second remaining)
                (recur (nnext remaining))))))
        attrs (or (lookup :attrs) (lookup :reverse-attrs) [])
        wildcard (boolean (lookup :wildcard?))]
    `(dpp/pattern ~attrs ~wildcard)))

(defmacro attr [attr-name & key-values]
  (let [lookup
        (fn [key]
          (loop [remaining key-values]
            (if (empty? remaining)
              nil
              (if (= key (first remaining))
                (second remaining)
                (recur (nnext remaining))))))
        present?
        (fn [key]
          (loop [remaining key-values]
            (if (empty? remaining)
              false
              (if (= key (first remaining))
                true
                (recur (nnext remaining))))))
        reverse? (boolean (lookup :reverse?))
        source-name
        (if reverse?
          (cond
            (= attr-name :ref) :_ref
            (= attr-name :ref2) :_ref2
            (= attr-name :ref3) :_ref3
            (= attr-name :component) :_component
            (= attr-name :ns/ref) :ns/_ref
            :else attr-name)
          attr-name)
        recursion-limit (lookup :recursion-limit)
        recursive? (boolean (lookup :recursive?))
        initial
        (if recursive?
          (if (nil? recursion-limit)
            `(dpp/recursive-attribute
              (db/database-view @*db)
              ~source-name)
            `(dpp/recursive-attribute-with-limit
              (db/database-view @*db)
              ~source-name
              ~recursion-limit))
          `(dpp/attribute
            (db/database-view @*db)
            ~source-name))
        with-pattern
        (if (present? :pattern)
          (if (nil? (lookup :pattern))
            initial
            `(dpp/with-pattern ~initial ~(lookup :pattern)))
          initial)
        with-alias
        (if (present? :as)
          `(dpp/with-alias ~with-pattern ~(lookup :as))
          with-pattern)
        with-limit
        (if (present? :limit)
          `(dpp/with-limit
            ~with-alias
            ~(if (nil? (lookup :limit))
               'None
               `(Some ~(lookup :limit))))
          with-alias)
        default-value
        (fn [value]
          (cond
            (nil? value)
            (list 'Datascript_runtime.Data_value.Nil)
            (keyword? value)
            `(Datascript_runtime.Data_value.Keyword ~(str value))
            (string? value)
            `(Datascript_runtime.Data_value.String ~value)
            (integer? value)
            `(Datascript_runtime.Data_value.Int ~value)
            (= true value)
            '(Datascript_runtime.Data_value.Bool true)
            (= false value)
            '(Datascript_runtime.Data_value.Bool false)
            :else
            value))
        with-default
        (if (present? :default)
          `(dpp/with-default
            ~with-limit
            ~(default-value (lookup :default)))
          with-limit)]
    (if (present? :xform)
      (let [xform (lookup :xform)]
        `(dpp/with-xform
          ~with-default
          ~(if (= xform 'db/datom?)
             'datascript.test.pull-parser/datom-pull-value?
             'datascript.test.pull-parser/increment-pull-value)))
      with-default)))

(defmacro source-pattern [source]
  (let [unquote-form
        (fn [form]
          (if (and (seq? form) (= 'quote (first form)))
            (second form)
            form))
        data-value-form
        (fn [value]
          (cond
            (nil? value)
            '(Datascript_runtime.Data_value.Nil)
            (keyword? value)
            `(Datascript_runtime.Data_value.Keyword ~(str value))
            (string? value)
            `(Datascript_runtime.Data_value.String ~value)
            (= true value)
            '(Datascript_runtime.Data_value.Bool true)
            (= false value)
            '(Datascript_runtime.Data_value.Bool false)
            :else
            `(Datascript_runtime.Data_value.Int ~value)))
        attr-spec
        (fn attr-spec [source-attr]
          (let [source-attr (unquote-form source-attr)]
            (if (keyword? source-attr)
              [source-attr []]
              (let [operation
                    (unquote-form (first source-attr))]
                (if (or
                     (= operation 'limit)
                     (= operation "limit")
                     (= operation 'default)
                     (= operation "default"))
                  (let [[attr options] (attr-spec (second source-attr))
                        option
                        (if (or
                             (= operation 'limit)
                             (= operation "limit"))
                          :limit
                          :default)]
                    [attr
                     (conj
                      options
                      [option (first (nnext source-attr))])])
                  (let [[attr options] (attr-spec operation)]
                    [attr
                     (loop [remaining (next source-attr)
                            result options]
                       (if (empty? remaining)
                         result
                         (recur
                          (nnext remaining)
                          (conj
                           result
                           [(first remaining)
                            (second remaining)]))))]))))))
        option-form
        (fn [[option value]]
          (if (= option :as)
            `(dpp/option-alias ~value)
            (if (= option :limit)
              (if (nil? value)
                '(dpp/option-unlimited)
                `(dpp/option-limit ~value))
              (if (= option :default)
                `(dpp/option-default ~(data-value-form value))
                (let [xform (unquote-form value)]
                  `(dpp/option-xform
                    ~(if (= xform 'datascript.db/datom?)
                       'datascript.test.pull-parser/datom-pull-value?
                       'datascript.test.pull-parser/increment-pull-value)))))))
        source-item
        (fn source-item [item]
          (let [item (unquote-form item)]
            (cond
              (or (= item '*) (= item "*"))
              'dpp/source-wildcard

              (keyword? item)
              `(dpp/source-attribute ~item)

              (map? item)
              `(dpp/source-group
                ~(list
                  'vec
                  (cons
                   'list
                   (map
                    (fn [entry]
                      (let [[source-attr options]
                            (attr-spec (first entry))
                            option-forms
                            (vec (map option-form options))
                            nested (unquote-form (second entry))]
                        (if (or
                             (vector? nested)
                             (seq? nested))
                          (if (empty? option-forms)
                            `(dpp/source-nested
                              ~source-attr
                              ~(list
                                'vec
                                (cons
                                 'list
                                 (map source-item nested))))
                            `(dpp/source-nested-options
                              ~source-attr
                              ~(list
                                'vec
                                (cons 'list option-forms))
                              ~(list
                                'vec
                                (cons
                                 'list
                                 (map source-item nested)))))
                          (if (empty? option-forms)
                            `(dpp/source-recursion
                              ~source-attr
                              ~(if (or
                                    (= nested '...)
                                    (= nested "..."))
                                 'None
                                 `(Some ~nested)))
                            `(dpp/source-recursion-options
                              ~source-attr
                              ~(list
                                'vec
                                (cons 'list option-forms))
                              ~(if (or
                                    (= nested '...)
                                    (= nested "..."))
                                 'None
                                 `(Some ~nested)))))))
                    item))))

              (or (vector? item) (seq? item))
              (let [[source-attr options] (attr-spec item)]
                `(dpp/source-options
                  ~source-attr
                  ~(list
                    'vec
                    (cons
                     'list
                     (map option-form options)))))

              :else
              `(dpp/source-attribute ~item))))]
    (let [source (unquote-form source)]
      (list
       'vec
       (cons 'list (map source-item source))))))

(defmacro invalid-source-pattern [source]
  (let [unquote-form
        (fn [form]
          (if (and (seq? form) (= 'quote (first form)))
            (second form)
            form))
        data-form
        (fn data-form [value]
          (let [value (unquote-form value)]
            (cond
              (nil? value)
              '(Datascript_runtime.Data_value.Nil)
              (keyword? value)
              `(Datascript_runtime.Data_value.Keyword ~(str value))
              (string? value)
              `(Datascript_runtime.Data_value.String ~value)
              (symbol? value)
              `(Datascript_runtime.Data_value.Symbol ~(str value))
              (= true value)
              '(Datascript_runtime.Data_value.Bool true)
              (= false value)
              '(Datascript_runtime.Data_value.Bool false)
              (vector? value)
              `(Datascript_runtime.Data_value.Vector
                ~(cons 'list (map data-form value)))
              (seq? value)
              `(Datascript_runtime.Data_value.List
                ~(cons 'list (map data-form value)))
              (map? value)
              `(Datascript_runtime.Data_value.Map
                ~(cons
                  'list
                  (map
                   (fn [entry]
                     `(tuple
                       ~(data-form (first entry))
                       ~(data-form (second entry))))
                   value)))
              :else
              `(Datascript_runtime.Data_value.Int ~value))))]
    (let [source (unquote-form source)]
      (list
       'vec
       (cons
        'list
        (map
         (fn [fragment]
           `(dpp/source-invalid ~(data-form fragment)))
         source))))))

(defn optional-data-value-equal?
  [^:option<Datascript_runtime.Data_value.t> left
   ^:option<Datascript_runtime.Data_value.t> right]
  (match left
    None (= right None)
    (Some left)
    (match right
      None false
      (Some right)
      (Datascript_runtime.Data_value.equal left right))))

(declare pull-pattern-equal?)

(defn pull-attr-equal?
  [^datascript.pull-parser/pull-attr left
   ^datascript.pull-parser/pull-attr right]
  (let [left-data (dpp/attr-data left)
        right-data (dpp/attr-data right)]
    (and
     (Datascript_runtime.Data_value.equal
      (.-alias left-data)
      (.-alias right-data))
     (optional-data-value-equal?
      (.-default left-data)
      (.-default right-data))
     (= (.-limit left-data) (.-limit right-data))
     (= (.-name left-data) (.-name right-data))
     (= (.-recursion-limit left-data)
        (.-recursion-limit right-data))
     (= (.-recursive left-data) (.-recursive right-data))
     (= (.-reverse left-data) (.-reverse right-data))
     (= (.-multival left-data) (.-multival right-data))
     (= (.-ref left-data) (.-ref right-data))
     (= (.-component left-data) (.-component right-data))
     (match (dpp/attr-pattern left)
       None (= None (dpp/attr-pattern right))
       (Some left-pattern)
       (match (dpp/attr-pattern right)
         None false
         (Some right-pattern)
         (pull-pattern-equal? left-pattern right-pattern))))))

(defn pull-attrs-equal?
  [^:vector<datascript.pull-parser/pull-attr> left
   ^:vector<datascript.pull-parser/pull-attr> right]
  (and
   (= (count left) (count right))
   (loop [index 0]
     (if (= index (count left))
       true
       (if
         (pull-attr-equal? (nth left index) (nth right index))
         (recur (inc index))
         false)))))

(defn pull-pattern-equal?
  [^datascript.pull-parser/PullPattern left
   ^datascript.pull-parser/PullPattern right]
  (and
   (= (:wildcard left) (:wildcard right))
   (pull-attrs-equal? (:attrs left) (:attrs right))
   (pull-attrs-equal?
    (:reverse-attrs left)
    (:reverse-attrs right))))

(deftest test-public-check
  (testing "true conditions return without changing control flow"
    (dpp/check
     true
     "a valid fragment"
     (Datascript_runtime.Data_value.Keyword ":ok"))
    (is true))
  (testing "false conditions preserve the upstream fragment error"
    (is
     (thrown-msg?
      "Expected a valid fragment, got: :bad"
      (dpp/check
       false
       "a valid fragment"
       (Datascript_runtime.Data_value.Keyword ":bad"))))))

(deftest test-public-parse-attr-spec
  (let [database (db/database-view @*db)
        name-spec (dpp/attr-name-spec :normal)
        expr-spec
        (dpp/attr-expr-spec
         name-spec
         [(dpp/option-alias :first)
          (dpp/option-alias :second)
          (dpp/option-default
           (Datascript_runtime.Data_value.String "fallback"))])
        expected
        (attr :normal :as :second :default "fallback")]
    (match (dpp/parse-attr-spec database name-spec)
      None (is false)
      (Some parsed)
      (is (pull-attr-equal? (attr :normal) parsed)))
    (is (= None (dpp/parse-attr-expr database name-spec)))
    (match (dpp/parse-attr-expr database expr-spec)
      None (is false)
      (Some parsed)
      (is (pull-attr-equal? expected parsed)))
    (match (dpp/parse-attr-spec database expr-spec)
      None (is false)
      (Some parsed)
      (is (pull-attr-equal? expected parsed)))
    (is
     (=
      None
      (dpp/parse-attr-spec
       database
       (dpp/invalid-attr-spec
        (Datascript_runtime.Data_value.Int 1)))))
    (is
     (thrown-msg?
      "Expected [attr-name attr-option+] | ['limit attr-name (positive-num | nil)] | ['default attr-name any-val], got: []"
      (dpp/parse-attr-spec
       database
       (dpp/invalid-attr-spec
        (Datascript_runtime.Data_value.Vector (list))))))))

(deftest test-public-parse-legacy-attr-spec
  (let [database (db/database-view @*db)
        multival (dpp/attr-name-spec :multival)
        limited (dpp/legacy-limit-spec multival (Some 3))
        unlimited (dpp/legacy-limit-spec multival None)
        defaulted
        (dpp/legacy-default-spec
         (dpp/attr-name-spec :normal)
         (Datascript_runtime.Data_value.String "fallback"))]
    (is
     (=
      None
      (dpp/parse-legacy-limit-expr database multival)))
    (match (dpp/parse-legacy-limit-expr database limited)
      None (is false)
      (Some parsed)
      (is (pull-attr-equal? (attr :multival :limit 3) parsed)))
    (match (dpp/parse-attr-spec database unlimited)
      None (is false)
      (Some parsed)
      (is (pull-attr-equal? (attr :multival :limit nil) parsed)))
    (is
     (=
      None
      (dpp/parse-legacy-default-expr database multival)))
    (match (dpp/parse-legacy-default-expr database defaulted)
      None (is false)
      (Some parsed)
      (is
       (pull-attr-equal?
        (attr :normal :default "fallback")
        parsed)))
    (is
     (thrown-msg?
      "Pull limit requires :db.cardinality/many"
      (dpp/parse-attr-spec
       database
       (dpp/legacy-limit-spec
        (dpp/attr-name-spec :normal)
        (Some 1)))))))

(deftest test-public-parse-map-spec
  (let [database (db/database-view @*db)
        ref-spec (dpp/attr-name-spec :ref)
        nested
        (dpp/parse-map-spec
         database
         ref-spec
         (dpp/map-pattern-value
          [(dpp/source-attribute :normal)]))
        recursive
        (dpp/parse-map-spec
         database
         (dpp/attr-expr-spec
          ref-spec
          [(dpp/option-alias :recursive-ref)])
         (dpp/map-recursion-value None))
        limited
        (dpp/parse-map-spec
         database
         ref-spec
         (dpp/map-recursion-value (Some 2)))]
    (is
     (pull-attr-equal?
      (attr
       :ref
       :ref? true
       :pattern (pattern :attrs [(attr :normal)]))
      nested))
    (is
     (pull-attr-equal?
      (attr
       :ref
       :ref? true
       :as :recursive-ref
       :recursive? true)
      recursive))
    (is
     (pull-attr-equal?
      (attr
       :ref
       :ref? true
       :recursive? true
       :recursion-limit 2)
      limited))
    (is
     (thrown-msg?
      "Nested pull pattern requires :db.type/ref"
      (dpp/parse-map-spec
       database
       (dpp/attr-name-spec :normal)
       (dpp/map-pattern-value []))))
    (is
     (thrown-msg?
      "Recursive pull limit must be positive"
      (dpp/parse-map-spec
       database
       ref-spec
       (dpp/map-recursion-value (Some 0)))))))

(deftest test-parse-pattern
  (are [pattern expected]
    (pull-pattern-equal?
     expected
     (dpp/parse-pattern @*db (source-pattern pattern)))
    [:normal]    (pattern :attrs [(attr :normal)])
    ['(:normal)] (pattern :attrs [(attr :normal)])
    [[:normal]]  (pattern :attrs [(attr :normal)])
    [:db/id]     (pattern :attrs [(attr :db/id)])

    ; wildcards
    ['*]         (pattern :attrs [(attr :db/id)] :wildcard? true)
    ["*"]        (pattern :attrs [(attr :db/id)] :wildcard? true)
    ['* :normal] (pattern :attrs [(attr :normal) (attr :db/id)] :wildcard? true)
    ['* :db/id]  (pattern :attrs [(attr :db/id)] :wildcard? true)
    ['* [:db/id :as :xxx]] (pattern :attrs [(attr :db/id :as :xxx)] :wildcard? true)

    ; refs
    [:ref]        (pattern :attrs [(attr :ref, :ref? true)])
    [:_ref]       (pattern :reverse-attrs [(attr :ref, :ref? true, :as :_ref, :reverse? true)])
    [:component]  (pattern :attrs [(attr :component, :ref? true, :component? true, :pattern dpp/default-pattern-component)])
    [:_component] (pattern :reverse-attrs [(attr :component, :ref? true, :component? true, :as :_component, :reverse? true)])

    ; reverse
    [:_ref]    (pattern :reverse-attrs [(attr :ref, :as :_ref, :ref? true, :reverse? true)])
    [:ns/_ref] (pattern :reverse-attrs [(attr :ns/ref, :as :ns/_ref, :ref? true, :reverse? true)])

    ; sorting
    [:c :b :a]            (pattern :attrs [(attr :a) (attr :b) (attr :c)])
    [:ref2 :ref3 :ref]    (pattern :attrs [(attr :ref, :ref? true) (attr :ref2, :ref? true) (attr :ref3, :ref? true)])
    [:_ref2 :_ref3 :_ref] (pattern :reverse-attrs [(attr :ref,  :ref? true, :as :_ref, :reverse? true)
                                                   (attr :ref2, :ref? true, :as :_ref2, :reverse? true)
                                                   (attr :ref3, :ref? true, :as :_ref3, :reverse? true)])
    [:ref2 '(:ref3 :as :ref) '(:ref :as :ref3)] (pattern :attrs [(attr :ref, :ref? true, :as :ref3)
                                                                 (attr :ref2, :ref? true)
                                                                 (attr :ref3, :ref? true, :as :ref)])

    ; as
    ['(:normal :as :normal2)]  (pattern :attrs [(attr :normal :as :normal2)])
    ['(:normal :as "normal2")] (pattern :attrs [(attr :normal :as "normal2")])
    ['(:normal :as 123)]       (pattern :attrs [(attr :normal :as 123)])
    ['(:normal :as nil)]       (pattern :attrs [(attr :normal :as nil)])
    ['(:ns/_ref :as :ns/ref)]  (pattern :reverse-attrs [(attr :ns/ref, :as :ns/ref, :ref? true, :reverse? true)])
    ['(:db/id :as :id)]        (pattern :attrs [(attr :db/id :as :id)])

    ; limit
    [:multival]                (pattern :attrs [(attr :multival, :multival? true, :limit 1000)])
    ['(:multival :limit 100)]  (pattern :attrs [(attr :multival, :multival? true, :limit 100)])
    ['(limit :multival 100)]   (pattern :attrs [(attr :multival, :multival? true, :limit 100)])
    ['(limit :multival nil)]   (pattern :attrs [(attr :multival, :multival? true, :limit nil)])
    ['("limit" :multival 100)] (pattern :attrs [(attr :multival, :multival? true, :limit 100)])
    [['limit :multival 100]]   (pattern :attrs [(attr :multival, :multival? true, :limit 100)])

    ; default
    ['(:multival :default :xyz)]  (pattern :attrs [(attr :multival, :multival? true, :limit 1000, :default :xyz)])
    ['(default :multival :xyz)]   (pattern :attrs [(attr :multival, :multival? true, :limit 1000, :default :xyz)])
    ['("default" :multival :xyz)] (pattern :attrs [(attr :multival, :multival? true, :limit 1000, :default :xyz)])
    [['default :multival :xyz]]   (pattern :attrs [(attr :multival, :multival? true, :limit 1000, :default :xyz)])

    ; xform
    [[:normal :xform 'inc]] (pattern :attrs [(attr :normal :xform inc)])
    [[:normal :xform inc]] (pattern :attrs [(attr :normal :xform inc)])
    #?@(:clj [[[:normal :xform 'datascript.db/datom?]] (pattern :attrs [(attr :normal :xform db/datom?)])])

    ; combined
    ['(:multival :limit 100 :default :xyz :as :other :xform inc)] (pattern :attrs [(attr :multival, :multival? true, :default :xyz, :limit 100, :as :other, :xform inc)])
    ['(:multival :xform inc :as :other :default :xyz :limit 100)] (pattern :attrs [(attr :multival, :multival? true, :default :xyz, :limit 100, :as :other, :xform inc)])
    ['((:multival :limit 100) :default :xyz)] (pattern :attrs [(attr :multival, :multival? true, :default :xyz, :limit 100)])
    ['((:multival :default :xyz) :limit 100)] (pattern :attrs [(attr :multival, :multival? true, :default :xyz, :limit 100)])

    ; combined
    ['(limit (default :multival :xyz) 100)] (pattern :attrs [(attr :multival, :multival? true, :default :xyz, :limit 100)])
    ['(default (limit :multival 100) :xyz)] (pattern :attrs [(attr :multival, :multival? true, :default :xyz, :limit 100)])
    ['(limit (:multival :default :xyz) 100)] (pattern :attrs [(attr :multival, :multival? true, :default :xyz, :limit 100)])
    ['(default (:multival :limit 100) :xyz)] (pattern :attrs [(attr :multival, :multival? true, :default :xyz, :limit 100)])
    ['(((limit :multival 100) :default :xyz))] (pattern :attrs [(attr :multival, :multival? true, :default :xyz, :limit 100)])
    ['(((default :multival :xyz) :limit 100))] (pattern :attrs [(attr :multival, :multival? true, :default :xyz, :limit 100)])
    
    ; repeated
    [:multival [:multival :default :xyz] [:multival :limit 100]] (pattern :attrs [(attr :multival, :multival? true, :limit 100)])
    [:ref {:ref '...}] (pattern :attrs [(attr :ref, :ref? true, :pattern nil, :recursive? true, :recursion-limit nil)])
    [{:ref '...} :ref] (pattern :attrs [(attr :ref, :ref? true)])
    
    ; map spec
    [{:ref [:normal]}]                    (pattern :attrs [(attr :ref, :ref? true, :pattern (pattern :attrs [(attr :normal)]))])
    [{:_ref [:normal]}]                   (pattern :reverse-attrs [(attr :ref, :as :_ref, :ref? true, :reverse? true, :pattern (pattern :attrs [(attr :normal)]))])
    [{:ref '[*]}]                         (pattern :attrs [(attr :ref, :ref? true, :pattern (pattern :wildcard? true, :attrs [(attr :db/id)]))])
    [{:ref [{:ref2 [{:ref3 '[*]}]}]}]     (pattern :attrs [(attr :ref, :ref? true, :pattern (pattern :attrs [(attr :ref2, :ref? true, :pattern (pattern :attrs [(attr :ref3, :ref? true, :pattern (pattern :wildcard? true, :attrs [(attr :db/id)]))]))]))])
    [{:ref [:normal] :ref2 [:normal2]}]   (pattern :attrs [(attr :ref, :ref? true, :pattern (pattern :attrs [(attr :normal)])) (attr :ref2, :ref? true, :pattern (pattern :attrs [(attr :normal2)]))])
    [{:ref [:normal]} {:ref2 [:normal2]}] (pattern :attrs [(attr :ref, :ref? true, :pattern (pattern :attrs [(attr :normal)])) (attr :ref2, :ref? true, :pattern (pattern :attrs [(attr :normal2)]))])
    [{'(:multiref :limit 100) [:normal]}] (pattern :attrs [(attr :multiref, :ref? true, :multival? true, :limit 100, :pattern (pattern :attrs [(attr :normal)]))])
    [{'(limit :multiref 100) [:normal]}]  (pattern :attrs [(attr :multiref, :ref? true, :multival? true, :limit 100, :pattern (pattern :attrs [(attr :normal)]))])
    [{:component 1}]                      (pattern :attrs [(attr :component, :ref? true, :component? true, :pattern nil, :recursive? true, :recursion-limit 1)])

    ; map spec limits
    [{:ref 100}]   (pattern :attrs         [(attr :ref,            :ref? true,                 :pattern nil, :recursive? true, :recursion-limit 100)])
    [{:ref '...}]  (pattern :attrs         [(attr :ref,            :ref? true,                 :pattern nil, :recursive? true, :recursion-limit nil)]) 
    [{:ref "..."}] (pattern :attrs         [(attr :ref,            :ref? true,                 :pattern nil, :recursive? true, :recursion-limit nil)])
    [{:_ref 100}]  (pattern :reverse-attrs [(attr :ref, :as :_ref, :ref? true, :reverse? true, :pattern nil, :recursive? true, :recursion-limit 100)])
    [{:_ref '...}] (pattern :reverse-attrs [(attr :ref, :as :_ref, :ref? true, :reverse? true, :pattern nil, :recursive? true, :recursion-limit nil)]))

  (testing "Error reporting"
    (are [pattern msg]
      (thrown-msg?
       msg
       (dpp/parse-pattern
        @*db
        (invalid-source-pattern pattern)))
      ; refs
      [:_normal] "Expected reverse attribute having :db.type/ref, got: :_normal"

      ; attr-expr
      ['(:multival :limit)] "Expected even number of opts, got: (:multival :limit)"
      
      ; limit
      ['(limit :multival)] "Expected ['limit attr-name (positive-number | nil)], got: (limit :multival)"
      ['(:normal :limit 100)] "Expected limit attribute having :db.cardinality/many, got: :normal"      
      ['(limit :normal 100)]  "Expected limit attribute having :db.cardinality/many, got: :normal"
      ['(:multival :limit :abc)] "Expected (positive-number | nil), got: :abc"
      ['(limit :multival :abc)]  "Expected (positive-number | nil), got: :abc"

      ; default
      ['(default :normal)] "Expected ['default attr-name any-value], got: (default :normal)"
      ['(default :normal 1 2)] "Expected ['default attr-name any-value], got: (default :normal 1 2)"

      ; xform
      [[:normal :xform 'unknown]] "Can't resolve symbol unknown"

      ; map spec
      [{:normal [:normal2]}] "Expected attribute having :db.type/ref, got: :normal"
      [{'(:ref :limit 100) [:normal]}] "Expected limit attribute having :db.cardinality/many, got: :ref"
      [{:ref :normal}] "Expected pattern to be sequential?, got: :normal")))
