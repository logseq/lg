(ns ^:no-doc datascript.parser
  #?(:cljs (:require-macros [datascript.parser :refer [deftrecord]]))
  (:require
    [clojure.set :as set]
    [datascript.db :as db]
    [datascript.util :as util]))

;; utils

(declare parse-binding)

#?(:clj
   (defmacro deftrecord
     "Define a closed parser record."
     [tagname fields & rest]
     `(defrecord ~tagname ~fields ~@rest)))

(type-alias data-value :Datascript_runtime.Data_value.t)

(defn ^:option<vector<Datascript_runtime.Data_value.t>> data-value-items
  [^:Datascript_runtime.Data_value.t form]
  (Datascript_runtime.Data_value.sequential_items form))

(defn ^boolean of-size?
  [^:Datascript_runtime.Data_value.t form ^:int size]
  (if-some [items (data-value-items form)]
    (= (count items) size)
    false))

(signature datascript.parser/parse-items [parsed]
  :fn<fn<data-value;option<parsed>>;vector<data-value>;option<vector<parsed>>>)

(defn- parse-items [parse-element items]
  (loop [remaining items
         parsed []]
    (if-some [item (first remaining)]
      (if-some [element (parse-element item)]
        (recur (subvec remaining 1) (conj parsed element))
        None)
      (Some parsed))))

(signature datascript.parser/parse-seq [parsed]
  :fn<fn<data-value;option<parsed>>;data-value;option<vector<parsed>>>)

(defn parse-seq [parse-element form]
  (if-some [items (data-value-items form)]
    (parse-items parse-element items)
    None))

(signature datascript.parser/value-present? [value]
  :fn<value;vector<value>;bool>)

(defn- value-present? [value values]
  (loop [remaining values]
    (if-some [candidate (first remaining)]
      (if (= candidate value)
        true
        (recur (subvec remaining 1)))
      false)))

(signature datascript.parser/distinct? [value]
  :fn<seqable<value>;bool>)

(defn distinct? [values]
  (loop [remaining (vec values)
         seen []]
    (if-some [value (first remaining)]
      (if (value-present? value seen)
        false
        (recur (subvec remaining 1) (conj seen value)))
      true)))

(defn with-source [obj _source]
  obj)

(defn source [obj]
  obj)

;; placeholder    = the symbol '_'
;; variable       = symbol starting with "?"
;; src-var        = symbol starting with "$"
;; rules-var      = the symbol "%"
;; constant       = any non-variable data literal
;; plain-symbol   = symbol that does not begin with "$" or "?"

(deftrecord Placeholder [])
(deftrecord Variable    [^:symbol symbol])
(deftrecord SrcVar      [^:symbol symbol])
(deftrecord DefaultSrc  [])
(deftrecord RulesVar    [])
(deftrecord Constant    [^:Datascript_runtime.Data_value.t value])
(deftrecord PlainSymbol [^:symbol symbol])

(defn ^:option<datascript.parser/Placeholder> parse-placeholder
  [^:Datascript_runtime.Data_value.t form]
  (match form
    (Datascript_runtime.Data_value.Symbol value)
    (if (= value "_") (Some (Placeholder.)) None)
    _ None))

(defn ^:option<datascript.parser/Variable> parse-variable
  [^:Datascript_runtime.Data_value.t form]
  (match form
    (Datascript_runtime.Data_value.Symbol value)
    (if (= (String.get value 0) \?)
      (Some (Variable. value))
      None)
    _ None))

(defn ^datascript.parser/Variable parse-var-required
  [^:Datascript_runtime.Data_value.t form]
  (if-some [variable (parse-variable form)]
    variable
    (util/raise "Cannot parse var, expected symbol starting with ?"
      {:error :parser/rule-var})))

(defn ^:vector<datascript.parser/Variable> parse-required-variables
  [^:vector<Datascript_runtime.Data_value.t> items]
  (mapv parse-var-required items))

(defn ^:option<datascript.parser/SrcVar> parse-src-var
  [^:Datascript_runtime.Data_value.t form]
  (match form
    (Datascript_runtime.Data_value.Symbol value)
    (if (= (String.get value 0) \$)
      (Some (SrcVar. value))
      None)
    _ None))

(defn ^:option<datascript.parser/RulesVar> parse-rules-var
  [^:Datascript_runtime.Data_value.t form]
  (match form
    (Datascript_runtime.Data_value.Symbol value)
    (if (= value "%") (Some (RulesVar.)) None)
    _ None))

(defn ^:option<Datascript_runtime.Data_value.t> parse-constant
  [^:Datascript_runtime.Data_value.t form]
  (if-some [_ (parse-variable form)]
    None
    (Some form)))

(defn ^:option<datascript.parser/PlainSymbol> parse-plain-symbol
  [^:Datascript_runtime.Data_value.t form]
  (match form
    (Datascript_runtime.Data_value.Symbol value)
    (if (and (not= (String.get value 0) \?)
             (not= (String.get value 0) \$)
             (not= value "%")
             (not= value "_"))
      (Some (PlainSymbol. value))
      None)
    _ None))

(defn ^:option<datascript.parser/Variable> parse-plain-variable
  [^:Datascript_runtime.Data_value.t form]
  (if-some [symbol (parse-plain-symbol form)]
    (Some (Variable. (:symbol symbol)))
    None))



;; fn-arg = (variable | constant | src-var)

(type-variant fn-arg
  (FnArgVariable :datascript.parser/Variable)
  (FnArgSource :datascript.parser/SrcVar)
  (FnArgConstant :data-value))

(defn ^:option<datascript.parser/fn-arg> parse-fn-arg
  [^:Datascript_runtime.Data_value.t form]
  (if-some [value (parse-variable form)]
    (Some (FnArgVariable value))
    (if-some [value (parse-src-var form)]
      (Some (FnArgSource value))
      (if-some [value (parse-constant form)]
        (Some (FnArgConstant value))
        None))))

(defn ^:option<vector<datascript.parser/fn-arg>> parse-fn-args
  [^:vector<Datascript_runtime.Data_value.t> items]
  (parse-items parse-fn-arg items))

;; rule-vars = [ variable+ | ([ variable+ ] variable*) ]

(deftrecord RuleVars
  [^:option<vector<datascript.parser/Variable>> required
   ^:vector<datascript.parser/Variable> free])

(defn ^boolean variables-distinct?
  [^:vector<datascript.parser/Variable> variables]
  (distinct? variables))

(defn ^datascript.parser/RuleVars parse-rule-var-items
  [^:vector<Datascript_runtime.Data_value.t> items]
  (if-some [first-item (first items)]
    (let [required-items (data-value-items first-item)
          required* (match required-items
                      None None
                      (Some values)
                      (Some (parse-required-variables values)))
          free* (parse-required-variables
                  (match required-items
                    None items
                    (Some _) (subvec items 1)))
          required-values (match required*
                            None []
                            (Some values) values)]
      (when (and (empty? required-values) (empty? free*))
        (util/raise
          "Cannot parse rule-vars, expected [ variable+ | ([ variable+ ] variable*) ]"
          {:error :parser/rule-vars}))
      (when-not (variables-distinct? (into required-values free*))
        (util/raise "Rule variables should be distinct"
          {:error :parser/rule-vars}))
      (RuleVars. required* free*))
    (util/raise
      "Cannot parse rule-vars, expected [ variable+ | ([ variable+ ] variable*) ]"
      {:error :parser/rule-vars})))

(defn ^datascript.parser/RuleVars parse-rule-vars
  [^:Datascript_runtime.Data_value.t form]
  (if-some [items (data-value-items form)]
    (parse-rule-var-items items)
    (util/raise "Cannot parse rule-vars, expected [ variable+ | ([ variable+ ] variable*) ]"
      {:error :parser/rule-vars})))

(defn ^:vector<symbol> flatten-rule-vars
  [^datascript.parser/RuleVars rule-vars]
  (vec
   (concat
    (match (:required rule-vars)
      None []
      (Some values) (mapv :symbol values))
    (mapv :symbol (:free rule-vars)))))

(defn ^:tuple<int;int> rule-vars-arity
  [^datascript.parser/RuleVars rule-vars]
  (tuple
   (match (:required rule-vars)
     None 0
     (Some values) (count values))
   (count (:free rule-vars))))

(defn ^:string join-rule-variable-names
  [^:vector<string> names]
  (if-some [first-name (first names)]
    (reduce
     (fn [^:string joined ^:string name]
       (str joined " " name))
     first-name
     (subvec names 1))
    ""))

(defn ^:string rule-vars-display
  [^datascript.parser/RuleVars rule-vars]
  (let [required-parts
        (if-some [required (.-required rule-vars)]
          [(str
            "["
            (join-rule-variable-names
             (mapv
              (fn [^datascript.parser/Variable variable]
                (str (.-symbol variable)))
              required))
            "]")]
          [])
        free-parts
        (mapv
         (fn [^datascript.parser/Variable variable]
           (str (.-symbol variable)))
         (.-free rule-vars))]
    (str
     "["
     (join-rule-variable-names
      (vec (concat required-parts free-parts)))
     "]")))


;; binding        = (bind-scalar | bind-tuple | bind-coll | bind-rel)
;; bind-scalar    = variable
;; bind-tuple     = [ (binding | '_')+ ]
;; bind-coll      = [ binding '...' ]
;; bind-rel       = [ [ (binding | '_')+ ] ]

(type-variant binding
  BindIgnore
  (BindScalar :datascript.parser/Variable)
  (BindTuple :vector<binding>)
  (BindColl :binding))

(defn ^:option<binding> parse-bind-ignore
  [^:Datascript_runtime.Data_value.t form]
  (match form
    (Datascript_runtime.Data_value.Symbol value)
    (if (= value "_") (Some BindIgnore) None)
    _ None))

(defn ^:option<binding> parse-bind-scalar
  [^:Datascript_runtime.Data_value.t form]
  (if-some [variable (parse-variable form)]
    (Some (BindScalar variable))
    None))

(defn ^:option<binding> parse-bind-coll
  [^:Datascript_runtime.Data_value.t form]
  (if-some [items (data-value-items form)]
    (if (= (count items) 2)
      (if-some [item (first items)]
        (if-some [marker (nth items 1)]
          (match marker
            (Datascript_runtime.Data_value.Symbol value)
            (if (= value "...")
              (Some (BindColl (parse-binding item)))
              None)
            _ None)
          None)
        None)
      None)
    None))

(defn ^:option<binding> parse-tuple-el
  [^:Datascript_runtime.Data_value.t form]
  (if-some [binding (parse-bind-ignore form)]
    (Some binding)
    (Some (parse-binding form))))

(defn ^:option<vector<binding>> parse-tuple-elements
  [^:vector<Datascript_runtime.Data_value.t> items]
  (loop [remaining items
         parsed []]
    (if (empty? remaining)
      (Some parsed)
      (if-some [item (first remaining)]
        (if-some [binding (parse-tuple-el item)]
          (recur (subvec remaining 1) (conj parsed binding))
          None)
        None))))

(defn ^:option<binding> parse-bind-tuple
  [^:Datascript_runtime.Data_value.t form]
  (if-some [items (data-value-items form)]
    (if-some [sub-bindings (parse-tuple-elements items)]
    (if-not (empty? sub-bindings)
      (Some (BindTuple sub-bindings))
      (util/raise "Tuple binding cannot be empty"
        {:error :parser/binding}))
    None)
    None))

(defn ^:option<binding> parse-bind-rel
  [^:Datascript_runtime.Data_value.t form]
  (if-some [items (data-value-items form)]
    (if (= (count items) 1)
      (if-some [item (first items)]
        (if-some [_ (data-value-items item)]
          (if-some [tuple-binding (parse-bind-tuple item)]
            (Some (BindColl tuple-binding))
            None)
          None)
        None)
      None)
    None))

(defn ^:binding parse-binding
  [^:Datascript_runtime.Data_value.t form]
  (if-some [binding (parse-bind-coll form)]
    binding
    (if-some [binding (parse-bind-rel form)]
      binding
      (if-some [binding (parse-bind-tuple form)]
        binding
        (if-some [binding (parse-bind-ignore form)]
          binding
          (if-some [binding (parse-bind-scalar form)]
            binding
            (util/raise
             "Cannot parse binding, expected (bind-scalar | bind-tuple | bind-coll | bind-rel)"
             {:error :parser/binding})))))))


;; find-spec        = ':find' (find-rel | find-coll | find-tuple | find-scalar)
;; find-rel         = find-elem+
;; find-coll        = [ find-elem '...' ]
;; find-scalar      = find-elem '.'
;; find-tuple       = [ find-elem+ ]
;; find-elem        = (variable | pull-expr | aggregate | custom-aggregate) 
;; pull-expr        = [ 'pull' src-var? variable pull-pattern ]
;; pull-pattern     = (constant | variable | plain-symbol)
;; aggregate        = [ aggregate-fn fn-arg+ ]
;; aggregate-fn     = plain-symbol
;; custom-aggregate = [ 'aggregate' variable fn-arg+ ]

(type-variant aggregate-function
  (AggregatePlain :datascript.parser/PlainSymbol)
  (AggregateCustom :datascript.parser/Variable))

(type-variant pull-pattern
  (PullVariable :datascript.parser/Variable)
  (PullConstant :data-value))

(deftrecord Aggregate
  [^:aggregate-function fn
   ^:vector<fn-arg> args])

(deftrecord Pull
  [^datascript.parser/SrcVar source
   ^datascript.parser/Variable variable
   ^:pull-pattern pattern])

(type-variant find-element
  (FindVariable :datascript.parser/Variable)
  (FindPullElement :datascript.parser/Pull)
  (FindAggregate :datascript.parser/Aggregate))

(deftrecord FindRel [^:vector<find-element> elements])
(deftrecord FindColl [^:find-element element])
(deftrecord FindScalar [^:find-element element])
(deftrecord FindTuple [^:vector<find-element> elements])

(type-variant find-spec
  (FindRelation :datascript.parser/FindRel)
  (FindCollection :datascript.parser/FindColl)
  (FindSingle :datascript.parser/FindScalar)
  (FindTupleResult :datascript.parser/FindTuple))

(defn fn-arg-vars [^:fn-arg arg]
  (match arg
    (FnArgVariable variable) [(:symbol variable)]
    (FnArgSource _) []
    (FnArgConstant _) []))

(defprotocol IFindVars
  (-find-vars [this] :vector<symbol>))

(extend-type Variable
  IFindVars
  (-find-vars [variable]
    [(:symbol variable)]))

(extend-type Aggregate
  IFindVars
  (-find-vars [aggregate]
    (if-some [arg (last (:args aggregate))]
      (fn-arg-vars arg)
      [])))

(extend-type Pull
  IFindVars
  (-find-vars [pull]
    [(:symbol (:variable pull))]))

(defprotocol IFindElements
  (find-elements [this] :vector<find-element>))

(extend-type FindRel
  IFindElements
  (find-elements [relation]
    (:elements relation)))

(extend-type FindColl
  IFindElements
  (find-elements [collection]
    [(:element collection)]))

(extend-type FindScalar
  IFindElements
  (find-elements [scalar]
    [(:element scalar)]))

(extend-type FindTuple
  IFindElements
  (find-elements [tuple-result]
    (:elements tuple-result)))

(signature datascript.parser/parse-aggregate
  :fn<Datascript_runtime.Data_value.t;option<datascript.parser/Aggregate>>)
(signature datascript.parser/parse-aggregate-custom
  :fn<Datascript_runtime.Data_value.t;option<datascript.parser/Aggregate>>)
(signature datascript.parser/parse-pull-expr
  :fn<Datascript_runtime.Data_value.t;option<datascript.parser/Pull>>)
(signature datascript.parser/parse-find-elem
  :fn<Datascript_runtime.Data_value.t;option<datascript.parser/find-element>>)
(signature datascript.parser/parse-find-rel
  :fn<Datascript_runtime.Data_value.t;option<datascript.parser/find-spec>>)
(signature datascript.parser/parse-find-coll
  :fn<Datascript_runtime.Data_value.t;option<datascript.parser/find-spec>>)
(signature datascript.parser/parse-find-scalar
  :fn<Datascript_runtime.Data_value.t;option<datascript.parser/find-spec>>)
(signature datascript.parser/parse-find-tuple
  :fn<Datascript_runtime.Data_value.t;option<datascript.parser/find-spec>>)
(signature datascript.parser/parse-find
  :fn<Datascript_runtime.Data_value.t;datascript.parser/find-spec>)

(defn- ^datascript.parser/Aggregate invalid-custom-aggregate []
  (Stdlib.invalid_arg
   "Cannot parse custom aggregate call, expect ['aggregate' variable fn-arg+]"))

(defn- ^datascript.parser/Pull invalid-pull-expression []
  (Stdlib.invalid_arg
   "Cannot parse pull expression, expect ['pull' src-var? variable (constant | variable | plain-symbol)]"))

(defn- ^:option<pull-pattern> parse-pull-pattern
  [^:Datascript_runtime.Data_value.t form]
  (if-some [variable (parse-variable form)]
    (Some (PullVariable variable))
    (if-some [variable (parse-plain-variable form)]
      (Some (PullVariable variable))
      (if-some [constant (parse-constant form)]
        (Some (PullConstant constant))
        None))))

(defn ^:option<datascript.parser/Aggregate> parse-aggregate
  [^:Datascript_runtime.Data_value.t form]
  (if-some [items (data-value-items form)]
    (if (>= (count items) 2)
      (if-some [function-form (first items)]
        (if-some [function-name (parse-plain-symbol function-form)]
          (if-some [arguments (parse-fn-args (subvec items 1))]
            (Some
             (Aggregate.
              (AggregatePlain function-name)
              arguments))
            None)
          None)
        None)
      None)
    None))

(defn ^:option<datascript.parser/Aggregate> parse-aggregate-custom
  [^:Datascript_runtime.Data_value.t form]
  (if-some [items (data-value-items form)]
    (if-some [head (first items)]
      (match head
        (Datascript_runtime.Data_value.Symbol value)
        (if (= value "aggregate")
          (if (>= (count items) 3)
            (if-some [function-form (nth items 1)]
              (if-some [function-name (parse-variable function-form)]
                (if-some [arguments (parse-fn-args (subvec items 2))]
                  (Some
                   (Aggregate.
                    (AggregateCustom function-name)
                    arguments))
                  (invalid-custom-aggregate))
                (invalid-custom-aggregate))
              (invalid-custom-aggregate))
            (invalid-custom-aggregate))
          None)
        _ None)
      None)
    None))

(defn ^:option<datascript.parser/Pull> parse-pull-expr
  [^:Datascript_runtime.Data_value.t form]
  (if-some [items (data-value-items form)]
    (if-some [head (first items)]
      (match head
        (Datascript_runtime.Data_value.Symbol value)
        (if (= value "pull")
          (if (and (<= 3 (count items)) (<= (count items) 4))
            (let [explicit-source? (= (count items) 4)
                  variable-index (if explicit-source? 2 1)
                  pattern-index (if explicit-source? 3 2)
                  source-form
                  (if explicit-source?
                    (nth items 1)
                    (Datascript_runtime.Data_value.Symbol "$"))
                  variable-form (nth items variable-index)
                  pattern-form (nth items pattern-index)]
              (if-some [source (parse-src-var source-form)]
                (if-some [variable (parse-variable variable-form)]
                  (if-some [pattern (parse-pull-pattern pattern-form)]
                    (Some (Pull. source variable pattern))
                    (invalid-pull-expression))
                  (invalid-pull-expression))
                (invalid-pull-expression)))
            (invalid-pull-expression))
          None)
        _ None)
      None)
    None))

(defn ^:option<datascript.parser/find-element> parse-find-elem
  [^:Datascript_runtime.Data_value.t form]
  (if-some [variable (parse-variable form)]
    (Some (FindVariable variable))
    (if-some [pull (parse-pull-expr form)]
      (Some (FindPullElement pull))
      (if-some [aggregate (parse-aggregate-custom form)]
        (Some (FindAggregate aggregate))
        (if-some [aggregate (parse-aggregate form)]
          (Some (FindAggregate aggregate))
          None)))))

(defn ^:option<vector<find-element>> parse-find-elements
  [^:vector<Datascript_runtime.Data_value.t> items]
  (loop [remaining items
         elements []]
    (if-some [item (first remaining)]
      (if-some [element (parse-find-elem item)]
        (recur (subvec remaining 1) (conj elements element))
        None)
      (Some elements))))

(defn ^:option<datascript.parser/find-spec> parse-find-rel
  [^:Datascript_runtime.Data_value.t form]
  (if-some [items (data-value-items form)]
    (if-some [elements (parse-find-elements items)]
      (Some (FindRelation (FindRel. elements)))
      None)
    None))

(defn ^:option<datascript.parser/find-spec> parse-find-coll
  [^:Datascript_runtime.Data_value.t form]
  (if-some [items (data-value-items form)]
    (if (= (count items) 1)
      (if-some [inner-form (first items)]
        (if-some [inner (data-value-items inner-form)]
          (if (= (count inner) 2)
            (if-some [marker (nth inner 1)]
              (match marker
                (Datascript_runtime.Data_value.Symbol value)
                (if (= value "...")
                  (if-some [element-form (first inner)]
                    (if-some [element (parse-find-elem element-form)]
                      (Some (FindCollection (FindColl. element)))
                      None)
                    None)
                  None)
                _ None)
              None)
            None)
          None)
        None)
      None)
    None))

(defn ^:option<datascript.parser/find-spec> parse-find-scalar
  [^:Datascript_runtime.Data_value.t form]
  (if-some [items (data-value-items form)]
    (if (= (count items) 2)
      (if-some [marker (nth items 1)]
        (match marker
          (Datascript_runtime.Data_value.Symbol value)
          (if (= value ".")
            (if-some [element-form (first items)]
              (if-some [element (parse-find-elem element-form)]
                (Some (FindSingle (FindScalar. element)))
                None)
              None)
            None)
          _ None)
        None)
      None)
    None))

(defn ^:option<datascript.parser/find-spec> parse-find-tuple
  [^:Datascript_runtime.Data_value.t form]
  (if-some [items (data-value-items form)]
    (if (= (count items) 1)
      (if-some [inner-form (first items)]
        (if-some [inner (data-value-items inner-form)]
          (if-some [elements (parse-find-elements inner)]
            (Some (FindTupleResult (FindTuple. elements)))
            None)
          None)
        None)
      None)
    None))

(defn ^:find-spec parse-find
  [^:Datascript_runtime.Data_value.t form]
  (if-some [find (parse-find-rel form)]
    find
    (if-some [find (parse-find-coll form)]
      find
      (if-some [find (parse-find-scalar form)]
        find
        (if-some [find (parse-find-tuple form)]
          find
          (util/raise
           "Cannot parse :find, expected: (find-rel | find-coll | find-tuple | find-scalar)"
           {:error :parser/find}))))))

(defn find-element-vars [^:find-element element]
  (match element
    (FindVariable variable) (-find-vars variable)
    (FindPullElement pull) (-find-vars pull)
    (FindAggregate aggregate) (-find-vars aggregate)))

(defn ^:vector<find-element> find-spec-elements [^:find-spec find]
  (match find
    (FindRelation relation) (find-elements relation)
    (FindCollection collection) (find-elements collection)
    (FindSingle scalar) (find-elements scalar)
    (FindTupleResult tuple-result) (find-elements tuple-result)))

(defn find-vars [^:find-spec find]
  (mapcat find-element-vars (find-spec-elements find)))

(defn aggregate? [^:find-element element]
  (match element
    (FindAggregate _) true
    _ false))

(defn pull? [^:find-element element]
  (match element
    (FindPullElement _) true
    _ false))

;; return-map  = (return-keys | return-syms | return-strs)
;; return-keys = ':keys' symbol+
;; return-syms = ':syms' symbol+
;; return-strs = ':strs' symbol+

(type-variant return-map
  (ReturnKeys :vector<keyword>)
  (ReturnSyms :vector<symbol>)
  (ReturnStrs :vector<string>))

(defn ^:keyword return-map-type
  [^:return-map return-map]
  (match return-map
    (ReturnKeys _) :keys
    (ReturnSyms _) :syms
    (ReturnStrs _) :strs))

(defn ^:int return-map-count
  [^:return-map return-map]
  (match return-map
    (ReturnKeys symbols) (count symbols)
    (ReturnSyms symbols) (count symbols)
    (ReturnStrs symbols) (count symbols)))

(defn ^:option<vector<string>> return-map-key-names
  [^:return-map return-map]
  (match return-map
    (ReturnKeys keys) (Some (mapv str keys))
    _ None))

(defn ^:option<vector<string>> return-map-symbol-names
  [^:return-map return-map]
  (match return-map
    (ReturnSyms keys) (Some (mapv str keys))
    _ None))

(defn ^:option<vector<string>> return-map-string-names
  [^:return-map return-map]
  (match return-map
    (ReturnStrs keys) (Some keys)
    _ None))

(defn ^:option<vector<symbol>> return-map-symbols
  [^:Datascript_runtime.Data_value.t form]
  (if-some [items (data-value-items form)]
    (reduce
     (fn [^:option<vector<symbol>> result
          ^:Datascript_runtime.Data_value.t item]
       (match result
         None None
         (Some symbols)
         (match item
           (Datascript_runtime.Data_value.Symbol symbol)
           (let [^:symbol symbol symbol]
             (Some (conj symbols symbol)))
           _ None)))
     (Some [])
     items)
    None))

(defn ^:keyword return-map-keyword [^:symbol symbol]
  (str ":" symbol))

(defn ^:option<datascript.parser/return-map> parse-return-map
  [^:keyword type ^:Datascript_runtime.Data_value.t form]
  (if-some [symbols (return-map-symbols form)]
    (when (not (empty? symbols))
      (case type
        :keys (ReturnKeys (mapv return-map-keyword symbols))
        :syms (ReturnSyms symbols)
        :strs (ReturnStrs symbols)
        nil))
    None))

;; with = [ variable+ ]

(declare parse-variables)

(defn ^:vector<datascript.parser/Variable> parse-with
  [^:Datascript_runtime.Data_value.t form]
  (if-some [items (data-value-items form)]
    (if-some [variables (parse-variables items)]
      variables
      (util/raise "Cannot parse :with clause, expected [ variable+ ]"
        {:error :parser/with, :form form}))
    (util/raise "Cannot parse :with clause, expected [ variable+ ]"
      {:error :parser/with, :form form})))


;; in = [ (src-var | rules-var | plain-symbol | binding)+ ]

(type-variant input-binding
  (InputSource :datascript.parser/SrcVar)
  (InputRules :datascript.parser/RulesVar)
  (InputValueBinding :binding))

(type-variant static-query-input
  (StaticSourceInput :datascript.parser/SrcVar)
  StaticRulesInput
  (StaticValueInput :binding))

(defn- ^:input-binding parse-in-binding
  [^:Datascript_runtime.Data_value.t form]
  (if-some [source (parse-src-var form)]
    (InputSource source)
    (if-some [rules (parse-rules-var form)]
      (InputRules rules)
      (InputValueBinding (parse-binding form)))))

(defn ^:vector<input-binding> parse-in
  [^:Datascript_runtime.Data_value.t form]
  (if-some [items (data-value-items form)]
    (mapv parse-in-binding items)
    (util/raise
     "Cannot parse :in clause, expected (src-var | % | plain-symbol | bind-scalar | bind-tuple | bind-coll | bind-rel)"
     {:error :parser/in, :form form})))

;; clause          = (data-pattern | pred-expr | fn-expr | rule-expr | not-clause | not-join-clause | or-clause | or-join-clause)
;; data-pattern    = [ src-var? (variable | constant | '_')+ ]
;; pred-expr       = [ [ pred fn-arg+ ] ]
;; pred            = (plain-symbol | variable)
;; fn-expr         = [ [ fn fn-arg+ ] binding ]
;; fn              = (plain-symbol | variable)
;; rule-expr       = [ src-var? rule-name (variable | constant | '_')+ ]
;; not-clause      = [ src-var? 'not' clause+ ]
;; not-join-clause = [ src-var? 'not-join' [ variable+ ] clause+ ]
;; or-clause       = [ src-var? 'or' (clause | and-clause)+ ]
;; or-join-clause  = [ src-var? 'or-join' rule-vars (clause | and-clause)+ ]
;; and-clause      = [ 'and' clause+ ]

(type-variant query-source
  DefaultSource
  (ExplicitSource :datascript.parser/SrcVar))

(type-variant pattern-element
  PatternPlaceholder
  (PatternVariable :datascript.parser/Variable)
  (PatternConstant :data-value))

(type-variant query-callable
  (StaticCallable :datascript.parser/PlainSymbol)
  (VariableCallable :datascript.parser/Variable))

(type-variant disjunction-kind
  PlainDisjunction
  JoinDisjunction)

(type-variant clause
  (PatternClause :query-source :vector<pattern-element>)
  (PredicateClause :query-callable :vector<fn-arg>)
  (FunctionClause :query-callable :vector<fn-arg> :binding)
  (RuleClause :query-source
              :datascript.parser/PlainSymbol
              :vector<pattern-element>)
  (NotClause :query-source
             :vector<datascript.parser/Variable>
             :vector<clause>
             :string)
  (OrClause :query-source
            :disjunction-kind
            :datascript.parser/RuleVars
            :vector<clause>
            :string)
  (AndClause :vector<clause>))

(signature datascript.parser/parse-clause
  :fn<Datascript_runtime.Data_value.t;datascript.parser/clause>)
(signature datascript.parser/parse-clauses
  :fn<vector<Datascript_runtime.Data_value.t>;option<vector<datascript.parser/clause>>>)
(signature datascript.parser/parse-and
  :fn<Datascript_runtime.Data_value.t;option<datascript.parser/clause>>)
(signature datascript.parser/parse-disjunction-clauses
  :fn<vector<Datascript_runtime.Data_value.t>;option<vector<datascript.parser/clause>>>)
(declare parse-clause parse-and)

(defn ^:option<pattern-element> parse-pattern-el
  [^:Datascript_runtime.Data_value.t form]
  (if-some [_ (parse-placeholder form)]
    (Some PatternPlaceholder)
    (if-some [value (parse-variable form)]
      (Some (PatternVariable value))
      (if-some [value (parse-constant form)]
        (Some (PatternConstant value))
        None))))

(defn ^:option<vector<pattern-element>> parse-pattern-elements
  [^:vector<Datascript_runtime.Data_value.t> items]
  (loop [remaining items
         parsed []]
    (if (empty? remaining)
      (Some parsed)
      (if-some [item (first remaining)]
        (if-some [element (parse-pattern-el item)]
          (recur (subvec remaining 1) (conj parsed element))
          None)
        None))))

(defn ^:option<tuple<query-source;vector<Datascript_runtime.Data_value.t>>>
  take-source
  [^:Datascript_runtime.Data_value.t form]
  (if-some [items (data-value-items form)]
    (if-some [head (first items)]
      (if-some [source (parse-src-var head)]
        (Some (tuple (ExplicitSource source) (subvec items 1)))
        (Some (tuple DefaultSource items)))
      (Some (tuple DefaultSource items)))
    None))
      
(defn ^:option<datascript.parser/clause> parse-pattern
  [^:Datascript_runtime.Data_value.t form]
  (if-some [source-and-form (take-source form)]
    (let [source (tuple-get source-and-form 0)
          next-form (tuple-get source-and-form 1)]
      (if-some [pattern (parse-pattern-elements next-form)]
        (if-not (empty? pattern)
          (Some (PatternClause source pattern))
          (util/raise "Pattern could not be empty"
            {:error :parser/where, :form form}))
        None))
    None))

(defn ^:option<tuple<query-callable;vector<fn-arg>>> parse-call
  [^:Datascript_runtime.Data_value.t form]
  (if-some [items (data-value-items form)]
    (if-some [fn-form (first items)]
      (let [callable
            (if-some [value (parse-plain-symbol fn-form)]
              (Some (StaticCallable value))
              (if-some [value (parse-variable fn-form)]
                (Some (VariableCallable value))
                None))]
        (if-some [callable callable]
          (if-some [args* (parse-fn-args (subvec items 1))]
            (Some (tuple callable args*))
            None)
          None))
      None)
    None))

(defn ^:option<datascript.parser/clause> parse-pred
  [^:Datascript_runtime.Data_value.t form]
  (if-some [items (data-value-items form)]
    (if (= (count items) 1)
      (if-some [call-form (first items)]
        (if-some [call (parse-call call-form)]
          (Some
           (PredicateClause
            (tuple-get call 0)
            (tuple-get call 1)))
          None)
        None)
      None)
    None))

(defn ^:option<datascript.parser/clause> parse-fn
  [^:Datascript_runtime.Data_value.t form]
  (if-some [items (data-value-items form)]
    (if (= (count items) 2)
      (if-some [call-form (first items)]
        (if-some [binding-form (nth items 1)]
          (if-some [call (parse-call call-form)]
            (Some
             (FunctionClause
              (tuple-get call 0)
              (tuple-get call 1)
              (parse-binding binding-form)))
            None)
          None)
        None)
      None)
    None))

(defn ^:option<datascript.parser/clause> parse-rule-expr
  [^:Datascript_runtime.Data_value.t form]
  (if-some [source-and-form (take-source form)]
    (let [source (tuple-get source-and-form 0)
          items (tuple-get source-and-form 1)]
      (if-some [name-form (first items)]
        (if-some [name* (parse-plain-symbol name-form)]
          (let [args (subvec items 1)]
            (if (empty? args)
              (util/raise "rule-expr requires at least one argument"
                {:error :parser/where})
              (if-some [args* (parse-pattern-elements args)]
                (Some (RuleClause source name* args*))
                (util/raise "Cannot parse rule-expr arguments"
                  {:error :parser/where}))))
          None)
        None))
    None))

(signature datascript.parser/fn-arg-variable-records
  :fn<datascript.parser/fn-arg;vector<datascript.parser/Variable>>)

(defn ^:vector<datascript.parser/Variable> fn-arg-variable-records
  [^:fn-arg arg]
  (match arg
    (FnArgVariable variable) [variable]
    (FnArgSource _) []
    (FnArgConstant _) []))

(defn ^:vector<datascript.parser/SrcVar> fn-arg-source-records
  [^:fn-arg arg]
  (match arg
    (FnArgSource source) [source]
    _ []))

(signature datascript.parser/binding-vars
  :fn<datascript.parser/binding;vector<datascript.parser/Variable>>)

(defn ^:vector<datascript.parser/Variable> binding-vars
  [^:binding binding]
  (match binding
    BindIgnore []
    (BindScalar variable) [variable]
    (BindTuple bindings) (vec (mapcat binding-vars bindings))
    (BindColl binding) (binding-vars binding)))

(signature datascript.parser/pattern-element-vars
  :fn<datascript.parser/pattern-element;vector<datascript.parser/Variable>>)
(signature datascript.parser/pattern-element-variable-symbol
  :fn<datascript.parser/pattern-element;option<symbol>>)

(defn ^:vector<datascript.parser/Variable> pattern-element-vars
  [^:pattern-element element]
  (match element
    PatternPlaceholder []
    (PatternVariable variable) [variable]
    (PatternConstant _) []))

(defn ^:option<symbol> pattern-element-variable-symbol
  [^:pattern-element element]
  (match element
    (PatternVariable variable) (Some (:symbol variable))
    _ None))

(signature datascript.parser/callable-vars
  :fn<datascript.parser/query-callable;vector<datascript.parser/Variable>>)

(defn ^:vector<datascript.parser/Variable> callable-vars
  [^:query-callable callable]
  (match callable
    (StaticCallable _) []
    (VariableCallable variable) [variable]))

(signature datascript.parser/rule-vars-values
  :fn<datascript.parser/RuleVars;vector<datascript.parser/Variable>>)

(defn ^:vector<datascript.parser/Variable> rule-vars-values
  [^datascript.parser/RuleVars vars]
  (vec
   (concat
    (if-some [required (:required vars)] required [])
    (:free vars))))

(signature datascript.parser/clause-vars
  :fn<datascript.parser/clause;vector<datascript.parser/Variable>>)

(defn ^:vector<datascript.parser/Variable> clause-vars
  [^:datascript.parser/clause clause]
  (match clause
    (PatternClause _ pattern)
    (vec (mapcat pattern-element-vars pattern))
    (PredicateClause callable args)
    (vec
     (concat (callable-vars callable)
             (mapcat fn-arg-variable-records args)))
    (FunctionClause callable args binding)
    (vec
     (concat (callable-vars callable)
             (mapcat fn-arg-variable-records args)
             (binding-vars binding)))
    (RuleClause _ _ args)
    (vec (mapcat pattern-element-vars args))
    (NotClause _ vars _ _) vars
    (OrClause _ _ vars _ _) (rule-vars-values vars)
    (AndClause clauses) (vec (mapcat clause-vars clauses))))

(defn collect-vars-distinct
  [^:vector<datascript.parser/clause> clauses]
  (vec (distinct (mapcat clause-vars clauses))))

(defn- ^:string auto-rule-variable
  [^:string variable ^:int seqid]
  (str variable "__auto__" seqid))

(defn- ^datascript.parser/Variable substitute-rule-variable
  [^:map<string;pattern-element> replacements
   ^:int seqid
   ^datascript.parser/Variable variable]
  (let [name (str (.-symbol variable))]
    (if-some [replacement (get replacements name)]
      (match replacement
        (PatternVariable replacement-variable)
        replacement-variable
        _
        (Stdlib.invalid_arg
         (str
          "Rule variable "
          name
          " must remain a variable in this position")))
      (Variable. (auto-rule-variable name seqid)))))

(defn- ^pattern-element substitute-rule-pattern-element
  [^:map<string;pattern-element> replacements
   ^:int seqid
   ^pattern-element element]
  (match element
    (PatternVariable variable)
    (let [name (str (.-symbol variable))]
      (if-some [replacement (get replacements name)]
        replacement
        (PatternVariable
         (Variable. (auto-rule-variable name seqid)))))
    _ element))

(defn- ^fn-arg substitute-rule-fn-arg
  [^:map<string;pattern-element> replacements
   ^:int seqid
   ^fn-arg argument]
  (match argument
    (FnArgVariable variable)
    (let [name (str (.-symbol variable))]
      (if-some [replacement (get replacements name)]
        (match replacement
          (PatternVariable replacement-variable)
          (FnArgVariable replacement-variable)
          (PatternConstant value)
          (FnArgConstant value)
          PatternPlaceholder
          (Stdlib.invalid_arg
           (str
            "Rule variable "
            name
            " cannot become a placeholder function argument")))
        (FnArgVariable
         (Variable. (auto-rule-variable name seqid)))))
    _ argument))

(defn- ^query-callable substitute-rule-callable
  [^:map<string;pattern-element> replacements
   ^:int seqid
   ^query-callable callable]
  (match callable
    (VariableCallable variable)
    (VariableCallable
     (substitute-rule-variable replacements seqid variable))
    _ callable))

(declare substitute-rule-binding
         substitute-rule-clause
         substitute-rule-clauses)

(defn- ^binding substitute-rule-binding
  [^:map<string;pattern-element> replacements
   ^:int seqid
   ^binding binding]
  (match binding
    BindIgnore BindIgnore
    (BindScalar variable)
    (BindScalar
     (substitute-rule-variable replacements seqid variable))
    (BindTuple bindings)
    (BindTuple
     (mapv
      (fn [^binding binding]
        (substitute-rule-binding replacements seqid binding))
      bindings))
    (BindColl binding)
    (BindColl
     (substitute-rule-binding replacements seqid binding))))

(defn- ^datascript.parser/RuleVars substitute-rule-vars
  [^:map<string;pattern-element> replacements
   ^:int seqid
   ^datascript.parser/RuleVars variables]
  (RuleVars.
   (match (.-required variables)
     None None
     (Some required)
     (Some
      (mapv
       (fn [^datascript.parser/Variable variable]
         (substitute-rule-variable
          replacements seqid variable))
       required)))
   (mapv
    (fn [^datascript.parser/Variable variable]
      (substitute-rule-variable replacements seqid variable))
    (.-free variables))))

(defn- ^clause substitute-rule-clause
  [^:map<string;pattern-element> replacements
   ^:int seqid
   ^clause clause]
  (match clause
    (PatternClause source pattern)
    (PatternClause
     source
     (mapv
      (fn [^pattern-element element]
        (substitute-rule-pattern-element
         replacements seqid element))
      pattern))
    (PredicateClause callable arguments)
    (PredicateClause
     (substitute-rule-callable replacements seqid callable)
     (mapv
      (fn [^fn-arg argument]
        (substitute-rule-fn-arg
         replacements seqid argument))
      arguments))
    (FunctionClause callable arguments binding)
    (FunctionClause
     (substitute-rule-callable replacements seqid callable)
     (mapv
      (fn [^fn-arg argument]
        (substitute-rule-fn-arg
         replacements seqid argument))
      arguments)
     (substitute-rule-binding replacements seqid binding))
    (RuleClause source name arguments)
    (RuleClause
     source
     name
     (mapv
      (fn [^pattern-element argument]
        (substitute-rule-pattern-element
         replacements seqid argument))
      arguments))
    (NotClause source variables clauses display)
    (NotClause
     source
     (mapv
      (fn [^datascript.parser/Variable variable]
        (substitute-rule-variable
         replacements seqid variable))
      variables)
     (substitute-rule-clauses replacements seqid clauses)
     display)
    (OrClause source kind variables clauses display)
    (OrClause
     source
     kind
     (substitute-rule-vars replacements seqid variables)
     (substitute-rule-clauses replacements seqid clauses)
     display)
    (AndClause clauses)
    (AndClause
     (substitute-rule-clauses replacements seqid clauses))))

(defn ^:private ^:vector<clause> substitute-rule-clauses
  [^:map<string;pattern-element> replacements
   ^:int seqid
   ^:vector<clause> clauses]
  (mapv
   (fn [^clause clause]
     (substitute-rule-clause replacements seqid clause))
   clauses))

(defn ^:option<vector<datascript.parser/clause>> parse-clauses
  [^:vector<Datascript_runtime.Data_value.t> clauses]
  (parse-items
   (fn [^data-value form]
     (Some (parse-clause form)))
   clauses))

(defn- validate-not [^:datascript.parser/clause clause]
  (match clause
    (NotClause _ vars _ _)
    (do
      (when (empty? vars)
        (util/raise "Join variables should not be empty"
          {:error :parser/where}))
      clause)
    _ clause))

(defn ^:option<datascript.parser/clause> parse-not
  [^:Datascript_runtime.Data_value.t form]
  (if-some [source-and-form (take-source form)]
    (let [source (tuple-get source-and-form 0)
          items (tuple-get source-and-form 1)]
      (if-some [head (first items)]
        (match head
          (Datascript_runtime.Data_value.Symbol value)
          (if (= value "not")
            (if-some [clauses* (parse-clauses (subvec items 1))]
              (if (empty? clauses*)
                (util/raise "Cannot parse 'not' clause"
                  {:error :parser/where})
                (Some
                 (validate-not
                  (NotClause
                   source
                   (collect-vars-distinct clauses*)
                   clauses*
                   (Datascript_runtime.Data_value.to_edn_string form)))))
              (util/raise "Cannot parse 'not' clause"
                {:error :parser/where}))
            None)
          _ None)
        None))
    None))

(defn ^:option<vector<datascript.parser/Variable>> parse-variables
  [^:vector<Datascript_runtime.Data_value.t> items]
  (parse-items parse-variable items))

(defn ^:option<datascript.parser/clause> parse-not-join
  [^:Datascript_runtime.Data_value.t form]
  (if-some [source-and-form (take-source form)]
    (let [source (tuple-get source-and-form 0)
          items (tuple-get source-and-form 1)]
      (if (>= (count items) 2)
        (if-some [head (first items)]
          (match head
            (Datascript_runtime.Data_value.Symbol value)
            (if (= value "not-join")
              (if-some [vars-form (nth items 1)]
                (if-some [var-items (data-value-items vars-form)]
                  (if-some [vars* (parse-variables var-items)]
                    (if-some [clauses* (parse-clauses (subvec items 2))]
                      (if (empty? clauses*)
                        (util/raise "Cannot parse 'not-join' clause"
                          {:error :parser/where})
                        (Some
                         (validate-not
                          (NotClause
                           source
                           vars*
                           clauses*
                           (Datascript_runtime.Data_value.to_edn_string form)))))
                      (util/raise "Cannot parse 'not-join' clause"
                        {:error :parser/where}))
                    (util/raise "Cannot parse 'not-join' variables"
                      {:error :parser/where}))
                  None)
                None)
              None)
            _ None)
          None)
        None))
    None))

(defn validate-or
  [^:datascript.parser/clause clause
   ^:Datascript_runtime.Data_value.t _form]
  (match clause
    (OrClause _ _ vars _ _)
    (do
      (let [required-empty
            (match (:required vars)
              None true
              (Some required) (empty? required))]
        (when (and required-empty (empty? (:free vars)))
          (util/raise "Join variables should not be empty"
            {:error :parser/where})))
      clause)
    _ clause))

(defn ^:option<vector<datascript.parser/clause>>
  parse-disjunction-clauses
  [^:vector<Datascript_runtime.Data_value.t> items]
  (loop [remaining items
         parsed []]
    (if (empty? remaining)
      (Some parsed)
      (if-some [item (first remaining)]
        (let [clause (if-some [and-clause (parse-and item)]
                       and-clause
                       (parse-clause item))]
          (recur (subvec remaining 1) (conj parsed clause)))
        None))))

(defn ^:option<datascript.parser/clause> parse-and
  [^:Datascript_runtime.Data_value.t form]
  (if-some [items (data-value-items form)]
    (if-some [head (first items)]
      (match head
        (Datascript_runtime.Data_value.Symbol value)
        (if (= value "and")
          (if-some [clauses* (parse-clauses (subvec items 1))]
            (if (pos? (count clauses*))
              (Some (AndClause clauses*))
              (util/raise "Cannot parse empty 'and' clause"
                {:error :parser/where}))
            None)
          None)
        _ None)
      None)
    None))

(defn ^:option<datascript.parser/clause> parse-or
  [^:Datascript_runtime.Data_value.t form]
  (if-some [source-and-form (take-source form)]
    (let [source (tuple-get source-and-form 0)
          items (tuple-get source-and-form 1)]
      (if-some [head (first items)]
        (match head
          (Datascript_runtime.Data_value.Symbol value)
          (if (= value "or")
            (if-some [clauses* (parse-disjunction-clauses
                                (subvec items 1))]
              (if (empty? clauses*)
                (util/raise "Cannot parse 'or' clause"
                  {:error :parser/where})
                (Some
                 (validate-or
                  (OrClause
                   source
                   PlainDisjunction
                   (RuleVars. None (collect-vars-distinct clauses*))
                   clauses*
                   (Datascript_runtime.Data_value.to_edn_string form))
                  form)))
              (util/raise "Cannot parse 'or' clause"
                {:error :parser/where}))
            None)
          _ None)
        None))
    None))

(defn ^:option<datascript.parser/clause> parse-or-join
  [^:Datascript_runtime.Data_value.t form]
  (if-some [source-and-form (take-source form)]
    (let [source (tuple-get source-and-form 0)
          items (tuple-get source-and-form 1)]
      (if (>= (count items) 2)
        (if-some [head (first items)]
          (match head
            (Datascript_runtime.Data_value.Symbol value)
            (if (= value "or-join")
              (if-some [vars-form (nth items 1)]
                (let [vars* (parse-rule-vars vars-form)]
                  (if-some [clauses* (parse-disjunction-clauses
                                      (subvec items 2))]
                    (if (empty? clauses*)
                      (util/raise "Cannot parse 'or-join' clause"
                        {:error :parser/where})
                      (Some
                       (validate-or
                        (OrClause
                         source
                         JoinDisjunction
                         vars*
                         clauses*
                         (Datascript_runtime.Data_value.to_edn_string form))
                        form)))
                    (util/raise "Cannot parse 'or-join' clause"
                      {:error :parser/where})))
                None)
              None)
            _ None)
          None)
        None))
    None))


#_(defn reorder-nots [parent-vars clauses]
    (loop [acc     []
           clauses clauses
           vars    (set parent-vars)
           pending []]
      (if-let [sufficient (not-empty (filter #(set/subset? (set (:vars %)) vars) pending))]
        (recur (into acc sufficient)
          clauses
          vars
          (remove (set sufficient) pending))
        (if-let [clause (first clauses)]
          (if (instance? Not clause)
            (recur acc (next clauses) vars (conj pending clause))
            (recur (conj acc clause)
              (next clauses)
              (into vars (collect-vars clause))
              pending))
          (if (empty? pending)
            acc
            (let [not     (first pending)
                  missing (->> (set/difference (set (:vars not)) vars)
                            (into #{} (map :symbol)))]
              (Stdlib.invalid_arg
               (str "Insufficient bindings: " missing
                    " are not bound in clause " (source not)))))))))

(defn ^:datascript.parser/clause parse-clause
  [^:Datascript_runtime.Data_value.t form]
  (if-some [clause (parse-not form)]
    clause
    (if-some [clause (parse-not-join form)]
      clause
      (if-some [clause (parse-or form)]
        clause
        (if-some [clause (parse-or-join form)]
          clause
          (if-some [clause (parse-pred form)]
            clause
            (if-some [clause (parse-fn form)]
              clause
              (if-some [clause (parse-rule-expr form)]
                clause
                (if-some [clause (parse-pattern form)]
                  clause
                  (util/raise
                   "Cannot parse clause, expected (data-pattern | pred-expr | fn-expr | rule-expr | not-clause | not-join-clause | or-clause | or-join-clause)"
                   {:error :parser/where}))))))))))

(defn ^:vector<datascript.parser/clause> parse-where
  [^:Datascript_runtime.Data_value.t form]
  (if-some [items (data-value-items form)]
    (if-some [clauses (parse-clauses items)]
      clauses
      (util/raise "Cannot parse :where clause"
        {:error :parser/where}))
    (util/raise "Cannot parse :where clause"
      {:error :parser/where})))


;; rule-branch = [rule-head clause+]
;; rule-head   = [rule-name rule-vars]
;; rule-name   = plain-symbol

(deftrecord RuleBranch
  [^datascript.parser/PlainSymbol name
   ^datascript.parser/RuleVars vars
   ^:vector<clause> clauses])
(deftrecord Rule
  [^datascript.parser/PlainSymbol name
   ^:vector<datascript.parser/RuleBranch> branches])

(defn ^datascript.parser/RuleBranch parse-rule
  [^:Datascript_runtime.Data_value.t form]
  (if-some [items (data-value-items form)]
    (if-some [head (first items)]
      (if-some [head-items (data-value-items head)]
        (if-some [name-form (first head-items)]
          (if-some [name* (parse-plain-symbol name-form)]
            (let [vars* (parse-rule-var-items (subvec head-items 1))
                  clauses-form (subvec items 1)]
              (if (empty? clauses-form)
                (util/raise "Rule branch should have clauses"
                  {:error :parser/rule})
                (if-some [clauses* (parse-clauses clauses-form)]
                  (RuleBranch. name* vars* clauses*)
                  (util/raise "Cannot parse rule clauses"
                    {:error :parser/rule}))))
            (util/raise "Cannot parse rule name"
              {:error :parser/rule}))
          (util/raise "Rule head cannot be empty"
            {:error :parser/rule}))
        (util/raise "Cannot parse rule head"
          {:error :parser/rule}))
      (util/raise "Cannot parse empty rule"
        {:error :parser/rule}))
    (util/raise "Cannot parse rule"
      {:error :parser/rule})))

(defn validate-arity
  [^datascript.parser/PlainSymbol name
   ^:vector<datascript.parser/RuleBranch> branches]
  (if-some [first-branch (first branches)]
    (let [vars0 (:vars first-branch)
          arity0 (rule-vars-arity vars0)]
      (doseq [branch (subvec branches 1)]
        (let [vars (:vars branch)]
          (when (not= arity0 (rule-vars-arity vars))
            (Stdlib.invalid_arg
             (str
              "Arity mismatch for rule '"
              (.-symbol name)
              "': "
              (rule-vars-display vars0)
              " vs. "
              (rule-vars-display vars)))))))
    (util/raise "Rule must contain at least one branch"
      {:error :parser/rule})))

(defn ^:vector<datascript.parser/Rule> add-rule-branch
  [^:vector<datascript.parser/Rule> rules
   ^datascript.parser/RuleBranch branch]
  (loop [remaining rules
         ^:vector<datascript.parser/Rule> result []
         found false]
    (if (empty? remaining)
      (if found
        result
        (conj result (Rule. (.-name branch) [branch])))
      (if-some [rule (first remaining)]
        (if (= (.-symbol (.-name rule)) (.-symbol (.-name branch)))
          (let [branches (conj (.-branches rule) branch)]
            (validate-arity (.-name rule) branches)
            (recur (subvec remaining 1)
                   (conj result (Rule. (.-name rule) branches))
                   true))
          (recur (subvec remaining 1)
                 (conj result rule)
                 found))
        result))))

(defn ^:vector<datascript.parser/Rule> parse-rules
  [^:Datascript_runtime.Data_value.t form]
  (if-some [items (data-value-items form)]
    (reduce
     (fn [^:vector<datascript.parser/Rule> rules
          ^:Datascript_runtime.Data_value.t rule-form]
       (add-rule-branch rules (parse-rule rule-form)))
     []
     items)
    (util/raise "Cannot parse rules"
      {:error :parser/rule})))


;; query

;; q* prefix because of https://dev.clojure.org/jira/browse/CLJS-2237
(deftrecord Query
  [^:find-spec qfind
   ^:option<vector<datascript.parser/Variable>> qwith
   ^:option<datascript.parser/return-map> qreturn-map
   ^:vector<input-binding> qin
   ^:vector<clause> qwhere])

(type-variant traversable
  TraversalAbsent
  (TraversalVector :vector<traversable>)
  (TraversalData :data-value)
  (TraversalSymbol :symbol)
  (TraversalKeyword :keyword)
  (TraversalString :string)
  (TraversalPlaceholder :datascript.parser/Placeholder)
  (TraversalVariable :datascript.parser/Variable)
  (TraversalSource :datascript.parser/SrcVar)
  (TraversalDefaultSource :datascript.parser/DefaultSrc)
  (TraversalRules :datascript.parser/RulesVar)
  (TraversalConstant :datascript.parser/Constant)
  (TraversalPlainSymbol :datascript.parser/PlainSymbol)
  (TraversalRuleVars :datascript.parser/RuleVars)
  (TraversalBinding :binding)
  (TraversalAggregate :datascript.parser/Aggregate)
  (TraversalPull :datascript.parser/Pull)
  (TraversalFind :find-spec)
  (TraversalReturnMap :return-map)
  (TraversalClause :clause)
  (TraversalRuleBranch :datascript.parser/RuleBranch)
  (TraversalRule :datascript.parser/Rule)
  (TraversalQuery :datascript.parser/Query))

(defn ^traversable variable-traversable [^:string name]
  (TraversalVariable (Variable. name)))

(defn ^traversable binding-traversable [^binding binding]
  (TraversalBinding binding))

(defn ^traversable find-traversable [^find-spec find]
  (TraversalFind find))

(defn ^traversable clause-traversable [^clause clause]
  (TraversalClause clause))

(defn ^traversable rule-traversable [^datascript.parser/Rule rule]
  (TraversalRule rule))

(defn ^traversable query-traversable [^datascript.parser/Query query]
  (TraversalQuery query))

(defn ^traversable data-traversable [^data-value value]
  (TraversalData value))

(defn ^traversable string-traversable [^:string value]
  (TraversalString value))

(defn ^:option<string> traversable-variable-name [^traversable node]
  (match node
    (TraversalVariable variable) (Some (str (.-symbol variable)))
    _ None))

(defn ^boolean traversable-clause? [^traversable node]
  (match node
    (TraversalClause _) true
    _ false))

(defn ^:option<clause> traversable-clause-value [^traversable node]
  (match node
    (TraversalClause clause) (Some clause)
    _ None))

(defn ^:string traversable-kind [^traversable node]
  (match node
    TraversalAbsent "absent"
    (TraversalVector _) "vector"
    (TraversalData _) "data"
    (TraversalSymbol _) "symbol"
    (TraversalKeyword _) "keyword"
    (TraversalString _) "string"
    (TraversalPlaceholder _) "placeholder"
    (TraversalVariable _) "variable"
    (TraversalSource _) "source"
    (TraversalDefaultSource _) "default-source"
    (TraversalRules _) "rules"
    (TraversalConstant _) "constant"
    (TraversalPlainSymbol _) "plain-symbol"
    (TraversalRuleVars _) "rule-vars"
    (TraversalBinding _) "binding"
    (TraversalAggregate _) "aggregate"
    (TraversalPull _) "pull"
    (TraversalFind _) "find"
    (TraversalReturnMap _) "return-map"
    (TraversalClause _) "clause"
    (TraversalRuleBranch _) "rule-branch"
    (TraversalRule _) "rule"
    (TraversalQuery _) "query"))

(defn- ^traversable query-source-traversable [^query-source source]
  (match source
    DefaultSource (TraversalDefaultSource (DefaultSrc.))
    (ExplicitSource source) (TraversalSource source)))

(defn- ^traversable pattern-element-traversable
  [^pattern-element element]
  (match element
    PatternPlaceholder (TraversalPlaceholder (Placeholder.))
    (PatternVariable variable) (TraversalVariable variable)
    (PatternConstant value) (TraversalData value)))

(defn- ^traversable fn-arg-traversable [^fn-arg argument]
  (match argument
    (FnArgVariable variable) (TraversalVariable variable)
    (FnArgSource source) (TraversalSource source)
    (FnArgConstant value) (TraversalData value)))

(defn- ^traversable query-callable-traversable
  [^query-callable callable]
  (match callable
    (StaticCallable function-name)
    (TraversalPlainSymbol function-name)
    (VariableCallable variable)
    (TraversalVariable variable)))

(defn- ^traversable aggregate-function-traversable
  [^aggregate-function function]
  (match function
    (AggregatePlain function-name)
    (TraversalPlainSymbol function-name)
    (AggregateCustom variable)
    (TraversalVariable variable)))

(defn- ^traversable pull-pattern-traversable [^pull-pattern pattern]
  (match pattern
    (PullVariable variable) (TraversalVariable variable)
    (PullConstant value) (TraversalData value)))

(defn- ^traversable find-element-traversable [^find-element element]
  (match element
    (FindVariable variable) (TraversalVariable variable)
    (FindPullElement pull) (TraversalPull pull)
    (FindAggregate aggregate) (TraversalAggregate aggregate)))

(defn- ^traversable input-binding-traversable [^input-binding input]
  (match input
    (InputSource source) (TraversalSource source)
    (InputRules rules) (TraversalRules rules)
    (InputValueBinding binding) (TraversalBinding binding)))

(defn- ^datascript.parser/Variable traversable-variable-exn
  [^traversable node]
  (match node
    (TraversalVariable variable) variable
    _ (Stdlib.invalid_arg "Expected variable traversal node")))

(defn- ^datascript.parser/SrcVar traversable-source-exn
  [^traversable node]
  (match node
    (TraversalSource source) source
    _ (Stdlib.invalid_arg "Expected source traversal node")))

(defn- ^datascript.parser/PlainSymbol traversable-plain-symbol-exn
  [^traversable node]
  (match node
    (TraversalPlainSymbol symbol) symbol
    _ (Stdlib.invalid_arg "Expected plain-symbol traversal node")))

(defn- ^data-value traversable-data-exn [^traversable node]
  (match node
    (TraversalData value) value
    (TraversalConstant constant) (.-value constant)
    _ (Stdlib.invalid_arg "Expected data traversal node")))

(defn- ^:symbol traversable-symbol-exn [^traversable node]
  (match node
    (TraversalSymbol symbol) symbol
    _ (Stdlib.invalid_arg "Expected symbol traversal node")))

(defn- ^:keyword traversable-keyword-exn [^traversable node]
  (match node
    (TraversalKeyword keyword) keyword
    _ (Stdlib.invalid_arg "Expected keyword traversal node")))

(defn- ^:string traversable-string-exn [^traversable node]
  (match node
    (TraversalString value) value
    _ (Stdlib.invalid_arg "Expected string traversal node")))

(defn- ^:vector<traversable> traversable-vector-exn
  [^traversable node]
  (match node
    (TraversalVector values) values
    _ (Stdlib.invalid_arg "Expected vector traversal node")))

(defn- ^binding traversable-binding-exn [^traversable node]
  (match node
    (TraversalBinding binding) binding
    _ (Stdlib.invalid_arg "Expected binding traversal node")))

(defn- ^clause traversable-clause-exn [^traversable node]
  (match node
    (TraversalClause clause) clause
    _ (Stdlib.invalid_arg "Expected clause traversal node")))

(defn- ^datascript.parser/RuleVars traversable-rule-vars-exn
  [^traversable node]
  (match node
    (TraversalRuleVars variables) variables
    _ (Stdlib.invalid_arg "Expected rule-vars traversal node")))

(defn- ^datascript.parser/RuleBranch traversable-rule-branch-exn
  [^traversable node]
  (match node
    (TraversalRuleBranch branch) branch
    _ (Stdlib.invalid_arg "Expected rule-branch traversal node")))

(defn- ^find-spec traversable-find-exn [^traversable node]
  (match node
    (TraversalFind find) find
    _ (Stdlib.invalid_arg "Expected find traversal node")))

(defn- ^return-map traversable-return-map-exn [^traversable node]
  (match node
    (TraversalReturnMap return-map) return-map
    _ (Stdlib.invalid_arg "Expected return-map traversal node")))

(defn- ^traversable variables-traversable
  [^:vector<datascript.parser/Variable> variables]
  (TraversalVector
   (mapv
    (fn [^datascript.parser/Variable variable]
      (TraversalVariable variable))
    variables)))

(defn- ^traversable bindings-traversable
  [^:vector<binding> bindings]
  (TraversalVector
   (mapv
    (fn [^binding binding]
      (TraversalBinding binding))
    bindings)))

(defn- ^traversable fn-args-traversable
  [^:vector<fn-arg> arguments]
  (TraversalVector (mapv fn-arg-traversable arguments)))

(defn- ^traversable pattern-elements-traversable
  [^:vector<pattern-element> elements]
  (TraversalVector (mapv pattern-element-traversable elements)))

(defn- ^traversable find-elements-traversable
  [^:vector<find-element> elements]
  (TraversalVector (mapv find-element-traversable elements)))

(defn- ^traversable clauses-traversable
  [^:vector<clause> clauses]
  (TraversalVector
   (mapv
    (fn [^clause clause]
      (TraversalClause clause))
    clauses)))

(defn- ^traversable rule-branches-traversable
  [^:vector<datascript.parser/RuleBranch> branches]
  (TraversalVector
   (mapv
    (fn [^datascript.parser/RuleBranch branch]
      (TraversalRuleBranch branch))
    branches)))

(defn- ^traversable inputs-traversable
  [^:vector<input-binding> inputs]
  (TraversalVector (mapv input-binding-traversable inputs)))

(defn- ^query-source traversable-query-source-exn [^traversable node]
  (match node
    (TraversalDefaultSource _) DefaultSource
    (TraversalSource source) (ExplicitSource source)
    _ (Stdlib.invalid_arg "Expected query-source traversal node")))

(defn- ^pattern-element traversable-pattern-element-exn
  [^traversable node]
  (match node
    (TraversalPlaceholder _) PatternPlaceholder
    (TraversalVariable variable) (PatternVariable variable)
    (TraversalData value) (PatternConstant value)
    (TraversalConstant constant) (PatternConstant (.-value constant))
    _ (Stdlib.invalid_arg "Expected pattern-element traversal node")))

(defn- ^fn-arg traversable-fn-arg-exn [^traversable node]
  (match node
    (TraversalVariable variable) (FnArgVariable variable)
    (TraversalSource source) (FnArgSource source)
    (TraversalData value) (FnArgConstant value)
    (TraversalConstant constant) (FnArgConstant (.-value constant))
    _ (Stdlib.invalid_arg "Expected function-argument traversal node")))

(defn- ^query-callable traversable-query-callable-exn
  [^traversable node]
  (match node
    (TraversalPlainSymbol function-name) (StaticCallable function-name)
    (TraversalVariable variable) (VariableCallable variable)
    _ (Stdlib.invalid_arg "Expected callable traversal node")))

(defn- ^aggregate-function traversable-aggregate-function-exn
  [^traversable node]
  (match node
    (TraversalPlainSymbol function-name) (AggregatePlain function-name)
    (TraversalVariable variable) (AggregateCustom variable)
    _ (Stdlib.invalid_arg "Expected aggregate-function traversal node")))

(defn- ^pull-pattern traversable-pull-pattern-exn [^traversable node]
  (match node
    (TraversalVariable variable) (PullVariable variable)
    (TraversalData value) (PullConstant value)
    (TraversalConstant constant) (PullConstant (.-value constant))
    _ (Stdlib.invalid_arg "Expected pull-pattern traversal node")))

(defn- ^find-element traversable-find-element-exn [^traversable node]
  (match node
    (TraversalVariable variable) (FindVariable variable)
    (TraversalPull pull) (FindPullElement pull)
    (TraversalAggregate aggregate) (FindAggregate aggregate)
    _ (Stdlib.invalid_arg "Expected find-element traversal node")))

(defn- ^input-binding traversable-input-binding-exn [^traversable node]
  (match node
    (TraversalSource source) (InputSource source)
    (TraversalRules rules) (InputRules rules)
    (TraversalBinding binding) (InputValueBinding binding)
    _ (Stdlib.invalid_arg "Expected input traversal node")))

(defn- ^:vector<datascript.parser/Variable> traversable-variables-exn
  [^traversable node]
  (mapv traversable-variable-exn (traversable-vector-exn node)))

(defn- ^:vector<binding> traversable-bindings-exn [^traversable node]
  (mapv traversable-binding-exn (traversable-vector-exn node)))

(defn- ^:vector<fn-arg> traversable-fn-args-exn [^traversable node]
  (mapv traversable-fn-arg-exn (traversable-vector-exn node)))

(defn- ^:vector<pattern-element> traversable-pattern-elements-exn
  [^traversable node]
  (mapv traversable-pattern-element-exn
        (traversable-vector-exn node)))

(defn- ^:vector<find-element> traversable-find-elements-exn
  [^traversable node]
  (mapv traversable-find-element-exn
        (traversable-vector-exn node)))

(defn- ^:vector<clause> traversable-clauses-exn [^traversable node]
  (mapv traversable-clause-exn (traversable-vector-exn node)))

(defn- ^:vector<datascript.parser/RuleBranch>
  traversable-rule-branches-exn
  [^traversable node]
  (mapv traversable-rule-branch-exn
        (traversable-vector-exn node)))

(defn- ^:vector<input-binding> traversable-inputs-exn
  [^traversable node]
  (mapv traversable-input-binding-exn
        (traversable-vector-exn node)))

(defn- ^:vector<traversable> data-values-traversable
  [^:list<data-value> values]
  (Rrbvec.of_list
   (List.map
    (fn [^data-value value]
      (TraversalData value))
    values)))

(defn- ^:vector<traversable> data-map-entries-traversable
  [^:list<tuple<data-value;data-value>> entries]
  (Rrbvec.of_list
   (List.map
    (fn [^:tuple<data-value;data-value> entry]
      (TraversalVector
       [(TraversalData (tuple-get entry 0))
        (TraversalData (tuple-get entry 1))]))
    entries)))

(defn- ^:vector<traversable> optional-data-values-traversable
  [^:list<option<data-value>> values]
  (let [^:vector<option<data-value>> values
        (Rrbvec.of_list values)]
    (loop [^:int idx 0
           ^:vector<traversable> result []]
      (if (< idx (count values))
        (recur
         (inc idx)
         (conj
          result
          (match (Rrbvec.nth values idx)
            None
            (TraversalData (Datascript_runtime.Data_value.Nil))
            (Some value)
            (TraversalData value))))
        result))))

(defn- ^:vector<traversable> data-value-children [^data-value value]
  (match value
    (Datascript_runtime.Data_value.List values)
    (data-values-traversable values)
    (Datascript_runtime.Data_value.Vector values)
    (data-values-traversable values)
    (Datascript_runtime.Data_value.Map entries)
    (data-map-entries-traversable entries)
    (Datascript_runtime.Data_value.Set values)
    (data-values-traversable values)
    (Datascript_runtime.Data_value.Tuple values)
    (optional-data-values-traversable values)
    _ []))

(defn- ^:vector<traversable> traversable-children [^traversable node]
  (match node
    TraversalAbsent []
    (TraversalVector values) values
    (TraversalData value) (data-value-children value)
    (TraversalSymbol _) []
    (TraversalKeyword _) []
    (TraversalString _) []
    (TraversalPlaceholder _) []
    (TraversalVariable variable)
    [(TraversalSymbol (.-symbol variable))]
    (TraversalSource source)
    [(TraversalSymbol (.-symbol source))]
    (TraversalDefaultSource _) []
    (TraversalRules _) []
    (TraversalConstant constant)
    [(TraversalData (.-value constant))]
    (TraversalPlainSymbol symbol)
    [(TraversalSymbol (.-symbol symbol))]
    (TraversalRuleVars variables)
    [(if-some [required (.-required variables)]
       (variables-traversable required)
       TraversalAbsent)
     (variables-traversable (.-free variables))]
    (TraversalBinding binding)
    (match binding
      BindIgnore []
      (BindScalar variable) [(TraversalVariable variable)]
      (BindTuple bindings) [(bindings-traversable bindings)]
      (BindColl binding) [(TraversalBinding binding)])
    (TraversalAggregate aggregate)
    [(aggregate-function-traversable (.-fn aggregate))
     (fn-args-traversable (.-args aggregate))]
    (TraversalPull pull)
    [(TraversalSource (.-source pull))
     (TraversalVariable (.-variable pull))
     (pull-pattern-traversable (.-pattern pull))]
    (TraversalFind find)
    (match find
      (FindRelation relation)
      [(find-elements-traversable (.-elements relation))]
      (FindCollection collection)
      [(find-element-traversable (.-element collection))]
      (FindSingle scalar)
      [(find-element-traversable (.-element scalar))]
      (FindTupleResult tuple-result)
      [(find-elements-traversable (.-elements tuple-result))])
    (TraversalReturnMap return-map)
    (match return-map
      (ReturnKeys keys)
      [(TraversalKeyword :keys)
       (TraversalVector
        (mapv
         (fn [^:keyword key] (TraversalKeyword key))
         keys))]
      (ReturnSyms symbols)
      [(TraversalKeyword :syms)
       (TraversalVector
        (mapv
         (fn [^:symbol symbol] (TraversalSymbol symbol))
         symbols))]
      (ReturnStrs strings)
      [(TraversalKeyword :strs)
       (TraversalVector
        (mapv
         (fn [^:string value] (TraversalString value))
         strings))])
    (TraversalClause clause)
    (match clause
      (PatternClause source pattern)
      [(query-source-traversable source)
       (pattern-elements-traversable pattern)]
      (PredicateClause callable arguments)
      [(query-callable-traversable callable)
       (fn-args-traversable arguments)]
      (FunctionClause callable arguments binding)
      [(query-callable-traversable callable)
       (fn-args-traversable arguments)
       (TraversalBinding binding)]
      (RuleClause source name arguments)
      [(query-source-traversable source)
       (TraversalPlainSymbol name)
       (pattern-elements-traversable arguments)]
      (NotClause source variables clauses _)
      [(query-source-traversable source)
       (variables-traversable variables)
       (clauses-traversable clauses)]
      (OrClause source _ variables clauses _)
      [(query-source-traversable source)
       (TraversalRuleVars variables)
       (clauses-traversable clauses)]
      (AndClause clauses)
      [(clauses-traversable clauses)])
    (TraversalRuleBranch branch)
    [(TraversalPlainSymbol (.-name branch))
     (TraversalRuleVars (.-vars branch))
     (clauses-traversable (.-clauses branch))]
    (TraversalRule rule)
    [(TraversalPlainSymbol (.-name rule))
     (rule-branches-traversable (.-branches rule))]
    (TraversalQuery query)
    [(TraversalFind (.-qfind query))
     (if-some [variables (.-qwith query)]
       (variables-traversable variables)
       TraversalAbsent)
     (if-some [return-map (.-qreturn-map query)]
       (TraversalReturnMap return-map)
       TraversalAbsent)
     (inputs-traversable (.-qin query))
     (clauses-traversable (.-qwhere query))]))

(defn- ^:vector<keyword> traversable-keywords-exn [^traversable node]
  (mapv traversable-keyword-exn (traversable-vector-exn node)))

(defn- ^:vector<symbol> traversable-symbols-exn [^traversable node]
  (mapv traversable-symbol-exn (traversable-vector-exn node)))

(defn- ^:vector<string> traversable-strings-exn [^traversable node]
  (mapv traversable-string-exn (traversable-vector-exn node)))

(defn- ^:list<option<data-value>> postwalk-optional-data-values
  [^:list<option<data-value>> values
   ^:fn<traversable;traversable> walk]
  (let [^:vector<option<data-value>> values
        (Rrbvec.of_list values)]
    (loop [^:int idx 0
           ^:vector<option<data-value>> result []]
      (if (< idx (count values))
        (let [value
              (match (Rrbvec.nth values idx)
                None (Datascript_runtime.Data_value.Nil)
                (Some value) value)
              value
              (traversable-data-exn
               (walk (TraversalData value)))
              transformed
              (match value
                (Datascript_runtime.Data_value.Nil) None
                _ (Some value))]
          (recur (inc idx) (conj result transformed)))
        (Rrbvec.to_list result)))))

(defn- ^:list<data-value> postwalk-data-values
  [^:list<data-value> values
   ^:fn<traversable;traversable> walk]
  (List.map
   (fn [^data-value value]
     (traversable-data-exn
      (walk (TraversalData value))))
   values))

(defn- ^data-value postwalk-data-value
  [^data-value value
   ^:fn<traversable;traversable> walk]
  (match value
    (Datascript_runtime.Data_value.List values)
    (Datascript_runtime.Data_value.List
     (postwalk-data-values values walk))
    (Datascript_runtime.Data_value.Vector values)
    (Datascript_runtime.Data_value.Vector
     (postwalk-data-values values walk))
    (Datascript_runtime.Data_value.Map entries)
    (Datascript_runtime.Data_value.Map
     (List.map
      (fn [^:tuple<data-value;data-value> entry]
        (tuple
         (tuple-get entry 0)
         (traversable-data-exn
          (walk
           (TraversalData
            (tuple-get entry 1))))))
      entries))
    (Datascript_runtime.Data_value.Set values)
    (Datascript_runtime.Data_value.Set
     (postwalk-data-values values walk))
    (Datascript_runtime.Data_value.Tuple values)
    (Datascript_runtime.Data_value.Tuple
     (postwalk-optional-data-values values walk))
    _ value))

(defn- ^traversable traversable-postwalk
  [^traversable node
   ^:fn<traversable;traversable> f
   ^boolean apply-root?]
  (let [walk
        (fn [^traversable child]
          (traversable-postwalk child f true))
        rebuilt
        (match node
          TraversalAbsent TraversalAbsent
          (TraversalVector values)
          (TraversalVector (mapv walk values))
          (TraversalData value)
          (TraversalData (postwalk-data-value value walk))
          (TraversalSymbol symbol) (TraversalSymbol symbol)
          (TraversalKeyword keyword) (TraversalKeyword keyword)
          (TraversalString value) (TraversalString value)
          (TraversalPlaceholder placeholder)
          (TraversalPlaceholder placeholder)
          (TraversalVariable variable)
          (TraversalVariable
           (Variable.
            (traversable-symbol-exn
             (walk (TraversalSymbol (.-symbol variable))))))
          (TraversalSource source)
          (TraversalSource
           (SrcVar.
            (traversable-symbol-exn
             (walk (TraversalSymbol (.-symbol source))))))
          (TraversalDefaultSource source)
          (TraversalDefaultSource source)
          (TraversalRules rules)
          (TraversalRules rules)
          (TraversalConstant constant)
          (TraversalConstant
           (Constant.
            (traversable-data-exn
             (walk (TraversalData (.-value constant))))))
          (TraversalPlainSymbol symbol)
          (TraversalPlainSymbol
           (PlainSymbol.
            (traversable-symbol-exn
             (walk (TraversalSymbol (.-symbol symbol))))))
          (TraversalRuleVars variables)
          (let [required
                (walk
                 (if-some [required (.-required variables)]
                   (variables-traversable required)
                   TraversalAbsent))
                required
                (match required
                  TraversalAbsent None
                  (TraversalVector _)
                  (Some (traversable-variables-exn required))
                  _
                  (Stdlib.invalid_arg
                   "Expected optional variable vector traversal node"))
                free
                (traversable-variables-exn
                 (walk (variables-traversable (.-free variables))))]
            (TraversalRuleVars (RuleVars. required free)))
          (TraversalBinding binding)
          (TraversalBinding
           (match binding
             BindIgnore BindIgnore
             (BindScalar variable)
             (BindScalar
              (traversable-variable-exn
               (walk (TraversalVariable variable))))
             (BindTuple bindings)
             (BindTuple
              (traversable-bindings-exn
               (walk (bindings-traversable bindings))))
             (BindColl binding)
             (BindColl
              (traversable-binding-exn
               (walk (TraversalBinding binding))))))
          (TraversalAggregate aggregate)
          (TraversalAggregate
           (Aggregate.
            (traversable-aggregate-function-exn
             (walk
              (aggregate-function-traversable
               (.-fn aggregate))))
            (traversable-fn-args-exn
             (walk (fn-args-traversable (.-args aggregate))))))
          (TraversalPull pull)
          (TraversalPull
           (Pull.
            (traversable-source-exn
             (walk (TraversalSource (.-source pull))))
            (traversable-variable-exn
             (walk (TraversalVariable (.-variable pull))))
            (traversable-pull-pattern-exn
             (walk
              (pull-pattern-traversable
               (.-pattern pull))))))
          (TraversalFind find)
          (TraversalFind
           (match find
             (FindRelation relation)
             (FindRelation
              (FindRel.
               (traversable-find-elements-exn
                (walk
                 (find-elements-traversable
                  (.-elements relation))))))
             (FindCollection collection)
             (FindCollection
              (FindColl.
               (traversable-find-element-exn
                (walk
                 (find-element-traversable
                  (.-element collection))))))
             (FindSingle scalar)
             (FindSingle
              (FindScalar.
               (traversable-find-element-exn
                (walk
                 (find-element-traversable
                  (.-element scalar))))))
             (FindTupleResult tuple-result)
             (FindTupleResult
              (FindTuple.
               (traversable-find-elements-exn
                (walk
                 (find-elements-traversable
                  (.-elements tuple-result))))))))
          (TraversalReturnMap return-map)
          (TraversalReturnMap
           (match return-map
             (ReturnKeys keys)
             (let [type
                   (traversable-keyword-exn
                    (walk (TraversalKeyword :keys)))]
               (if (= type :keys)
                 (ReturnKeys
                  (traversable-keywords-exn
                   (walk
                    (TraversalVector
                     (mapv
                      (fn [^:keyword key]
                        (TraversalKeyword key))
                      keys)))))
                 (Stdlib.invalid_arg
                  "Return-map traversal cannot change :keys type")))
             (ReturnSyms symbols)
             (let [type
                   (traversable-keyword-exn
                    (walk (TraversalKeyword :syms)))]
               (if (= type :syms)
                 (ReturnSyms
                  (traversable-symbols-exn
                   (walk
                    (TraversalVector
                     (mapv
                      (fn [^:symbol symbol]
                        (TraversalSymbol symbol))
                      symbols)))))
                 (Stdlib.invalid_arg
                  "Return-map traversal cannot change :syms type")))
             (ReturnStrs strings)
             (let [type
                   (traversable-keyword-exn
                    (walk (TraversalKeyword :strs)))]
               (if (= type :strs)
                 (ReturnStrs
                  (traversable-strings-exn
                   (walk
                    (TraversalVector
                     (mapv
                      (fn [^:string value]
                        (TraversalString value))
                      strings)))))
                 (Stdlib.invalid_arg
                  "Return-map traversal cannot change :strs type")))))
          (TraversalClause clause)
          (TraversalClause
           (match clause
             (PatternClause source pattern)
             (PatternClause
              (traversable-query-source-exn
               (walk (query-source-traversable source)))
              (traversable-pattern-elements-exn
               (walk (pattern-elements-traversable pattern))))
             (PredicateClause callable arguments)
             (PredicateClause
              (traversable-query-callable-exn
               (walk (query-callable-traversable callable)))
              (traversable-fn-args-exn
               (walk (fn-args-traversable arguments))))
             (FunctionClause callable arguments binding)
             (FunctionClause
              (traversable-query-callable-exn
               (walk (query-callable-traversable callable)))
              (traversable-fn-args-exn
               (walk (fn-args-traversable arguments)))
              (traversable-binding-exn
               (walk (TraversalBinding binding))))
             (RuleClause source name arguments)
             (RuleClause
              (traversable-query-source-exn
               (walk (query-source-traversable source)))
              (traversable-plain-symbol-exn
               (walk (TraversalPlainSymbol name)))
              (traversable-pattern-elements-exn
               (walk (pattern-elements-traversable arguments))))
             (NotClause source variables clauses display)
             (NotClause
              (traversable-query-source-exn
               (walk (query-source-traversable source)))
              (traversable-variables-exn
               (walk (variables-traversable variables)))
              (traversable-clauses-exn
               (walk (clauses-traversable clauses)))
              display)
             (OrClause source kind variables clauses display)
             (OrClause
              (traversable-query-source-exn
               (walk (query-source-traversable source)))
              kind
              (traversable-rule-vars-exn
               (walk (TraversalRuleVars variables)))
              (traversable-clauses-exn
               (walk (clauses-traversable clauses)))
              display)
             (AndClause clauses)
             (AndClause
              (traversable-clauses-exn
               (walk (clauses-traversable clauses))))))
          (TraversalRuleBranch branch)
          (TraversalRuleBranch
           (RuleBranch.
            (traversable-plain-symbol-exn
             (walk (TraversalPlainSymbol (.-name branch))))
            (traversable-rule-vars-exn
             (walk (TraversalRuleVars (.-vars branch))))
            (traversable-clauses-exn
             (walk (clauses-traversable (.-clauses branch))))))
          (TraversalRule rule)
          (TraversalRule
           (Rule.
            (traversable-plain-symbol-exn
             (walk (TraversalPlainSymbol (.-name rule))))
            (traversable-rule-branches-exn
             (walk
              (rule-branches-traversable
               (.-branches rule))))))
          (TraversalQuery query)
          (let [with-node
                (walk
                 (if-some [variables (.-qwith query)]
                   (variables-traversable variables)
                   TraversalAbsent))
                with
                (match with-node
                  TraversalAbsent None
                  (TraversalVector _)
                  (Some (traversable-variables-exn with-node))
                  _
                  (Stdlib.invalid_arg
                   "Expected optional :with traversal node"))
                return-map-node
                (walk
                 (if-some [return-map (.-qreturn-map query)]
                   (TraversalReturnMap return-map)
                   TraversalAbsent))
                return-map
                (match return-map-node
                  TraversalAbsent None
                  (TraversalReturnMap _)
                  (Some
                   (traversable-return-map-exn
                    return-map-node))
                  _
                  (Stdlib.invalid_arg
                   "Expected optional return-map traversal node"))]
            (TraversalQuery
             (Query.
              (traversable-find-exn
               (walk (TraversalFind (.-qfind query))))
              with
              return-map
              (traversable-inputs-exn
               (walk (inputs-traversable (.-qin query))))
              (traversable-clauses-exn
               (walk
                (clauses-traversable
                 (.-qwhere query))))))))]
    (if apply-root?
      (f rebuilt)
      rebuilt)))

(defn- ^:vector<traversable> collect-traversable
  [^:fn<traversable;bool> pred
   ^traversable node
   ^:vector<traversable> acc]
  (if (pred node)
    (conj acc node)
    (reduce
     (fn [^:vector<traversable> result ^traversable child]
       (collect-traversable pred child result))
     acc
     (traversable-children node))))

(defn- ^:vector<datascript.parser/Variable> collect-vars-traversable
  [^:vector<datascript.parser/Variable> acc
   ^traversable node]
  (match node
    (TraversalVariable variable) (conj acc variable)
    (TraversalClause clause)
    (match clause
      (NotClause _ variables _ _) (into acc variables)
      (OrClause _ _ variables _ _)
      (into acc (rule-vars-values variables))
      _
      (reduce collect-vars-traversable
              acc
              (traversable-children node)))
    _
    (reduce collect-vars-traversable
            acc
            (traversable-children node))))

(defprotocol ITraversable
  (-collect
   [this
    ^:fn<traversable;bool> pred
    ^:vector<traversable> acc]
   :vector<traversable>)
  (-collect-vars
   [this ^:vector<datascript.parser/Variable> acc]
   :vector<datascript.parser/Variable>)
  (-postwalk
   [this ^:fn<traversable;traversable> f]
   :traversable))

(extend-type datascript.parser/traversable
  ITraversable
  (-collect
    [^traversable node
     ^:fn<traversable;bool> pred
     ^:vector<traversable> acc]
    (reduce
     (fn [^:vector<traversable> result ^traversable child]
       (collect-traversable pred child result))
     acc
     (traversable-children node)))
  (-collect-vars
    [^traversable node
     ^:vector<datascript.parser/Variable> acc]
    (reduce collect-vars-traversable
            acc
            (traversable-children node)))
  (-postwalk
    [^traversable node
     ^:fn<traversable;traversable> f]
    (traversable-postwalk node f false)))

(defn collect
  ([^:fn<traversable;bool> pred ^traversable form]
   (collect pred form []))
  ([^:fn<traversable;bool> pred
    ^traversable form
    ^:vector<traversable> acc]
   (if (pred form)
     (conj acc form)
     (-collect form pred acc))))

(defn ^traversable postwalk
  [^traversable form
   ^:fn<traversable;traversable> f]
  (f (-postwalk form f)))

(signature datascript.parser/relation-find
  :fn<vector<string>;datascript.parser/find-spec>)
(signature datascript.parser/relation-find-elements
  :fn<vector<datascript.parser/find-element>;datascript.parser/find-spec>)
(signature datascript.parser/aggregate-find-element
  :fn<string;vector<datascript.parser/fn-arg>;datascript.parser/find-element>)
(signature datascript.parser/custom-aggregate-find-element
  :fn<string;vector<datascript.parser/fn-arg>;datascript.parser/find-element>)
(signature datascript.parser/pull-find-element
  :fn<string;Datascript_runtime.Data_value.t;datascript.parser/find-element>)
(signature datascript.parser/pull-source-find-element
  :fn<string;string;Datascript_runtime.Data_value.t;datascript.parser/find-element>)
(signature datascript.parser/pull-variable-find-element
  :fn<string;string;datascript.parser/find-element>)
(signature datascript.parser/pull-source-variable-find-element
  :fn<string;string;string;datascript.parser/find-element>)
(signature datascript.parser/collection-find
  :fn<string;datascript.parser/find-spec>)
(signature datascript.parser/collection-find-element
  :fn<datascript.parser/find-element;datascript.parser/find-spec>)
(signature datascript.parser/single-find
  :fn<string;datascript.parser/find-spec>)
(signature datascript.parser/single-find-element
  :fn<datascript.parser/find-element;datascript.parser/find-spec>)
(signature datascript.parser/tuple-find
  :fn<vector<string>;datascript.parser/find-spec>)
(signature datascript.parser/tuple-find-elements
  :fn<vector<datascript.parser/find-element>;datascript.parser/find-spec>)
(signature datascript.parser/static-query
  :fn<datascript.parser/find-spec;vector<vector<datascript.parser/pattern-element>>;datascript.parser/Query>)
(signature datascript.parser/pattern-variable
  :fn<string;datascript.parser/pattern-element>)
(signature datascript.parser/pattern-constant
  :fn<Datascript_runtime.Data_value.t;datascript.parser/pattern-element>)
(signature datascript.parser/pattern-attribute
  :fn<keyword;datascript.parser/pattern-element>)
(signature datascript.parser/static-db-query
  :fn<datascript.parser/find-spec;vector<vector<datascript.parser/pattern-element>>;datascript.parser/Query>)
(signature datascript.parser/static-db-query-with-scalars
  :fn<datascript.parser/find-spec;vector<vector<datascript.parser/pattern-element>>;vector<string>;datascript.parser/Query>)
(signature datascript.parser/static-db-query-with-bindings
  :fn<datascript.parser/find-spec;vector<vector<datascript.parser/pattern-element>>;vector<datascript.parser/binding>;datascript.parser/Query>)
(signature datascript.parser/static-query-with-bindings
  :fn<datascript.parser/find-spec;vector<vector<datascript.parser/pattern-element>>;vector<datascript.parser/binding>;datascript.parser/Query>)
(signature datascript.parser/static-db-query-clauses-with-bindings
  :fn<datascript.parser/find-spec;vector<datascript.parser/clause>;vector<datascript.parser/binding>;datascript.parser/Query>)
(signature datascript.parser/static-query-clauses-with-bindings
  :fn<datascript.parser/find-spec;vector<datascript.parser/clause>;vector<datascript.parser/binding>;datascript.parser/Query>)
(signature datascript.parser/static-db-query-clauses-with-inputs
  :fn<datascript.parser/find-spec;vector<datascript.parser/clause>;vector<datascript.parser/static-query-input>;datascript.parser/Query>)
(signature datascript.parser/static-query-clauses-with-inputs
  :fn<datascript.parser/find-spec;vector<datascript.parser/clause>;vector<datascript.parser/static-query-input>;datascript.parser/Query>)
(signature datascript.parser/make-static-rules-input
  :fn<unit;datascript.parser/static-query-input>)
(signature datascript.parser/make-static-source-input
  :fn<string;datascript.parser/static-query-input>)
(signature datascript.parser/make-static-value-input
  :fn<datascript.parser/binding;datascript.parser/static-query-input>)
(signature datascript.parser/static-query-inputs
  :fn<datascript.parser/Query;option<vector<datascript.parser/static-query-input>>>)
(signature datascript.parser/static-input-rules?
  :fn<datascript.parser/static-query-input;bool>)
(signature datascript.parser/static-input-source-name
  :fn<datascript.parser/static-query-input;option<string>>)
(signature datascript.parser/static-input-binding
  :fn<datascript.parser/static-query-input;option<datascript.parser/binding>>)
(signature datascript.parser/query-with
  :fn<datascript.parser/Query;vector<string>;datascript.parser/Query>)
(signature datascript.parser/query-return-keys
  :fn<datascript.parser/Query;vector<string>;datascript.parser/Query>)
(signature datascript.parser/query-return-symbols
  :fn<datascript.parser/Query;vector<string>;datascript.parser/Query>)
(signature datascript.parser/query-return-strings
  :fn<datascript.parser/Query;vector<string>;datascript.parser/Query>)
(signature datascript.parser/static-db-query-clauses
  :fn<datascript.parser/find-spec;vector<datascript.parser/clause>;datascript.parser/Query>)
(signature datascript.parser/static-db-query-clauses-with-scalars
  :fn<datascript.parser/find-spec;vector<datascript.parser/clause>;vector<string>;datascript.parser/Query>)
(signature datascript.parser/pattern-clause
  :fn<vector<datascript.parser/pattern-element>;datascript.parser/clause>)
(signature datascript.parser/explicit-pattern-clause
  :fn<string;vector<datascript.parser/pattern-element>;datascript.parser/clause>)
(signature datascript.parser/query-source-name
  :fn<datascript.parser/query-source;option<string>>)
(signature datascript.parser/greater-than-clause
  :fn<datascript.parser/fn-arg;datascript.parser/fn-arg;datascript.parser/clause>)
(signature datascript.parser/static-predicate-clause
  :fn<string;vector<datascript.parser/fn-arg>;datascript.parser/clause>)
(signature datascript.parser/static-function-clause
  :fn<string;vector<datascript.parser/fn-arg>;datascript.parser/binding;datascript.parser/clause>)
(signature datascript.parser/variable-function-clause
  :fn<string;vector<datascript.parser/fn-arg>;datascript.parser/binding;datascript.parser/clause>)
(signature datascript.parser/variable-predicate-clause
  :fn<string;vector<datascript.parser/fn-arg>;datascript.parser/clause>)
(signature datascript.parser/static-not-clause
  :fn<vector<datascript.parser/clause>;string;datascript.parser/clause>)
(signature datascript.parser/static-source-not-clause
  :fn<string;vector<datascript.parser/clause>;string;datascript.parser/clause>)
(signature datascript.parser/static-not-join-clause
  :fn<vector<string>;vector<datascript.parser/clause>;string;datascript.parser/clause>)
(signature datascript.parser/static-source-not-join-clause
  :fn<string;vector<string>;vector<datascript.parser/clause>;string;datascript.parser/clause>)
(signature datascript.parser/static-and-clause
  :fn<vector<datascript.parser/clause>;datascript.parser/clause>)
(signature datascript.parser/static-or-clause
  :fn<vector<datascript.parser/clause>;string;datascript.parser/clause>)
(signature datascript.parser/static-source-or-clause
  :fn<string;vector<datascript.parser/clause>;string;datascript.parser/clause>)
(signature datascript.parser/and-clause-clauses
  :fn<datascript.parser/clause;option<vector<datascript.parser/clause>>>)
(signature datascript.parser/rule-vars-names
  :fn<datascript.parser/RuleVars;vector<string>>)
(signature datascript.parser/or-clause-parts
  :fn<datascript.parser/clause;option<tuple<vector<string>;vector<string>;vector<datascript.parser/clause>>>>)
(signature datascript.parser/or-clause-source-name
  :fn<datascript.parser/clause;option<string>>)
(signature datascript.parser/or-clause-display
  :fn<datascript.parser/clause;option<string>>)
(signature datascript.parser/or-clause-join?
  :fn<datascript.parser/clause;bool>)
(signature datascript.parser/static-or-join-clause
  :fn<vector<string>;vector<string>;vector<datascript.parser/clause>;string;datascript.parser/clause>)
(signature datascript.parser/static-source-or-join-clause
  :fn<string;vector<string>;vector<string>;vector<datascript.parser/clause>;string;datascript.parser/clause>)
(signature datascript.parser/static-rule-clause
  :fn<string;vector<datascript.parser/pattern-element>;datascript.parser/clause>)
(signature datascript.parser/static-source-rule-clause
  :fn<string;string;vector<datascript.parser/pattern-element>;datascript.parser/clause>)
(signature datascript.parser/static-rule-branch
  :fn<string;vector<string>;vector<datascript.parser/clause>;datascript.parser/RuleBranch>)
(signature datascript.parser/static-rule-branch-with-vars
  :fn<string;vector<string>;vector<string>;vector<datascript.parser/clause>;datascript.parser/RuleBranch>)
(signature datascript.parser/static-rules
  :fn<vector<datascript.parser/RuleBranch>;vector<datascript.parser/Rule>>)
(signature datascript.parser/rule-clause-parts
  :fn<datascript.parser/clause;option<tuple<string;vector<datascript.parser/pattern-element>>>>)
(signature datascript.parser/rule-clause-source-name
  :fn<datascript.parser/clause;option<string>>)
(signature datascript.parser/rule-branches
  :fn<vector<datascript.parser/Rule>;string;option<vector<datascript.parser/RuleBranch>>>)
(signature datascript.parser/rule-branch-parameter-names
  :fn<datascript.parser/RuleBranch;vector<string>>)
(signature datascript.parser/rule-branch-required-parameter-names
  :fn<datascript.parser/RuleBranch;vector<string>>)
(signature datascript.parser/rule-branch-clauses
  :fn<datascript.parser/RuleBranch;vector<datascript.parser/clause>>)
(signature datascript.parser/pattern-element-constant
  :fn<datascript.parser/pattern-element;option<Datascript_runtime.Data_value.t>>)
(signature datascript.parser/variable-argument
  :fn<string;datascript.parser/fn-arg>)
(signature datascript.parser/source-argument
  :fn<string;datascript.parser/fn-arg>)
(signature datascript.parser/constant-argument
  :fn<Datascript_runtime.Data_value.t;datascript.parser/fn-arg>)
(signature datascript.parser/pattern-placeholder
  :fn<unit;datascript.parser/pattern-element>)
(signature datascript.parser/argument-variable-name
  :fn<datascript.parser/fn-arg;option<string>>)
(signature datascript.parser/argument-constant
  :fn<datascript.parser/fn-arg;option<Datascript_runtime.Data_value.t>>)
(signature datascript.parser/argument-source-name
  :fn<datascript.parser/fn-arg;option<string>>)
(signature datascript.parser/static-callable-name
  :fn<datascript.parser/query-callable;option<string>>)
(signature datascript.parser/variable-callable-name
  :fn<datascript.parser/query-callable;option<string>>)
(signature datascript.parser/scalar-input
  :fn<string;datascript.parser/binding>)
(signature datascript.parser/ignore-input
  :fn<unit;datascript.parser/binding>)
(signature datascript.parser/tuple-input
  :fn<vector<datascript.parser/binding>;datascript.parser/binding>)
(signature datascript.parser/collection-input
  :fn<datascript.parser/binding;datascript.parser/binding>)
(signature datascript.parser/static-query-value-bindings
  :fn<datascript.parser/Query;option<vector<datascript.parser/binding>>>)
(signature datascript.parser/static-query-has-source?
  :fn<datascript.parser/Query;bool>)
(signature datascript.parser/binding-ignore?
  :fn<datascript.parser/binding;bool>)
(signature datascript.parser/binding-scalar-variable
  :fn<datascript.parser/binding;option<string>>)
(signature datascript.parser/binding-tuple-items
  :fn<datascript.parser/binding;option<vector<datascript.parser/binding>>>)
(signature datascript.parser/binding-collection-item
  :fn<datascript.parser/binding;option<datascript.parser/binding>>)
(signature datascript.parser/binding-variable-names
  :fn<datascript.parser/binding;vector<string>>)
(signature datascript.parser/static-query-patterns
  :fn<datascript.parser/Query;option<vector<vector<datascript.parser/pattern-element>>>>)
(signature datascript.parser/find-variable-names
  :fn<datascript.parser/find-spec;option<vector<string>>>)
(signature datascript.parser/find-projection-variable-names
  :fn<datascript.parser/find-spec;option<vector<string>>>)
(signature datascript.parser/find-element-aggregate
  :fn<datascript.parser/find-element;option<datascript.parser/Aggregate>>)
(signature datascript.parser/aggregate-function-name
  :fn<datascript.parser/Aggregate;option<string>>)
(signature datascript.parser/aggregate-custom-variable-name
  :fn<datascript.parser/Aggregate;option<string>>)
(signature datascript.parser/aggregate-constant-arguments
  :fn<datascript.parser/Aggregate;option<vector<Datascript_runtime.Data_value.t>>>)
(signature datascript.parser/find-element-pull
  :fn<datascript.parser/find-element;option<datascript.parser/Pull>>)
(signature datascript.parser/pull-variable-name
  :fn<datascript.parser/Pull;string>)
(signature datascript.parser/pull-source-name
  :fn<datascript.parser/Pull;string>)
(signature datascript.parser/pull-pattern-value
  :fn<datascript.parser/Pull;option<Datascript_runtime.Data_value.t>>)
(signature datascript.parser/pull-pattern-variable-name
  :fn<datascript.parser/Pull;option<string>>)
(signature datascript.parser/relation-find?
  :fn<datascript.parser/find-spec;bool>)
(signature datascript.parser/collection-find?
  :fn<datascript.parser/find-spec;bool>)
(signature datascript.parser/single-find?
  :fn<datascript.parser/find-spec;bool>)

(defn ^:find-element variable-find-element [^:string variable]
  (FindVariable (Variable. variable)))

(defn ^:pattern-element pattern-variable [^:string variable]
  (PatternVariable (Variable. variable)))

(defn ^:pattern-element pattern-constant
  [^:Datascript_runtime.Data_value.t value]
  (PatternConstant value))

(defn ^:option<Datascript_runtime.Data_value.t> pattern-element-constant
  [^:pattern-element element]
  (match element
    (PatternConstant value) (Some value)
    _ None))

(defn ^:pattern-element pattern-attribute [^:keyword attr]
  (pattern-constant
   (Datascript_runtime.Data_value.Keyword (str attr))))

(defn ^:clause pattern-clause
  [^:vector<pattern-element> pattern]
  (PatternClause DefaultSource pattern))

(defn ^:clause explicit-pattern-clause
  [^:string source ^:vector<pattern-element> pattern]
  (PatternClause (ExplicitSource (SrcVar. source)) pattern))

(defn ^:option<string> query-source-name [^:query-source source]
  (match source
    DefaultSource None
    (ExplicitSource source) (Some (str (.-symbol source)))))

(defn ^:fn-arg variable-argument [^:string variable]
  (FnArgVariable (Variable. variable)))

(defn ^:fn-arg source-argument [^:string source]
  (FnArgSource (SrcVar. source)))

(defn ^:fn-arg constant-argument
  [^:Datascript_runtime.Data_value.t value]
  (FnArgConstant value))

(defn ^:pattern-element pattern-placeholder []
  PatternPlaceholder)

(defn ^:option<string> argument-variable-name [^:fn-arg argument]
  (match argument
    (FnArgVariable variable) (Some (str (.-symbol variable)))
    _ None))

(defn ^:option<Datascript_runtime.Data_value.t> argument-constant
  [^:fn-arg argument]
  (match argument
    (FnArgConstant value) (Some value)
    _ None))

(defn ^:option<string> argument-source-name [^:fn-arg argument]
  (match argument
    (FnArgSource source) (Some (str (.-symbol source)))
    _ None))

(defn ^:option<string> static-callable-name
  [^:query-callable callable]
  (match callable
    (StaticCallable name) (Some (str (.-symbol name)))
    (VariableCallable _) None))

(defn ^:option<string> variable-callable-name
  [^:query-callable callable]
  (match callable
    (StaticCallable _) None
    (VariableCallable variable) (Some (str (.-symbol variable)))))

(defn ^:clause greater-than-clause
  [^:fn-arg left ^:fn-arg right]
  (static-predicate-clause ">" [left right]))

(defn ^:clause static-predicate-clause
  [^:string function-name ^:vector<fn-arg> arguments]
  (PredicateClause
   (StaticCallable (PlainSymbol. function-name))
   arguments))

(defn ^:clause static-function-clause
  [^:string function-name
   ^:vector<fn-arg> arguments
   ^:binding binding]
  (FunctionClause
   (StaticCallable (PlainSymbol. function-name))
   arguments
   binding))

(defn ^:clause variable-function-clause
  [^:string variable
   ^:vector<fn-arg> arguments
   ^:binding binding]
  (FunctionClause
   (VariableCallable (Variable. variable))
   arguments
   binding))

(defn ^:clause variable-predicate-clause
  [^:string variable ^:vector<fn-arg> arguments]
  (PredicateClause
   (VariableCallable (Variable. variable))
   arguments))

(defn ^:clause static-not-clause
  [^:vector<clause> clauses ^:string display]
  (NotClause
   DefaultSource
   (collect-vars-distinct clauses)
   clauses
   display))

(defn ^:clause static-source-not-clause
  [^:string source ^:vector<clause> clauses ^:string display]
  (NotClause
   (ExplicitSource (SrcVar. source))
   (collect-vars-distinct clauses)
   clauses
   display))

(defn ^:clause static-not-join-clause
  [^:vector<string> variables
   ^:vector<clause> clauses
   ^:string display]
  (NotClause
   DefaultSource
   (mapv
    (fn [^:string variable]
      (Variable. variable))
    variables)
   clauses
   display))

(defn ^:clause static-source-not-join-clause
  [^:string source
   ^:vector<string> variables
   ^:vector<clause> clauses
   ^:string display]
  (NotClause
   (ExplicitSource (SrcVar. source))
   (mapv
    (fn [^:string variable]
      (Variable. variable))
    variables)
   clauses
   display))

(defn ^:clause static-and-clause
  [^:vector<clause> clauses]
  (if (empty? clauses)
    (Stdlib.invalid_arg "Cannot create an empty and clause")
    (AndClause clauses)))

(defn ^:option<vector<clause>> and-clause-clauses
  [^clause clause]
  (match clause
    (AndClause clauses) (Some clauses)
    _ None))

(defn ^:vector<string> rule-vars-names
  [^datascript.parser/RuleVars variables]
  (mapv
   (fn [^datascript.parser/Variable variable]
     (str (.-symbol variable)))
   (rule-vars-values variables)))

(defn ^:vector<string> rule-vars-required-names
  [^datascript.parser/RuleVars variables]
  (if-some [required (.-required variables)]
    (mapv
     (fn [^datascript.parser/Variable variable]
       (str (.-symbol variable)))
     required)
    []))

(defn ^:option<tuple<vector<string>;vector<string>;vector<clause>>>
  or-clause-parts
  [^clause clause]
  (match clause
    (OrClause _ _ variables branches _)
    (Some
     (tuple
      (rule-vars-required-names variables)
      (rule-vars-names variables)
      branches))
    _ None))

(defn ^:option<string> or-clause-source-name
  [^clause clause]
  (match clause
    (OrClause source _ _ _ _) (query-source-name source)
    _ None))

(defn ^:option<string> or-clause-display
  [^clause clause]
  (match clause
    (OrClause _ _ _ _ display) (Some display)
    _ None))

(defn ^boolean or-clause-join?
  [^clause clause]
  (match clause
    (OrClause _ PlainDisjunction _ _ _) false
    (OrClause _ JoinDisjunction _ _ _) true
    _ false))

(defn ^:clause static-or-clause-for-source
  [^query-source source
   ^:vector<clause> branches
   ^:string display]
  (if-some [first-branch (first branches)]
    (OrClause
     source
     PlainDisjunction
     (RuleVars.
      None
      (collect-vars-distinct [first-branch]))
     branches
     display)
    (Stdlib.invalid_arg "Cannot create an empty or clause")))

(defn ^:clause static-or-clause
  [^:vector<clause> branches ^:string display]
  (static-or-clause-for-source DefaultSource branches display))

(defn ^:clause static-source-or-clause
  [^:string source
   ^:vector<clause> branches
   ^:string display]
  (static-or-clause-for-source
   (ExplicitSource (SrcVar. source))
   branches
   display))

(defn ^:clause static-or-join-clause-for-source
  [^query-source source
   ^:vector<string> required
   ^:vector<string> free
   ^:vector<clause> branches
   ^:string display]
  (let [all-variables (vec (concat required free))]
    (if (empty? all-variables)
      (Stdlib.invalid_arg "Join variables should not be empty")
      (if (not (= (count all-variables)
                  (count (distinct all-variables))))
        (Stdlib.invalid_arg "Rule variables should be distinct")
        (if (empty? branches)
          (Stdlib.invalid_arg "Cannot create an empty or-join clause")
          (OrClause
           source
           JoinDisjunction
           (RuleVars.
            (if (empty? required)
              None
              (Some
               (mapv
                (fn [^:string variable]
                  (Variable. variable))
                required)))
            (mapv
             (fn [^:string variable]
               (Variable. variable))
             free))
           branches
           display))))))

(defn ^:clause static-or-join-clause
  [^:vector<string> required
   ^:vector<string> free
   ^:vector<clause> branches
   ^:string display]
  (static-or-join-clause-for-source
   DefaultSource required free branches display))

(defn ^:clause static-source-or-join-clause
  [^:string source
   ^:vector<string> required
   ^:vector<string> free
   ^:vector<clause> branches
   ^:string display]
  (static-or-join-clause-for-source
   (ExplicitSource (SrcVar. source))
   required
   free
   branches
   display))

(defn ^:clause static-rule-clause
  [^:string rule-name ^:vector<pattern-element> arguments]
  (RuleClause
   DefaultSource
   (PlainSymbol. rule-name)
   arguments))

(defn ^:clause static-source-rule-clause
  [^:string source
   ^:string rule-name
   ^:vector<pattern-element> arguments]
  (RuleClause
   (ExplicitSource (SrcVar. source))
   (PlainSymbol. rule-name)
   arguments))

(defn ^datascript.parser/Variable static-rule-variable
  [^:string parameter]
  (if
   (and
    (not (= parameter ""))
    (= (subs parameter 0 1) "?"))
    (Variable. parameter)
    (Stdlib.invalid_arg
     (str
      "Cannot parse var, expected symbol starting with ?, got: "
      parameter))))

(defn ^datascript.parser/RuleBranch static-rule-branch
  [^:string rule-name
   ^:vector<string> parameters
   ^:vector<clause> clauses]
  (if (empty? clauses)
    (Stdlib.invalid_arg "Rule branch should have clauses")
    (RuleBranch.
     (PlainSymbol. rule-name)
     (RuleVars.
      None
      (mapv
       static-rule-variable
       parameters))
     clauses)))

(defn ^datascript.parser/RuleBranch static-rule-branch-with-vars
  [^:string rule-name
   ^:vector<string> required
   ^:vector<string> free
   ^:vector<clause> clauses]
  (if (empty? clauses)
    (Stdlib.invalid_arg "Rule branch should have clauses")
    (RuleBranch.
     (PlainSymbol. rule-name)
     (RuleVars.
      (if (empty? required)
        None
        (Some
         (mapv
          static-rule-variable
          required)))
      (mapv
       static-rule-variable
       free))
     clauses)))

(defn ^:vector<datascript.parser/Rule> static-rules
  [^:vector<datascript.parser/RuleBranch> branches]
  (reduce add-rule-branch [] branches))

(defn ^:option<tuple<string;vector<pattern-element>>> rule-clause-parts
  [^clause clause]
  (match clause
    (RuleClause _ name arguments)
    (Some (tuple (str (.-symbol name)) arguments))
    _ None))

(defn ^:option<string> rule-clause-source-name
  [^clause clause]
  (match clause
    (RuleClause source _ _) (query-source-name source)
    _ None))

(defn ^:option<vector<datascript.parser/RuleBranch>> rule-branches
  [^:vector<datascript.parser/Rule> rules ^:string rule-name]
  (if-some [rule
            (first
             (filter
              (fn [^datascript.parser/Rule rule]
                (= rule-name (str (.-symbol (.-name rule)))))
              rules))]
    (Some (.-branches rule))
    None))

(defn ^:vector<string> rule-branch-parameter-names
  [^datascript.parser/RuleBranch branch]
  (rule-vars-names (.-vars branch)))

(defn ^:vector<string> rule-branch-required-parameter-names
  [^datascript.parser/RuleBranch branch]
  (rule-vars-required-names (.-vars branch)))

(defn ^:vector<clause> rule-branch-clauses
  [^datascript.parser/RuleBranch branch]
  (.-clauses branch))

(defn ^:find-spec relation-find [^:vector<string> variables]
  (FindRelation
   (FindRel. (mapv variable-find-element variables))))

(defn ^:find-spec relation-find-elements
  [^:vector<datascript.parser/find-element> elements]
  (FindRelation (FindRel. elements)))

(defn ^:find-element aggregate-find-element
  [^:string function-name
   ^:vector<datascript.parser/fn-arg> arguments]
  (FindAggregate
   (Aggregate.
    (AggregatePlain (PlainSymbol. function-name))
    arguments)))

(defn ^:find-element custom-aggregate-find-element
  [^:string variable ^:vector<fn-arg> arguments]
  (FindAggregate
   (Aggregate.
    (AggregateCustom (Variable. variable))
    arguments)))

(declare
 pull-source-find-element
 pull-source-variable-find-element)

(defn ^:find-element pull-find-element
  [^:string variable
   ^:Datascript_runtime.Data_value.t pattern]
  (pull-source-find-element "$" variable pattern))

(defn ^:find-element pull-source-find-element
  [^:string source
   ^:string variable
   ^:Datascript_runtime.Data_value.t pattern]
  (FindPullElement
   (Pull.
    (SrcVar. source)
    (Variable. variable)
    (PullConstant pattern))))

(defn ^:find-element pull-variable-find-element
  [^:string variable ^:string pattern-variable]
  (pull-source-variable-find-element
   "$" variable pattern-variable))

(defn ^:find-element pull-source-variable-find-element
  [^:string source
   ^:string variable
   ^:string pattern-variable]
  (FindPullElement
   (Pull.
    (SrcVar. source)
    (Variable. variable)
    (PullVariable (Variable. pattern-variable)))))

(defn ^:find-spec collection-find [^:string variable]
  (collection-find-element
   (variable-find-element variable)))

(defn ^:find-spec collection-find-element
  [^datascript.parser/find-element element]
  (FindCollection (FindColl. element)))

(defn ^:find-spec single-find [^:string variable]
  (single-find-element
   (variable-find-element variable)))

(defn ^:find-spec single-find-element
  [^datascript.parser/find-element element]
  (FindSingle (FindScalar. element)))

(defn ^:find-spec tuple-find [^:vector<string> variables]
  (tuple-find-elements
   (mapv variable-find-element variables)))

(defn ^:find-spec tuple-find-elements
  [^:vector<datascript.parser/find-element> elements]
  (FindTupleResult (FindTuple. elements)))

(defn ^datascript.parser/Query static-query
  [^:find-spec find
   ^:vector<vector<datascript.parser/pattern-element>> patterns]
  (Query.
   find
   None
   None
   []
   (mapv
    (fn [^:vector<datascript.parser/pattern-element> pattern]
      (PatternClause DefaultSource pattern))
    patterns)))

(defn ^datascript.parser/Query static-db-query
  [^:find-spec find
   ^:vector<vector<datascript.parser/pattern-element>> patterns]
  (let [query (static-query find patterns)]
    (Query.
     (.-qfind query)
     (.-qwith query)
     (.-qreturn-map query)
     [(InputSource (SrcVar. "$"))]
     (.-qwhere query))))

(defn ^datascript.parser/Query static-db-query-clauses
  [^:find-spec find ^:vector<clause> clauses]
  (Query.
   find
   None
   None
   [(InputSource (SrcVar. "$"))]
   clauses))

(defn ^:binding scalar-input [^:string variable]
  (BindScalar (Variable. variable)))

(defn ^:binding ignore-input []
  BindIgnore)

(defn ^:binding tuple-input [^:vector<binding> bindings]
  (BindTuple bindings))

(defn ^:binding collection-input [^:binding binding]
  (BindColl binding))

(defn ^datascript.parser/Query static-db-query-with-bindings
  [^:find-spec find
   ^:vector<vector<datascript.parser/pattern-element>> patterns
   ^:vector<binding> bindings]
  (let [query (static-db-query find patterns)]
    (Query.
     (.-qfind query)
     (.-qwith query)
     (.-qreturn-map query)
     (vec
      (concat
       (.-qin query)
       (mapv
        (fn [^:binding binding]
          (InputValueBinding binding))
        bindings)))
     (.-qwhere query))))

(defn ^datascript.parser/Query static-query-with-bindings
  [^:find-spec find
   ^:vector<vector<datascript.parser/pattern-element>> patterns
   ^:vector<binding> bindings]
  (let [query (static-query find patterns)]
    (Query.
     (.-qfind query)
     (.-qwith query)
     (.-qreturn-map query)
     (mapv
      (fn [^:binding binding]
        (InputValueBinding binding))
      bindings)
     (.-qwhere query))))

(defn ^datascript.parser/Query static-db-query-clauses-with-bindings
  [^:find-spec find
   ^:vector<clause> clauses
   ^:vector<binding> bindings]
  (Query.
   find
   None
   None
   (vec
    (concat
     [(InputSource (SrcVar. "$"))]
     (mapv
      (fn [^:binding binding]
        (InputValueBinding binding))
      bindings)))
   clauses))

(defn ^datascript.parser/Query static-query-clauses-with-bindings
  [^:find-spec find
   ^:vector<clause> clauses
   ^:vector<binding> bindings]
  (Query.
   find
   None
   None
   (mapv
    (fn [^:binding binding]
      (InputValueBinding binding))
    bindings)
   clauses))

(defn ^static-query-input make-static-rules-input []
  StaticRulesInput)

(defn ^static-query-input make-static-source-input [^:string source]
  (StaticSourceInput (SrcVar. source)))

(defn ^static-query-input make-static-value-input [^binding binding]
  (StaticValueInput binding))

(defn ^datascript.parser/input-binding static-input-binding-form
  [^static-query-input input]
  (match input
    (StaticSourceInput source) (InputSource source)
    StaticRulesInput (InputRules (RulesVar.))
    (StaticValueInput binding) (InputValueBinding binding)))

(defn ^datascript.parser/Query static-db-query-clauses-with-inputs
  [^:find-spec find
   ^:vector<clause> clauses
   ^:vector<static-query-input> inputs]
  (Query.
   find
   None
   None
   (vec
    (concat
     [(InputSource (SrcVar. "$"))]
     (mapv static-input-binding-form inputs)))
   clauses))

(defn ^datascript.parser/Query static-query-clauses-with-inputs
  [^:find-spec find
   ^:vector<clause> clauses
   ^:vector<static-query-input> inputs]
  (Query.
   find
   None
   None
   (mapv static-input-binding-form inputs)
   clauses))

(defn ^:option<binding> query-input-value-binding
  [^input-binding input]
  (match input
    (InputValueBinding binding) (Some binding)
    _ None))

(defn ^:option<vector<static-query-input>> static-query-inputs
  [^datascript.parser/Query query]
  (loop [remaining (.-qin query)
         inputs []]
    (if-some [input (first remaining)]
      (cond
        (not (empty? (input-binding-sources input)))
        (if-some [source (first (input-binding-sources input))]
          (recur
           (subvec remaining 1)
           (conj inputs (StaticSourceInput source)))
          None)

        (input-binding-rules? input)
        (recur
         (subvec remaining 1)
         (conj inputs (make-static-rules-input)))

        :else
        (if-some [binding (query-input-value-binding input)]
          (recur
           (subvec remaining 1)
           (conj inputs (make-static-value-input binding)))
          None))
      (Some inputs))))

(defn ^boolean static-input-rules? [^static-query-input input]
  (match input
    StaticRulesInput true
    _ false))

(defn ^:option<string> static-input-source-name
  [^static-query-input input]
  (match input
    (StaticSourceInput source) (Some (str (.-symbol source)))
    _ None))

(defn ^:option<binding> static-input-binding
  [^static-query-input input]
  (match input
    (StaticValueInput binding) (Some binding)
    _ None))

(defn ^datascript.parser/Query query-with
  [^datascript.parser/Query query ^:vector<string> variables]
  (let [find-variables
        (if-some [variables
                  (find-variable-names (.-qfind query))]
          variables
          [])
        find-variable-set (set find-variables)]
    (cond
      (not (= (count variables) (count (distinct variables))))
      (Stdlib.invalid_arg "Vars used in :with should be distinct")

      (some
       (fn [^:string variable]
         (contains? find-variable-set variable))
       variables)
      (Stdlib.invalid_arg
       ":find and :with should not use same variables")

      :else
      (Query.
       (.-qfind query)
       (Some
        (mapv
         (fn [^:string variable]
           (Variable. variable))
         variables))
       (.-qreturn-map query)
       (.-qin query)
       (.-qwhere query)))))

(defn ^datascript.parser/Query query-return-map
  [^datascript.parser/Query query ^:return-map return-map]
  (let [type (str (return-map-type return-map))
        find (.-qfind query)]
    (cond
      (match find
        (FindSingle _) true
        _ false)
      (Stdlib.invalid_arg
       (str type " does not work with single-scalar :find"))

      (match find
        (FindCollection _) true
        _ false)
      (Stdlib.invalid_arg
       (str type " does not work with collection :find"))

      (not
       (= (return-map-count return-map)
          (count (find-spec-elements find))))
      (Stdlib.invalid_arg
       (str "Count of " type " must match count of :find"))

      :else
      (Query.
       find
       (.-qwith query)
       (Some return-map)
       (.-qin query)
       (.-qwhere query)))))

(defn ^datascript.parser/Query query-return-keys
  [^datascript.parser/Query query ^:vector<string> keys]
  (query-return-map
   query
   (ReturnKeys
    (mapv (fn [^:string key] (keyword key)) keys))))

(defn ^datascript.parser/Query query-return-symbols
  [^datascript.parser/Query query ^:vector<string> keys]
  (query-return-map
   query
   (ReturnSyms
    (mapv (fn [^:string key] (symbol key)) keys))))

(defn ^datascript.parser/Query query-return-strings
  [^datascript.parser/Query query ^:vector<string> keys]
  (query-return-map query (ReturnStrs keys)))

(defn ^datascript.parser/Query static-db-query-with-scalars
  [^:find-spec find
   ^:vector<vector<datascript.parser/pattern-element>> patterns
   ^:vector<string> variables]
  (static-db-query-with-bindings
   find patterns (mapv scalar-input variables)))

(defn ^datascript.parser/Query static-db-query-clauses-with-scalars
  [^:find-spec find
   ^:vector<clause> clauses
   ^:vector<string> variables]
  (let [query (static-db-query-clauses find clauses)]
    (Query.
     (.-qfind query)
     (.-qwith query)
     (.-qreturn-map query)
     (vec
      (concat
       (.-qin query)
       (mapv
        (fn [^:string variable]
          (InputValueBinding (scalar-input variable)))
        variables)))
     (.-qwhere query))))

(defn ^:option<binding> static-value-input-binding
  [^:input-binding input]
  (match input
    (InputValueBinding binding) (Some binding)
    _ None))

(defn ^:option<vector<binding>> static-query-value-bindings
  [^datascript.parser/Query query]
  (let [inputs (.-qin query)
        remaining
        (if-some [first-input (first inputs)]
          (match first-input
            (InputSource _) (subvec inputs 1)
            _ inputs)
          inputs)]
    (loop [remaining remaining
           bindings []]
      (if-some [input (first remaining)]
        (if-some [binding (static-value-input-binding input)]
          (recur
           (subvec remaining 1)
           (conj bindings binding))
          None)
        (Some bindings)))))

(defn static-query-has-source?
  [^datascript.parser/Query query]
  (if-some [input (first (.-qin query))]
    (match input
      (InputSource _) true
      _ false)
    false))

(defn binding-ignore? [^:binding binding]
  (match binding
    BindIgnore true
    _ false))

(defn ^:option<string> binding-scalar-variable [^:binding binding]
  (match binding
    (BindScalar variable) (Some (str (.-symbol variable)))
    _ None))

(defn ^:option<vector<binding>> binding-tuple-items [^:binding binding]
  (match binding
    (BindTuple bindings) (Some bindings)
    _ None))

(defn ^:option<binding> binding-collection-item [^:binding binding]
  (match binding
    (BindColl item) (Some item)
    _ None))

(defn ^:vector<string> binding-variable-names [^:binding binding]
  (mapv
   (fn [^datascript.parser/Variable variable]
     (str (.-symbol variable)))
   (binding-vars binding)))

(defn ^:option<vector<datascript.parser/pattern-element>>
  default-pattern
  [^:clause clause]
  (match clause
    (PatternClause DefaultSource pattern) (Some pattern)
    _ None))

(defn ^:option<vector<vector<datascript.parser/pattern-element>>>
  static-query-patterns
  [^datascript.parser/Query query]
  (loop [remaining (.-qwhere query)
         patterns []]
    (if-some [clause (first remaining)]
      (if-some [pattern (default-pattern clause)]
        (recur (subvec remaining 1) (conj patterns pattern))
        None)
      (Some patterns))))

(defn ^:option<string> find-variable-name [^:find-element element]
  (match element
    (FindVariable variable) (Some (str (.-symbol variable)))
    _ None))

(defn ^:option<vector<string>> find-variable-names [^:find-spec find]
  (loop [remaining (find-spec-elements find)
         variables []]
    (if-some [element (first remaining)]
      (if-some [variable (find-variable-name element)]
        (recur (subvec remaining 1) (conj variables variable))
        None)
      (Some variables))))

(defn ^:option<vector<string>> find-projection-variable-names
  [^:find-spec find]
  (loop [remaining (find-spec-elements find)
         variables []]
    (if-some [element (first remaining)]
      (if-some [variable (find-variable-name element)]
        (recur (subvec remaining 1) (conj variables variable))
        (if-some [aggregate (find-element-aggregate element)]
          (if-some [argument (last (.-args aggregate))]
            (if-some [variable (argument-variable-name argument)]
              (recur
               (subvec remaining 1)
               (conj variables variable))
              None)
            None)
          (if-some [pull (find-element-pull element)]
            (recur
             (subvec remaining 1)
             (conj variables (pull-variable-name pull)))
            None)))
      (Some variables))))

(defn ^:option<datascript.parser/Aggregate> find-element-aggregate
  [^:find-element element]
  (match element
    (FindAggregate aggregate) (Some aggregate)
    _ None))

(defn ^:option<string> aggregate-function-name
  [^datascript.parser/Aggregate aggregate]
  (match (.-fn aggregate)
    (AggregatePlain function-name)
    (Some (str (.-symbol function-name)))
    _ None))

(defn ^:option<string> aggregate-custom-variable-name
  [^datascript.parser/Aggregate aggregate]
  (match (.-fn aggregate)
    (AggregateCustom variable)
    (Some (str (.-symbol variable)))
    _ None))

(defn ^:option<vector<Datascript_runtime.Data_value.t>>
  aggregate-constant-arguments
  [^datascript.parser/Aggregate aggregate]
  (let [arguments (.-args aggregate)
        parameter-count (dec (count arguments))]
    (if (< parameter-count 0)
      None
      (loop [remaining
             (subvec arguments 0 parameter-count)
             values []]
        (if-some [argument (first remaining)]
          (if-some [value (argument-constant argument)]
            (recur
             (subvec remaining 1)
             (conj values value))
            None)
          (Some values))))))

(defn ^:option<datascript.parser/Pull> find-element-pull
  [^:find-element element]
  (match element
    (FindPullElement pull) (Some pull)
    _ None))

(defn ^:string pull-variable-name [^datascript.parser/Pull pull]
  (str (.-symbol (.-variable pull))))

(defn ^:string pull-source-name [^datascript.parser/Pull pull]
  (str (.-symbol (.-source pull))))

(defn ^:option<Datascript_runtime.Data_value.t> pull-pattern-value
  [^datascript.parser/Pull pull]
  (match (.-pattern pull)
    (PullConstant pattern) (Some pattern)
    _ None))

(defn ^:option<string> pull-pattern-variable-name
  [^datascript.parser/Pull pull]
  (match (.-pattern pull)
    (PullVariable variable) (Some (str (.-symbol variable)))
    _ None))

(defn relation-find? [^:find-spec find]
  (match find
    (FindRelation _) true
    _ false))

(defn collection-find? [^:find-spec find]
  (match find
    (FindCollection _) true
    _ false))

(defn single-find? [^:find-spec find]
  (match find
    (FindSingle _) true
    _ false))

(defn ^:option<query-source> explicit-input [^clause clause]
  (let [explicit-source
        (fn [^query-source source]
          (match source
            DefaultSource None
            (ExplicitSource _) (Some source)))]
    (match clause
      (PatternClause source _) (Some source)
      (RuleClause source _ _) (explicit-source source)
      (NotClause source _ _ _) (explicit-source source)
      (OrClause source _ _ _ _) (explicit-source source)
      _ None)))

(signature datascript.parser/clause-has-input?
  :fn<datascript.parser/clause;bool>)
(declare clause-has-input?)

(defn ^boolean clauses-have-input?
  [^:vector<datascript.parser/clause> clauses]
  (match (some clause-has-input? clauses)
    None false
    (Some _) true))

(defn ^boolean clause-has-input?
  [^:datascript.parser/clause clause]
  (match clause
    (NotClause _ _ clauses _)
    (or
     (some? (explicit-input clause))
     (clauses-have-input? clauses))
    (OrClause _ _ _ clauses _)
    (or
     (some? (explicit-input clause))
     (clauses-have-input? clauses))
    (AndClause clauses) (clauses-have-input? clauses)
    _ (some? (explicit-input clause))))

(defn ^:vector<input-binding> default-in
  [^:vector<datascript.parser/clause> qwhere]
  (if (clauses-have-input? qwhere)
    [(InputSource (SrcVar. "$"))]
    []))

(defn ^:vector<datascript.parser/Variable> input-binding-vars
  [^:input-binding input]
  (match input
    (InputSource _) []
    (InputRules _) []
    (InputValueBinding binding) (binding-vars binding)))

(defn ^:vector<datascript.parser/SrcVar> input-binding-sources
  [^:input-binding input]
  (match input
    (InputSource source) [source]
    _ []))

(defn input-binding-rules? [^:input-binding input]
  (match input
    (InputRules _) true
    _ false))

(signature datascript.parser/source-values
  :fn<datascript.parser/query-source;vector<datascript.parser/SrcVar>>)

(defn ^:vector<datascript.parser/SrcVar> source-values
  [^:query-source source]
  (match source
    DefaultSource []
    (ExplicitSource source) [source]))

(signature datascript.parser/clause-sources
  :fn<datascript.parser/clause;vector<datascript.parser/SrcVar>>)

(defn ^:vector<datascript.parser/SrcVar> clause-sources
  [^:datascript.parser/clause clause]
  (match clause
      (PatternClause source _) (source-values source)
      (PredicateClause _ args)
      (vec (mapcat fn-arg-source-records args))
      (FunctionClause _ args _)
      (vec (mapcat fn-arg-source-records args))
      (RuleClause source _ _) (source-values source)
      (NotClause source _ clauses _)
      (vec
       (concat
        (source-values source)
        (mapcat clause-sources clauses)))
      (OrClause source _ _ clauses _)
      (vec
       (concat
        (source-values source)
        (mapcat clause-sources clauses)))
      (AndClause clauses) (vec (mapcat clause-sources clauses))))

(signature datascript.parser/validate-static-query-sources
  :fn<datascript.parser/Query;unit>)

(defn validate-static-query-sources [^datascript.parser/Query query]
  (let [known
        (if-some [inputs (static-query-inputs query)]
          (reduce
           (fn [^:vector<string> names ^static-query-input input]
             (if-some [name (static-input-source-name input)]
               (conj names name)
               names))
           []
           inputs)
          [])
        unknown
        (reduce
         (fn [^:vector<string> names ^datascript.parser/SrcVar source]
           (let [name (str (.-symbol source))]
             (if
              (or
               (some (fn [^:string known-name] (= known-name name)) known)
               (some (fn [^:string unknown-name] (= unknown-name name)) names))
               names
               (conj names name))))
         []
         (mapcat clause-sources (.-qwhere query)))]
    (if (empty? unknown)
      nil
      (Stdlib.invalid_arg
       (str
        "Where uses unknown source vars: ["
        (reduce
         (fn [^:string result ^:string name]
           (if (= result "") name (str result " " name)))
         ""
         unknown)
        "]")))))

(signature datascript.parser/clause-has-rule?
  :fn<datascript.parser/clause;bool>)
(declare clause-has-rule?)

(defn ^boolean clauses-have-rule?
  [^:vector<datascript.parser/clause> clauses]
  (match (some clause-has-rule? clauses)
    None false
    (Some _) true))

(defn ^boolean clause-has-rule?
  [^:datascript.parser/clause clause]
  (match clause
    (RuleClause _ _ _) true
    (NotClause _ _ clauses _) (clauses-have-rule? clauses)
    (OrClause _ _ _ clauses _) (clauses-have-rule? clauses)
    (AndClause clauses) (clauses-have-rule? clauses)
    _ false))

(defn- ^:Datascript_runtime.Data_value.t query-section-form
  [^:vector<Datascript_runtime.Data_value.t> items]
  (Datascript_runtime.Data_value.vector_of_vector items))

(defn- ^:option<keyword> query-keyword
  [^:Datascript_runtime.Data_value.t form]
  (match form
    (Datascript_runtime.Data_value.Keyword keyword)
    (let [^:keyword keyword keyword]
      (Some keyword))
    _ None))

(defn ^:map<keyword;vector<Datascript_runtime.Data_value.t>> query->map
  [^:Datascript_runtime.Data_value.t query]
  (if-some [items (data-value-items query)]
    (loop [remaining items
           parsed {}
           ^:option<keyword> section None]
      (if-some [item (first remaining)]
        (if-some [keyword (query-keyword item)]
          (recur (subvec remaining 1) parsed (Some keyword))
          (if-some [section section]
            (recur
             (subvec remaining 1)
             (assoc
              parsed
              section
              (conj (get parsed section []) item))
             (Some section))
            (recur (subvec remaining 1) parsed None)))
        parsed))
    (util/raise "Query should be a vector or a map"
      {:error :parser/query, :form query})))

(defn- ^:vector<string> variable-names
  [^:vector<datascript.parser/Variable> variables]
  (mapv
   (fn [^datascript.parser/Variable variable]
     (str (.-symbol variable)))
   variables))

(defn- ^boolean name-present?
  [^:vector<string> names ^:string name]
  (match
   (some (fn [^:string candidate] (= candidate name)) names)
   None false
   (Some _) true))

(defn- ^:vector<string> distinct-names
  [^:vector<string> names]
  (reduce
   (fn [^:vector<string> result ^:string name]
     (if (name-present? result name)
       result
       (conj result name)))
   []
   names))

(defn- ^:vector<string> names-difference
  [^:vector<string> names ^:vector<string> excluded]
  (reduce
   (fn [^:vector<string> result ^:string name]
     (if (or
          (name-present? excluded name)
          (name-present? result name))
       result
       (conj result name)))
   []
   names))

(defn- ^:string names-display [^:vector<string> names]
  (str "[" (join-rule-variable-names names) "]"))

(defn- ^:vector<string> query-find-vars
  [^:find-spec find]
  (vec (mapcat find-element-vars (find-spec-elements find))))

(defn- ^:vector<string> query-with-vars
  [^:option<vector<datascript.parser/Variable>> with]
  (if-some [variables with]
    (variable-names variables)
    []))

(defn- ^:vector<string> query-input-vars
  [^:vector<input-binding> inputs]
  (vec (mapcat variable-names (mapv input-binding-vars inputs))))

(defn- ^:vector<string> query-where-vars
  [^:vector<datascript.parser/clause> clauses]
  (vec (mapcat variable-names (mapv clause-vars clauses))))

(defn- ^:vector<string> query-input-source-names
  [^:vector<input-binding> inputs]
  (mapv
   (fn [^datascript.parser/SrcVar source]
     (str (.-symbol source)))
   (vec (mapcat input-binding-sources inputs))))

(defn- ^:int query-rules-input-count
  [^:vector<input-binding> inputs]
  (count (filter input-binding-rules? inputs)))

(defn validate-query
  [^datascript.parser/Query query
   ^:Datascript_runtime.Data_value.t _form
   ^:map<keyword;vector<Datascript_runtime.Data_value.t>> form-map]
  (let [find-vars (query-find-vars (.-qfind query))
        with-vars (query-with-vars (.-qwith query))
        input-vars (query-input-vars (.-qin query))
        where-vars (query-where-vars (.-qwhere query))
        known-vars (vec (concat where-vars input-vars))
        unknown
        (names-difference
         (vec (concat find-vars with-vars))
         known-vars)
        shared (names-difference find-vars
                                 (names-difference find-vars with-vars))]
    (when (not (empty? unknown))
      (Stdlib.invalid_arg
       (str "Query for unknown vars: " (names-display unknown))))
    (when (not (empty? shared))
      (Stdlib.invalid_arg
       (str
        ":find and :with should not use same variables: "
        (names-display shared)))))

  (if-some [return-map (.-qreturn-map query)]
    (let [type (str (return-map-type return-map))
          find (.-qfind query)]
      (cond
        (single-find? find)
        (Stdlib.invalid_arg
         (str type " does not work with single-scalar :find"))

        (collection-find? find)
        (Stdlib.invalid_arg
         (str type " does not work with collection :find"))

        (not
         (= (return-map-count return-map)
            (count (find-spec-elements find))))
        (Stdlib.invalid_arg
         (str "Count of " type " must match count of :find"))

        :else nil))
    nil)

  (let [return-map-count
        (count
         (filter
          (fn [^:keyword key] (contains? form-map key))
          [:keys :syms :strs]))]
    (when (< 1 return-map-count)
      (Stdlib.invalid_arg
       "Only one of :keys/:syms/:strs must be present")))

  (let [input-vars (query-input-vars (.-qin query))
        input-sources (query-input-source-names (.-qin query))
        rules-count (query-rules-input-count (.-qin query))]
    (when
        (or
         (not (= (count input-vars)
                 (count (distinct-names input-vars))))
         (not (= (count input-sources)
                 (count (distinct-names input-sources))))
         (< 1 rules-count))
      (Stdlib.invalid_arg "Vars used in :in should be distinct")))

  (let [with-vars (query-with-vars (.-qwith query))]
    (when
        (not (= (count with-vars)
                (count (distinct-names with-vars))))
      (Stdlib.invalid_arg "Vars used in :with should be distinct")))

  (validate-static-query-sources query)

  (when
      (and
       (clauses-have-rule? (.-qwhere query))
       (= 0 (query-rules-input-count (.-qin query))))
    (Stdlib.invalid_arg "Missing rules var '%' in :in")))

(defn ^datascript.parser/Query parse-query
  [^Datascript_runtime.Data_value.t query]
  (let [form-map (query->map query)
        where
        (parse-where
         (query-section-form (get form-map :where [])))
        find
        (parse-find
         (query-section-form (get form-map :find [])))
        with
        (if-some [items (get form-map :with)]
          (Some (parse-with (query-section-form items)))
          None)
        return-map
        (if-some [items (get form-map :keys)]
          (parse-return-map :keys (query-section-form items))
          (if-some [items (get form-map :syms)]
            (parse-return-map :syms (query-section-form items))
            (if-some [items (get form-map :strs)]
              (parse-return-map :strs (query-section-form items))
              None)))
        inputs
        (if-some [items (get form-map :in)]
          (parse-in (query-section-form items))
          (default-in where))
        result (Query. find with return-map inputs where)]
    (validate-query result query form-map)
    result))
