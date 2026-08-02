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

(defn  data-value-items
  [ form]
  (Datascript_runtime.Data_value.sequential_items form))

(defn  of-size?
  [ form  size]
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

(defn  parse-placeholder
  [ form]
  (match form
    (Datascript_runtime.Data_value.Symbol value)
    (if (= value "_") (Some (Placeholder.)) None)
    _ None))

(defn  parse-variable
  [ form]
  (match form
    (Datascript_runtime.Data_value.Symbol value)
    (if (= (String.get value 0) \?)
      (Some (Variable. value))
      None)
    _ None))

(defn  parse-var-required
  [ form]
  (if-some [variable (parse-variable form)]
    variable
    (util/raise "Cannot parse var, expected symbol starting with ?"
      {:error :parser/rule-var})))

(defn  parse-required-variables
  [ items]
  (mapv parse-var-required items))

(defn  parse-src-var
  [ form]
  (match form
    (Datascript_runtime.Data_value.Symbol value)
    (if (= (String.get value 0) \$)
      (Some (SrcVar. value))
      None)
    _ None))

(defn  parse-rules-var
  [ form]
  (match form
    (Datascript_runtime.Data_value.Symbol value)
    (if (= value "%") (Some (RulesVar.)) None)
    _ None))

(defn  parse-constant
  [ form]
  (if-some [_ (parse-variable form)]
    None
    (Some form)))

(defn  parse-plain-symbol
  [ form]
  (match form
    (Datascript_runtime.Data_value.Symbol value)
    (if (and (not= (String.get value 0) \?)
             (not= (String.get value 0) \$)
             (not= value "%")
             (not= value "_"))
      (Some (PlainSymbol. value))
      None)
    _ None))

(defn  parse-plain-variable
  [ form]
  (if-some [symbol (parse-plain-symbol form)]
    (Some (Variable. (:symbol symbol)))
    None))



;; fn-arg = (variable | constant | src-var)

(type-variant fn-arg
  (FnArgVariable :datascript.parser/Variable)
  (FnArgSource :datascript.parser/SrcVar)
  (FnArgConstant :data-value))

(defn  parse-fn-arg
  [ form]
  (if-some [value (parse-variable form)]
    (Some (FnArgVariable value))
    (if-some [value (parse-src-var form)]
      (Some (FnArgSource value))
      (if-some [value (parse-constant form)]
        (Some (FnArgConstant value))
        None))))

(defn  parse-fn-args
  [ items]
  (parse-items parse-fn-arg items))

;; rule-vars = [ variable+ | ([ variable+ ] variable*) ]

(deftrecord RuleVars
  [^:option<vector<datascript.parser/Variable>> required
   ^:vector<datascript.parser/Variable> free])

(defn  variables-distinct?
  [ variables]
  (distinct? variables))

(defn  parse-rule-var-items
  [ items]
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

(defn  parse-rule-vars
  [ form]
  (if-some [items (data-value-items form)]
    (parse-rule-var-items items)
    (util/raise "Cannot parse rule-vars, expected [ variable+ | ([ variable+ ] variable*) ]"
      {:error :parser/rule-vars})))

(signature datascript.parser/flatten-rule-vars
  :fn<datascript.parser/RuleVars;vector<symbol>>)
(defn  flatten-rule-vars
  [rule-vars]
  (vec
   (concat
    (match (:required rule-vars)
      None []
      (Some values) (mapv :symbol values))
    (mapv :symbol (:free rule-vars)))))

(signature datascript.parser/rule-vars-arity
  :fn<datascript.parser/RuleVars;tuple<int;int>>)
(defn  rule-vars-arity
  [rule-vars]
  (tuple
   (match (:required rule-vars)
     None 0
     (Some values) (count values))
   (count (:free rule-vars))))

(signature datascript.parser/join-rule-variable-names
  :fn<vector<string>;string>)
(defn  join-rule-variable-names
  [names]
  (if-some [first-name (first names)]
    (reduce
     (fn [ joined  name]
       (str joined " " name))
     first-name
     (subvec names 1))
    ""))

(signature datascript.parser/rule-vars-display
  :fn<datascript.parser/RuleVars;string>)
(defn  rule-vars-display
  [rule-vars]
  (let [required-parts
        (if-some [required (.-required rule-vars)]
          [(str
            "["
            (join-rule-variable-names
             (mapv
              (fn [ variable]
                (str (.-symbol variable)))
              required))
            "]")]
          [])
        free-parts
        (mapv
         (fn [ variable]
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

(defn  parse-bind-ignore
  [ form]
  (match form
    (Datascript_runtime.Data_value.Symbol value)
    (if (= value "_") (Some BindIgnore) None)
    _ None))

(defn  parse-bind-scalar
  [ form]
  (if-some [variable (parse-variable form)]
    (Some (BindScalar variable))
    None))

(defn  parse-bind-coll
  [ form]
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

(defn  parse-tuple-el
  [ form]
  (if-some [binding (parse-bind-ignore form)]
    (Some binding)
    (Some (parse-binding form))))

(defn  parse-tuple-elements
  [ items]
  (loop [remaining items
         parsed []]
    (if (empty? remaining)
      (Some parsed)
      (if-some [item (first remaining)]
        (if-some [binding (parse-tuple-el item)]
          (recur (subvec remaining 1) (conj parsed binding))
          None)
        None))))

(defn  parse-bind-tuple
  [ form]
  (if-some [items (data-value-items form)]
    (if-some [sub-bindings (parse-tuple-elements items)]
    (if-not (empty? sub-bindings)
      (Some (BindTuple sub-bindings))
      (util/raise "Tuple binding cannot be empty"
        {:error :parser/binding}))
    None)
    None))

(defn  parse-bind-rel
  [ form]
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

(defn  parse-binding
  [ form]
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

(defn fn-arg-vars [ arg]
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

(defn-  invalid-custom-aggregate []
  (Stdlib.invalid_arg
   "Cannot parse custom aggregate call, expect ['aggregate' variable fn-arg+]"))

(defn-  invalid-pull-expression []
  (Stdlib.invalid_arg
   "Cannot parse pull expression, expect ['pull' src-var? variable (constant | variable | plain-symbol)]"))

(defn-  parse-pull-pattern
  [ form]
  (if-some [variable (parse-variable form)]
    (Some (PullVariable variable))
    (if-some [variable (parse-plain-variable form)]
      (Some (PullVariable variable))
      (if-some [constant (parse-constant form)]
        (Some (PullConstant constant))
        None))))

(defn  parse-aggregate
  [ form]
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

(defn  parse-aggregate-custom
  [ form]
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

(defn  parse-pull-expr
  [ form]
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

(defn  parse-find-elem
  [ form]
  (if-some [variable (parse-variable form)]
    (Some (FindVariable variable))
    (if-some [pull (parse-pull-expr form)]
      (Some (FindPullElement pull))
      (if-some [aggregate (parse-aggregate-custom form)]
        (Some (FindAggregate aggregate))
        (if-some [aggregate (parse-aggregate form)]
          (Some (FindAggregate aggregate))
          None)))))

(defn  parse-find-elements
  [ items]
  (loop [remaining items
         elements []]
    (if-some [item (first remaining)]
      (if-some [element (parse-find-elem item)]
        (recur (subvec remaining 1) (conj elements element))
        None)
      (Some elements))))

(defn  parse-find-rel
  [ form]
  (if-some [items (data-value-items form)]
    (if-some [elements (parse-find-elements items)]
      (Some (FindRelation (FindRel. elements)))
      None)
    None))

(defn  parse-find-coll
  [ form]
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

(defn  parse-find-scalar
  [ form]
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

(defn  parse-find-tuple
  [ form]
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

(defn  parse-find
  [ form]
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

(defn find-element-vars [ element]
  (match element
    (FindVariable variable) (-find-vars variable)
    (FindPullElement pull) (-find-vars pull)
    (FindAggregate aggregate) (-find-vars aggregate)))

(defn  find-spec-elements [ find]
  (match find
    (FindRelation relation) (find-elements relation)
    (FindCollection collection) (find-elements collection)
    (FindSingle scalar) (find-elements scalar)
    (FindTupleResult tuple-result) (find-elements tuple-result)))

(defn find-vars [ find]
  (mapcat find-element-vars (find-spec-elements find)))

(defn aggregate? [ element]
  (match element
    (FindAggregate _) true
    _ false))

(defn pull? [ element]
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

(defn  return-map-type
  [ return-map]
  (match return-map
    (ReturnKeys _) :keys
    (ReturnSyms _) :syms
    (ReturnStrs _) :strs))

(defn  return-map-count
  [ return-map]
  (match return-map
    (ReturnKeys symbols) (count symbols)
    (ReturnSyms symbols) (count symbols)
    (ReturnStrs symbols) (count symbols)))

(defn  return-map-key-names
  [ return-map]
  (match return-map
    (ReturnKeys keys) (Some (mapv str keys))
    _ None))

(defn  return-map-symbol-names
  [ return-map]
  (match return-map
    (ReturnSyms keys) (Some (mapv str keys))
    _ None))

(defn  return-map-string-names
  [ return-map]
  (match return-map
    (ReturnStrs keys) (Some keys)
    _ None))

(defn  return-map-symbols
  [ form]
  (if-some [items (data-value-items form)]
    (reduce
     (fn [ result
           item]
       (match result
         None None
         (Some symbols)
         (match item
           (Datascript_runtime.Data_value.Symbol symbol)
           (let [ symbol symbol]
             (Some (conj symbols symbol)))
           _ None)))
     (Some [])
     items)
    None))

(signature datascript.parser/return-map-keyword
  :fn<symbol;keyword>)
(defn return-map-keyword [symbol]
  (str ":" symbol))

(defn  parse-return-map
  [ type  form]
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

(defn  parse-with
  [ form]
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

(defn-  parse-in-binding
  [ form]
  (if-some [source (parse-src-var form)]
    (InputSource source)
    (if-some [rules (parse-rules-var form)]
      (InputRules rules)
      (InputValueBinding (parse-binding form)))))

(defn  parse-in
  [ form]
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

(defn  parse-pattern-el
  [ form]
  (if-some [_ (parse-placeholder form)]
    (Some PatternPlaceholder)
    (if-some [value (parse-variable form)]
      (Some (PatternVariable value))
      (if-some [value (parse-constant form)]
        (Some (PatternConstant value))
        None))))

(defn  parse-pattern-elements
  [ items]
  (loop [remaining items
         parsed []]
    (if (empty? remaining)
      (Some parsed)
      (if-some [item (first remaining)]
        (if-some [element (parse-pattern-el item)]
          (recur (subvec remaining 1) (conj parsed element))
          None)
        None))))

(defn
  take-source
  [ form]
  (if-some [items (data-value-items form)]
    (if-some [head (first items)]
      (if-some [source (parse-src-var head)]
        (Some (tuple (ExplicitSource source) (subvec items 1)))
        (Some (tuple DefaultSource items)))
      (Some (tuple DefaultSource items)))
    None))
      
(defn  parse-pattern
  [ form]
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

(defn  parse-call
  [ form]
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

(defn  parse-pred
  [ form]
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

(defn  parse-fn
  [ form]
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

(defn  parse-rule-expr
  [ form]
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

(defn  fn-arg-variable-records
  [ arg]
  (match arg
    (FnArgVariable variable) [variable]
    (FnArgSource _) []
    (FnArgConstant _) []))

(defn  fn-arg-source-records
  [ arg]
  (match arg
    (FnArgSource source) [source]
    _ []))

(signature datascript.parser/binding-vars
  :fn<datascript.parser/binding;vector<datascript.parser/Variable>>)

(defn  binding-vars
  [ binding]
  (match binding
    BindIgnore []
    (BindScalar variable) [variable]
    (BindTuple bindings) (vec (mapcat binding-vars bindings))
    (BindColl binding) (binding-vars binding)))

(signature datascript.parser/pattern-element-vars
  :fn<datascript.parser/pattern-element;vector<datascript.parser/Variable>>)
(signature datascript.parser/pattern-element-variable-symbol
  :fn<datascript.parser/pattern-element;option<symbol>>)

(defn  pattern-element-vars
  [ element]
  (match element
    PatternPlaceholder []
    (PatternVariable variable) [variable]
    (PatternConstant _) []))

(defn  pattern-element-variable-symbol
  [ element]
  (match element
    (PatternVariable variable) (Some (:symbol variable))
    _ None))

(signature datascript.parser/callable-vars
  :fn<datascript.parser/query-callable;vector<datascript.parser/Variable>>)

(defn  callable-vars
  [ callable]
  (match callable
    (StaticCallable _) []
    (VariableCallable variable) [variable]))

(signature datascript.parser/rule-vars-values
  :fn<datascript.parser/RuleVars;vector<datascript.parser/Variable>>)

(defn  rule-vars-values
  [ vars]
  (vec
   (concat
    (if-some [required (:required vars)] required [])
    (:free vars))))

(signature datascript.parser/clause-vars
  :fn<datascript.parser/clause;vector<datascript.parser/Variable>>)

(defn  clause-vars
  [ clause]
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
  [ clauses]
  (vec (distinct (mapcat clause-vars clauses))))

(defn- auto-rule-variable
  [variable seqid]
  (str variable "__auto__" seqid))

(defn- substitute-rule-variable
  [replacements seqid variable]
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

(defn-  substitute-rule-pattern-element
  [replacements
    seqid
    element]
  (match element
    (PatternVariable variable)
    (let [name (str (.-symbol variable))]
      (if-some [replacement (get replacements name)]
        replacement
        (PatternVariable
         (Variable. (auto-rule-variable name seqid)))))
    _ element))

(defn-  substitute-rule-fn-arg
  [replacements
    seqid
    argument]
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

(defn-  substitute-rule-callable
  [ replacements
    seqid
    callable]
  (match callable
    (VariableCallable variable)
    (VariableCallable
     (substitute-rule-variable replacements seqid variable))
    _ callable))

(declare substitute-rule-binding
         substitute-rule-clause
         substitute-rule-clauses)

(defn-  substitute-rule-binding
  [ replacements
    seqid
    binding]
  (match binding
    BindIgnore BindIgnore
    (BindScalar variable)
    (BindScalar
     (substitute-rule-variable replacements seqid variable))
    (BindTuple bindings)
    (BindTuple
     (mapv
      (fn [ binding]
        (substitute-rule-binding replacements seqid binding))
      bindings))
    (BindColl binding)
    (BindColl
     (substitute-rule-binding replacements seqid binding))))

(defn-  substitute-rule-vars
  [ replacements
    seqid
    ^datascript.parser/RuleVars variables]
  (RuleVars.
   (match (.-required variables)
     None None
     (Some required)
     (Some
      (mapv
       (fn [ variable]
         (substitute-rule-variable
          replacements seqid variable))
       required)))
   (mapv
    (fn [ variable]
      (substitute-rule-variable replacements seqid variable))
    (.-free variables))))

(defn-  substitute-rule-clause
  [ replacements
    seqid
    clause]
  (match clause
    (PatternClause source pattern)
    (PatternClause
     source
     (mapv
      (fn [ element]
        (substitute-rule-pattern-element
         replacements seqid element))
      pattern))
    (PredicateClause callable arguments)
    (PredicateClause
     (substitute-rule-callable replacements seqid callable)
     (mapv
      (fn [ argument]
        (substitute-rule-fn-arg
         replacements seqid argument))
      arguments))
    (FunctionClause callable arguments binding)
    (FunctionClause
     (substitute-rule-callable replacements seqid callable)
     (mapv
      (fn [ argument]
        (substitute-rule-fn-arg
         replacements seqid argument))
      arguments)
     (substitute-rule-binding replacements seqid binding))
    (RuleClause source name arguments)
    (RuleClause
     source
     name
     (mapv
      (fn [ argument]
        (substitute-rule-pattern-element
         replacements seqid argument))
      arguments))
    (NotClause source variables clauses display)
    (NotClause
     source
     (mapv
      (fn [ variable]
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

(defn ^:private substitute-rule-clauses
  [ replacements
    seqid
    clauses]
  (mapv
   (fn [ clause]
     (substitute-rule-clause replacements seqid clause))
   clauses))

(defn parse-clauses
  [ clauses]
  (parse-items
   (fn [ form]
     (Some (parse-clause form)))
   clauses))

(defn- validate-not [ clause]
  (match clause
    (NotClause _ vars _ _)
    (do
      (when (empty? vars)
        (util/raise "Join variables should not be empty"
          {:error :parser/where}))
      clause)
    _ clause))

(defn  parse-not
  [ form]
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

(defn  parse-variables
  [ items]
  (parse-items parse-variable items))

(defn  parse-not-join
  [ form]
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
  [ clause
    _form]
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

(defn
  parse-disjunction-clauses
  [ items]
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

(defn  parse-and
  [ form]
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

(defn  parse-or
  [ form]
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

(defn  parse-or-join
  [ form]
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

(defn parse-clause
  [ form]
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

(defn parse-where
  [ form]
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

(defn parse-rule
  [ form]
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

(signature datascript.parser/validate-arity
  :fn<datascript.parser/PlainSymbol;vector<datascript.parser/RuleBranch>;unit>)
(defn validate-arity
  [name branches]
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

(signature datascript.parser/add-rule-branch
  :fn<vector<datascript.parser/Rule>;datascript.parser/RuleBranch;vector<datascript.parser/Rule>>)
(defn add-rule-branch
  [rules branch]
    (loop [remaining rules
          result []
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

(defn  parse-rules
  [ form]
  (if-some [items (data-value-items form)]
    (reduce
     (fn [ rules
           rule-form]
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

(defn  variable-traversable [ name]
  (TraversalVariable (Variable. name)))

(defn  binding-traversable [ binding]
  (TraversalBinding binding))

(defn  find-traversable [ find]
  (TraversalFind find))

(defn  clause-traversable [ clause]
  (TraversalClause clause))

(defn  rule-traversable [ rule]
  (TraversalRule rule))

(defn  query-traversable [ query]
  (TraversalQuery query))

(defn  data-traversable [ value]
  (TraversalData value))

(defn  string-traversable [ value]
  (TraversalString value))

(defn  traversable-variable-name [ node]
  (match node
    (TraversalVariable variable) (Some (str (.-symbol variable)))
    _ None))

(defn  traversable-clause? [ node]
  (match node
    (TraversalClause _) true
    _ false))

(defn  traversable-clause-value [ node]
  (match node
    (TraversalClause clause) (Some clause)
    _ None))

(defn  traversable-kind [ node]
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

(defn-  query-source-traversable [ source]
  (match source
    DefaultSource (TraversalDefaultSource (DefaultSrc.))
    (ExplicitSource source) (TraversalSource source)))

(defn-  pattern-element-traversable
  [ element]
  (match element
    PatternPlaceholder (TraversalPlaceholder (Placeholder.))
    (PatternVariable variable) (TraversalVariable variable)
    (PatternConstant value) (TraversalData value)))

(defn-  fn-arg-traversable [ argument]
  (match argument
    (FnArgVariable variable) (TraversalVariable variable)
    (FnArgSource source) (TraversalSource source)
    (FnArgConstant value) (TraversalData value)))

(defn-  query-callable-traversable
  [ callable]
  (match callable
    (StaticCallable function-name)
    (TraversalPlainSymbol function-name)
    (VariableCallable variable)
    (TraversalVariable variable)))

(defn-  aggregate-function-traversable
  [ function]
  (match function
    (AggregatePlain function-name)
    (TraversalPlainSymbol function-name)
    (AggregateCustom variable)
    (TraversalVariable variable)))

(defn-  pull-pattern-traversable [ pattern]
  (match pattern
    (PullVariable variable) (TraversalVariable variable)
    (PullConstant value) (TraversalData value)))

(defn-  find-element-traversable [ element]
  (match element
    (FindVariable variable) (TraversalVariable variable)
    (FindPullElement pull) (TraversalPull pull)
    (FindAggregate aggregate) (TraversalAggregate aggregate)))

(defn-  input-binding-traversable [ input]
  (match input
    (InputSource source) (TraversalSource source)
    (InputRules rules) (TraversalRules rules)
    (InputValueBinding binding) (TraversalBinding binding)))

(defn-  traversable-variable-exn
  [ node]
  (match node
    (TraversalVariable variable) variable
    _ (Stdlib.invalid_arg "Expected variable traversal node")))

(defn-  traversable-source-exn
  [ node]
  (match node
    (TraversalSource source) source
    _ (Stdlib.invalid_arg "Expected source traversal node")))

(defn-  traversable-plain-symbol-exn
  [ node]
  (match node
    (TraversalPlainSymbol symbol) symbol
    _ (Stdlib.invalid_arg "Expected plain-symbol traversal node")))

(defn-  traversable-data-exn [ node]
  (match node
    (TraversalData value) value
    (TraversalConstant constant) (.-value constant)
    _ (Stdlib.invalid_arg "Expected data traversal node")))

(defn-  traversable-symbol-exn [ node]
  (match node
    (TraversalSymbol symbol) symbol
    _ (Stdlib.invalid_arg "Expected symbol traversal node")))

(defn-  traversable-keyword-exn [ node]
  (match node
    (TraversalKeyword keyword) keyword
    _ (Stdlib.invalid_arg "Expected keyword traversal node")))

(defn-  traversable-string-exn [ node]
  (match node
    (TraversalString value) value
    _ (Stdlib.invalid_arg "Expected string traversal node")))

(defn-  traversable-vector-exn
  [ node]
  (match node
    (TraversalVector values) values
    _ (Stdlib.invalid_arg "Expected vector traversal node")))

(defn-  traversable-binding-exn [ node]
  (match node
    (TraversalBinding binding) binding
    _ (Stdlib.invalid_arg "Expected binding traversal node")))

(defn-  traversable-clause-exn [ node]
  (match node
    (TraversalClause clause) clause
    _ (Stdlib.invalid_arg "Expected clause traversal node")))

(defn-  traversable-rule-vars-exn
  [ node]
  (match node
    (TraversalRuleVars variables) variables
    _ (Stdlib.invalid_arg "Expected rule-vars traversal node")))

(defn-  traversable-rule-branch-exn
  [ node]
  (match node
    (TraversalRuleBranch branch) branch
    _ (Stdlib.invalid_arg "Expected rule-branch traversal node")))

(defn-  traversable-find-exn [ node]
  (match node
    (TraversalFind find) find
    _ (Stdlib.invalid_arg "Expected find traversal node")))

(defn-  traversable-return-map-exn [ node]
  (match node
    (TraversalReturnMap return-map) return-map
    _ (Stdlib.invalid_arg "Expected return-map traversal node")))

(defn-  variables-traversable
  [ variables]
  (TraversalVector
   (mapv
    (fn [ variable]
      (TraversalVariable variable))
    variables)))

(defn-  bindings-traversable
  [ bindings]
  (TraversalVector
   (mapv
    (fn [ binding]
      (TraversalBinding binding))
    bindings)))

(defn-  fn-args-traversable
  [ arguments]
  (TraversalVector (mapv fn-arg-traversable arguments)))

(defn-  pattern-elements-traversable
  [ elements]
  (TraversalVector (mapv pattern-element-traversable elements)))

(defn-  find-elements-traversable
  [ elements]
  (TraversalVector (mapv find-element-traversable elements)))

(defn-  clauses-traversable
  [ clauses]
  (TraversalVector
   (mapv
    (fn [ clause]
      (TraversalClause clause))
    clauses)))

(defn-  rule-branches-traversable
  [ branches]
  (TraversalVector
   (mapv
    (fn [ branch]
      (TraversalRuleBranch branch))
    branches)))

(defn-  inputs-traversable
  [ inputs]
  (TraversalVector (mapv input-binding-traversable inputs)))

(defn-  traversable-query-source-exn [ node]
  (match node
    (TraversalDefaultSource _) DefaultSource
    (TraversalSource source) (ExplicitSource source)
    _ (Stdlib.invalid_arg "Expected query-source traversal node")))

(defn-  traversable-pattern-element-exn
  [ node]
  (match node
    (TraversalPlaceholder _) PatternPlaceholder
    (TraversalVariable variable) (PatternVariable variable)
    (TraversalData value) (PatternConstant value)
    (TraversalConstant constant) (PatternConstant (.-value constant))
    _ (Stdlib.invalid_arg "Expected pattern-element traversal node")))

(defn-  traversable-fn-arg-exn [ node]
  (match node
    (TraversalVariable variable) (FnArgVariable variable)
    (TraversalSource source) (FnArgSource source)
    (TraversalData value) (FnArgConstant value)
    (TraversalConstant constant) (FnArgConstant (.-value constant))
    _ (Stdlib.invalid_arg "Expected function-argument traversal node")))

(defn-  traversable-query-callable-exn
  [ node]
  (match node
    (TraversalPlainSymbol function-name) (StaticCallable function-name)
    (TraversalVariable variable) (VariableCallable variable)
    _ (Stdlib.invalid_arg "Expected callable traversal node")))

(defn-  traversable-aggregate-function-exn
  [ node]
  (match node
    (TraversalPlainSymbol function-name) (AggregatePlain function-name)
    (TraversalVariable variable) (AggregateCustom variable)
    _ (Stdlib.invalid_arg "Expected aggregate-function traversal node")))

(defn-  traversable-pull-pattern-exn [ node]
  (match node
    (TraversalVariable variable) (PullVariable variable)
    (TraversalData value) (PullConstant value)
    (TraversalConstant constant) (PullConstant (.-value constant))
    _ (Stdlib.invalid_arg "Expected pull-pattern traversal node")))

(defn-  traversable-find-element-exn [ node]
  (match node
    (TraversalVariable variable) (FindVariable variable)
    (TraversalPull pull) (FindPullElement pull)
    (TraversalAggregate aggregate) (FindAggregate aggregate)
    _ (Stdlib.invalid_arg "Expected find-element traversal node")))

(defn-  traversable-input-binding-exn [ node]
  (match node
    (TraversalSource source) (InputSource source)
    (TraversalRules rules) (InputRules rules)
    (TraversalBinding binding) (InputValueBinding binding)
    _ (Stdlib.invalid_arg "Expected input traversal node")))

(defn-  traversable-variables-exn
  [ node]
  (mapv traversable-variable-exn (traversable-vector-exn node)))

(defn-  traversable-bindings-exn [ node]
  (mapv traversable-binding-exn (traversable-vector-exn node)))

(defn-  traversable-fn-args-exn [ node]
  (mapv traversable-fn-arg-exn (traversable-vector-exn node)))

(defn-  traversable-pattern-elements-exn
  [ node]
  (mapv traversable-pattern-element-exn
        (traversable-vector-exn node)))

(defn-  traversable-find-elements-exn
  [ node]
  (mapv traversable-find-element-exn
        (traversable-vector-exn node)))

(defn-  traversable-clauses-exn [ node]
  (mapv traversable-clause-exn (traversable-vector-exn node)))

(defn-
  traversable-rule-branches-exn
  [ node]
  (mapv traversable-rule-branch-exn
        (traversable-vector-exn node)))

(defn-  traversable-inputs-exn
  [ node]
  (mapv traversable-input-binding-exn
        (traversable-vector-exn node)))

(defn-  data-values-traversable
  [values]
  (Rrbvec.of_list
   (List.map
    (fn [ value]
      (TraversalData value))
    values)))

(defn-  data-map-entries-traversable
  [entries]
  (Rrbvec.of_list
   (List.map
    (fn [entry]
      (match entry
        (tuple key value)
        (TraversalVector
         [(TraversalData key)
          (TraversalData value)])))
    entries)))

(defn-  optional-data-values-traversable
  [values]
  (let [ values
        (Rrbvec.of_list values)]
    (loop [ idx 0
            result []]
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

(defn-  data-value-children [ value]
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

(defn-  traversable-children [ node]
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
         (fn [ key] (TraversalKeyword key))
         keys))]
      (ReturnSyms symbols)
      [(TraversalKeyword :syms)
       (TraversalVector
        (mapv
         (fn [ symbol] (TraversalSymbol symbol))
         symbols))]
      (ReturnStrs strings)
      [(TraversalKeyword :strs)
       (TraversalVector
        (mapv
         (fn [ value] (TraversalString value))
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

(defn-  traversable-keywords-exn [ node]
  (mapv traversable-keyword-exn (traversable-vector-exn node)))

(defn-  traversable-symbols-exn [ node]
  (mapv traversable-symbol-exn (traversable-vector-exn node)))

(defn-  traversable-strings-exn [ node]
  (mapv traversable-string-exn (traversable-vector-exn node)))

(defn-  postwalk-optional-data-values
  [values
    walk]
  (let [ values
        (Rrbvec.of_list values)]
    (loop [ idx 0
            result []]
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

(defn-  postwalk-data-values
  [values
    walk]
  (List.map
   (fn [ value]
     (traversable-data-exn
      (walk (TraversalData value))))
   values))

(defn-  postwalk-data-value
  [ value
    walk]
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
      (fn [entry]
        (match entry
          (tuple key value)
          (tuple
           key
           (traversable-data-exn
            (walk (TraversalData value))))))
      entries))
    (Datascript_runtime.Data_value.Set values)
    (Datascript_runtime.Data_value.Set
     (postwalk-data-values values walk))
    (Datascript_runtime.Data_value.Tuple values)
    (Datascript_runtime.Data_value.Tuple
     (postwalk-optional-data-values values walk))
    _ value))

(defn-  traversable-postwalk
  [ node
    f
    apply-root?]
  (let [walk
        (fn [ child]
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
                      (fn [ key]
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
                      (fn [ symbol]
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
                      (fn [ value]
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

(defn-  collect-traversable
  [ pred
    node
    acc]
  (if (pred node)
    (conj acc node)
    (reduce
     (fn [ result  child]
       (collect-traversable pred child result))
     acc
     (traversable-children node))))

(defn-  collect-vars-traversable
  [ acc
    node]
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
     pred
     acc]
   :vector<traversable>)
  (-collect-vars
   [this  acc]
   :vector<datascript.parser/Variable>)
  (-postwalk
   [this  f]
   :traversable))

(extend-type datascript.parser/traversable
  ITraversable
  (-collect
    [ node
      pred
      acc]
    (reduce
     (fn [ result  child]
       (collect-traversable pred child result))
     acc
     (traversable-children node)))
  (-collect-vars
    [ node
      acc]
    (reduce collect-vars-traversable
            acc
            (traversable-children node)))
  (-postwalk
    [ node
      f]
    (traversable-postwalk node f false)))

(defn collect
  ([pred form]
   (collect pred form []))
  ([pred form ^:vector<traversable> acc]
   (if (pred form)
     (conj acc form)
     (-collect form pred acc))))

(defn  postwalk
  [ form
    f]
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
(signature datascript.parser/expand-rule-branch
  :fn<datascript.parser/RuleBranch;vector<datascript.parser/pattern-element>;int;vector<datascript.parser/clause>>)
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

(defn  variable-find-element [ variable]
  (FindVariable (Variable. variable)))

(defn  pattern-variable [ variable]
  (PatternVariable (Variable. variable)))

(defn  pattern-constant
  [ value]
  (PatternConstant value))

(defn  pattern-element-constant
  [ element]
  (match element
    (PatternConstant value) (Some value)
    _ None))

(defn  pattern-attribute [ attr]
  (pattern-constant
   (Datascript_runtime.Data_value.Keyword (str attr))))

(defn  pattern-clause
  [ pattern]
  (PatternClause DefaultSource pattern))

(defn  explicit-pattern-clause
  [ source  pattern]
  (PatternClause (ExplicitSource (SrcVar. source)) pattern))

(defn  query-source-name [ source]
  (match source
    DefaultSource None
    (ExplicitSource source) (Some (str (.-symbol source)))))

(defn  variable-argument [ variable]
  (FnArgVariable (Variable. variable)))

(defn  source-argument [ source]
  (FnArgSource (SrcVar. source)))

(defn  constant-argument
  [ value]
  (FnArgConstant value))

(defn  pattern-placeholder []
  PatternPlaceholder)

(defn  argument-variable-name [ argument]
  (match argument
    (FnArgVariable variable) (Some (str (.-symbol variable)))
    _ None))

(defn  argument-constant
  [ argument]
  (match argument
    (FnArgConstant value) (Some value)
    _ None))

(defn  argument-source-name [ argument]
  (match argument
    (FnArgSource source) (Some (str (.-symbol source)))
    _ None))

(defn  static-callable-name
  [ callable]
  (match callable
    (StaticCallable name) (Some (str (.-symbol name)))
    (VariableCallable _) None))

(defn  variable-callable-name
  [ callable]
  (match callable
    (StaticCallable _) None
    (VariableCallable variable) (Some (str (.-symbol variable)))))

(defn  greater-than-clause
  [ left  right]
  (static-predicate-clause ">" [left right]))

(defn  static-predicate-clause
  [ function-name  arguments]
  (PredicateClause
   (StaticCallable (PlainSymbol. function-name))
   arguments))

(defn  static-function-clause
  [ function-name
    arguments
    binding]
  (FunctionClause
   (StaticCallable (PlainSymbol. function-name))
   arguments
   binding))

(defn  variable-function-clause
  [ variable
    arguments
    binding]
  (FunctionClause
   (VariableCallable (Variable. variable))
   arguments
   binding))

(defn  variable-predicate-clause
  [ variable  arguments]
  (PredicateClause
   (VariableCallable (Variable. variable))
   arguments))

(defn  static-not-clause
  [ clauses  display]
  (NotClause
   DefaultSource
   (collect-vars-distinct clauses)
   clauses
   display))

(defn  static-source-not-clause
  [ source  clauses  display]
  (NotClause
   (ExplicitSource (SrcVar. source))
   (collect-vars-distinct clauses)
   clauses
   display))

(defn  static-not-join-clause
  [ variables
    clauses
    display]
  (NotClause
   DefaultSource
   (mapv
    (fn [ variable]
      (Variable. variable))
    variables)
   clauses
   display))

(defn  static-source-not-join-clause
  [ source
    variables
    clauses
    display]
  (NotClause
   (ExplicitSource (SrcVar. source))
   (mapv
    (fn [ variable]
      (Variable. variable))
    variables)
   clauses
   display))

(defn  static-and-clause
  [ clauses]
  (if (empty? clauses)
    (Stdlib.invalid_arg "Cannot create an empty and clause")
    (AndClause clauses)))

(defn  and-clause-clauses
  [ clause]
  (match clause
    (AndClause clauses) (Some clauses)
    _ None))

(defn  rule-vars-names
  [variables]
  (mapv
   (fn [ variable]
     (str (.-symbol variable)))
   (rule-vars-values variables)))

(signature datascript.parser/rule-vars-required-names
  :fn<datascript.parser/RuleVars;vector<string>>)
(defn  rule-vars-required-names
  [variables]
  (if-some [required (.-required variables)]
    (mapv
     (fn [ variable]
       (str (.-symbol variable)))
     required)
    []))

(defn
  or-clause-parts
  [ clause]
  (match clause
    (OrClause _ _ variables branches _)
    (Some
     (tuple
      (rule-vars-required-names variables)
      (rule-vars-names variables)
      branches))
    _ None))

(defn  or-clause-source-name
  [ clause]
  (match clause
    (OrClause source _ _ _ _) (query-source-name source)
    _ None))

(defn  or-clause-display
  [ clause]
  (match clause
    (OrClause _ _ _ _ display) (Some display)
    _ None))

(defn  or-clause-join?
  [ clause]
  (match clause
    (OrClause _ PlainDisjunction _ _ _) false
    (OrClause _ JoinDisjunction _ _ _) true
    _ false))

(defn  static-or-clause-for-source
  [ source
    branches
    display]
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

(defn  static-or-clause
  [ branches  display]
  (static-or-clause-for-source DefaultSource branches display))

(defn  static-source-or-clause
  [ source
    branches
    display]
  (static-or-clause-for-source
   (ExplicitSource (SrcVar. source))
   branches
   display))

(defn  static-or-join-clause-for-source
  [ source
    required
    free
    branches
    display]
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
                (fn [ variable]
                  (Variable. variable))
                required)))
            (mapv
             (fn [ variable]
               (Variable. variable))
             free))
           branches
           display))))))

(defn  static-or-join-clause
  [ required
    free
    branches
    display]
  (static-or-join-clause-for-source
   DefaultSource required free branches display))

(defn  static-source-or-join-clause
  [ source
    required
    free
    branches
    display]
  (static-or-join-clause-for-source
   (ExplicitSource (SrcVar. source))
   required
   free
   branches
   display))

(defn  static-rule-clause
  [ rule-name  arguments]
  (RuleClause
   DefaultSource
   (PlainSymbol. rule-name)
   arguments))

(defn  static-source-rule-clause
  [ source
    rule-name
    arguments]
  (RuleClause
   (ExplicitSource (SrcVar. source))
   (PlainSymbol. rule-name)
   arguments))

(defn  static-rule-variable
  [ parameter]
  (if
   (and
    (not (= parameter ""))
    (= (subs parameter 0 1) "?"))
    (Variable. parameter)
    (Stdlib.invalid_arg
     (str
      "Cannot parse var, expected symbol starting with ?, got: "
      parameter))))

(defn  static-rule-branch
  [ rule-name
    parameters
    clauses]
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

(defn  static-rule-branch-with-vars
  [ rule-name
    required
    free
    clauses]
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

(defn  static-rules
  [ branches]
  (reduce add-rule-branch [] branches))

(defn  expand-rule-branch
  [ branch
    arguments
    seqid]
  (let [parameters (rule-vars-names (.-vars branch))]
    (if (= (count parameters) (count arguments))
      (let [replacements
            (reduce-kv
             (fn [ replacements
                   index
                   parameter]
               (assoc replacements parameter (nth arguments index)))
             {}
             parameters)]
        (substitute-rule-clauses
         replacements seqid (.-clauses branch)))
      (Stdlib.invalid_arg "Rule arity mismatch"))))

(defn  rule-clause-parts
  [ clause]
  (match clause
    (RuleClause _ name arguments)
    (Some (tuple (str (.-symbol name)) arguments))
    _ None))

(defn  rule-clause-source-name
  [ clause]
  (match clause
    (RuleClause source _ _) (query-source-name source)
    _ None))

(defn  rule-branches
  [ rules  rule-name]
  (if-some [rule
            (first
             (filter
              (fn [ rule]
                (= rule-name (str (.-symbol (.-name rule)))))
              rules))]
    (Some (.-branches rule))
    None))

(defn  rule-branch-parameter-names
  [ branch]
  (rule-vars-names (.-vars branch)))

(defn  rule-branch-required-parameter-names
  [ branch]
  (rule-vars-required-names (.-vars branch)))

(defn  rule-branch-clauses
  [ branch]
  (.-clauses branch))

(defn  relation-find [ variables]
  (FindRelation
   (FindRel. (mapv variable-find-element variables))))

(defn  relation-find-elements
  [ elements]
  (FindRelation (FindRel. elements)))

(defn  aggregate-find-element
  [ function-name
    arguments]
  (FindAggregate
   (Aggregate.
    (AggregatePlain (PlainSymbol. function-name))
    arguments)))

(defn  custom-aggregate-find-element
  [ variable  arguments]
  (FindAggregate
   (Aggregate.
    (AggregateCustom (Variable. variable))
    arguments)))

(declare
 pull-source-find-element
 pull-source-variable-find-element)

(defn  pull-find-element
  [ variable
    pattern]
  (pull-source-find-element "$" variable pattern))

(defn  pull-source-find-element
  [ source
    variable
    pattern]
  (FindPullElement
   (Pull.
    (SrcVar. source)
    (Variable. variable)
    (PullConstant pattern))))

(defn  pull-variable-find-element
  [ variable  pattern-variable]
  (pull-source-variable-find-element
   "$" variable pattern-variable))

(defn  pull-source-variable-find-element
  [ source
    variable
    pattern-variable]
  (FindPullElement
   (Pull.
    (SrcVar. source)
    (Variable. variable)
    (PullVariable (Variable. pattern-variable)))))

(defn  collection-find [ variable]
  (collection-find-element
   (variable-find-element variable)))

(defn  collection-find-element
  [ element]
  (FindCollection (FindColl. element)))

(defn  single-find [ variable]
  (single-find-element
   (variable-find-element variable)))

(defn  single-find-element
  [ element]
  (FindSingle (FindScalar. element)))

(defn  tuple-find [ variables]
  (tuple-find-elements
   (mapv variable-find-element variables)))

(defn  tuple-find-elements
  [ elements]
  (FindTupleResult (FindTuple. elements)))

(defn  static-query
  [ find
    patterns]
  (Query.
   find
   None
   None
   []
   (mapv
    (fn [ pattern]
      (PatternClause DefaultSource pattern))
    patterns)))

(defn  static-db-query
  [ find
    patterns]
  (let [query (static-query find patterns)]
    (Query.
     (.-qfind query)
     (.-qwith query)
     (.-qreturn-map query)
     [(InputSource (SrcVar. "$"))]
     (.-qwhere query))))

(defn  static-db-query-clauses
  [ find  clauses]
  (Query.
   find
   None
   None
   [(InputSource (SrcVar. "$"))]
   clauses))

(defn  scalar-input [ variable]
  (BindScalar (Variable. variable)))

(defn  ignore-input []
  BindIgnore)

(defn  tuple-input [ bindings]
  (BindTuple bindings))

(defn  collection-input [ binding]
  (BindColl binding))

(defn  static-db-query-with-bindings
  [ find
    patterns
    bindings]
  (let [query (static-db-query find patterns)]
    (Query.
     (.-qfind query)
     (.-qwith query)
     (.-qreturn-map query)
     (vec
      (concat
       (.-qin query)
       (mapv
        (fn [ binding]
          (InputValueBinding binding))
        bindings)))
     (.-qwhere query))))

(defn  static-query-with-bindings
  [ find
    patterns
    bindings]
  (let [query (static-query find patterns)]
    (Query.
     (.-qfind query)
     (.-qwith query)
     (.-qreturn-map query)
     (mapv
      (fn [ binding]
        (InputValueBinding binding))
      bindings)
     (.-qwhere query))))

(defn  static-db-query-clauses-with-bindings
  [ find
    clauses
    bindings]
  (Query.
   find
   None
   None
   (vec
    (concat
     [(InputSource (SrcVar. "$"))]
     (mapv
      (fn [ binding]
        (InputValueBinding binding))
      bindings)))
   clauses))

(defn  static-query-clauses-with-bindings
  [ find
    clauses
    bindings]
  (Query.
   find
   None
   None
   (mapv
    (fn [ binding]
      (InputValueBinding binding))
    bindings)
   clauses))

(defn  make-static-rules-input []
  StaticRulesInput)

(defn  make-static-source-input [ source]
  (StaticSourceInput (SrcVar. source)))

(defn  make-static-value-input [ binding]
  (StaticValueInput binding))

(defn  static-input-binding-form
  [ input]
  (match input
    (StaticSourceInput source) (InputSource source)
    StaticRulesInput (InputRules (RulesVar.))
    (StaticValueInput binding) (InputValueBinding binding)))

(defn  static-db-query-clauses-with-inputs
  [ find
    clauses
    inputs]
  (Query.
   find
   None
   None
   (vec
    (concat
     [(InputSource (SrcVar. "$"))]
     (mapv static-input-binding-form inputs)))
   clauses))

(defn  static-query-clauses-with-inputs
  [ find
    clauses
    inputs]
  (Query.
   find
   None
   None
   (mapv static-input-binding-form inputs)
   clauses))

(defn  query-input-value-binding
  [ input]
  (match input
    (InputValueBinding binding) (Some binding)
    _ None))

(defn  static-query-inputs
  [ query]
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

(defn  static-input-rules? [ input]
  (match input
    StaticRulesInput true
    _ false))

(defn  static-input-source-name
  [ input]
  (match input
    (StaticSourceInput source) (Some (str (.-symbol source)))
    _ None))

(defn  static-input-binding
  [ input]
  (match input
    (StaticValueInput binding) (Some binding)
    _ None))

(defn  query-with
  [ query  variables]
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
       (fn [ variable]
         (contains? find-variable-set variable))
       variables)
      (Stdlib.invalid_arg
       ":find and :with should not use same variables")

      :else
      (Query.
       (.-qfind query)
       (Some
        (mapv
         (fn [ variable]
           (Variable. variable))
         variables))
       (.-qreturn-map query)
       (.-qin query)
       (.-qwhere query)))))

(defn  query-return-map
  [ query  return-map]
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

(defn  query-return-keys
  [ query  keys]
  (query-return-map
   query
   (ReturnKeys
    (mapv (fn [ key] (keyword key)) keys))))

(defn  query-return-symbols
  [ query  keys]
  (query-return-map
   query
   (ReturnSyms
    (mapv (fn [ key] (symbol key)) keys))))

(defn  query-return-strings
  [ query  keys]
  (query-return-map query (ReturnStrs keys)))

(defn  static-db-query-with-scalars
  [ find
    patterns
    variables]
  (static-db-query-with-bindings
   find patterns (mapv scalar-input variables)))

(defn  static-db-query-clauses-with-scalars
  [ find
    clauses
    variables]
  (let [query (static-db-query-clauses find clauses)]
    (Query.
     (.-qfind query)
     (.-qwith query)
     (.-qreturn-map query)
     (vec
      (concat
       (.-qin query)
       (mapv
        (fn [ variable]
          (InputValueBinding (scalar-input variable)))
        variables)))
     (.-qwhere query))))

(defn  static-value-input-binding
  [ input]
  (match input
    (InputValueBinding binding) (Some binding)
    _ None))

(defn  static-query-value-bindings
  [ query]
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
  [ query]
  (if-some [input (first (.-qin query))]
    (match input
      (InputSource _) true
      _ false)
    false))

(defn binding-ignore? [ binding]
  (match binding
    BindIgnore true
    _ false))

(defn  binding-scalar-variable [ binding]
  (match binding
    (BindScalar variable) (Some (str (.-symbol variable)))
    _ None))

(defn  binding-tuple-items [ binding]
  (match binding
    (BindTuple bindings) (Some bindings)
    _ None))

(defn  binding-collection-item [ binding]
  (match binding
    (BindColl item) (Some item)
    _ None))

(defn  binding-variable-names [ binding]
  (mapv
   (fn [ variable]
     (str (.-symbol variable)))
   (binding-vars binding)))

(defn
  default-pattern
  [ clause]
  (match clause
    (PatternClause DefaultSource pattern) (Some pattern)
    _ None))

(defn
  static-query-patterns
  [ query]
  (loop [remaining (.-qwhere query)
         patterns []]
    (if-some [clause (first remaining)]
      (if-some [pattern (default-pattern clause)]
        (recur (subvec remaining 1) (conj patterns pattern))
        None)
      (Some patterns))))

(defn  find-variable-name [ element]
  (match element
    (FindVariable variable) (Some (str (.-symbol variable)))
    _ None))

(defn  find-variable-names [ find]
  (loop [remaining (find-spec-elements find)
         variables []]
    (if-some [element (first remaining)]
      (if-some [variable (find-variable-name element)]
        (recur (subvec remaining 1) (conj variables variable))
        None)
      (Some variables))))

(defn  find-projection-variable-names
  [ find]
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

(defn  find-element-aggregate
  [ element]
  (match element
    (FindAggregate aggregate) (Some aggregate)
    _ None))

(defn  aggregate-function-name
  [ aggregate]
  (match (.-fn aggregate)
    (AggregatePlain function-name)
    (Some (str (.-symbol function-name)))
    _ None))

(defn  aggregate-custom-variable-name
  [ aggregate]
  (match (.-fn aggregate)
    (AggregateCustom variable)
    (Some (str (.-symbol variable)))
    _ None))

(defn
  aggregate-constant-arguments
  [ aggregate]
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

(defn  find-element-pull
  [ element]
  (match element
    (FindPullElement pull) (Some pull)
    _ None))

(defn  pull-variable-name [ pull]
  (str (.-symbol (.-variable pull))))

(defn  pull-source-name [ pull]
  (str (.-symbol (.-source pull))))

(defn  pull-pattern-value
  [ pull]
  (match (.-pattern pull)
    (PullConstant pattern) (Some pattern)
    _ None))

(defn  pull-pattern-variable-name
  [ pull]
  (match (.-pattern pull)
    (PullVariable variable) (Some (str (.-symbol variable)))
    _ None))

(defn relation-find? [ find]
  (match find
    (FindRelation _) true
    _ false))

(defn collection-find? [ find]
  (match find
    (FindCollection _) true
    _ false))

(defn single-find? [ find]
  (match find
    (FindSingle _) true
    _ false))

(defn  explicit-input [ clause]
  (let [explicit-source
        (fn [ source]
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

(defn  clauses-have-input?
  [ clauses]
  (match (some clause-has-input? clauses)
    None false
    (Some _) true))

(defn  clause-has-input?
  [ clause]
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

(defn  default-in
  [ qwhere]
  (if (clauses-have-input? qwhere)
    [(InputSource (SrcVar. "$"))]
    []))

(defn  input-binding-vars
  [ input]
  (match input
    (InputSource _) []
    (InputRules _) []
    (InputValueBinding binding) (binding-vars binding)))

(defn  input-binding-sources
  [ input]
  (match input
    (InputSource source) [source]
    _ []))

(defn input-binding-rules? [ input]
  (match input
    (InputRules _) true
    _ false))

(signature datascript.parser/source-values
  :fn<datascript.parser/query-source;vector<datascript.parser/SrcVar>>)

(defn  source-values
  [ source]
  (match source
    DefaultSource []
    (ExplicitSource source) [source]))

(signature datascript.parser/clause-sources
  :fn<datascript.parser/clause;vector<datascript.parser/SrcVar>>)

(defn  clause-sources
  [ clause]
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

(signature datascript.parser/name-present?
  :fn<vector<string>;string;bool>)
(declare name-present?)

(defn validate-static-query-sources [ query]
  (let [known
        (if-some [inputs (static-query-inputs query)]
          (reduce
           (fn [ names  input]
             (if-some [name (static-input-source-name input)]
               (conj names name)
               names))
           []
           inputs)
          [])
        unknown
        (reduce
         (fn [ names  source]
           (let [name (str (.-symbol source))]
             (if
             (or
               (name-present? known name)
               (name-present? names name))
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
         (fn [ result  name]
           (if (= result "") name (str result " " name)))
         ""
         unknown)
        "]")))))

(signature datascript.parser/clause-has-rule?
  :fn<datascript.parser/clause;bool>)
(declare clause-has-rule?)

(defn  clauses-have-rule?
  [ clauses]
  (match (some clause-has-rule? clauses)
    None false
    (Some _) true))

(defn  clause-has-rule?
  [ clause]
  (match clause
    (RuleClause _ _ _) true
    (NotClause _ _ clauses _) (clauses-have-rule? clauses)
    (OrClause _ _ _ clauses _) (clauses-have-rule? clauses)
    (AndClause clauses) (clauses-have-rule? clauses)
    _ false))

(defn-  query-section-form
  [ items]
  (Datascript_runtime.Data_value.vector_of_vector items))

(defn-  query-keyword
  [ form]
  (match form
    (Datascript_runtime.Data_value.Keyword keyword)
    (let [ keyword keyword]
      (Some keyword))
    _ None))

(signature datascript.parser/query->map
  :fn<Datascript_runtime.Data_value.t;map<keyword;vector<Datascript_runtime.Data_value.t>>>)
(defn query->map
  [ query]
  (if-some [items (data-value-items query)]
    (loop [remaining items
           parsed {}
            section None]
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

(signature datascript.parser/variable-names
  :fn<vector<datascript.parser/Variable>;vector<string>>)
(defn-  variable-names
  [variables]
  (mapv
   (fn [ variable]
     (str (.-symbol variable)))
   variables))

(defn-  name-present?
  [ names  name]
  (match
   (some (fn [ candidate] (= candidate name)) names)
   None false
   (Some _) true))

(defn-  distinct-names
  [ names]
  (reduce
   (fn [ result  name]
     (if (name-present? result name)
       result
       (conj result name)))
   []
   names))

(defn-  names-difference
  [ names  excluded]
  (reduce
   (fn [ result  name]
     (if (or
          (name-present? excluded name)
          (name-present? result name))
       result
       (conj result name)))
   []
   names))

(defn-  names-display [ names]
  (str "[" (join-rule-variable-names names) "]"))

(signature datascript.parser/query-find-vars
  :fn<datascript.parser/find-spec;vector<string>>)
(defn- query-find-vars
  [ find]
  (vec (mapcat find-element-vars (find-spec-elements find))))

(defn- query-with-vars
  [ with]
  (if-some [variables with]
    (variable-names variables)
    []))

(defn- query-input-vars
  [ inputs]
  (vec (mapcat variable-names (mapv input-binding-vars inputs))))

(defn- query-where-vars
  [ clauses]
  (vec (mapcat variable-names (mapv clause-vars clauses))))

(defn- query-input-source-names
  [ inputs]
  (mapv
   (fn [ source]
     (str (.-symbol source)))
   (vec (mapcat input-binding-sources inputs))))

(defn-  query-rules-input-count
  [ inputs]
  (count (filter input-binding-rules? inputs)))

(defn validate-query
  [query
    _form
    form-map]
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
          (fn [ key] (contains? form-map key))
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

(defn  parse-query
  [ query]
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
