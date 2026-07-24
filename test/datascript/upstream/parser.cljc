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
  (loop [remaining items
         parsed []]
    (if (empty? remaining)
      (Some parsed)
      (if-some [item (first remaining)]
        (if-some [arg (parse-fn-arg item)]
          (recur (subvec remaining 1) (conj parsed arg))
          None)
        None))))

;; rule-vars = [ variable+ | ([ variable+ ] variable*) ]

(deftrecord RuleVars
  [^:option<vector<datascript.parser/Variable>> required
   ^:vector<datascript.parser/Variable> free])

(defn ^boolean variables-distinct?
  [^:vector<datascript.parser/Variable> variables]
  (loop [remaining variables
         ^:set<symbol> seen (set-of :symbol)]
    (if-some [variable (first remaining)]
      (let [symbol (.-symbol variable)]
        (if (contains? seen symbol)
          false
          (recur (subvec remaining 1) (conj seen symbol))))
      true)))

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
        (util/raise "Cannot parse empty rule-vars"
          {:error :parser/rule-vars}))
      (when-not (variables-distinct? (into required-values free*))
        (util/raise "Rule variables should be distinct"
          {:error :parser/rule-vars}))
      (RuleVars. required* free*))
    (util/raise "Cannot parse empty rule-vars"
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

(defn find-element-vars [^:find-element element]
  (match element
    (FindVariable variable) [(:symbol variable)]
    (FindPullElement pull) [(:symbol (:variable pull))]
    (FindAggregate aggregate)
    (if-some [arg (last (:args aggregate))]
      (fn-arg-vars arg)
      [])))

(defn ^:vector<find-element> find-elements [^:find-spec find]
  (match find
    (FindRelation relation) (:elements relation)
    (FindCollection collection) [(:element collection)]
    (FindSingle scalar) [(:element scalar)]
    (FindTupleResult tuple-result) (:elements tuple-result)))

(defn find-vars [^:find-spec find]
  (mapcat find-element-vars (find-elements find)))

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

(defn ^:vector<datascript.parser/Variable> parse-with
  [^:Datascript_runtime.Data_value.t form]
  (if-some [items (data-value-items form)]
    (parse-required-variables items)
    (util/raise "Cannot parse :with clause, expected [ variable+ ]"
      {:error :parser/with, :form form})))


;; in = [ (src-var | rules-var | plain-symbol | binding)+ ]

(type-variant input-binding
  (InputSource :datascript.parser/SrcVar)
  (InputRules :datascript.parser/RulesVar)
  (InputValueBinding :binding))

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

(type-variant clause
  (PatternClause :query-source :vector<pattern-element>)
  (PredicateClause :query-callable :vector<fn-arg>)
  (FunctionClause :query-callable :vector<fn-arg> :binding)
  (RuleClause :query-source
              :datascript.parser/PlainSymbol
              :vector<pattern-element>)
  (NotClause :query-source
             :vector<datascript.parser/Variable>
             :vector<clause>)
  (OrClause :query-source
            :datascript.parser/RuleVars
            :vector<clause>)
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
    (NotClause _ vars _) vars
    (OrClause _ vars _) (rule-vars-values vars)
    (AndClause clauses) (vec (mapcat clause-vars clauses))))

(defn collect-vars-distinct
  [^:vector<datascript.parser/clause> clauses]
  (vec (distinct (mapcat clause-vars clauses))))

(defn ^:option<vector<datascript.parser/clause>> parse-clauses
  [^:vector<Datascript_runtime.Data_value.t> clauses]
  (loop [remaining clauses
         parsed []]
    (if (empty? remaining)
      (Some parsed)
      (if-some [clause (first remaining)]
        (recur (subvec remaining 1)
               (conj parsed (parse-clause clause)))
        None))))

(defn- validate-not [^:datascript.parser/clause clause]
  (match clause
    (NotClause _ vars _)
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
              (Some
               (validate-not
                (NotClause source
                           (collect-vars-distinct clauses*)
                           clauses*)))
              (util/raise "Cannot parse 'not' clause"
                {:error :parser/where}))
            None)
          _ None)
        None))
    None))

(defn ^:option<vector<datascript.parser/Variable>> parse-variables
  [^:vector<Datascript_runtime.Data_value.t> items]
  (loop [remaining items
         parsed []]
    (if (empty? remaining)
      (Some parsed)
      (if-some [item (first remaining)]
        (if-some [variable (parse-variable item)]
          (recur (subvec remaining 1) (conj parsed variable))
          None)
        None))))

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
                      (Some
                       (validate-not
                        (NotClause source vars* clauses*)))
                      (util/raise "Cannot parse 'not-join' clauses"
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

(defn validate-or [^:datascript.parser/clause clause]
  (match clause
    (OrClause _ vars _)
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
              (Some
               (validate-or
                (OrClause
                 source
                 (RuleVars. None (collect-vars-distinct clauses*))
                 clauses*)))
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
                    (Some
                     (validate-or
                      (OrClause source vars* clauses*)))
                    (util/raise "Cannot parse 'or-join' clauses"
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
            (util/raise "Rule arity mismatch"
              {:error :parser/rule})))))
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

(signature datascript.parser/relation-find
  :fn<vector<string>;datascript.parser/find-spec>)
(signature datascript.parser/collection-find
  :fn<string;datascript.parser/find-spec>)
(signature datascript.parser/single-find
  :fn<string;datascript.parser/find-spec>)
(signature datascript.parser/tuple-find
  :fn<vector<string>;datascript.parser/find-spec>)
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
(signature datascript.parser/static-db-query-clauses
  :fn<datascript.parser/find-spec;vector<datascript.parser/clause>;datascript.parser/Query>)
(signature datascript.parser/static-db-query-clauses-with-scalars
  :fn<datascript.parser/find-spec;vector<datascript.parser/clause>;vector<string>;datascript.parser/Query>)
(signature datascript.parser/pattern-clause
  :fn<vector<datascript.parser/pattern-element>;datascript.parser/clause>)
(signature datascript.parser/greater-than-clause
  :fn<datascript.parser/fn-arg;datascript.parser/fn-arg;datascript.parser/clause>)
(signature datascript.parser/variable-argument
  :fn<string;datascript.parser/fn-arg>)
(signature datascript.parser/constant-argument
  :fn<Datascript_runtime.Data_value.t;datascript.parser/fn-arg>)
(signature datascript.parser/argument-variable-name
  :fn<datascript.parser/fn-arg;option<string>>)
(signature datascript.parser/argument-constant
  :fn<datascript.parser/fn-arg;option<Datascript_runtime.Data_value.t>>)
(signature datascript.parser/static-callable-name
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

(defn ^:pattern-element pattern-attribute [^:keyword attr]
  (pattern-constant
   (Datascript_runtime.Data_value.Keyword (str attr))))

(defn ^:clause pattern-clause
  [^:vector<pattern-element> pattern]
  (PatternClause DefaultSource pattern))

(defn ^:fn-arg variable-argument [^:string variable]
  (FnArgVariable (Variable. variable)))

(defn ^:fn-arg constant-argument
  [^:Datascript_runtime.Data_value.t value]
  (FnArgConstant value))

(defn ^:option<string> argument-variable-name [^:fn-arg argument]
  (match argument
    (FnArgVariable variable) (Some (str (.-symbol variable)))
    _ None))

(defn ^:option<Datascript_runtime.Data_value.t> argument-constant
  [^:fn-arg argument]
  (match argument
    (FnArgConstant value) (Some value)
    _ None))

(defn ^:option<string> static-callable-name
  [^:query-callable callable]
  (match callable
    (StaticCallable name) (Some (str (.-symbol name)))
    (VariableCallable _) None))

(defn ^:clause greater-than-clause
  [^:fn-arg left ^:fn-arg right]
  (PredicateClause
   (StaticCallable (PlainSymbol. ">"))
   [left right]))

(defn ^:find-spec relation-find [^:vector<string> variables]
  (FindRelation
   (FindRel. (mapv variable-find-element variables))))

(defn ^:find-spec collection-find [^:string variable]
  (FindCollection
   (FindColl. (variable-find-element variable))))

(defn ^:find-spec single-find [^:string variable]
  (FindSingle
   (FindScalar. (variable-find-element variable))))

(defn ^:find-spec tuple-find [^:vector<string> variables]
  (FindTupleResult
   (FindTuple. (mapv variable-find-element variables))))

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
  (if-some [source-input (first (.-qin query))]
    (match source-input
      (InputSource _)
      (loop [remaining (subvec (.-qin query) 1)
             bindings []]
        (if-some [input (first remaining)]
          (if-some [binding (static-value-input-binding input)]
            (recur
             (subvec remaining 1)
             (conj bindings binding))
            None)
          (Some bindings)))
      _ None)
    None))

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
  (loop [remaining (find-elements find)
         variables []]
    (if-some [element (first remaining)]
      (if-some [variable (find-variable-name element)]
        (recur (subvec remaining 1) (conj variables variable))
        None)
      (Some variables))))

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
    (PatternClause _ _) true
    (PredicateClause _ _) false
    (FunctionClause _ _ _) false
    (RuleClause source _ _)
    (match source
      DefaultSource false
      (ExplicitSource _) true)
    (NotClause source _ clauses)
    (or
     (match source
       DefaultSource false
       (ExplicitSource _) true)
     (clauses-have-input? clauses))
    (OrClause source _ clauses)
    (or
     (match source
       DefaultSource false
       (ExplicitSource _) true)
     (clauses-have-input? clauses))
    (AndClause clauses) (clauses-have-input? clauses)))

(defn default-in [^:vector<datascript.parser/clause> qwhere]
  (if (clauses-have-input? qwhere)
    '[$]
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
      (PredicateClause _ _) []
      (FunctionClause _ _ _) []
      (RuleClause source _ _) (source-values source)
      (NotClause source _ clauses)
      (vec
       (concat
        (source-values source)
        (mapcat clause-sources clauses)))
      (OrClause source _ clauses)
      (vec
       (concat
        (source-values source)
        (mapcat clause-sources clauses)))
      (AndClause clauses) (vec (mapcat clause-sources clauses))))

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
    (NotClause _ _ clauses) (clauses-have-rule? clauses)
    (OrClause _ _ clauses) (clauses-have-rule? clauses)
    (AndClause clauses) (clauses-have-rule? clauses)
    _ false))

(defn ^datascript.parser/Query parse-query
  [^datascript.parser/Query query]
  query)
