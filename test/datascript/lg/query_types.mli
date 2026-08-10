(ns datascript.lg.query-types)

(type-alias result
  :Datascript_runtime.Query_value.result<datascript.db/database-view>)

(type-alias source
  :Datascript_runtime.Query_value.source<datascript.db/database-view>)

(type-alias binding-value
  :Datascript_runtime.Query_value.binding_value<datascript.db/database-view>)

(type-alias callable
  :Datascript_runtime.Query_value.callable<datascript.db/database-view>)

(type-alias relation
  :Datascript_runtime.Query_value.relation<datascript.db/database-view>)

(type-alias rules
  :vector<datascript.parser/Rule>)

(type-alias input
  :Datascript_runtime.Query_value.input<datascript.db/database-view;rules>)

(type-alias output
  :Datascript_runtime.Query_value.output<datascript.db/database-view>)

(type-alias context
  :Datascript_runtime.Query_value.context<datascript.db/database-view;rules>)

(type-variant predicate-operand
  (PredicateColumn :int)
  (PredicateResult
   :Datascript_runtime.Query_value.result<datascript.db/database-view>))

(type-variant static-predicate-function
  (ComparisonStaticPredicate :datascript.built-ins/query-function)
  (PureStaticPredicate :datascript.built-ins/query-function))

(type-variant rule-call-argument
  (RuleCallVariable :string :vector<result>)
  (RuleCallConstant :Datascript_runtime.Data_value.t))

(type-variant rule-call
  (RuleCall :string :vector<rule-call-argument>))

(type-alias rule-path :vector<rule-call>)

(signature datascript.lg.query-types/entity-result
  :fn<int;result>)

(signature datascript.lg.query-types/attr-result
  :fn<keyword;result>)

(signature datascript.lg.query-types/value-result
  :fn<Datascript_runtime.Data_value.t;result>)

(signature datascript.lg.query-types/metadata-result
  :fn<Datascript_runtime.Data_value.t;Datascript_runtime.Data_value.t;result>)

(signature datascript.lg.query-types/result-value
  :fn<result;option<Datascript_runtime.Data_value.t>>)

(signature datascript.lg.query-types/result-metadata
  :fn<result;option<Datascript_runtime.Data_value.t>>)

(signature datascript.lg.query-types/database-result
  :fn<datascript.db/database-view;result>)

(signature datascript.lg.query-types/pull-result
  :fn<Datascript_runtime.Data_value.t;result>)

(signature datascript.lg.query-types/added-result
  :fn<bool;result>)

(signature datascript.lg.query-types/callable
  :fn<fn<vector<result>;option<Datascript_runtime.Data_value.t>>;callable>)

(signature datascript.lg.query-types/callable-result
  :fn<callable;result>)

(signature datascript.lg.query-types/result-callable
  :fn<result;option<callable>>)

(signature datascript.lg.query-types/invoke-callable
  :fn<callable;vector<result>;option<Datascript_runtime.Data_value.t>>)

(signature datascript.lg.query-types/result-nil?
  :fn<result;bool>)

(signature datascript.lg.query-types/complement-result
  :fn<vector<result>;result>)

(signature datascript.lg.query-types/metadata-function-result
  :fn<vector<result>;result>)

(signature datascript.lg.query-types/value-type-function-result
  :fn<vector<result>;result>)

(signature datascript.lg.query-types/function-binding-result
  :fn<datascript.parser/binding;result;binding-value>)

(signature datascript.lg.query-types/database-view-source
  :fn<datascript.db/database-view;source>)

(signature datascript.lg.query-types/relation-source
  :fn<vector<array<result>>;source>)

(signature datascript.lg.query-types/source-database
  :fn<source;option<datascript.db/database-view>>)

(signature datascript.lg.query-types/source-rows
  :fn<source;option<vector<array<result>>>>)

(signature datascript.lg.query-types/scalar-binding
  :fn<result;binding-value>)

(signature datascript.lg.query-types/collection-binding
  :fn<vector<binding-value>;binding-value>)

(signature datascript.lg.query-types/binding-result
  :fn<binding-value;option<result>>)

(signature datascript.lg.query-types/binding-items
  :fn<binding-value;option<vector<binding-value>>>)

(signature datascript.lg.query-types/source-input
  :fn<source;input>)

(signature datascript.lg.query-types/rules-input
  :fn<rules;input>)

(signature datascript.lg.query-types/binding-input
  :fn<binding-value;input>)

(signature datascript.lg.query-types/input-source
  :fn<input;option<source>>)

(signature datascript.lg.query-types/input-rules
  :fn<input;option<rules>>)

(signature datascript.lg.query-types/input-binding
  :fn<input;option<binding-value>>)

(signature datascript.lg.query-types/output-relation
  :fn<output;option<vector<array<result>>>>)

(signature datascript.lg.query-types/output-collection
  :fn<output;option<vector<result>>>)

(signature datascript.lg.query-types/output-scalar
  :fn<output;option<option<result>>>)

(signature datascript.lg.query-types/output-tuple
  :fn<output;option<option<array<result>>>>)

(signature datascript.lg.query-types/output-keyword-relation
  :fn<output;option<vector<map<string;result>>>>)

(signature datascript.lg.query-types/output-symbol-relation
  :fn<output;option<vector<map<string;result>>>>)

(signature datascript.lg.query-types/output-string-relation
  :fn<output;option<vector<map<string;result>>>>)

(signature datascript.lg.query-types/output-keyword-tuple
  :fn<output;option<option<map<string;result>>>>)

(signature datascript.lg.query-types/output-symbol-tuple
  :fn<output;option<option<map<string;result>>>>)

(signature datascript.lg.query-types/output-string-tuple
  :fn<output;option<option<map<string;result>>>>)

(signature datascript.lg.query-types/row-get
  :fn<array<result>;int;option<result>>)

(signature datascript.lg.query-types/empty-row
  :fn<unit;array<result>>)

(signature datascript.lg.query-types/project-row
  :fn<array<result>;array<int>;array<result>>)

(signature datascript.lg.query-types/distinct-rows
  :fn<vector<array<result>>;vector<array<result>>>)

(signature datascript.lg.query-types/query-with-variable-names
  :fn<datascript.parser/Query;vector<string>>)

(signature datascript.lg.query-types/group-rows
  :fn<vector<array<result>>;array<int>;vector<vector<array<result>>>>)

(signature datascript.lg.query-types/join-rows
  :fn<array<result>;array<int>;array<result>;array<int>;array<result>>)

(signature datascript.lg.query-types/concat-rows
  :fn<array<result>;array<result>;array<result>>)

(signature datascript.lg.query-types/datom-row
  :fn<datascript.db/Datom;array<result>>)

(signature datascript.lg.query-types/relation
  :fn<map<string;int>;vector<array<result>>;map<string;datascript.db/database-view>;relation>)

(signature datascript.lg.query-types/datom-relation
  :fn<map<string;int>;array<int>;vector<datascript.db/Datom>;map<string;datascript.db/database-view>;relation>)

(signature datascript.lg.query-types/index-attrs
  :fn<vector<string>;map<string;int>>)

(signature datascript.lg.query-types/pattern-relation
  :fn<vector<string>;array<int>;vector<datascript.db/Datom>;map<string;datascript.db/database-view>;relation>)

(signature datascript.lg.query-types/lookup-db-pattern
  :fn<datascript.db/database-view;vector<datascript.parser/pattern-element>;relation>)

(signature datascript.lg.query-types/lookup-db-patterns
  :fn<datascript.db/database-view;vector<vector<datascript.parser/pattern-element>>;relation>)

(signature datascript.lg.query-types/relation-pattern-step
  :fn<map<string;result>;vector<result>;datascript.parser/pattern-element;result;option<tuple<map<string;result>;vector<result>>>>)

(signature datascript.lg.query-types/relation-pattern-row
  :fn<vector<datascript.parser/pattern-element>;array<result>;option<array<result>>>)

(signature datascript.lg.query-types/relation-pattern-variables
  :fn<vector<datascript.parser/pattern-element>;vector<string>>)

(signature datascript.lg.query-types/binding-relation
  :fn<datascript.parser/binding;binding-value;relation>)

(signature datascript.lg.query-types/tuple-binding-relation
  :fn<vector<datascript.parser/binding>;vector<binding-value>;relation>)

(signature datascript.lg.query-types/collection-binding-relation
  :fn<datascript.parser/binding;vector<binding-value>;relation>)

(signature datascript.lg.query-types/execute-db-query
  :fn<datascript.db/database-view;datascript.parser/Query;output>)

(signature datascript.lg.query-types/execute-query
  :fn<datascript.parser/Query;vector<input>;output>)

(signature datascript.lg.query-types/empty-relation
  :fn<map<string;int>;map<string;datascript.db/database-view>;relation>)

(signature datascript.lg.query-types/relation-result
  :fn<relation;string;array<result>;option<result>>)

(signature datascript.lg.query-types/relation-attrs
  :fn<relation;map<string;int>>)

(signature datascript.lg.query-types/relation-rows
  :fn<relation;vector<array<result>>>)

(signature datascript.lg.query-types/relation-lookup-databases
  :fn<relation;map<string;datascript.db/database-view>>)

(signature datascript.lg.query-types/relation-with-rows
  :fn<relation;vector<array<result>>;relation>)

(signature datascript.lg.query-types/sum-relation
  :fn<relation;relation;relation>)

(signature datascript.lg.query-types/product-relation
  :fn<relation;relation;relation>)

(signature datascript.lg.query-types/resolve-lookup-result
  :fn<datascript.db/database-view;result;result>)

(signature datascript.lg.query-types/hash-join
  :fn<relation;relation;relation>)

(signature datascript.lg.query-types/product-attrs
  :fn<map<string;int>;map<string;int>;map<string;int>>)

(signature datascript.lg.query-types/merge-lookup-databases
  :fn<map<string;datascript.db/database-view>;map<string;datascript.db/database-view>;map<string;datascript.db/database-view>>)

(signature datascript.lg.query-types/relation-lookup-database
  :fn<relation;string;option<datascript.db/database-view>>)

(signature datascript.lg.query-types/context
  :fn<vector<relation>;map<string;source>;rules;context>)

(signature datascript.lg.query-types/context-relations
  :fn<context;vector<relation>>)

(signature datascript.lg.query-types/context-sources
  :fn<context;map<string;source>>)

(signature datascript.lg.query-types/context-rules
  :fn<context;rules>)

(signature datascript.lg.query-types/pattern-attr-constraint
  :fn<option<datascript.parser/pattern-element>;option<option<keyword>>>)

(signature datascript.lg.query-types/pattern-lookup-databases
  :fn<datascript.db/database-view;vector<datascript.parser/pattern-element>;option<keyword>;map<string;datascript.db/database-view>>)

(signature datascript.lg.query-types/constant-relation-result
  :fn<relation;string;option<result>>)

(signature datascript.lg.query-types/result-entity-id
  :fn<datascript.db/database-view;result;option<int>>)

(signature datascript.lg.query-types/resolve-source-pattern
  :fn<datascript.db/database-view;map<string;source>;string;datascript.parser/query-source;relation;relation;vector<datascript.parser/pattern-element>;relation>)

(signature datascript.lg.query-types/query-source-database
  :fn<datascript.db/database-view;map<string;source>;string;datascript.db/database-view>)

(signature datascript.lg.query-types/join-query-parts
  :fn<vector<string>;string>)

(signature datascript.lg.query-types/query-binding-description
  :fn<datascript.parser/binding;string>)

(signature datascript.lg.query-types/differ-predicate-matches?
  :fn<array<result>;array<predicate-operand>;bool>)

(signature datascript.lg.query-types/resolve-predicate
  :fn<datascript.db/database-view;map<string;source>;relation;relation;datascript.parser/query-callable;vector<datascript.parser/fn-arg>;relation>)

(signature datascript.lg.query-types/resolve-function
  :fn<datascript.db/database-view;map<string;source>;relation;relation;datascript.parser/query-callable;vector<datascript.parser/fn-arg>;datascript.parser/binding;relation>)

(signature datascript.lg.query-types/resolve-not
  :fn<datascript.db/database-view;map<string;source>;string;relation;relation;rules;rule-path;vector<datascript.parser/Variable>;vector<datascript.parser/clause>;string;relation>)

(signature datascript.lg.query-types/resolve-or
  :fn<datascript.db/database-view;map<string;source>;string;relation;relation;rules;rule-path;vector<string>;vector<string>;vector<datascript.parser/clause>;bool;string;relation>)

(signature datascript.lg.query-types/resolve-or-branch
  :fn<datascript.db/database-view;map<string;source>;string;relation;relation;rules;rule-path;datascript.parser/clause;relation>)

(signature datascript.lg.query-types/resolve-rule-branch
  :fn<datascript.db/database-view;map<string;source>;string;relation;relation;rules;rule-path;vector<datascript.parser/pattern-element>;datascript.parser/RuleBranch;relation>)

(signature datascript.lg.query-types/resolve-rule
  :fn<datascript.db/database-view;map<string;source>;string;relation;relation;rules;rule-path;string;vector<datascript.parser/pattern-element>;relation>)

(signature datascript.lg.query-types/resolve-static-clauses
  :fn<datascript.db/database-view;map<string;source>;string;relation;relation;rules;rule-path;vector<datascript.parser/clause>;relation>)

(signature datascript.lg.query-types/ensure-empty-relation-variables
  :fn<relation;seqable<string>;relation>)

(signature datascript.lg.query-types/rows-match-on-variables?
  :fn<relation;array<result>;relation;array<result>;vector<string>;bool>)

(signature datascript.lg.query-types/project-relation-variables
  :fn<relation;vector<string>;relation>)

(signature datascript.lg.query-types/variable-set-display
  :fn<vector<datascript.parser/Variable>;string>)
