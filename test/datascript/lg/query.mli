(ns datascript.lg.query)

(type-variant context-resolution
  (ContextResult :datascript.lg.query-types/result)
  (ContextSource :datascript.lg.query-types/source)
  (ContextAggregate
   :datascript.built-ins/built-in-aggregate-function))

(type-alias collect-row
  :array<option<datascript.lg.query-types/result>>)

(type-alias rule-arguments
  :vector<datascript.parser/pattern-element>)

(type-alias rule-call-history
  :map<string;vector<rule-arguments>>)

(type-record rule-frame
  (prefix-variable-names :set<string>)
  (prefix-context :datascript.lg.query-types/context)
  (clauses :vector<datascript.parser/clause>)
  (used-args :rule-call-history)
  (pending-guards :vector<datascript.parser/clause>))

(signature datascript.lg.query/append-rule-clauses
  :fn<vector<datascript.parser/clause>;vector<datascript.parser/clause>;vector<datascript.parser/clause>>)

(signature datascript.lg.query/append-rule-frames
  :fn<vector<rule-frame>;vector<rule-frame>;vector<rule-frame>>)

(type-alias tuple-getter
  :fn<array<datascript.lg.query-types/result>;datascript.lg.query-types/result>)

(type-variant tuple-key
  (SingleTupleKey :datascript.lg.query-types/result)
  (CompositeTupleKey :vector<datascript.lg.query-types/result>))

(type-alias tuple-key-getter
  :fn<array<datascript.lg.query-types/result>;tuple-key>)

(type-alias tuple-call
  :fn<array<datascript.lg.query-types/result>;option<Datascript_runtime.Data_value.t>>)

(type-alias query-cache
  :datascript.lru/cache-state<Datascript_runtime.Data_value.t;datascript.parser/Query>)

(signature datascript.lg.query/*lookup-attrs*
  :set<string>)

(signature datascript.lg.query/*implicit-source*
  :option<datascript.db/database-view>)

(signature datascript.lg.query/*implicit-source-name*
  :string)

(signature datascript.lg.query/*query-cache*
  :query-cache)

(type-record aggregate-context-state
  (seen :map<string;bool>)
  (attrs :map<string;int>)
  (values :vector<datascript.lg.query-types/result>))

(signature datascript.lg.query/attrs-string
  :fn<map<string;int>;string>)

(signature datascript.lg.query/intersect-keys
  :fn<map<string;int>;map<string;int>;set<string>>)

(signature datascript.lg.query/same-keys?
  :fn<map<string;int>;map<string;int>;bool>)

(signature datascript.lg.query/binding-source
  :fn<datascript.parser/binding;string>)

(signature datascript.lg.query/resolve-in
  :fn<datascript.lg.query-types/context;tuple<datascript.parser/input-binding;datascript.lg.query-types/input>;datascript.lg.query-types/context>)

(signature datascript.lg.query/add-free-pattern-variable
  :fn<set<string>;option<Datascript_runtime.Data_value.t>;set<string>>)

(signature datascript.lg.query/lookup-pattern-coll
  :fn<datascript.lg.query-types/context;vector<array<datascript.lg.query-types/result>>;vector<Datascript_runtime.Data_value.t>;datascript.lg.query-types/relation>)

(signature datascript.lg.query/lookup-pattern
  :fn<datascript.lg.query-types/context;datascript.lg.query-types/source;vector<Datascript_runtime.Data_value.t>;datascript.lg.query-types/relation>)

(signature datascript.lg.query/query-form-list
  :fn<vector<Datascript_runtime.Data_value.t>;Datascript_runtime.Data_value.t>)

(signature datascript.lg.query/walk-collect
  :fn<Datascript_runtime.Data_value.t;fn<Datascript_runtime.Data_value.t;bool>;vector<Datascript_runtime.Data_value.t>>)

(signature datascript.lg.query/missing-vars
  :fn<set<string>;vector<string>;set<string>>)

(signature datascript.lg.query/variable-set-string
  :fn<set<string>;string>)
