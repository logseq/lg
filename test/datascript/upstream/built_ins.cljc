(ns datascript.built-ins)

(type-variant query-function
  Equal
  NotEqual
  Less
  Greater
  LessEqual
  GreaterEqual
  Add
  Subtract
  Multiply
  Divide
  Quotient
  Remainder
  Modulo
  Increment
  Decrement
  Maximum
  Minimum
  Zero
  Positive
  Negative
  Even
  Odd
  Compare
  Random
  RandomInt
  TrueValue
  FalseValue
  NilValue
  SomeValue
  NotValue
  AndValues
  OrValues
  Complement
  Identical
  Identity
  Keyword
  Metadata
  Name
  Namespace
  ValueType
  Vector
  List
  Set
  HashMap
  ArrayMap
  Count
  Range
  NotEmpty
  Empty
  Contains
  StringValue
  Substring
  Get
  PrStr
  PrintStr
  PrintlnStr
  PrnStr
  RegexFind
  RegexMatches
  RegexSequence
  RegexPattern
  Differ
  GetElse
  GetSome
  Missing
  Tuple
  Blank
  Includes
  StartsWith
  EndsWith
  LowerCase
  UpperCase
  Capitalize
  Join
  IndexOf
  Escape
  LastIndexOf
  Replace
  ReplaceFirst
  Reverse
  Split
  SplitLines
  Trim
  TrimNewline
  TrimLeft
  TrimRight
  Number
  Integer
  String
  Boolean
  KeywordValue)

(def query-fns
  {'= Equal
   '== Equal
   'not= NotEqual
   '!= NotEqual
   '< Less
   '> Greater
   '<= LessEqual
   '>= GreaterEqual
   '+ Add
   '- Subtract
   '* Multiply
   '/ Divide
   'quot Quotient
   'rem Remainder
   'mod Modulo
   'inc Increment
   'dec Decrement
   'max Maximum
   'min Minimum
   'zero? Zero
   'pos? Positive
   'neg? Negative
   'even? Even
   'odd? Odd
   'compare Compare
   'rand Random
   'rand-int RandomInt
   'true? TrueValue
   'false? FalseValue
   'nil? NilValue
   'some? SomeValue
   'not NotValue
   'and AndValues
   'or OrValues
   'complement Complement
   'identical? Identical
   'identity Identity
   'ground Identity
   'untuple Identity
   'keyword Keyword
   'meta Metadata
   'name Name
   'namespace Namespace
   'type ValueType
   'vector Vector
   'tuple Tuple
   'list List
   'set Set
   'hash-map HashMap
   'array-map ArrayMap
   'count Count
   'range Range
   'not-empty NotEmpty
   'empty? Empty
   'contains? Contains
   'str StringValue
   'subs Substring
   'get Get
   'pr-str PrStr
   'print-str PrintStr
   'println-str PrintlnStr
   'prn-str PrnStr
   're-find RegexFind
   're-matches RegexMatches
   're-seq RegexSequence
   're-pattern RegexPattern
   '-differ? Differ
   'get-else GetElse
   'get-some GetSome
   'missing? Missing
   'clojure.string/blank? Blank
   'clojure.string/includes? Includes
   'clojure.string/starts-with? StartsWith
   'clojure.string/ends-with? EndsWith
   'clojure.string/lower-case LowerCase
   'clojure.string/upper-case UpperCase
   'clojure.string/capitalize Capitalize
   'clojure.string/join Join
   'clojure.string/index-of IndexOf
   'clojure.string/escape Escape
   'clojure.string/last-index-of LastIndexOf
   'clojure.string/replace Replace
   'clojure.string/replace-first ReplaceFirst
   'clojure.string/reverse Reverse
   'clojure.string/split Split
   'clojure.string/split-lines SplitLines
   'clojure.string/trim Trim
   'clojure.string/trim-newline TrimNewline
   'clojure.string/triml TrimLeft
   'clojure.string/trimr TrimRight
   'number? Number
   'integer? Integer
   'string? String
   'boolean? Boolean
   'keyword? KeywordValue})

(type-variant built-in-aggregate-function
  Sum
  Average
  Median
  Variance
  StandardDeviation
  Distinct
  AggregateMinimum
  AggregateMaximum
  AggregateRandom
  Sample
  AggregateCount
  CountDistinct)

(def aggregates
  {'sum Sum
   'avg Average
   'median Median
   'variance Variance
   'stddev StandardDeviation
   'distinct Distinct
   'min AggregateMinimum
   'max AggregateMaximum
   'rand AggregateRandom
   'sample Sample
   'count AggregateCount
   'count-distinct CountDistinct})

(signature datascript.built-ins/aggregate-function
  :fn<string;option<datascript.built-ins/built-in-aggregate-function>>)
(signature datascript.built-ins/comparison-function
  :fn<string;option<datascript.built-ins/query-function>>)
(signature datascript.built-ins/apply-comparison
  :fn<datascript.built-ins/query-function;vector<Datascript_runtime.Data_value.t>;option<bool>>)
(signature datascript.built-ins/ordered-values?
  :fn<datascript.built-ins/query-function;vector<Datascript_runtime.Data_value.t>;bool>)
(signature datascript.built-ins/pure-function
  :fn<string;option<datascript.built-ins/query-function>>)
(signature datascript.built-ins/apply-pure-function
  :fn<datascript.built-ins/query-function;vector<Datascript_runtime.Data_value.t>;option<Datascript_runtime.Data_value.t>>)
(signature datascript.built-ins/get-else-function?
  :fn<datascript.built-ins/query-function;bool>)
(signature datascript.built-ins/get-some-function?
  :fn<datascript.built-ins/query-function;bool>)
(signature datascript.built-ins/missing-function?
  :fn<datascript.built-ins/query-function;bool>)
(signature datascript.built-ins/differ-function?
  :fn<datascript.built-ins/query-function;bool>)
(signature datascript.built-ins/complement-function?
  :fn<datascript.built-ins/query-function;bool>)
(signature datascript.built-ins/metadata-function?
  :fn<datascript.built-ins/query-function;bool>)
(signature datascript.built-ins/value-type-function?
  :fn<datascript.built-ins/query-function;bool>)
(signature datascript.built-ins/apply-differ
  :fn<vector<Datascript_runtime.Data_value.t>;bool>)
(signature datascript.built-ins/sum-aggregate?
  :fn<datascript.built-ins/built-in-aggregate-function;bool>)
(signature datascript.built-ins/count-aggregate?
  :fn<datascript.built-ins/built-in-aggregate-function;bool>)
(signature datascript.built-ins/count-distinct-aggregate?
  :fn<datascript.built-ins/built-in-aggregate-function;bool>)
(signature datascript.built-ins/average-aggregate?
  :fn<datascript.built-ins/built-in-aggregate-function;bool>)
(signature datascript.built-ins/median-aggregate?
  :fn<datascript.built-ins/built-in-aggregate-function;bool>)
(signature datascript.built-ins/variance-aggregate?
  :fn<datascript.built-ins/built-in-aggregate-function;bool>)
(signature datascript.built-ins/standard-deviation-aggregate?
  :fn<datascript.built-ins/built-in-aggregate-function;bool>)
(signature datascript.built-ins/distinct-aggregate?
  :fn<datascript.built-ins/built-in-aggregate-function;bool>)
(signature datascript.built-ins/minimum-aggregate?
  :fn<datascript.built-ins/built-in-aggregate-function;bool>)
(signature datascript.built-ins/maximum-aggregate?
  :fn<datascript.built-ins/built-in-aggregate-function;bool>)
(signature datascript.built-ins/random-aggregate?
  :fn<datascript.built-ins/built-in-aggregate-function;bool>)
(signature datascript.built-ins/sample-aggregate?
  :fn<datascript.built-ins/built-in-aggregate-function;bool>)

(defn aggregate-function
  [function-name]
  (case function-name
    "sum" (Some Sum)
    "avg" (Some Average)
    "median" (Some Median)
    "variance" (Some Variance)
    "stddev" (Some StandardDeviation)
    "distinct" (Some Distinct)
    "min" (Some AggregateMinimum)
    "max" (Some AggregateMaximum)
    "rand" (Some AggregateRandom)
    "sample" (Some Sample)
    "count" (Some AggregateCount)
    "count-distinct" (Some CountDistinct)
    None))

(defn comparison-function
  [function-name]
  (case function-name
    "=" (Some Equal)
    "==" (Some Equal)
    "not=" (Some NotEqual)
    "!=" (Some NotEqual)
    "<" (Some Less)
    ">" (Some Greater)
    "<=" (Some LessEqual)
    ">=" (Some GreaterEqual)
    "zero?" (Some Zero)
    "pos?" (Some Positive)
    "neg?" (Some Negative)
    "even?" (Some Even)
    "odd?" (Some Odd)
    "true?" (Some TrueValue)
    "false?" (Some FalseValue)
    "nil?" (Some NilValue)
    "some?" (Some SomeValue)
    "not" (Some NotValue)
    "number?" (Some Number)
    "integer?" (Some Integer)
    "string?" (Some String)
    "boolean?" (Some Boolean)
    "keyword?" (Some KeywordValue)
    "empty?" (Some Empty)
    "contains?" (Some Contains)
    "missing?" (Some Missing)
    "clojure.string/blank?" (Some Blank)
    "clojure.string/includes?" (Some Includes)
    "clojure.string/starts-with?" (Some StartsWith)
    "clojure.string/ends-with?" (Some EndsWith)
    None))

(defn- data-value-truthy?
  [value]
  (if (Datascript_runtime.Data_value.is_nil value)
    false
    (match (Datascript_runtime.Data_value.bool_value value)
      (Some boolean-value) boolean-value
      None true)))

(defn values-equal?
  [values]
  (if (<= (count values) 1)
    true
    (let [expected (nth values 0)]
      (every?
       (fn [value]
         (Datascript_runtime.Data_value.equal expected value))
       (subvec values 1)))))

(defn ordered-values?
  [function
    values]
  (if (empty? values)
    (match function
      Less false
      Greater false
      LessEqual true
      GreaterEqual true
      _ false)
    (loop [index 1]
      (if (>= index (count values))
        true
        (let [comparison
              (Datascript_runtime.Data_value.compare
               (nth values (- index 1))
               (nth values index))
              ordered?
              (match function
                Less (neg? comparison)
                Greater (pos? comparison)
                LessEqual (not (pos? comparison))
                GreaterEqual (not (neg? comparison))
                _ false)]
          (if ordered?
            (recur (+ index 1))
            false))))))

(defn- first-value
  [values]
  (if (= 0 (count values))
    (Datascript_runtime.Data_value.Nil)
    (nth values 0)))

(defn- second-value
  [values]
  (if (<= (count values) 1)
    (Datascript_runtime.Data_value.Nil)
    (nth values 1)))

(defn- require-seqable-count
  [value]
  (if-some [count
            (Datascript_runtime.Data_value.count_value value)]
    count
    (Stdlib.invalid_arg
     (str
      (Datascript_runtime.Data_value.to_edn_string value)
      " is not ISeqable"))))

(defn- counted-type-name
  [value]
  (match value
    (Datascript_runtime.Data_value.Int _) "number"
    (Datascript_runtime.Data_value.Wide_int _) "number"
    (Datascript_runtime.Data_value.Float _) "number"
    (Datascript_runtime.Data_value.Ref _) "number"
    (Datascript_runtime.Data_value.Bool _) "boolean"
    (Datascript_runtime.Data_value.Symbol _) "cljs.core/Symbol"
    (Datascript_runtime.Data_value.Keyword _) "cljs.core/Keyword"
    (Datascript_runtime.Data_value.Uuid _) "cljs.core/UUID"
    _ "object"))

(defn- require-counted-value
  [value]
  (if-some [count
            (Datascript_runtime.Data_value.count_value value)]
    count
    (Stdlib.invalid_arg
     (str
      "No protocol method ICounted.-count defined for type "
      (counted-type-name value)
      ": "
      (Datascript_runtime.Data_value.to_edn_string value)))))

(defn apply-comparison
  [function
    values]
  (match function
    Equal (Some (values-equal? values))
    NotEqual (Some (not (values-equal? values)))
    Less (Some (ordered-values? function values))
    Greater (Some (ordered-values? function values))
    LessEqual (Some (ordered-values? function values))
    GreaterEqual (Some (ordered-values? function values))
    Zero
    (Some
     (Datascript_runtime.Data_value.is_zero
      (first-value values)))
    Positive
    (Some
     (Datascript_runtime.Data_value.is_positive
      (first-value values)))
    Negative
    (Some
     (Datascript_runtime.Data_value.is_negative
      (first-value values)))
    Even
    (Some
     (Datascript_runtime.Data_value.is_even
      (first-value values)))
    Odd
    (Some
     (Datascript_runtime.Data_value.is_odd
      (first-value values)))
    TrueValue
    (Some
     (Datascript_runtime.Data_value.equal
      (first-value values)
      (Datascript_runtime.Data_value.Bool true)))
    FalseValue
    (Some
     (Datascript_runtime.Data_value.equal
      (first-value values)
      (Datascript_runtime.Data_value.Bool false)))
    NilValue
    (Some
     (Datascript_runtime.Data_value.is_nil
      (first-value values)))
    SomeValue
    (Some
     (not
      (Datascript_runtime.Data_value.is_nil
       (first-value values))))
    NotValue
    (Some
     (not
      (data-value-truthy? (first-value values))))
    Number
    (match (first-value values)
      (Datascript_runtime.Data_value.Int _) (Some true)
      (Datascript_runtime.Data_value.Wide_int _) (Some true)
      (Datascript_runtime.Data_value.Float _) (Some true)
      (Datascript_runtime.Data_value.Ref _) (Some true)
      _ (Some false))
    Integer
    (Some
     (Datascript_runtime.Data_value.is_integer
      (first-value values)))
    String
    (match (first-value values)
      (Datascript_runtime.Data_value.String _) (Some true)
      _ (Some false))
    Boolean
    (match (first-value values)
      (Datascript_runtime.Data_value.Bool _) (Some true)
      _ (Some false))
    KeywordValue
    (match (first-value values)
      (Datascript_runtime.Data_value.Keyword _) (Some true)
      _ (Some false))
    Empty
    (let [value (first-value values)]
      (Some (= (require-seqable-count value) 0)))
    Contains
    (Datascript_runtime.Data_value.contains_key
     (first-value values)
     (second-value values))
    Blank
    (Some
     (Datascript_runtime.Data_value.string_blank
      (if (= 0 (count values))
        (Datascript_runtime.Data_value.Nil)
        (nth values 0))))
    Includes
    (if (= 0 (count values))
      None
      (Datascript_runtime.Data_value.string_includes
       (nth values 0)
       (if (= 1 (count values))
         None
         (Some (nth values 1)))))
    StartsWith
    (if (= 0 (count values))
      None
      (Datascript_runtime.Data_value.string_starts_with
       (nth values 0)
       (if (= 1 (count values))
         None
         (Some (nth values 1)))))
    EndsWith
    (if (= 0 (count values))
      None
      (Datascript_runtime.Data_value.string_ends_with
       (nth values 0)
       (if (= 1 (count values))
         None
         (Some (nth values 1)))))
    _ None))

(defn pure-function
  [function-name]
  (case function-name
    "identity" (Some Identity)
    "ground" (Some Identity)
    "untuple" (Some Identity)
    "+" (Some Add)
    "-" (Some Subtract)
    "*" (Some Multiply)
    "/" (Some Divide)
    "quot" (Some Quotient)
    "rem" (Some Remainder)
    "mod" (Some Modulo)
    "inc" (Some Increment)
    "dec" (Some Decrement)
    "max" (Some Maximum)
    "min" (Some Minimum)
    "compare" (Some Compare)
    "rand" (Some Random)
    "rand-int" (Some RandomInt)
    "keyword" (Some Keyword)
    "meta" (Some Metadata)
    "name" (Some Name)
    "namespace" (Some Namespace)
    "type" (Some ValueType)
    "vector" (Some Vector)
    "tuple" (Some Tuple)
    "list" (Some List)
    "set" (Some Set)
    "hash-map" (Some HashMap)
    "array-map" (Some ArrayMap)
    "and" (Some AndValues)
    "or" (Some OrValues)
    "complement" (Some Complement)
    "identical?" (Some Identical)
    "count" (Some Count)
    "range" (Some Range)
    "not-empty" (Some NotEmpty)
    "str" (Some StringValue)
    "subs" (Some Substring)
    "get" (Some Get)
    "pr-str" (Some PrStr)
    "print-str" (Some PrintStr)
    "println-str" (Some PrintlnStr)
    "prn-str" (Some PrnStr)
    "get-else" (Some GetElse)
    "get-some" (Some GetSome)
    "-differ?" (Some Differ)
    "re-find" (Some RegexFind)
    "re-matches" (Some RegexMatches)
    "re-seq" (Some RegexSequence)
    "re-pattern" (Some RegexPattern)
    "clojure.string/lower-case" (Some LowerCase)
    "clojure.string/upper-case" (Some UpperCase)
    "clojure.string/capitalize" (Some Capitalize)
    "clojure.string/join" (Some Join)
    "clojure.string/index-of" (Some IndexOf)
    "clojure.string/escape" (Some Escape)
    "clojure.string/last-index-of" (Some LastIndexOf)
    "clojure.string/replace" (Some Replace)
    "clojure.string/replace-first" (Some ReplaceFirst)
    "clojure.string/reverse" (Some Reverse)
    "clojure.string/split" (Some Split)
    "clojure.string/split-lines" (Some SplitLines)
    "clojure.string/trim" (Some Trim)
    "clojure.string/trim-newline" (Some TrimNewline)
    "clojure.string/triml" (Some TrimLeft)
    "clojure.string/trimr" (Some TrimRight)
    None))

(defn- and-values
  [values]
  (loop [remaining values
         result (Datascript_runtime.Data_value.Bool true)]
    (if-some [value (first remaining)]
      (if (data-value-truthy? value)
        (recur (subvec remaining 1) value)
        value)
      result)))

(defn- or-values
  [values]
  (loop [remaining values
         result (Datascript_runtime.Data_value.Nil)]
    (if-some [value (first remaining)]
      (if (data-value-truthy? value)
        value
        (recur (subvec remaining 1) value))
      result)))

(defn add-values
  [values]
  (Datascript_runtime.Data_value.add values))

(defn data-map
  [values]
  (if (= 0 (mod (count values) 2))
    (loop [remaining values
            entries {}]
      (if-some [key (first remaining)]
        (if-some [value (first (subvec remaining 1))]
          (recur (subvec remaining 2) (assoc entries key value))
          None)
        (Some
         (Datascript_runtime.Data_value.map_of_data_map entries))))
    (Stdlib.invalid_arg
     (str
      "No value supplied for key: "
      (Datascript_runtime.Data_value.to_clojure_string
       (nth values (- (count values) 1)))))))

(defn apply-pure-function
  [function
    values]
  (match function
    Identity
    (Some (first-value values))
    Add (add-values values)
    Subtract
    (Datascript_runtime.Data_value.subtract values)
    Multiply
    (Datascript_runtime.Data_value.multiply values)
    Divide
    (Datascript_runtime.Data_value.divide values)
    Quotient
    (Datascript_runtime.Data_value.quotient values)
    Remainder
    (Datascript_runtime.Data_value.remainder values)
    Modulo
    (Datascript_runtime.Data_value.modulo values)
    Increment
    (if (= 0 (count values))
      (Some (Datascript_runtime.Data_value.Float ##NaN))
      (Datascript_runtime.Data_value.increment (nth values 0)))
    Decrement
    (if (= 0 (count values))
      (Some (Datascript_runtime.Data_value.Float ##NaN))
      (Datascript_runtime.Data_value.decrement (nth values 0)))
    Maximum
    (Datascript_runtime.Data_value.maximum values)
    Minimum
    (Datascript_runtime.Data_value.minimum values)
    Compare
    (let [left
          (if (= 0 (count values))
            (Datascript_runtime.Data_value.Nil)
            (nth values 0))
          right
          (if (<= (count values) 1)
            (Datascript_runtime.Data_value.Nil)
            (nth values 1))]
      (if-some
        [comparison
         (Datascript_runtime.Data_value.compare_query_values
          left right)]
        (Some (Datascript_runtime.Data_value.Int comparison))
        None))
    Random
    (Datascript_runtime.Data_value.random_value values)
    RandomInt
    (Datascript_runtime.Data_value.random_int_value values)
    Keyword
    (Datascript_runtime.Data_value.keyword_from_values values)
    Metadata
    (Some (Datascript_runtime.Data_value.Nil))
    ValueType
    (Some
     (Datascript_runtime.Data_value.runtime_type_value
      (if (= 0 (count values))
        (Datascript_runtime.Data_value.Nil)
        (nth values 0))))
    Name
    (Datascript_runtime.Data_value.name_value
     (if (= 0 (count values))
       (Datascript_runtime.Data_value.Nil)
       (nth values 0)))
    Namespace
    (Datascript_runtime.Data_value.namespace_value
     (if (= 0 (count values))
       (Datascript_runtime.Data_value.Nil)
       (nth values 0)))
    Vector
    (Some (Datascript_runtime.Data_value.vector_of_vector values))
    Tuple
    (Some (Datascript_runtime.Data_value.vector_of_vector values))
    List
    (Some (Datascript_runtime.Data_value.list_of_vector values))
    Set
    (let [value (first-value values)]
      (if-some [set
                (Datascript_runtime.Data_value.set_value value)]
        (Some set)
        (Stdlib.invalid_arg
         (str
          (Datascript_runtime.Data_value.to_edn_string value)
          " is not ISeqable"))))
    AndValues (Some (and-values values))
    OrValues (Some (or-values values))
    Identical
    (Datascript_runtime.Data_value.identical_value values)
    HashMap
    (if-some [map (data-map values)]
      (Some (Datascript_runtime.Data_value.as_hash_map map))
      None)
    ArrayMap
    (if-some [map (data-map values)]
      (Some (Datascript_runtime.Data_value.as_array_map map))
      None)
    Count
    (Some
     (Datascript_runtime.Data_value.Int
      (require-counted-value (first-value values))))
    Range
    (Datascript_runtime.Data_value.range_value values)
    NotEmpty
    (let [value (first-value values)]
      (Some
       (if (= (require-seqable-count value) 0)
         (Datascript_runtime.Data_value.Nil)
         value)))
    StringValue
    (Datascript_runtime.Data_value.string_value values)
    Substring
    (Datascript_runtime.Data_value.substring values)
    Get
    (if (= 2 (count values))
      (Datascript_runtime.Data_value.get_or_default
       (nth values 0)
       (nth values 1)
       (Datascript_runtime.Data_value.Nil))
      (if (= 3 (count values))
        (Datascript_runtime.Data_value.get_or_default
         (nth values 0)
         (nth values 1)
         (nth values 2))
        (Stdlib.invalid_arg
         (str "Invalid arity: " (count values)))))
    PrStr
    (Datascript_runtime.Data_value.pr_str values)
    PrintStr
    (Datascript_runtime.Data_value.print_str values)
    PrintlnStr
    (Datascript_runtime.Data_value.println_str values)
    PrnStr
    (Datascript_runtime.Data_value.prn_str values)
    RegexFind
    (Datascript_runtime.Data_value.regex_find_value values)
    RegexMatches
    (Datascript_runtime.Data_value.regex_matches_value values)
    RegexSequence
    (Datascript_runtime.Data_value.regex_sequence_value values)
    RegexPattern
    (Datascript_runtime.Data_value.regex_pattern
     (first-value values))
    LowerCase
    (Datascript_runtime.Data_value.string_lower_case values)
    UpperCase
    (Datascript_runtime.Data_value.string_upper_case values)
    Capitalize
    (Datascript_runtime.Data_value.string_capitalize values)
    Join
    (Datascript_runtime.Data_value.string_join values)
    IndexOf
    (Datascript_runtime.Data_value.string_index_of values)
    Escape
    (Datascript_runtime.Data_value.string_escape values)
    LastIndexOf
    (Datascript_runtime.Data_value.string_last_index_of values)
    Replace
    (Datascript_runtime.Data_value.string_replace true values)
    ReplaceFirst
    (Datascript_runtime.Data_value.string_replace false values)
    Reverse
    (Datascript_runtime.Data_value.string_reverse values)
    Split
    (Datascript_runtime.Data_value.string_split values)
    SplitLines
    (Datascript_runtime.Data_value.string_split_lines values)
    Trim
    (Datascript_runtime.Data_value.string_trim values)
    TrimNewline
    (Datascript_runtime.Data_value.string_trim_newline values)
    TrimLeft
    (Datascript_runtime.Data_value.string_trim_left values)
    TrimRight
    (Datascript_runtime.Data_value.string_trim_right values)
    _ None))

(defn get-else-function? [function]
  (match function
    GetElse true
    _ false))

(defn get-some-function? [function]
  (match function
    GetSome true
    _ false))

(defn missing-function? [function]
  (match function
    Missing true
    _ false))

(defn differ-function? [function]
  (match function
    Differ true
    _ false))

(defn complement-function? [function]
  (match function
    Complement true
    _ false))

(defn metadata-function? [function]
  (match function
    Metadata true
    _ false))

(defn value-type-function? [function]
  (match function
    ValueType true
    _ false))

(defn apply-differ
  [values]
  (let [middle (quot (count values) 2)
        left (subvec values 0 middle)
        right (subvec values middle)]
    (not
     (and
      (= (count left) (count right))
      (every?
       (fn [index]
         (Datascript_runtime.Data_value.equal
          (nth left index)
          (nth right index)))
       (range (count left)))))))

(defn sum-aggregate?
  [function]
  (match function
    Sum true
    _ false))

(defn count-aggregate?
  [function]
  (match function
    AggregateCount true
    _ false))

(defn count-distinct-aggregate?
  [function]
  (match function
    CountDistinct true
    _ false))

(defn average-aggregate?
  [function]
  (match function
    Average true
    _ false))

(defn median-aggregate?
  [function]
  (match function
    Median true
    _ false))

(defn variance-aggregate?
  [function]
  (match function
    Variance true
    _ false))

(defn standard-deviation-aggregate?
  [function]
  (match function
    StandardDeviation true
    _ false))

(defn distinct-aggregate?
  [function]
  (match function
    Distinct true
    _ false))

(defn minimum-aggregate?
  [function]
  (match function
    AggregateMinimum true
    _ false))

(defn maximum-aggregate?
  [function]
  (match function
    AggregateMaximum true
    _ false))

(defn random-aggregate?
  [function]
  (match function
    AggregateRandom true
    _ false))

(defn sample-aggregate?
  [function]
  (match function
    Sample true
    _ false))
