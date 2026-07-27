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
  Count
  Range
  NotEmpty
  Empty
  Contains
  StringValue
  Substring
  Get
  PrintedString
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
   'array-map HashMap
   'count Count
   'range Range
   'not-empty NotEmpty
   'empty? Empty
   'contains? Contains
   'str StringValue
   'subs Substring
   'get Get
   'pr-str PrintedString
   'print-str PrintedString
   'println-str PrintedString
   'prn-str PrintedString
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
    "re-find" (Some RegexFind)
    "missing?" (Some Missing)
    None))

(defn- ^:bool data-value-truthy?
  [^:Datascript_runtime.Data_value.t value]
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
          false)))))

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
    (if (= 1 (count values))
      (match (nth values 0)
        (Datascript_runtime.Data_value.Int value)
        (Some (= value 0))
        _ None)
      None)
    Positive
    (if (= 1 (count values))
      (match (nth values 0)
        (Datascript_runtime.Data_value.Int value)
        (Some (> value 0))
        _ None)
      None)
    Negative
    (if (= 1 (count values))
      (match (nth values 0)
        (Datascript_runtime.Data_value.Int value)
        (Some (< value 0))
        _ None)
      None)
    Even
    (if (= 1 (count values))
      (match (nth values 0)
        (Datascript_runtime.Data_value.Int value)
        (Some (= 0 (mod value 2)))
        _ None)
      None)
    Odd
    (if (= 1 (count values))
      (match (nth values 0)
        (Datascript_runtime.Data_value.Int value)
        (Some (not (= 0 (mod value 2))))
        _ None)
      None)
    TrueValue
    (if (= 1 (count values))
      (Some
       (Datascript_runtime.Data_value.equal
        (nth values 0)
        (Datascript_runtime.Data_value.Bool true)))
      None)
    FalseValue
    (if (= 1 (count values))
      (Some
       (Datascript_runtime.Data_value.equal
        (nth values 0)
        (Datascript_runtime.Data_value.Bool false)))
      None)
    NilValue
    (if (= 1 (count values))
      (Some
       (Datascript_runtime.Data_value.is_nil
        (nth values 0)))
      None)
    SomeValue
    (if (= 1 (count values))
      (Some
       (not
        (Datascript_runtime.Data_value.is_nil
         (nth values 0))))
      None)
    NotValue
    (if (= 1 (count values))
      (Some
       (not
        (data-value-truthy? (nth values 0))))
      None)
    Number
    (if (= 1 (count values))
      (match (nth values 0)
        (Datascript_runtime.Data_value.Int _) (Some true)
        (Datascript_runtime.Data_value.Wide_int _) (Some true)
        (Datascript_runtime.Data_value.Float _) (Some true)
        (Datascript_runtime.Data_value.Ref _) (Some true)
        _ (Some false))
      None)
    Integer
    (if (= 1 (count values))
      (match (nth values 0)
        (Datascript_runtime.Data_value.Int _) (Some true)
        (Datascript_runtime.Data_value.Wide_int _) (Some true)
        (Datascript_runtime.Data_value.Ref _) (Some true)
        _ (Some false))
      None)
    String
    (if (= 1 (count values))
      (match (nth values 0)
        (Datascript_runtime.Data_value.String _) (Some true)
        _ (Some false))
      None)
    Boolean
    (if (= 1 (count values))
      (match (nth values 0)
        (Datascript_runtime.Data_value.Bool _) (Some true)
        _ (Some false))
      None)
    KeywordValue
    (if (= 1 (count values))
      (match (nth values 0)
        (Datascript_runtime.Data_value.Keyword _) (Some true)
        _ (Some false))
      None)
    Empty
    (if (= 1 (count values))
      (if-some [value
                (Datascript_runtime.Data_value.count_value
                 (nth values 0))]
        (Some (= value 0))
        None)
      None)
    Contains
    (if (= 2 (count values))
      (Datascript_runtime.Data_value.contains_key
       (nth values 0)
       (nth values 1))
      None)
    RegexFind
    (if (= 2 (count values))
      (Datascript_runtime.Data_value.regex_find
       (nth values 0)
       (nth values 1))
      None)
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
    "inc" (Some Increment)
    "dec" (Some Decrement)
    "keyword" (Some Keyword)
    "name" (Some Name)
    "namespace" (Some Namespace)
    "vector" (Some Vector)
    "tuple" (Some Tuple)
    "list" (Some List)
    "hash-map" (Some HashMap)
    "array-map" (Some HashMap)
    "and" (Some AndValues)
    "or" (Some OrValues)
    "count" (Some Count)
    "not-empty" (Some NotEmpty)
    "get" (Some Get)
    "get-else" (Some GetElse)
    "get-some" (Some GetSome)
    "-differ?" (Some Differ)
    "re-pattern" (Some RegexPattern)
    None))

(defn- ^:Datascript_runtime.Data_value.t and-values
  [^:vector<Datascript_runtime.Data_value.t> values]
  (loop [remaining values
         result (Datascript_runtime.Data_value.Bool true)]
    (if-some [value (first remaining)]
      (if (data-value-truthy? value)
        (recur (subvec remaining 1) value)
        value)
      result)))

(defn- ^:Datascript_runtime.Data_value.t or-values
  [^:vector<Datascript_runtime.Data_value.t> values]
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
    None))

(defn apply-pure-function
  [function
    values]
  (match function
    Identity
    (if (= 1 (count values))
      (nth values 0)
      None)
    Add (add-values values)
    Subtract
    (Datascript_runtime.Data_value.subtract values)
    Multiply
    (Datascript_runtime.Data_value.multiply values)
    Increment
    (if (= 1 (count values))
      (Datascript_runtime.Data_value.increment (nth values 0))
      None)
    Decrement
    (if (= 1 (count values))
      (Datascript_runtime.Data_value.decrement (nth values 0))
      None)
    Keyword
    (Datascript_runtime.Data_value.keyword_from_values values)
    Name
    (if (= 1 (count values))
      (Datascript_runtime.Data_value.name_value (nth values 0))
      None)
    Namespace
    (if (= 1 (count values))
      (Datascript_runtime.Data_value.namespace_value
       (nth values 0))
      None)
    Vector
    (Some (Datascript_runtime.Data_value.vector_of_vector values))
    Tuple
    (Some (Datascript_runtime.Data_value.vector_of_vector values))
    List
    (Some (Datascript_runtime.Data_value.list_of_vector values))
    AndValues (Some (and-values values))
    OrValues (Some (or-values values))
    HashMap
    (data-map values)
    Count
    (if (= 1 (count values))
      (if-some [value
                (Datascript_runtime.Data_value.count_value
                 (nth values 0))]
        (Some (Datascript_runtime.Data_value.Int value))
        None)
      None)
    NotEmpty
    (if (= 1 (count values))
      (if-some [value
                (Datascript_runtime.Data_value.count_value
                 (nth values 0))]
        (Some
         (if (= value 0)
           (Datascript_runtime.Data_value.Nil)
           (nth values 0)))
        None)
      None)
    Get
    (if (= 2 (count values))
      (Datascript_runtime.Data_value.map_get
       (nth values 0)
       (nth values 1))
      None)
    RegexPattern
    (if (= 1 (count values))
      (Datascript_runtime.Data_value.regex_pattern (nth values 0))
      None)
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

(defn ^:bool apply-differ
  [^:vector<Datascript_runtime.Data_value.t> values]
  (let [middle (quot (count values) 2)
        left (subvec values 0 middle)
        right (subvec values middle)]
    (not
     (and
      (= (count left) (count right))
      (every?
       (fn [^:int index]
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
