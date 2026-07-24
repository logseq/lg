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

(def ^:map<symbol;datascript.built-ins/query-function> query-fns
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

(def ^:map<symbol;datascript.built-ins/built-in-aggregate-function> aggregates
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
