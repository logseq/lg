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
