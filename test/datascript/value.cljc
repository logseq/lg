(module Datascript_value
  (type-variant value
    Nil
    (IntValue :int)
    (FloatValue :float)
    (StringValue :string)
    (SymbolValue :string)
    (BoolValue :bool)
    (KeywordValue :string)
    (UuidValue :string)
    (InstantValue :int)
    (RegexValue :string)
    (RefValue :int)
    (ListValue :list<value>)
    (VectorValue :list<value>)
    (MapValue :list<tuple<value;value>>)
    (SetValue :list<value>)
    (TupleValue :list<option<value>>)
    TxRef)

  (defn nil-value []
    Nil)

  (defn int-value [^:int value]
    (IntValue value))

  (defn float-value [^:float value]
    (FloatValue value))

  (defn string-value [^:string value]
    (StringValue value))

  (defn symbol-value [^:string value]
    (SymbolValue value))

  (defn bool-value [^:bool value]
    (BoolValue value))

  (defn keyword-value [^:string value]
    (KeywordValue value))

  (defn map-value [^:list<tuple<value;value>> entries]
    (MapValue entries))

  (defn truthy? [^:value value]
    (match value
      Nil false
      (BoolValue flag) flag
      _ true))

  (defn map-get
    [^:value key ^:value value]
    (match value
      (MapValue entries)
        (List.assoc_opt key entries)
      _ None)))
