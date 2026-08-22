type scalar = Name | Namespace | Keyword | Symbol

let scalar_source_name = function
  | Name -> "name"
  | Namespace -> "namespace"
  | Keyword -> "keyword"
  | Symbol -> "symbol"
