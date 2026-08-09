let bindings _namespace = []

let is_core_namespace = function
  | "clojure.core" | "cljs.core" -> true
  | _ -> false

let lookup_qualified_member _name = None
