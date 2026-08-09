let bindings = function
  | "clojure.data" -> Core_data.bindings
  | "clojure.string" -> Core_string.bindings
  | _ -> []

let is_core_namespace = function
  | "clojure.core" | "cljs.core" | "clojure.data" -> true
  | _ -> false

let lookup_qualified_member name =
  match String.split_on_char '/' name with
  | [ namespace; member ] -> List.assoc_opt member (bindings namespace)
  | _ -> None
