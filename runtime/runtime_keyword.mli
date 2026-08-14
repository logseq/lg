type t = string

val of_string : string -> t
val cljs_name : t -> string
val cljs_namespace : t -> string option
val compare_identifier : string -> string -> int
