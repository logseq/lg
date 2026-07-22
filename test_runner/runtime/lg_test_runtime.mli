type test_case = {
  namespace : string;
  name : string;
  body : unit -> unit;
}

type assertion_failure = {
  context : string list;
  expected : string;
  message : string;
}

val register : string -> string -> (unit -> unit) -> unit
val cases : unit -> test_case list
val clear : unit -> unit
val register_once_fixture : string -> ((unit -> unit) -> unit) -> unit
val register_each_fixture : string -> ((unit -> unit) -> unit) -> unit
val namespace_once_fixtures : string -> ((unit -> unit) -> unit) list
val namespace_each_fixtures : string -> ((unit -> unit) -> unit) list
val apply_fixtures : ((unit -> 'a) -> 'a) list -> (unit -> 'a) -> 'a
val begin_case : unit -> unit
val pass : unit -> unit
val finish : unit -> unit
val invoke : (unit -> 'a) -> 'a
val exception_message : exn -> string
val fail : string -> string -> unit
val finish_case : unit -> int * assertion_failure list
val with_context : string -> (unit -> 'a) -> 'a
val grouped_cases : unit -> (string * test_case list) list
val is_performance_case : test_case -> bool
val run : string -> unit
