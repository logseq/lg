type value = {
  rendered : string;
  type_name : string;
}

val clear : unit -> unit
val publish : string -> string -> unit
val take : unit -> value option
