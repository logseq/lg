type detected = {
  generation : int;
  paths : string list;
  content_hash : string;
}

type completed = {
  generation : int;
  paths : string list;
  content_hash : string;
  elapsed_ms : int;
}

type rejected = {
  generation : int;
  paths : string list;
  error : Lg.Compiler.compile_error;
  elapsed_ms : int;
}

type event =
  | Change_detected of detected
  | Reload_committed of completed
  | Reload_rejected of rejected

type t

val create :
  session:Session.t ->
  paths:string list ->
  settle_seconds:float ->
  now:(unit -> float) ->
  (t, Lg.Compiler.compile_error) result

val poll : t -> event option
