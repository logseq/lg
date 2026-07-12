module type S = sig
  type t

  val create : owner:string list -> name:string -> t
  val owner : t -> string list
  val name : t -> string
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val to_string : t -> string
end

module Make () : S = struct
  type t = {
    owner : string list;
    name : string;
  }

  let create ~owner ~name = { owner; name }
  let owner id = id.owner
  let name id = id.name
  let compare = Stdlib.compare
  let equal left right = compare left right = 0

  let to_string id =
    match id.owner with
    | [] -> id.name
    | owner -> String.concat "." owner ^ "/" ^ id.name
end
