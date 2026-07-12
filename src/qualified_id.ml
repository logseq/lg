module type S = sig
  type t

  val create : owner:string list -> name:string -> t
  val of_string : string -> t
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

  let of_string value =
    match String.rindex_opt value '/' with
    | None -> create ~owner:[] ~name:value
    | Some index ->
        let owner = String.sub value 0 index in
        let name =
          String.sub value (index + 1) (String.length value - index - 1)
        in
        create ~owner:[ owner ] ~name
  let owner id = id.owner
  let name id = id.name
  let compare = Stdlib.compare
  let equal left right = compare left right = 0

  let to_string id =
    match id.owner with
    | [] -> id.name
    | owner -> String.concat "." owner ^ "/" ^ id.name
end
