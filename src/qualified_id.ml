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
    comparison_key : string;
    source_name : string;
  }

  let interned = Hashtbl.create 1024
  let source_interned = Hashtbl.create 1024

  let create ~owner ~name =
    let comparison_key = String.concat "\000" owner ^ "\001" ^ name in
    match Hashtbl.find_opt interned comparison_key with
    | Some id -> id
    | None ->
        let id =
          {
            owner;
            name;
            comparison_key;
            source_name =
              (match owner with
              | [] -> name
              | owner -> String.concat "." owner ^ "/" ^ name);
          }
        in
        Hashtbl.add interned comparison_key id;
        id

  let of_string value =
    match Hashtbl.find_opt source_interned value with
    | Some id -> id
    | None ->
        let id =
          match String.rindex_opt value '/' with
          | None -> create ~owner:[] ~name:value
          | Some index ->
              let owner = String.sub value 0 index in
              let name =
                String.sub value (index + 1) (String.length value - index - 1)
              in
              create ~owner:[ owner ] ~name
        in
        Hashtbl.add source_interned value id;
        id
  let owner id = id.owner
  let name id = id.name

  let compare left right = String.compare left.comparison_key right.comparison_key

  let equal left right = String.equal left.comparison_key right.comparison_key

  let to_string id = id.source_name
end
