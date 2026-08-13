let contains_index vector index =
  index >= 0 && index < Rrbvec.length vector

type packed_vector = Packed_vector : 'a Rrbvec.t -> packed_vector

module Metadata_key = struct
  type t = packed_vector

  let equal (Packed_vector left) (Packed_vector right) =
    Obj.repr left == Obj.repr right

  let hash (Packed_vector vector) = Hashtbl.hash (Obj.repr vector)
end

module Metadata_table = Hashtbl.Make (Metadata_key)

let metadata_table = Metadata_table.create 16

let metadata vector =
  Metadata_table.find_opt metadata_table (Packed_vector vector)
  |> Option.value ~default:Lg_edn_backend.Nil

let with_metadata vector metadata =
  let key = Packed_vector vector in
  (match metadata with
  | Lg_edn_backend.Nil -> Metadata_table.remove metadata_table key
  | metadata -> Metadata_table.replace metadata_table key metadata);
  vector

let preserve_metadata source target =
  match metadata source with
  | Lg_edn_backend.Nil -> target
  | metadata -> with_metadata target metadata

let assoc vector index value =
  let updated =
    if index = Rrbvec.length vector then Rrbvec.push_back vector value
    else Rrbvec.set vector index value
  in
  preserve_metadata vector updated
let rseq vector = Rrbvec.rev vector
let nth vector index = Rrbvec.nth vector index

let nth_default vector index not_found =
  if contains_index vector index then Rrbvec.nth vector index else not_found

let find_entry vector index =
  if contains_index vector index then Some (index, Rrbvec.nth vector index)
  else None

let kv_reduce_protocol vector fn accumulator =
  let rec loop accumulator index =
    if index >= Rrbvec.length vector then accumulator
    else loop (fn accumulator index (Rrbvec.nth vector index)) (index + 1)
  in
  loop accumulator 0
