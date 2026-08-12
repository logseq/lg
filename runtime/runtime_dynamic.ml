type _ nominal_tag = ..
type nominal = Nominal : 'a nominal_tag * 'a * Obj.t option -> nominal
type _ nominal_tag += Uuid_tag : Runtime_uuid.t nominal_tag

let dynamic_marker = ref ()

type 'a hash_trie =
  | Hash_empty
  | Hash_leaf of int * 'a list
  | Hash_branch of int * 'a hash_trie array

type t = {
  marker : unit ref;
  payload : payload;
  sequence : (unit -> t Seq.t) option;
  sequential : bool;
  metadata : t option;
  type_name : string option;
  nominal : nominal option;
  cached_hash : int option;
}

and payload =
  | Nil
  | Int of int
  | Float of float
  | Char of char
  | String of string
  | Regex of string
  | Symbol of string
  | Keyword of string
  | Bool of bool
  | Array of t array
  | List
  | Vector of t Rrbvec.t
  | Seq
  | Set of set_payload
  | Map of map_payload
  | Opaque of string

and set_payload = { values : t list; set_index : t hash_trie }
and map_payload = {
  entries : (t * t) Rrbvec.t;
  map_index : (t * int) hash_trie;
  size : int;
}

let keyword_cache : (string, t) Hashtbl.t = Hashtbl.create 256

let map_entries (map : map_payload) = Rrbvec.to_list map.entries

let make ?sequence ?(sequential = false) ?metadata ?type_name
    ?cached_hash payload =
  {
    marker = dynamic_marker;
    payload;
    sequence;
    sequential;
    metadata;
    type_name;
    nominal = None;
    cached_hash;
  }

let with_metadata value metadata = { value with metadata = Some metadata }
let with_nominal tag payload value =
  let original_metadata = Option.map Obj.repr value.metadata in
  { value with nominal = Some (Nominal (tag, payload, original_metadata)) }

let with_sequence value sequence = { value with sequence = Some sequence }

let nominal value = value.nominal

let unpack_nominal expected_tag value =
  match value.nominal with
  | Some (Nominal (actual_tag, payload, _))
    when Obj.repr actual_tag = Obj.repr expected_tag ->
      Some (Obj.obj (Obj.repr payload))
  | Some _ | None -> None

let nominal_metadata_is_original value =
  match value.nominal with
  | Some (Nominal (_, _, original_metadata)) -> (
      match (value.metadata, original_metadata) with
      | None, None -> true
      | Some metadata, Some original -> Obj.repr metadata == original
      | None, Some _ | Some _, None -> false)
  | None -> false

let nil = make Nil
let int value = make (Int value)
let float value = make (Float value)
let char value = make (Char value)
let string value =
  make ~cached_hash:(Runtime_hash.hash_string value) (String value)
let uuid value = with_nominal Uuid_tag value (string (Runtime_uuid.to_string value))

let as_uuid value : Runtime_uuid.t =
  match nominal value with
  | Some (Nominal (Uuid_tag, uuid, _)) -> uuid
  | _ -> invalid_arg "dynamic value is not a UUID"

let symbol value =
  make ~cached_hash:(Runtime_hash.hash_symbol value) (Symbol value)

let keyword value =
  match Hashtbl.find_opt keyword_cache value with
  | Some keyword -> keyword
  | None ->
      let keyword =
        make ~cached_hash:(Runtime_hash.hash_keyword value) (Keyword value)
      in
      Hashtbl.add keyword_cache value keyword;
      keyword
let bool value = make (Bool value)
let regex value = make (Regex value)
let unit () = nil

let as_unit value =
  match value.payload with
  | Nil -> ()
  | _ -> invalid_arg "dynamic value is not unit"

let opaque name = make ~type_name:name (Opaque name)

let metadata value = Option.value value.metadata ~default:nil

let str_float = string_of_float

let pr_str_float value =
  if Float.is_nan value then "##NaN"
  else if value = Float.infinity then "##Inf"
  else if value = Float.neg_infinity then "##-Inf"
  else string_of_float value

let rec to_string ~pr value = to_string_payload ~pr value

and to_string_payload ~pr value =
  let join values =
    values |> List.map (to_string ~pr:true) |> String.concat " "
  in
  match value.payload with
  | Nil -> "nil"
  | Int value -> string_of_int value
  | Float value -> if pr then pr_str_float value else str_float value
  | Char value -> String.make 1 value
  | String value -> if pr then Printf.sprintf "%S" value else value
  | Regex value -> if pr then Printf.sprintf "#%S" value else value
  | Symbol value | Keyword value -> value
  | Bool value -> string_of_bool value
  | Array values ->
      "#js ["
      ^ (values |> Array.to_list |> List.map (to_string ~pr:true)
        |> String.concat " ")
      ^ "]"
  | List -> "(" ^ join (List.of_seq (to_seq value)) ^ ")"
  | Vector values -> "[" ^ join (Rrbvec.to_list values) ^ "]"
  | Seq -> "(" ^ join (List.of_seq (to_seq value)) ^ ")"
  | Set set -> "#{" ^ join set.values ^ "}"
  | Map map ->
      let entries =
        map_entries map
        |> List.map (fun (key, value) ->
               to_string ~pr:true key ^ " " ^ to_string ~pr:true value)
        |> String.concat ", "
      in
      "{" ^ entries ^ "}"
  | Opaque name -> "<" ^ name ^ ">"

and to_seq value =
  match (value.payload, value.sequence) with
  | Nil, _ -> Seq.empty
  | String value, None ->
      value |> String.to_seq |> Seq.map (fun character -> make (Char character))
  | _, Some sequence -> sequence ()
  | _, None ->
      invalid_arg
        ("dynamic value is not seqable: " ^ to_string ~pr:true value)

let first_value value =
  match (to_seq value) () with Seq.Nil -> nil | Seq.Cons (first, _) -> first

let ffirst_value value = first_value (first_value value)

let str value = to_string ~pr:false value
let pr_str value = to_string ~pr:true value

let list values =
  make ~sequential:true ~sequence:(fun () -> List.to_seq values) List

let vector values =
  make ~sequential:true
    ~sequence:(fun () -> Rrbvec.to_seq values)
    (Vector values)

let vec_value value =
  match value.payload with
  | Vector values -> vector values
  | _ -> vector (Rrbvec.of_list (List.of_seq (to_seq value)))

let array values = make ~sequence:(fun () -> Array.to_seq values) (Array values)

let array_copy value =
  match value.payload with
  | Array values -> array (Array.copy values)
  | _ -> invalid_arg "aclone expects an array"

let regex_match = function
  | None -> nil
  | Some [ Some value ] -> string value
  | Some captures ->
      captures
      |> List.map (function Some value -> string value | None -> nil)
      |> Rrbvec.of_list |> vector

let regex_group = function None -> nil | Some value -> string value

let regex_match_sequence matches =
  if Array.length matches = 0 then nil
  else
    make ~sequential:true
      ~sequence:(fun () ->
        matches
        |> Array.to_seq
        |> Seq.map (fun captures -> regex_match (Some captures)))
      Seq

let seq values =
  make ~sequential:true ~sequence:(fun () -> values) Seq

let seq_cons value tail =
  make ~sequential:true ~sequence:(fun () -> Seq.cons value tail) Seq

let nominal_identity_equal left right =
  match (left.nominal, right.nominal) with
  | Some (Nominal (left_tag, left_payload, _)),
    Some (Nominal (right_tag, right_payload, _)) ->
      Obj.repr left_tag = Obj.repr right_tag
      && Obj.repr left_payload == Obj.repr right_payload
  | (Some _ | None), (Some _ | None) -> false

let same_nominal_type left right =
  match left.nominal with
  | None -> true
  | Some (Nominal (left_tag, _, _)) -> (
      match right.nominal with
      | Some (Nominal (right_tag, _, _)) ->
          Obj.repr left_tag = Obj.repr right_tag
      | None -> false)

let expand_record_extension_entries entries =
  List.concat_map
    (fun ((key, value) as entry) ->
      match (key.payload, value.payload) with
      | Keyword ":__lg/extmap", Map extensions -> map_entries extensions
      | _ -> [ entry ])
    entries

let rec hash value =
  match value.cached_hash with
  | Some cached_hash -> cached_hash
  | None ->
      match value.payload with
      | Nil -> 0
      | Bool true -> 1231
      | Bool false -> 1237
      | Int value -> Runtime_hash.hash_int value
      | Float value -> Runtime_hash.hash_float value
      | Char value -> Char.code value
      | String value -> Runtime_hash.hash_string value
      | Regex value -> Runtime_hash.hash_string value
      | Symbol value -> Runtime_hash.hash_symbol value
      | Keyword value -> Runtime_hash.hash_keyword value
      | Array _ | List | Vector _ | Seq ->
          value |> to_seq |> Seq.map hash |> Runtime_hash.hash_ordered
      | Set set ->
          set.values |> List.to_seq |> Seq.map hash
          |> Runtime_hash.hash_unordered
      | Map map when Option.is_some value.type_name ->
          let name = Option.get value.type_name in
          let entry_hashes =
            map_entries map |> expand_record_extension_entries
            |> List.to_seq
            |> Seq.map (fun (key, entry_value) ->
                   [ hash key; hash entry_value ] |> List.to_seq
                   |> Runtime_hash.hash_ordered)
          in
          Runtime_hash.hash_combine (Runtime_hash.hash_string name)
            (Runtime_hash.hash_unordered entry_hashes)
      | Map map ->
          map_entries map |> List.to_seq
          |> Seq.map (fun (key, value) ->
                 [ hash key; hash value ] |> List.to_seq
                 |> Runtime_hash.hash_ordered)
          |> Runtime_hash.hash_unordered
      | Opaque name -> Runtime_hash.hash_string name

let hash_trie_slot hash shift = (hash lsr shift) land 31

let hash_trie_position bitmap bit =
  Runtime_int.popcount_32 (bitmap land (bit - 1))

let array_insert values position value =
  let length = Array.length values in
  Array.init (length + 1) (fun index ->
      if index < position then values.(index)
      else if index = position then value
      else values.(index - 1))

let rec hash_trie_find equal value value_hash shift = function
  | Hash_empty -> None
  | Hash_leaf (existing_hash, values) ->
      if value_hash = existing_hash then List.find_opt (equal value) values
      else None
  | Hash_branch (bitmap, children) ->
      let slot = hash_trie_slot value_hash shift in
      let bit = 1 lsl slot in
      if bitmap land bit = 0 then None
      else
        hash_trie_find equal value value_hash (shift + 5)
          children.(hash_trie_position bitmap bit)

let rec hash_trie_add equal value value_hash shift = function
  | Hash_empty -> Hash_leaf (value_hash, [ value ])
  | Hash_leaf (existing_hash, values) as unchanged ->
      if value_hash = existing_hash then
        if List.exists (equal value) values then unchanged
        else Hash_leaf (existing_hash, value :: values)
      else
        let existing_slot = hash_trie_slot existing_hash shift in
        let value_slot = hash_trie_slot value_hash shift in
        if existing_slot = value_slot then
          let child =
            hash_trie_add equal value value_hash (shift + 5) unchanged
          in
          Hash_branch (1 lsl existing_slot, [| child |])
        else
          let existing_bit = 1 lsl existing_slot in
          let value_bit = 1 lsl value_slot in
          let children =
            if existing_slot < value_slot then
              [| unchanged; Hash_leaf (value_hash, [ value ]) |]
            else [| Hash_leaf (value_hash, [ value ]); unchanged |]
          in
          Hash_branch (existing_bit lor value_bit, children)
  | Hash_branch (bitmap, children) as unchanged ->
      let slot = hash_trie_slot value_hash shift in
      let bit = 1 lsl slot in
      let position = hash_trie_position bitmap bit in
      if bitmap land bit = 0 then
        Hash_branch
          ( bitmap lor bit,
            array_insert children position (Hash_leaf (value_hash, [ value ])) )
      else
        let child =
          hash_trie_add equal value value_hash (shift + 5) children.(position)
        in
        if child == children.(position) then unchanged
        else
          let updated = Array.copy children in
          updated.(position) <- child;
          Hash_branch (bitmap, updated)

let rec hash_trie_assoc equal value value_hash shift = function
  | Hash_empty -> Hash_leaf (value_hash, [ value ])
  | Hash_leaf (existing_hash, values) as unchanged ->
      if value_hash = existing_hash then
        let rec replace prefix = function
          | [] -> Hash_leaf (existing_hash, List.rev (value :: prefix))
          | existing :: rest when equal value existing ->
              Hash_leaf
                (existing_hash, List.rev_append prefix (value :: rest))
          | existing :: rest -> replace (existing :: prefix) rest
        in
        replace [] values
      else
        let existing_slot = hash_trie_slot existing_hash shift in
        let value_slot = hash_trie_slot value_hash shift in
        if existing_slot = value_slot then
          let child =
            hash_trie_assoc equal value value_hash (shift + 5) unchanged
          in
          Hash_branch (1 lsl existing_slot, [| child |])
        else
          let existing_bit = 1 lsl existing_slot in
          let value_bit = 1 lsl value_slot in
          let inserted = Hash_leaf (value_hash, [ value ]) in
          let children =
            if existing_slot < value_slot then [| unchanged; inserted |]
            else [| inserted; unchanged |]
          in
          Hash_branch (existing_bit lor value_bit, children)
  | Hash_branch (bitmap, children) ->
      let slot = hash_trie_slot value_hash shift in
      let bit = 1 lsl slot in
      let position = hash_trie_position bitmap bit in
      if bitmap land bit = 0 then
        Hash_branch
          ( bitmap lor bit,
            array_insert children position
              (Hash_leaf (value_hash, [ value ])) )
      else
        let child =
          hash_trie_assoc equal value value_hash (shift + 5)
            children.(position)
        in
        let updated = Array.copy children in
        updated.(position) <- child;
        Hash_branch (bitmap, updated)

let map_entry_equal equal (key, _) (existing_key, _) =
  equal key existing_key

let map_index_find equal key index =
  hash_trie_find (map_entry_equal equal) (key, 0) (hash key) 0 index

let map_index_assoc equal index key position =
  hash_trie_assoc (map_entry_equal equal) (key, position) (hash key) 0 index

let map_index_of_entries equal entries =
  entries |> List.mapi (fun position (key, _) -> (key, position))
  |> List.fold_left
       (fun index (key, position) ->
         map_index_assoc equal index key position)
       Hash_empty

let map_find_entry equal key (map : map_payload) =
  Option.bind (map_index_find equal key map.map_index) (fun (_, position) ->
      Rrbvec.nth_opt map.entries position)

let set_index_find equal value index =
  hash_trie_find equal value (hash value) 0 index

let set_index_add equal index value =
  hash_trie_add equal value (hash value) 0 index

let rec equal left right =
  equal_payload left right

and equal_payload left right =
  match (left.payload, right.payload) with
  | Nil, Nil -> true
  | Int left, Int right -> left = right
  | Float left, Float right -> left = right
  | Char left, Char right -> left = right
  | String left, String right -> left = right
  | Regex left, Regex right -> left = right
  | Symbol left, Symbol right -> left = right
  | Keyword left, Keyword right -> left = right
  | Bool left, Bool right -> left = right
  | Array left, Array right -> left == right
  | (List | Vector _ | Seq), (List | Vector _ | Seq) ->
      Seq.equal equal (to_seq left) (to_seq right)
  | Set left_set, Set right_set ->
      let left_values = left_set.values in
      let right_values = right_set.values in
      List.length left_values = List.length right_values
      &&
      List.for_all
        (fun value ->
          Option.is_some (set_index_find equal value right_set.set_index))
        left_values
  | Map left_map, Map right_map ->
      let left_entries = map_entries left_map in
      let right_entries = map_entries right_map in
      List.length left_entries = List.length right_entries
      &&
      List.for_all
        (fun (key, value) ->
          match map_find_entry equal key right_map with
          | Some (_, other_value) -> equal value other_value
          | None -> false)
        left_entries
  | Opaque _, Opaque _ -> nominal_identity_equal left right
  | _ -> false

let make_map_payload ?type_name payload =
  make ?type_name
    ~sequence:(fun () ->
      map_entries payload |> List.to_seq
      |> Seq.map (fun (key, value) -> vector (Rrbvec.of_list [ key; value ])))
    (Map payload)

let make_map ?type_name entries index =
  make_map_payload ?type_name
    {
      entries = Rrbvec.of_list entries;
      map_index = index;
      size = List.length entries;
    }

let map entries = make_map entries (map_index_of_entries equal entries)

let runtime_dynamic_value value =
  match Sys.backend_type with
  | Other "Melange" -> (
      try
        let candidate : t = Obj.obj (Obj.repr value) in
        if candidate.marker == dynamic_marker then Some candidate else None
      with _ -> None)
  | Native | Bytecode | Other _ -> (
      try
        let representation = Obj.repr value in
        if
          (not (Obj.is_int representation))
          && Obj.tag representation = 0
          && Obj.field representation 0 == Obj.repr dynamic_marker
        then Some (Obj.obj representation)
        else None
      with Invalid_argument _ -> None)

let is_runtime_dynamic value = Option.is_some (runtime_dynamic_value value)

let polymorphic_equal left right =
  if left == right then true
  else
    match (runtime_dynamic_value left, runtime_dynamic_value right) with
    | Some left, Some right -> equal left right
    | Some _, None | None, Some _ -> false
    | None, None -> (
        try left = right with Invalid_argument _ -> false)

let polymorphic_hash value =
  match runtime_dynamic_value value with
  | Some value -> hash value
  | None -> Hashtbl.hash value

let polymorphic_str value =
  match runtime_dynamic_value value with
  | Some value -> str value
  | None ->
    let representation = Obj.repr value in
    if (not (Obj.is_int representation)) && Obj.tag representation = Obj.string_tag
    then Obj.obj representation
    else "<value>"

let polymorphic_pr_str value =
  match runtime_dynamic_value value with
  | Some value -> pr_str value
  | None ->
    let representation = Obj.repr value in
    if (not (Obj.is_int representation)) && Obj.tag representation = Obj.string_tag
    then Obj.obj representation
    else "<value>"

let equal_arguments = function
  | [] | [ _ ] -> true
  | first :: rest -> List.for_all (equal first) rest

let numeric_equal left right =
  match (left.payload, right.payload) with
  | Int left, Int right -> left = right
  | Float left, Float right -> left = right
  | Int left, Float right -> float_of_int left = right
  | Float left, Int right -> left = float_of_int right
  | _ -> false

let numeric_equal_arguments = function
  | [] | [ _ ] -> true
  | first :: rest -> List.for_all (numeric_equal first) rest

let numeric_ordering name int_predicate float_predicate left right =
  match (left.payload, right.payload) with
  | Int left, Int right -> int_predicate left right
  | Float left, Float right -> float_predicate left right
  | Int left, Float right -> float_predicate (float_of_int left) right
  | Float left, Int right -> float_predicate left (float_of_int right)
  | _ -> invalid_arg (name ^ " expects numeric arguments")

let numeric_less = numeric_ordering "<" ( < ) ( < )
let numeric_less_equal = numeric_ordering "<=" ( <= ) ( <= )
let numeric_greater = numeric_ordering ">" ( > ) ( > )
let numeric_greater_equal = numeric_ordering ">=" ( >= ) ( >= )

let numeric_binary name int_operation float_operation left right =
  match (left.payload, right.payload) with
  | Int left, Int right -> int (int_operation left right)
  | Float left, Float right -> float (float_operation left right)
  | Int left, Float right -> float (float_operation (float_of_int left) right)
  | Float left, Int right -> float (float_operation left (float_of_int right))
  | _ -> invalid_arg (name ^ " expects numeric arguments")

let numeric_add = numeric_binary "+" ( + ) ( +. )
let numeric_subtract = numeric_binary "-" ( - ) ( -. )
let numeric_multiply = numeric_binary "*" ( * ) ( *. )
let numeric_divide = numeric_binary "/" ( / ) ( /. )

let numeric_add_arguments arguments =
  List.fold_left numeric_add (int 0) arguments

let numeric_multiply_arguments arguments =
  List.fold_left numeric_multiply (int 1) arguments

let numeric_subtract_arguments = function
  | [] -> invalid_arg "- expects at least one argument"
  | [ value ] -> numeric_subtract (int 0) value
  | first :: rest -> List.fold_left numeric_subtract first rest

let numeric_divide_arguments = function
  | [] | [ _ ] -> invalid_arg "/ expects at least two arguments"
  | first :: rest -> List.fold_left numeric_divide first rest

let payload_rank = function
  | Nil -> 0
  | Int _ | Float _ -> 1
  | Char _ -> 2
  | String _ -> 3
  | Symbol _ -> 4
  | Keyword _ -> 5
  | Bool _ -> 6
  | Array _ -> 7
  | List | Vector _ | Seq -> 8
  | Set _ -> 9
  | Map _ -> 10
  | Opaque _ -> 11
  | Regex _ -> 12

let rec compare_sequences left right =
  match (Seq.uncons left, Seq.uncons right) with
  | None, None -> 0
  | None, Some _ -> -1
  | Some _, None -> 1
  | Some (left, left_rest), Some (right, right_rest) ->
      let result = compare left right in
      if result = 0 then compare_sequences left_rest right_rest else result

and compare_entries (left_key, left_value) (right_key, right_value) =
  let key_result = compare left_key right_key in
  if key_result = 0 then compare left_value right_value else key_result

and compare_entry_lists left right =
  let left = List.sort compare_entries left in
  let right = List.sort compare_entries right in
  let rec loop left right =
    match (left, right) with
    | [], [] -> 0
    | [], _ -> -1
    | _, [] -> 1
    | left_entry :: left_rest, right_entry :: right_rest ->
        let result = compare_entries left_entry right_entry in
        if result = 0 then loop left_rest right_rest else result
  in
  loop left right

and compare_identifier left right =
  Runtime_keyword.compare_identifier left right

and compare left right =
  match (left.payload, right.payload) with
  | Nil, Nil -> 0
  | Nil, _ -> -1
  | _, Nil -> 1
  | Int left, Int right -> Stdlib.compare left right
  | Float left, Float right -> Stdlib.compare left right
  | Int left, Float right -> Stdlib.compare (float_of_int left) right
  | Float left, Int right -> Stdlib.compare left (float_of_int right)
  | Char left, Char right -> Stdlib.compare left right
  | String left, String right -> String.compare left right
  | Symbol left, Symbol right | Keyword left, Keyword right ->
      compare_identifier left right
  | Regex left, Regex right -> String.compare left right
  | Bool left, Bool right -> Bool.compare left right
  | Array left, Array right ->
      let rec compare_at index =
        if index = Array.length left then
          Int.compare (Array.length left) (Array.length right)
        else if index = Array.length right then 1
        else
          let result = compare left.(index) right.(index) in
          if result = 0 then compare_at (index + 1) else result
      in
      compare_at 0
  | (List | Vector _ | Seq), (List | Vector _ | Seq) ->
      compare_sequences (to_seq left) (to_seq right)
  | Set left, Set right ->
      compare_sequences
        (List.sort compare left.values |> List.to_seq)
        (List.sort compare right.values |> List.to_seq)
  | Map left, Map right ->
      compare_entry_lists (map_entries left) (map_entries right)
  | _ ->
      let rank = Int.compare (payload_rank left.payload) (payload_rank right.payload) in
      if rank <> 0 then rank else invalid_arg "dynamic values are not comparable"

let sort collection =
  collection |> to_seq |> List.of_seq |> List.sort compare |> list

let is_comparable value =
  match value.payload with
  | Nil | Int _ | Float _ | Char _ | String _ | Symbol _ | Keyword _ | Bool _
  | Array _ | Regex _ ->
      true
  | List | Vector _ | Seq | Set _ | Map _ | Opaque _ ->
      false

let set sequence =
  let reversed_values, set_index =
    Seq.fold_left
      (fun (values, index) value ->
        if Option.is_some (set_index_find equal value index) then (values, index)
        else (value :: values, set_index_add equal index value))
      ([], Hash_empty) sequence
  in
  let values = List.rev reversed_values in
  make ~sequence:(fun () -> List.to_seq values)
    (Set { values; set_index })

let conj_collection collection value =
  match collection.payload with
  | Nil -> list [ value ]
  | List -> list (value :: List.of_seq (to_seq collection))
  | Seq -> seq_cons value (to_seq collection)
  | Vector values -> vector (Rrbvec.push_back values value)
  | Set set ->
      if Option.is_some (set_index_find equal value set.set_index) then collection
      else
        let values = value :: set.values in
        let set_index = set_index_add equal set.set_index value in
        make ~sequence:(fun () -> List.to_seq values)
          (Set { values; set_index })
  | _ -> invalid_arg "dynamic conj expects a collection"

let assoc value key replacement =
  match value.payload with
  | Nil -> map [ (key, replacement) ]
  | Vector values ->
      let index =
        match key.payload with
        | Int index -> index
        | _ -> invalid_arg "dynamic vector assoc expects an integer index"
      in
      vector (Rrbvec.set values index replacement)
  | Map map ->
      (match map_index_find equal key map.map_index with
      | None ->
          let position = map.size in
          make_map_payload
            {
              entries = Rrbvec.push_back map.entries (key, replacement);
              map_index = map_index_assoc equal map.map_index key position;
              size = map.size + 1;
            }
      | Some (_, position) ->
          make_map_payload
            {
              map with
              entries = Rrbvec.set map.entries position (key, replacement);
              map_index = map_index_assoc equal map.map_index key position;
            })
  | _ -> invalid_arg "dynamic value is not associative"

let dissoc value key =
  match value.payload with
  | Nil -> value
  | Map map_payload ->
      map
        (List.filter
           (fun (entry, _) -> not (equal key entry))
           (map_entries map_payload))
  | _ -> invalid_arg "dynamic value is not associative"

let vals value =
  match value.payload with
  | Map map -> map_entries map |> List.map snd |> Rrbvec.of_list |> vector
  | _ -> invalid_arg "vals expects a map"

let keys value =
  match value.payload with
  | Map map -> map_entries map |> List.map fst |> Rrbvec.of_list |> vector
  | _ -> invalid_arg "keys expects a map"

let array_get value index =
  match value.payload with
  | Array values -> Array.get values index
  | _ -> invalid_arg "aget expects an array"

let array_set array index value =
  match array.payload with
  | Array values -> Array.set values index value
  | _ -> invalid_arg "aset expects an array"

let array_unsafe_set array index value =
  match array.payload with
  | Array values -> Array.unsafe_set values index value
  | _ -> invalid_arg "unsafe-aset expects an array"

let vector_nth_opt value index =
  if index < 0 then None
  else
    match value.payload with
    | Vector values -> Rrbvec.nth_opt values index
    | _ -> (
        match Seq.drop index (to_seq value) () with
        | Seq.Nil -> None
        | Seq.Cons (item, _) -> Some item)

let subvec_value value start stop =
  let values = List.of_seq (to_seq value) in
  let length = List.length values in
  if start < 0 || stop < start || stop > length then
    invalid_arg "subvec indexes are out of bounds"
  else
    values |> List.to_seq |> Seq.drop start |> Seq.take (stop - start)
    |> List.of_seq |> Rrbvec.of_list |> vector

let payload_get value key =
  match (value.payload, key.payload) with
  | Map map, _ -> map_find_entry equal key map |> Option.map snd
  | Vector values, Int index -> Rrbvec.nth_opt values index
  | _ -> None

let get value key = Option.value (payload_get value key) ~default:nil

let indexed_get value index =
  match (value.payload, index.payload) with
  | Array values, Int index -> Array.get values index
  | _ -> get value index

let get_default value key default =
  Option.value (payload_get value key) ~default

let contains value key =
  match (value.payload, key.payload) with
  | Map map, _ -> Option.is_some (map_index_find equal key map.map_index)
  | Set set, _ -> Option.is_some (set_index_find equal key set.set_index)
  | Vector values, Int index ->
      index >= 0 && index < Rrbvec.length values
  | Array values, Int index ->
      index >= 0 && index < Array.length values
  | _ -> false

let find value key =
  if contains value key then
    Some (vector (Rrbvec.of_list [ key; get value key ]))
  else None

let select_keys value keys =
  let lookup_supported =
    match value.payload with
    | Nil | Map _ -> true
    | _ -> false
  in
  if not lookup_supported then invalid_arg "select-keys expects an associative value"
  else
    let missing = opaque "select-keys-missing" in
    Seq.fold_left
      (fun selected key ->
        let selected_value = get_default value key missing in
        if selected_value == missing then selected
        else assoc selected key selected_value)
      (map []) keys

let rec get_in value keys =
  match keys () with
  | Seq.Nil -> value
  | Seq.Cons (key, rest) -> get_in (get value key) rest

let get_in_default value keys default =
  let rec loop current keys =
    match keys () with
    | Seq.Nil -> current
    | Seq.Cons (key, rest) ->
        if contains current key then loop (get current key) rest else default
  in
  loop value keys

let entries value =
  match value.payload with
  | Map map -> map_entries map
  | _ -> invalid_arg "dynamic value is not a map"

let map_without_keys value keys =
  match value.payload with
  | Map map_payload ->
      map_entries map_payload
      |> List.filter (fun (entry, _) ->
             not (List.exists (fun key -> equal key entry) keys))
      |> map
  | Nil -> map []
  | _ -> invalid_arg "dynamic value is not a map"

let empty value =
  let emptied =
    match value.payload with
    | Nil -> nil
    | List -> list []
    | Vector _ -> vector Rrbvec.empty
    | Seq -> seq Seq.empty
    | Set _ -> set Seq.empty
    | Map _ -> map []
    | String _ -> string ""
    | _ -> invalid_arg "dynamic value is not a collection"
  in
  { emptied with metadata = value.metadata }

let butlast value =
  let rec drop_last acc = function
    | [] | [ _ ] -> List.rev acc
    | item :: rest -> drop_last (item :: acc) rest
  in
  list (drop_last [] (List.of_seq (to_seq value)))

let pair value =
  match List.of_seq (to_seq value) with
  | [ key; value ] -> (key, value)
  | _ -> invalid_arg "dynamic map entry must contain a key and value"

let into target source =
  match target.payload with
  | List ->
      list
        (Seq.fold_left
           (fun values value -> value :: values)
           (List.of_seq (to_seq target))
           source)
  | Vector values ->
      vector (Rrbvec.append_list values (List.of_seq source))
  | Seq -> seq (Seq.append (to_seq target) source)
  | Map _ ->
      Seq.fold_left
        (fun map entry ->
          let key, value = pair entry in
          assoc map key value)
        target source
  | _ -> invalid_arg "dynamic into target is not a collection"

let is_sequential value = value.sequential && Option.is_some value.sequence
let is_seqable value =
  match value.payload with Nil -> true | _ -> Option.is_some value.sequence

let truthy value =
  match value.payload with Nil | Bool false -> false | _ -> true

let is_nil value = match value.payload with Nil -> true | _ -> false
let is_symbol value = match value.payload with Symbol _ -> true | _ -> false
let is_keyword value = match value.payload with Keyword _ -> true | _ -> false
let is_string value = match value.payload with String _ -> true | _ -> false
let is_int value = match value.payload with Int _ -> true | _ -> false
let is_float value = match value.payload with Float _ -> true | _ -> false

let is_number value =
  match value.payload with Int _ | Float _ -> true | _ -> false

let is_zero value =
  match value.payload with
  | Int value -> value = 0
  | Float value -> value = 0.
  | _ -> invalid_arg "zero? expects a numeric value"

let is_positive value =
  match value.payload with
  | Int value -> value > 0
  | Float value -> value > 0.
  | _ -> invalid_arg "pos? expects a numeric value"

let is_negative value =
  match value.payload with
  | Int value -> value < 0
  | Float value -> value < 0.
  | _ -> invalid_arg "neg? expects a numeric value"

let is_bool value = match value.payload with Bool _ -> true | _ -> false
let is_array value = match value.payload with Array _ -> true | _ -> false
let is_list value = match value.payload with List -> true | _ -> false
let is_vector value = match value.payload with Vector _ -> true | _ -> false
let is_seq value = match value.payload with List | Seq -> true | _ -> false
let is_map value =
  match value.payload with Map _ -> true | _ -> false
let is_set value = match value.payload with Set _ -> true | _ -> false
let is_coll value =
  match value.payload with
  | List | Vector _ | Seq | Set _ | Map _ -> true
  | _ -> false
let is_instance value type_name = value.type_name = Some type_name

let is_true value =
  match value.payload with Bool true -> true | _ -> false

let is_false value =
  match value.payload with Bool false -> true | _ -> false

let is_some value = not (is_nil value)

let identical left right =
  left == right
  ||
  match (left.payload, right.payload) with
  | (Nil | Int _ | Float _ | Char _ | String _ | Symbol _ | Keyword _ | Bool _),
    (Nil | Int _ | Float _ | Char _ | String _ | Symbol _ | Keyword _ | Bool _) ->
      equal left right
  | _ -> (
      nominal_identity_equal left right)

let count_value value =
  match value.payload with
  | Nil -> 0
  | Array values -> Array.length values
  | Vector values -> Rrbvec.length values
  | Set set -> List.length set.values
  | Map map -> map.size
  | _ -> Seq.length (to_seq value)

let hash_unordered_coll value =
  value |> to_seq |> Seq.map hash |> Runtime_hash.hash_unordered

let as_transient value =
  match value.payload with
  | Vector _ | Set _ | Map _ -> value
  | _ -> invalid_arg "transient expects an editable collection"

let persistent value =
  match value.payload with
  | Vector _ | Set _ | Map _ -> value
  | _ -> invalid_arg "persistent! expects a transient collection"

let conj_bang collection value = conj_collection collection value

let dissoc_bang collection key = dissoc collection key

let disj_bang collection value =
  match collection.payload with
  | Set set_payload ->
      let values =
        List.filter
          (fun candidate -> not (equal candidate value))
          set_payload.values
      in
      set (List.to_seq values)
  | _ -> invalid_arg "disj! expects a transient set"

let set_union sets = sets |> List.to_seq |> Seq.flat_map to_seq |> set

let set_intersection sets =
  match sets with
  | [] -> set Seq.empty
  | first :: rest ->
      first |> to_seq
      |> Seq.filter (fun value ->
             List.for_all
            (fun candidate -> candidate |> to_seq |> Seq.exists (equal value))
               rest)
      |> set

let set_difference first rest =
  first |> to_seq
  |> Seq.filter (fun value ->
         not
           (List.exists
              (fun candidate -> candidate |> to_seq |> Seq.exists (equal value))
              rest))
  |> set

let set_subset left right =
  left |> to_seq
  |> Seq.for_all (fun value -> right |> to_seq |> Seq.exists (equal value))

let as_int value =
  match value.payload with
  | Int value -> value
  | _ -> invalid_arg "expected int"

let to_int value =
  match value.payload with
  | Int value -> value
  | Float value -> int_of_float value
  | _ -> invalid_arg "expected numeric value"

let as_float value =
  match value.payload with
  | Float value -> value
  | _ -> invalid_arg "expected float"

let as_char value =
  match value.payload with
  | Char value -> value
  | _ -> invalid_arg "expected char"

let as_string value =
  match value.payload with
  | String value -> value
  | _ -> invalid_arg "expected string"

let as_symbol value =
  match value.payload with
  | Symbol value -> value
  | _ -> invalid_arg "expected symbol"

let as_keyword value =
  match value.payload with
  | Keyword value -> value
  | _ -> invalid_arg "expected keyword"

let as_bool value =
  match value.payload with
  | Bool value -> value
  | _ -> invalid_arg "expected bool"

let as_identifier value =
  match value.payload with
  | String value | Symbol value | Keyword value -> value
  | _ -> invalid_arg "expected string, symbol, or keyword"

let as_named_identifier value =
  match value.payload with
  | Symbol value | Keyword value -> value
  | _ -> invalid_arg "expected symbol or keyword"
