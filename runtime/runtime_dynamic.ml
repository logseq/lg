type t = {
  payload : payload;
  sequence : (unit -> t Seq.t) option;
  sequential : bool;
  protocols : protocol list;
  metadata : t option;
  type_name : string option;
}

and payload =
  | Nil
  | Int of int
  | Float of float
  | Char of char
  | String of string
  | Symbol of string
  | Keyword of string
  | Bool of bool
  | Function of (t list -> t)
  | List
  | Vector
  | Seq
  | Set of t list
  | Map of (t * t) list
  | Reference of dynamic_reference
  | Opaque of string * (string * (unit -> t)) list

and dynamic_reference = { get : unit -> t; set : t -> t }
and protocol = { id : string; methods : (string * (t list -> t)) list }

let protocol id methods = { id; methods }

let make ?sequence ?(sequential = false) ?(protocols = []) ?metadata ?type_name
    payload =
  { payload; sequence; sequential; protocols; metadata; type_name }

let with_protocols value protocols = { value with protocols }
let with_metadata value metadata = { value with metadata = Some metadata }
let nil = make Nil
let int value = make (Int value)
let float value = make (Float value)
let char value = make (Char value)
let string value = make (String value)
let symbol value = make (Symbol value)
let keyword value = make (Keyword value)
let bool value = make (Bool value)
let function_ value = make (Function value)

let reference get set =
  make ~type_name:"clojure.lang.Atom" (Reference { get; set })

let opaque name fields = make ~type_name:name (Opaque (name, fields))
let metadata value = Option.value value.metadata ~default:nil

let rec to_string ~pr value =
  let join values =
    values |> List.map (to_string ~pr:true) |> String.concat " "
  in
  match value.payload with
  | Nil -> "nil"
  | Int value -> string_of_int value
  | Float value -> string_of_float value
  | Char value -> String.make 1 value
  | String value -> if pr then Printf.sprintf "%S" value else value
  | Symbol value | Keyword value -> value
  | Bool value -> string_of_bool value
  | Function _ -> "<function>"
  | Reference _ -> "#object[clojure.lang.Atom]"
  | List -> "(" ^ join (List.of_seq (to_seq value)) ^ ")"
  | Vector -> "[" ^ join (List.of_seq (to_seq value)) ^ "]"
  | Seq -> "(" ^ join (List.of_seq (to_seq value)) ^ ")"
  | Set values -> "#{" ^ join values ^ "}"
  | Map entries ->
      let entries =
        entries
        |> List.map (fun (key, value) ->
               to_string ~pr:true key ^ " " ^ to_string ~pr:true value)
        |> String.concat ", "
      in
      "{" ^ entries ^ "}"
  | Opaque (name, _) -> "<" ^ name ^ ">"

and to_seq value =
  match value.sequence with
  | Some sequence -> sequence ()
  | None -> invalid_arg "dynamic value is not seqable"

let str value = to_string ~pr:false value
let pr_str value = to_string ~pr:true value

let list values =
  make ~sequential:true ~sequence:(fun () -> List.to_seq values) List

let vector values =
  make ~sequential:true
    ~sequence:(fun () -> values |> Rrbvec.to_list |> List.to_seq)
    Vector

let regex_match = function
  | None -> nil
  | Some [ Some value ] -> string value
  | Some captures ->
      captures
      |> List.map (function Some value -> string value | None -> nil)
      |> Rrbvec.of_list |> vector

let seq values =
  let values = Seq.memoize values in
  make ~sequential:true ~sequence:(fun () -> values) Seq

let map entries =
  make
    ~sequence:(fun () ->
      entries |> List.to_seq
      |> Seq.map (fun (key, value) -> vector (Rrbvec.of_list [ key; value ])))
    (Map entries)

let record type_name entries =
  make ~type_name
    ~sequence:(fun () ->
      entries |> List.to_seq
      |> Seq.map (fun (key, value) -> vector (Rrbvec.of_list [ key; value ])))
    (Map entries)

let rec equal left right =
  match (left.payload, right.payload) with
  | Nil, Nil -> true
  | Int left, Int right -> left = right
  | Float left, Float right -> left = right
  | Char left, Char right -> left = right
  | String left, String right -> left = right
  | Symbol left, Symbol right -> left = right
  | Keyword left, Keyword right -> left = right
  | Bool left, Bool right -> left = right
  | List, List | Vector, Vector | Seq, Seq ->
      Seq.equal equal (to_seq left) (to_seq right)
  | Set left, Set right ->
      List.length left = List.length right
      && List.for_all (fun value -> List.exists (equal value) right) left
  | Map left, Map right ->
      List.length left = List.length right
      && List.for_all
           (fun (key, value) ->
             List.exists
               (fun (other_key, other_value) ->
                 equal key other_key && equal value other_value)
               right)
           left
  | Opaque _, Opaque _ -> false
  | Reference _, Reference _ -> false
  | Function _, Function _ -> false
  | _ -> false

let compare left right =
  match (left.payload, right.payload) with
  | Nil, Nil -> 0
  | Nil, _ -> -1
  | _, Nil -> 1
  | Int left, Int right -> Stdlib.compare left right
  | Float left, Float right -> Stdlib.compare left right
  | Int left, Float right -> Stdlib.compare (float_of_int left) right
  | Float left, Int right -> Stdlib.compare left (float_of_int right)
  | Char left, Char right -> Stdlib.compare left right
  | String left, String right
  | Symbol left, Symbol right
  | Keyword left, Keyword right ->
      String.compare left right
  | Bool left, Bool right -> Bool.compare left right
  | _ -> invalid_arg "dynamic values are not comparable"

let class_ value =
  let name =
    match value.payload with
    | Nil -> None
    | Int _ -> Some "java.lang.Long"
    | Float _ -> Some "java.lang.Double"
    | Char _ -> Some "java.lang.Character"
    | String _ -> Some "java.lang.String"
    | Symbol _ -> Some "clojure.lang.Symbol"
    | Keyword _ -> Some "clojure.lang.Keyword"
    | Bool _ -> Some "java.lang.Boolean"
    | Function _ -> Some "clojure.lang.AFunction"
    | Reference _ -> Some "clojure.lang.Atom"
    | List -> Some "clojure.lang.PersistentList"
    | Vector -> Some "clojure.lang.PersistentVector"
    | Seq -> Some "clojure.lang.ISeq"
    | Set _ -> Some "clojure.lang.PersistentHashSet"
    | Map _ -> Some "clojure.lang.PersistentArrayMap"
    | Opaque (name, _) -> Some name
  in
  match name with None -> nil | Some name -> string name

let is_comparable value =
  match value.payload with
  | Nil | Int _ | Float _ | Char _ | String _ | Symbol _ | Keyword _ | Bool _ ->
      true
  | Function _ | Reference _ | List | Vector | Seq | Set _ | Map _ | Opaque _ ->
      false

let set sequence =
  let values =
    Seq.fold_left
      (fun values value ->
        if List.exists (equal value) values then values else value :: values)
      [] sequence
    |> List.rev
  in
  make ~sequence:(fun () -> List.to_seq values) (Set values)

let cons value collection = seq (Seq.cons value (to_seq collection))

let conj collection value =
  match collection.payload with
  | Nil -> list [ value ]
  | List -> list (value :: List.of_seq (to_seq collection))
  | Vector ->
      let values = collection |> to_seq |> List.of_seq |> Rrbvec.of_list in
      vector (Rrbvec.push_back values value)
  | Set values ->
      if List.exists (equal value) values then collection
      else
        make
          ~sequence:(fun () -> List.to_seq (value :: values))
          (Set (value :: values))
  | _ -> invalid_arg "dynamic conj expects a collection"

let assoc value key replacement =
  match value.payload with
  | Nil -> map [ (key, replacement) ]
  | Map entries ->
      let rec replace acc = function
        | [] -> List.rev ((key, replacement) :: acc)
        | (existing_key, _) :: rest when equal key existing_key ->
            List.rev_append acc ((key, replacement) :: rest)
        | entry :: rest -> replace (entry :: acc) rest
      in
      map (replace [] entries)
  | _ -> invalid_arg "dynamic value is not associative"

let merge values =
  let merge_one result value =
    match value.payload with
    | Nil -> result
    | Map entries ->
        List.fold_left
          (fun result (key, value) -> assoc result key value)
          result entries
    | _ -> invalid_arg "dynamic merge expects maps"
  in
  List.fold_left merge_one nil values

let dissoc value key =
  match value.payload with
  | Map entries ->
      map
        (List.filter
           (fun (existing_key, _) -> not (equal key existing_key))
           entries)
  | _ -> invalid_arg "dynamic value is not associative"

let get value key =
  match (value.payload, key.payload) with
  | Opaque (_, fields), Keyword keyword -> (
      match List.assoc_opt keyword fields with
      | Some project -> project ()
      | None -> nil)
  | Map entries, _ -> (
      match
        List.find_opt (fun (entry_key, _) -> equal key entry_key) entries
      with
      | Some (_, value) -> value
      | None -> nil)
  | _ -> nil

let get_default value key default =
  match (value.payload, key.payload) with
  | Opaque (_, fields), Keyword keyword -> (
      match List.assoc_opt keyword fields with
      | Some project -> project ()
      | None -> default)
  | Map entries, _ -> (
      match
        List.find_opt (fun (entry_key, _) -> equal key entry_key) entries
      with
      | Some (_, value) -> value
      | None -> default)
  | _ -> default

let contains value key =
  match (value.payload, key.payload) with
  | Opaque (_, fields), Keyword keyword -> List.mem_assoc keyword fields
  | Map entries, _ ->
      List.exists (fun (entry_key, _) -> equal key entry_key) entries
  | Set values, _ -> List.exists (equal key) values
  | Vector, Int index -> index >= 0 && index < Seq.length (to_seq value)
  | _ -> false

let entries value =
  match value.payload with
  | Map entries -> entries
  | _ -> invalid_arg "dynamic value is not a map"

let empty value =
  match value.payload with
  | List -> list []
  | Vector -> vector Rrbvec.empty
  | Seq -> seq Seq.empty
  | Map _ -> map []
  | _ -> invalid_arg "dynamic value is not a collection"

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
  | Vector ->
      vector (Rrbvec.of_list (List.of_seq (Seq.append (to_seq target) source)))
  | Seq -> seq (Seq.append (to_seq target) source)
  | Map _ ->
      Seq.fold_left
        (fun map entry ->
          let key, value = pair entry in
          assoc map key value)
        target source
  | _ -> invalid_arg "dynamic into target is not a collection"

let is_sequential value = value.sequential
let is_seqable value = Option.is_some value.sequence

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
let is_list value = match value.payload with List -> true | _ -> false
let is_vector value = match value.payload with Vector -> true | _ -> false
let is_seq value = match value.payload with List | Seq -> true | _ -> false
let is_map value = match value.payload with Map _ -> true | _ -> false
let is_set value = match value.payload with Set _ -> true | _ -> false
let is_coll value = is_seqable value
let is_instance value type_name = value.type_name = Some type_name

let call value arguments =
  match value.payload with
  | Function function_ -> function_ arguments
  | _ -> invalid_arg "dynamic value is not callable"

let deref value =
  match value.payload with
  | Reference reference -> reference.get ()
  | _ -> invalid_arg "dynamic deref expects a reference"

let reset value replacement =
  match value.payload with
  | Reference reference -> reference.set replacement
  | _ -> invalid_arg "dynamic reset expects a reference"

let swap value update_fn arguments =
  reset value (call update_fn (deref value :: arguments))

let rec hash value =
  match value.payload with
  | Nil -> 0
  | Bool true -> 1231
  | Bool false -> 1237
  | Int value -> Runtime_hash.hash_int value
  | Float value -> Runtime_hash.hash_float value
  | Char value -> Char.code value
  | String value -> Runtime_hash.hash_string value
  | Symbol value -> Runtime_hash.hash_symbol value
  | Keyword value -> Runtime_hash.hash_keyword value
  | List | Vector | Seq ->
      value |> to_seq |> Seq.map hash |> Runtime_hash.hash_ordered
  | Set values ->
      values |> List.to_seq |> Seq.map hash |> Runtime_hash.hash_unordered
  | Map entries ->
      entries |> List.to_seq
      |> Seq.map (fun (key, value) ->
          [ hash key; hash value ] |> List.to_seq |> Runtime_hash.hash_ordered)
      |> Runtime_hash.hash_unordered
  | Function _ -> 0
  | Reference _ -> 0
  | Opaque (name, _) -> Runtime_hash.hash_string name

let hash_unordered_coll value =
  value |> to_seq |> Seq.map hash |> Runtime_hash.hash_unordered

let group_by key_fn pack_key pack_item sequence =
  Seq.fold_left
    (fun groups item ->
      let key = pack_key (key_fn item) in
      let item = pack_item item in
      let values =
        match get groups key with
        | { payload = Nil; _ } -> Rrbvec.empty
        | { payload = Vector; sequence = Some values; _ } ->
            values () |> List.of_seq |> Rrbvec.of_list
        | _ -> invalid_arg "group-by value is not a vector"
      in
      assoc groups key (vector (Rrbvec.push_back values item)))
    (map []) sequence

let has_protocol value protocol_id =
  List.exists (fun protocol -> protocol.id = protocol_id) value.protocols

let invoke value protocol_id method_name arguments =
  match
    List.find_opt (fun protocol -> protocol.id = protocol_id) value.protocols
  with
  | None -> invalid_arg ("missing protocol " ^ protocol_id)
  | Some protocol -> (
      match List.assoc_opt method_name protocol.methods with
      | None ->
          invalid_arg
            ("missing protocol method " ^ protocol_id ^ "/" ^ method_name)
      | Some method_ -> method_ arguments)

let as_transient value =
  if has_protocol value "IEditableCollection" then
    invoke value "IEditableCollection" "-as-transient" []
  else
    match value.payload with
    | Vector | Set _ | Map _ -> value
    | _ -> invalid_arg "transient expects an editable collection"

let persistent value =
  if has_protocol value "ITransientCollection" then
    invoke value "ITransientCollection" "-persistent!" []
  else
    match value.payload with
    | Vector | Set _ | Map _ -> value
    | _ -> invalid_arg "persistent! expects a transient collection"

let conj_bang collection value =
  if has_protocol collection "ITransientCollection" then
    invoke collection "ITransientCollection" "-conj!" [ value ]
  else conj collection value

let assoc_bang collection key value = assoc collection key value

let disj_bang collection value =
  if has_protocol collection "ITransientSet" then
    invoke collection "ITransientSet" "-disjoin!" [ value ]
  else
    match collection.payload with
    | Set values ->
        let values =
          List.filter (fun candidate -> not (equal candidate value)) values
        in
        make ~sequence:(fun () -> List.to_seq values) (Set values)
    | _ -> invalid_arg "disj! expects a transient set"

let invoke_function value arguments =
  match value.payload with
  | Function function_ -> function_ arguments
  | Set values -> (
      match arguments with
      | [ candidate ] ->
          List.find_opt (equal candidate) values |> Option.value ~default:nil
      | _ -> invalid_arg "dynamic set expects one argument")
  | _ -> invalid_arg "dynamic value is not a function"

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

let rec update_in target path update_fn arguments =
  match path () with
  | Seq.Nil -> invoke_function update_fn (target :: arguments)
  | Seq.Cons (key, rest) ->
      let nested =
        match target.payload with
        | Nil -> nil
        | Map _ -> get target key
        | _ -> invalid_arg "update-in target is not a map"
      in
      let updated = update_in nested rest update_fn arguments in
      let target =
        match target.payload with
        | Nil -> map []
        | Map _ -> target
        | _ -> target
      in
      assoc target key updated

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
