type _ nominal_tag = ..
type nominal = Nominal : 'a nominal_tag * 'a -> nominal
type _ nominal_tag += Uuid_tag : Runtime_uuid.t nominal_tag
type _ nominal_tag += Host_tag : Obj.t nominal_tag

let dynamic_marker = ref ()

type t = {
  marker : unit ref;
  payload : payload;
  sequence : (unit -> t Seq.t) option;
  sequential : bool;
  protocols : protocol list;
  metadata : t option;
  type_name : string option;
  nominal : nominal option;
  associative : (t -> t -> t) option;
  lookup : (t -> t -> t) option;
  printer : (unit -> string) option;
}

and payload =
  | Nil
  | Int of int64
  | Float of float
  | Char of char
  | String of string
  | Symbol of string
  | Keyword of string
  | Bool of bool
  | Function of (t list -> t)
  | Array of t array
  | List
  | Vector
  | Seq
  | Set of t list
  | Map of (t * t) list
  | Reference of dynamic_reference
  | Record of string * (string * (unit -> t)) list * (string * t) list
  | Opaque of string * (string * (unit -> t)) list

and dynamic_reference = { get : unit -> t; set : t -> t }
and protocol = { id : string; methods : (string * (t list -> t)) list }

let protocol id methods = { id; methods }

let protocol_extensions : ((string * string), t -> t) Hashtbl.t =
  Hashtbl.create 32

let lookup_extensions : (string, t -> t -> t -> t) Hashtbl.t =
  Hashtbl.create 32

let printer_extensions : (string, t -> string) Hashtbl.t = Hashtbl.create 32

let register_protocol_extension type_name protocol_id repack =
  Hashtbl.replace protocol_extensions (type_name, protocol_id) repack

let register_lookup_extension type_name lookup =
  Hashtbl.replace lookup_extensions type_name lookup

let register_printer_extension type_name printer =
  Hashtbl.replace printer_extensions type_name printer

let lookup_extension value =
  Option.bind value.type_name (fun type_name ->
      Hashtbl.find_opt lookup_extensions type_name)

let printer_extension value =
  Option.bind value.type_name (fun type_name ->
      Hashtbl.find_opt printer_extensions type_name)

let has_protocol_extension value protocol_id =
  match value.type_name with
  | None -> false
  | Some type_name -> Hashtbl.mem protocol_extensions (type_name, protocol_id)

let protocol_extension value protocol_id =
  match value.type_name with
  | None -> None
  | Some type_name ->
      Hashtbl.find_opt protocol_extensions (type_name, protocol_id)
      |> Option.map (fun repack -> repack value)

let make ?sequence ?(sequential = false) ?(protocols = []) ?metadata ?type_name
    payload =
  {
    marker = dynamic_marker;
    payload;
    sequence;
    sequential;
    protocols;
    metadata;
    type_name;
    nominal = None;
    associative = None;
    lookup = None;
    printer = None;
  }

let with_protocols value protocols = { value with protocols }
let with_metadata value metadata = { value with metadata = Some metadata }
let with_nominal tag payload value =
  { value with nominal = Some (Nominal (tag, payload)) }

let with_assoc value associative = { value with associative = Some associative }
let with_lookup value lookup = { value with lookup = Some lookup }
let with_sequence value sequence = { value with sequence = Some sequence }
let with_printer value printer = { value with printer = Some printer }

let nominal value = value.nominal

let unpack_nominal expected_tag value =
  match value.nominal with
  | Some (Nominal (actual_tag, payload))
    when Obj.repr actual_tag = Obj.repr expected_tag ->
      Some (Obj.repr payload)
  | Some _ | None -> None

let nil = make Nil
let int value = make (Int value)
let float value = make (Float value)
let char value = make (Char value)
let string value = make (String value)
let uuid value = with_nominal Uuid_tag value (string (Runtime_uuid.to_string value))

let as_uuid value : Runtime_uuid.t =
  match nominal value with
  | Some (Nominal (Uuid_tag, uuid)) -> uuid
  | _ -> invalid_arg "dynamic value is not a UUID"

let symbol value = make (Symbol value)
let keyword value = make (Keyword value)
let bool value = make (Bool value)
let unit () = nil

let as_unit value =
  match value.payload with
  | Nil -> ()
  | _ -> invalid_arg "dynamic value is not unit"

let function_ value = make (Function value)

let is_function value =
  match value.payload with Function _ -> true | _ -> false

let reference get set =
  make ~type_name:"clojure.lang.Atom" (Reference { get; set })

let opaque name fields = make ~type_name:name (Opaque (name, fields))

let host name value =
  with_nominal Host_tag (Obj.repr value) (opaque name [])

let as_host name value =
  match (value.type_name, nominal value) with
  | Some actual_name, Some (Nominal (Host_tag, payload))
    when String.equal actual_name name ->
      Obj.obj payload
  | _ -> invalid_arg ("dynamic value is not " ^ name)

let narrow_like (type expected_type) (expected : expected_type) value :
    expected_type =
  let payload =
    match value.nominal with
    | Some (Nominal (_, payload)) -> Some (Obj.repr payload)
    | None -> (
        match value.payload with
        | Int value -> Some (Obj.repr value)
        | Float value -> Some (Obj.repr value)
        | Char value -> Some (Obj.repr value)
        | String value | Symbol value | Keyword value -> Some (Obj.repr value)
        | Bool value -> Some (Obj.repr value)
        | Nil | Function _ | Array _ | List | Vector | Seq | Set _ | Map _
        | Reference _ | Record _ | Opaque _ ->
            None)
  in
  match payload with
  | None -> invalid_arg "dynamic value cannot be narrowed by a value witness"
  | Some payload ->
      let expected = Obj.repr expected in
      let same_shape =
        if Obj.is_int expected || Obj.is_int payload then
          Obj.is_int expected && Obj.is_int payload
        else
          Obj.tag expected = Obj.tag payload
          && Obj.size expected = Obj.size payload
      in
      if same_shape then Obj.obj payload
      else invalid_arg "dynamic value does not match its value witness"

let metadata value = Option.value value.metadata ~default:nil

let rec to_string ~pr value =
  match (pr, value.printer) with
  | true, Some printer -> printer ()
  | true, None -> (
      match printer_extension value with
      | Some printer -> printer value
      | None -> to_string_payload ~pr value)
  | _ -> to_string_payload ~pr value

and to_string_payload ~pr value =
  let join values =
    values |> List.map (to_string ~pr:true) |> String.concat " "
  in
  match value.payload with
  | Nil -> "nil"
  | Int value -> Int64.to_string value
  | Float value -> string_of_float value
  | Char value -> String.make 1 value
  | String value -> if pr then Printf.sprintf "%S" value else value
  | Symbol value | Keyword value -> value
  | Bool value -> string_of_bool value
  | Function _ -> "<function>"
  | Array values ->
      "#js ["
      ^ (values |> Array.to_list |> List.map (to_string ~pr:true)
        |> String.concat " ")
      ^ "]"
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
  | Record (name, _, _) | Opaque (name, _) -> "<" ^ name ^ ">"

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
    ~sequence:(fun () -> values |> Rrbvec.to_list |> List.to_seq)
    Vector

let vec_value value =
  vector (Rrbvec.of_list (List.of_seq (to_seq value)))

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

let lazy_record type_name fields extension_fields =
  let entries () =
    List.map (fun (key, project) -> (keyword key, project ())) fields
    @ List.map (fun (key, value) -> (keyword key, value)) extension_fields
  in
  make ~type_name
    ~sequence:(fun () ->
      entries () |> List.to_seq
      |> Seq.map (fun (key, value) -> vector (Rrbvec.of_list [ key; value ])))
    (Record (type_name, fields, extension_fields))

let find_protocol value protocol_id =
  match
    List.find_opt (fun protocol -> protocol.id = protocol_id) value.protocols
  with
  | Some _ as protocol -> protocol
  | None ->
      Option.bind (protocol_extension value protocol_id) (fun extended ->
          List.find_opt
            (fun protocol -> protocol.id = protocol_id)
            extended.protocols)

let find_protocol_method value protocol_id method_name =
  Option.bind (find_protocol value protocol_id) (fun protocol ->
      List.assoc_opt method_name protocol.methods)

let nominal_identity_equal left right =
  match (left.nominal, right.nominal) with
  | Some (Nominal (left_tag, left_payload)),
    Some (Nominal (right_tag, right_payload)) ->
      Obj.repr left_tag = Obj.repr right_tag
      && Obj.repr left_payload == Obj.repr right_payload
  | (Some _ | None), (Some _ | None) -> false

let same_nominal_type left right =
  match left.nominal with
  | None -> true
  | Some (Nominal (left_tag, _)) -> (
      match right.nominal with
      | Some (Nominal (right_tag, _)) ->
          Obj.repr left_tag = Obj.repr right_tag
      | None -> false)

let expand_record_extension_entries entries =
  List.concat_map
    (fun ((key, value) as entry) ->
      match (key.payload, value.payload) with
      | Keyword ":__lg/extmap", Map extensions -> extensions
      | _ -> [ entry ])
    entries

let rec equal left right =
  match find_protocol_method left "IEquiv" "-equiv" with
  | Some _ when not (same_nominal_type left right) -> false
  | Some equiv -> (
      match (equiv [ right ]).payload with
      | Bool result -> result
      | _ -> invalid_arg "IEquiv/-equiv must return bool")
  | None -> (
  match (left.payload, right.payload) with
  | Nil, Nil -> true
  | Int left, Int right -> left = right
  | Float left, Float right -> left = right
  | Char left, Char right -> left = right
  | String left, String right -> left = right
  | Symbol left, Symbol right -> left = right
  | Keyword left, Keyword right -> left = right
  | Bool left, Bool right -> left = right
  | Array left, Array right -> left == right
  | (List | Vector | Seq), (List | Vector | Seq) ->
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
  | Map left_entries, Record (right_name, right_fields, right_extensions)
    when left.type_name = Some right_name && right.type_name = Some right_name ->
      let left_entries = expand_record_extension_entries left_entries in
      let right_entries =
        List.map (fun (key, project) -> (keyword key, project ())) right_fields
        @ List.map (fun (key, value) -> (keyword key, value)) right_extensions
      in
      List.length left_entries = List.length right_entries
      && List.for_all
           (fun (key, value) ->
             List.exists
               (fun (other_key, other_value) ->
                 equal key other_key && equal value other_value)
               right_entries)
           left_entries
  | Record _, Map _ -> equal right left
  | Record (left_name, left_fields, left_extensions),
    Record (right_name, right_fields, right_extensions) ->
      let entries fields extensions =
        List.map (fun (key, project) -> (key, project ())) fields @ extensions
      in
      let left = entries left_fields left_extensions in
      let right = entries right_fields right_extensions in
      String.equal left_name right_name
      && List.length left = List.length right
      && List.for_all
           (fun (key, value) ->
             List.exists
               (fun (other_key, other_value) ->
                 String.equal key other_key && equal value other_value)
               right)
           left
  | Opaque _, Opaque _ -> nominal_identity_equal left right
  | Reference _, Reference _ -> false
  | Function _, Function _ -> false
  | _ -> false)

let is_runtime_dynamic value =
  try
    let representation = Obj.repr value in
    (not (Obj.is_int representation))
    && Obj.tag representation = 0
    && Obj.field representation 0 == Obj.repr dynamic_marker
  with Invalid_argument _ -> false

let polymorphic_equal left right =
  if left == right then true
  else if is_runtime_dynamic left && is_runtime_dynamic right then
    equal (Obj.magic left) (Obj.magic right)
  else
    try left = right with Invalid_argument _ -> false

let equal_arguments = function
  | [] | [ _ ] -> true
  | first :: rest -> List.for_all (equal first) rest

let numeric_equal left right =
  match (left.payload, right.payload) with
  | Int left, Int right -> left = right
  | Float left, Float right -> left = right
  | Int left, Float right -> Int64.to_float left = right
  | Float left, Int right -> left = Int64.to_float right
  | _ -> false

let numeric_equal_arguments = function
  | [] | [ _ ] -> true
  | first :: rest -> List.for_all (numeric_equal first) rest

let equality_function = function_ (fun arguments -> bool (equal_arguments arguments))

let inequality_function =
  function_ (fun arguments -> bool (not (equal_arguments arguments)))

let payload_rank = function
  | Nil -> 0
  | Int _ | Float _ -> 1
  | Char _ -> 2
  | String _ -> 3
  | Symbol _ -> 4
  | Keyword _ -> 5
  | Bool _ -> 6
  | Array _ -> 7
  | List | Vector | Seq -> 8
  | Set _ -> 9
  | Map _ | Record _ -> 10
  | Function _ -> 11
  | Reference _ -> 12
  | Opaque _ -> 13

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

and compare left right =
  match (left.payload, right.payload) with
  | Nil, Nil -> 0
  | Nil, _ -> -1
  | _, Nil -> 1
  | Int left, Int right -> Stdlib.compare left right
  | Float left, Float right -> Stdlib.compare left right
  | Int left, Float right -> Stdlib.compare (Int64.to_float left) right
  | Float left, Int right -> Stdlib.compare left (Int64.to_float right)
  | Char left, Char right -> Stdlib.compare left right
  | String left, String right
  | Symbol left, Symbol right
  | Keyword left, Keyword right ->
      String.compare left right
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
  | (List | Vector | Seq), (List | Vector | Seq) ->
      compare_sequences (to_seq left) (to_seq right)
  | Set left, Set right ->
      compare_sequences
        (List.sort compare left |> List.to_seq)
        (List.sort compare right |> List.to_seq)
  | Map left, Map right -> compare_entry_lists left right
  | Record (left_name, left_fields, left_extensions),
    Record (right_name, right_fields, right_extensions) ->
      let name_result = String.compare left_name right_name in
      if name_result <> 0 then name_result
      else
        let entries fields extensions =
          List.map
            (fun (key, project) -> (keyword key, project ()))
            fields
          @ List.map (fun (key, value) -> (keyword key, value)) extensions
        in
        compare_entry_lists
          (entries left_fields left_extensions)
          (entries right_fields right_extensions)
  | _ ->
      let rank = Int.compare (payload_rank left.payload) (payload_rank right.payload) in
      if rank <> 0 then rank else invalid_arg "dynamic values are not comparable"

let compare_int64 left right = Int64.of_int (compare left right)

let unary_function name fn =
  function_ (function
    | [ value ] -> fn value
    | _ -> invalid_arg (name ^ " expects one argument"))

let binary_function name fn =
  function_ (function
    | [ left; right ] -> fn left right
    | _ -> invalid_arg (name ^ " expects two arguments"))

let int_quot = Int64.div
let int_rem = Int64.rem

let int_binary_function name fn =
  binary_function name (fun left right ->
      match (left.payload, right.payload) with
      | Int left, Int right -> int (fn left right)
      | _ -> invalid_arg (name ^ " expects integer arguments"))

let quot_function = int_binary_function "quot" int_quot
let rem_function = int_binary_function "rem" int_rem

let clojure_mod left right =
  let remainder = Int64.rem left right in
  if remainder = 0L || (remainder > 0L) = (right > 0L) then remainder
  else Int64.add remainder right

let mod_function = int_binary_function "mod" clojure_mod

let int_inc = Int64.succ
let int_dec = Int64.pred
let int_max = Stdlib.max
let int_min = Stdlib.min
let int_zero value = value = 0L
let int_positive value = value > 0L
let int_negative value = value < 0L
let int_even value = Int64.rem value 2L = 0L
let int_odd value = Int64.rem value 2L <> 0L
let int_compare = Int64.compare

let numeric_unary_function name int_fn float_fn =
  unary_function name (fun value ->
      match value.payload with
      | Int value -> int (int_fn value)
      | Float value -> float (float_fn value)
      | _ -> invalid_arg (name ^ " expects a numeric argument"))

let inc_function = numeric_unary_function "inc" int_inc (fun value -> value +. 1.)
let dec_function = numeric_unary_function "dec" int_dec (fun value -> value -. 1.)

let numeric_predicate_function name int_predicate float_predicate =
  unary_function name (fun value ->
      match value.payload with
      | Int value -> bool (int_predicate value)
      | Float value -> bool (float_predicate value)
      | _ -> invalid_arg (name ^ " expects a numeric argument"))

let zero_function =
  numeric_predicate_function "zero?" int_zero (( = ) 0.)

let positive_function =
  numeric_predicate_function "pos?" int_positive (fun value -> value > 0.)

let negative_function =
  numeric_predicate_function "neg?" int_negative (fun value -> value < 0.)

let integer_predicate_function name predicate =
  unary_function name (fun value ->
      match value.payload with
      | Int value -> bool (predicate value)
      | _ -> invalid_arg (name ^ " expects an integer argument"))

let even_function = integer_predicate_function "even?" int_even
let odd_function = integer_predicate_function "odd?" int_odd
let compare_function =
  binary_function "compare" (fun left right -> int (Int64.of_int (compare left right)))

let extremum_function name select =
  function_ (function
    | [] -> invalid_arg (name ^ " expects at least one argument")
    | first :: rest -> List.fold_left select first rest)

let max_function =
  extremum_function "max" (fun left right ->
      if compare left right >= 0 then left else right)

let min_function =
  extremum_function "min" (fun left right ->
      if compare left right <= 0 then left else right)

let rand_function =
  function_ (function
    | [] -> float (Runtime_random.rand 1.)
    | [ { payload = Int bound; _ } ] ->
        float (Runtime_random.rand (Int64.to_float bound))
    | [ { payload = Float bound; _ } ] -> float (Runtime_random.rand bound)
    | [ _ ] -> invalid_arg "rand expects a numeric bound"
    | _ -> invalid_arg "rand expects zero or one argument")

let rand_int_function =
  unary_function "rand-int" (fun value ->
      match value.payload with
      | Int bound ->
          int (Int64.of_int (Runtime_random.rand_int (Int64.to_int bound)))
      | _ -> invalid_arg "rand-int expects an integer bound")

let sort collection =
  collection |> to_seq |> List.of_seq |> List.sort compare |> list

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
    | Array _ -> Some "js/Array"
    | Reference _ -> Some "clojure.lang.Atom"
    | List -> Some "clojure.lang.PersistentList"
    | Vector -> Some "clojure.lang.PersistentVector"
    | Seq -> Some "clojure.lang.ISeq"
    | Set _ -> Some "clojure.lang.PersistentHashSet"
    | Map _ -> Some "clojure.lang.PersistentArrayMap"
    | Record (name, _, _) -> Some name
    | Opaque (name, _) -> Some name
  in
  match name with None -> nil | Some name -> string name

let is_comparable value =
  match value.payload with
  | Nil | Int _ | Float _ | Char _ | String _ | Symbol _ | Keyword _ | Bool _
  | Array _ ->
      true
  | Function _ | Reference _ | List | Vector | Seq | Set _ | Map _ | Record _
  | Opaque _ ->
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
  | Seq -> seq (Seq.cons value (to_seq collection))
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
  match value.associative with
  | Some associative -> associative key replacement
  | None -> (
      match value.payload with
      | Nil -> map [ (key, replacement) ]
      | Vector ->
          let values = value |> to_seq |> List.of_seq |> Rrbvec.of_list in
          let index =
            match key.payload with
            | Int index -> Int64.to_int index
            | _ -> invalid_arg "dynamic vector assoc expects an integer index"
          in
          vector (Rrbvec.set values index replacement)
      | Map entries ->
          let rec replace acc = function
            | [] -> List.rev ((key, replacement) :: acc)
            | (existing_key, _) :: rest when equal key existing_key ->
                List.rev_append acc ((key, replacement) :: rest)
            | entry :: rest -> replace (entry :: acc) rest
          in
          map (replace [] entries)
      | _ -> invalid_arg "dynamic value is not associative")

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
  | Nil -> value
  | Map entries ->
      map
        (List.filter
           (fun (existing_key, _) -> not (equal key existing_key))
           entries)
  | _ -> invalid_arg "dynamic value is not associative"

let dissoc_function =
  function_ (function
    | target :: keys -> List.fold_left dissoc target keys
    | [] -> invalid_arg "dissoc expects a collection")

let select_keys value keys =
  match value.payload with
  | Map entries ->
      Seq.fold_left
        (fun selected key ->
          match
            List.find_opt
              (fun (existing_key, _) -> equal key existing_key)
              entries
          with
          | Some (_, selected_value) -> assoc selected key selected_value
          | None -> selected)
        (map []) keys
  | _ -> invalid_arg "select-keys expects a map"

let vals value =
  match value.payload with
  | Map entries ->
      entries |> List.map snd |> Rrbvec.of_list |> vector
  | _ -> invalid_arg "vals expects a map"

let keys value =
  match value.payload with
  | Map entries ->
      entries |> List.map fst |> Rrbvec.of_list |> vector
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
    match Seq.drop index (to_seq value) () with
    | Seq.Nil -> None
    | Seq.Cons (item, _) -> Some item

let subvec_value value start stop =
  let values = List.of_seq (to_seq value) in
  let length = List.length values in
  if start < 0 || stop < start || stop > length then
    invalid_arg "subvec indexes are out of bounds"
  else
    values |> List.to_seq |> Seq.drop start |> Seq.take (stop - start)
    |> List.of_seq |> Rrbvec.of_list |> vector

let nominal_field_name = function
  | Keyword keyword -> Some keyword
  | String field ->
      Some
        (if String.starts_with ~prefix:":" field then field else ":" ^ field)
  | _ -> None

let payload_get value key =
  match (value.payload, key.payload) with
  | Record (_, fields, extensions), key -> (
      match nominal_field_name key with
      | Some field -> (
          match List.assoc_opt field fields with
          | Some project -> Some (project ())
          | None -> List.assoc_opt field extensions)
      | None -> None)
  | Opaque (_, fields), key -> (
      match nominal_field_name key with
      | Some field -> List.assoc_opt field fields |> Option.map (fun get -> get ())
      | None -> None)
  | Map entries, _ ->
      List.find_opt (fun (entry_key, _) -> equal key entry_key) entries
      |> Option.map snd
  | Vector, Int index -> vector_nth_opt value (Int64.to_int index)
  | _ -> None

let get value key =
  match payload_get value key with
  | Some result -> result
  | None -> (
      match value.lookup with
      | Some lookup -> lookup key nil
      | None -> (
          match lookup_extension value with
          | Some lookup -> lookup value key nil
          | None -> nil))

let indexed_get value index =
  match (value.payload, index.payload) with
  | Array values, Int index -> Array.get values (Int64.to_int index)
  | _ -> get value index

let get_default value key default =
  match payload_get value key with
  | Some result -> result
  | None -> (
      match value.lookup with
      | Some lookup -> lookup key default
      | None -> (
          match lookup_extension value with
          | Some lookup -> lookup value key default
          | None -> default))

let contains value key =
  match (value.payload, key.payload) with
  | Record (_, fields, extensions), key -> (
      match nominal_field_name key with
      | Some field -> List.mem_assoc field fields || List.mem_assoc field extensions
      | None -> false)
  | Opaque (_, fields), key -> (
      match nominal_field_name key with
      | Some field -> List.mem_assoc field fields
      | None -> false)
  | Map entries, _ ->
      List.exists (fun (entry_key, _) -> equal key entry_key) entries
  | Set values, _ -> List.exists (equal key) values
  | Vector, Int index ->
      index >= 0L && index < Int64.of_int (Seq.length (to_seq value))
  | Array values, Int index ->
      index >= 0L && index < Int64.of_int (Array.length values)
  | _ -> false

let entries value =
  match value.payload with
  | Map entries -> entries
  | Record (_, fields, extensions) ->
      List.map (fun (key, project) -> (keyword key, project ())) fields
      @ List.map (fun (key, value) -> (keyword key, value)) extensions
  | _ -> invalid_arg "dynamic value is not a map"

let map_without_keys value keys =
  match value.payload with
  | Map entries ->
      entries
      |> List.filter (fun (key, _) ->
             not (List.exists (equal key) keys))
      |> map
  | Nil -> map []
  | _ -> invalid_arg "dynamic value is not a map"

let empty value =
  let emptied =
    match value.payload with
    | Nil -> nil
    | List -> list []
    | Vector -> vector Rrbvec.empty
    | Seq -> seq Seq.empty
    | Set _ -> set Seq.empty
    | Map _ -> map []
    | String _ -> string ""
    (* Emptyable supplies the empty representation for custom collections. *)
    | _ -> (
        match find_protocol_method value "Emptyable" "-empty" with
        | Some method_ -> method_ []
        | None -> invalid_arg "dynamic value is not a collection")
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
  | Int value -> value = 0L
  | Float value -> value = 0.
  | _ -> invalid_arg "zero? expects a numeric value"

let is_positive value =
  match value.payload with
  | Int value -> value > 0L
  | Float value -> value > 0.
  | _ -> invalid_arg "pos? expects a numeric value"

let is_negative value =
  match value.payload with
  | Int value -> value < 0L
  | Float value -> value < 0.
  | _ -> invalid_arg "neg? expects a numeric value"

let is_bool value = match value.payload with Bool _ -> true | _ -> false
let is_array value = match value.payload with Array _ -> true | _ -> false
let is_list value = match value.payload with List -> true | _ -> false
let is_vector value = match value.payload with Vector -> true | _ -> false
let is_seq value = match value.payload with List | Seq -> true | _ -> false
let is_map value =
  match value.payload with Map _ | Record _ -> true | _ -> false
let is_set value = match value.payload with Set _ -> true | _ -> false
let is_coll value =
  match value.payload with
  | List | Vector | Seq | Set _ | Map _ | Record _ -> true
  | _ -> false
let is_instance value type_name = value.type_name = Some type_name

let call value arguments =
  match value.payload with
  | Function function_ -> function_ arguments
  | Map _ | Record _ -> (
      match arguments with
      | [ key ] -> get value key
      | [ key; default ] -> get_default value key default
      | _ -> invalid_arg "dynamic map expects one or two arguments")
  | Set values -> (
      match arguments with
      | [ candidate ] ->
          List.find_opt (equal candidate) values |> Option.value ~default:nil
      | _ -> invalid_arg "dynamic set expects one argument")
  | _ -> invalid_arg "dynamic value is not callable"

let predicate_function name predicate =
  unary_function name (fun value -> bool (predicate value))

let is_true value =
  match value.payload with Bool true -> true | _ -> false

let is_false value =
  match value.payload with Bool false -> true | _ -> false

let is_some value = not (is_nil value)
let not_value value = not (truthy value)
let true_function = predicate_function "true?" is_true
let false_function = predicate_function "false?" is_false
let nil_function = predicate_function "nil?" is_nil
let some_function = predicate_function "some?" is_some
let bool_not value = not value
let not_function = predicate_function "not" not_value

let identical left right =
  left == right
  ||
  match (left.payload, right.payload) with
  | (Nil | Int _ | Float _ | Char _ | String _ | Symbol _ | Keyword _ | Bool _),
    (Nil | Int _ | Float _ | Char _ | String _ | Symbol _ | Keyword _ | Bool _) ->
      equal left right
  | _ -> (
      nominal_identity_equal left right)

let identical_function =
  binary_function "identical?" (fun left right -> bool (identical left right))

let identity_value value = value
let identity_function = unary_function "identity" identity_value

let complement_value predicate =
  function_ (fun arguments -> bool (not (truthy (call predicate arguments))))

let complement_function = unary_function "complement" complement_value

let strip_keyword_prefix value =
  if String.starts_with ~prefix:":" value then
    String.sub value 1 (String.length value - 1)
  else value

let identifier_value = function
  | { payload = String value | Symbol value | Keyword value; _ } -> value
  | _ -> invalid_arg "expected string, symbol, or keyword"

let named_identifier_value = function
  | { payload = Symbol value | Keyword value; _ } -> value
  | _ -> invalid_arg "expected symbol or keyword"

let keyword_value value =
  match value.payload with
  | Keyword _ -> value
  | _ -> keyword (":" ^ strip_keyword_prefix (identifier_value value))

let keyword_function =
  function_ (function
    | [ value ] -> keyword_value value
    | [ namespace; name ] ->
        keyword
          (":" ^ strip_keyword_prefix (identifier_value namespace) ^ "/"
         ^ strip_keyword_prefix (identifier_value name))
    | _ -> invalid_arg "keyword expects one or two arguments")

let identifier_name value =
  let identifier = strip_keyword_prefix (identifier_value value) in
  match String.rindex_opt identifier '/' with
  | Some index ->
      String.sub identifier (index + 1) (String.length identifier - index - 1)
  | None -> identifier

let identifier_namespace value =
  let identifier = strip_keyword_prefix (named_identifier_value value) in
  match String.rindex_opt identifier '/' with
  | Some index -> string (String.sub identifier 0 index)
  | None -> nil

let name_value value = string (identifier_name value)
let name_function = unary_function "name" name_value
let namespace_function = unary_function "namespace" identifier_namespace
let meta_function = unary_function "meta" metadata
let type_function = unary_function "type" class_

let vector_function =
  function_ (fun arguments -> vector (Rrbvec.of_list arguments))

let list_function = function_ list

let set_function =
  unary_function "set" (fun value -> set (to_seq value))

let map_from_arguments name arguments =
  let rec pairs entries = function
    | [] -> map (List.rev entries)
    | key :: value :: rest -> pairs ((key, value) :: entries) rest
    | [ _ ] -> invalid_arg (name ^ " expects an even number of arguments")
  in
  pairs [] arguments

let hash_map_function =
  function_ (map_from_arguments "hash-map")

let array_map_function =
  function_ (map_from_arguments "array-map")

let count_value value =
  match value.payload with
  | Nil -> 0
  | Array values -> Array.length values
  | Set values -> List.length values
  | Map entries -> List.length entries
  | _ -> Seq.length (to_seq value)

let count_function =
  unary_function "count" (fun value -> int (Int64.of_int (count_value value)))

let range_sequence start stop step =
  if step = 0L then invalid_arg "range step must not be zero";
  Seq.unfold
    (fun current ->
      if (step > 0L && current >= stop) || (step < 0L && current <= stop) then
        None
      else Some (int current, Int64.add current step))
    start

let range_function =
  function_ (function
    | [ { payload = Int stop; _ } ] -> seq (range_sequence 0L stop 1L)
    | [ { payload = Int start; _ }; { payload = Int stop; _ } ] ->
        seq (range_sequence start stop 1L)
    | [ { payload = Int start; _ }; { payload = Int stop; _ };
        { payload = Int step; _ } ] ->
        seq (range_sequence start stop step)
    | [ _ ] | [ _; _ ] | [ _; _; _ ] ->
        invalid_arg "range expects integer arguments"
    | _ -> invalid_arg "range expects one to three arguments")

let not_empty_function =
  unary_function "not-empty" (fun value ->
      if Seq.is_empty (to_seq value) then nil else value)

let empty_predicate_value value =
  match value.payload with Nil -> true | _ -> Seq.is_empty (to_seq value)

let empty_predicate_function =
  predicate_function "empty?" empty_predicate_value

let contains_function =
  binary_function "contains?" (fun value key -> bool (contains value key))

let str_value value = str value

let str_function =
  function_ (fun arguments ->
      string (String.concat "" (List.map str arguments)))

let subs_function =
  function_ (function
    | [ source; start ] ->
        let source =
          match source.payload with
          | String value -> value
          | _ -> invalid_arg "subs expects a string"
        in
        let start =
          match start.payload with
          | Int value -> Int64.to_int value
          | _ -> invalid_arg "subs expects integer indexes"
        in
        string (String.sub source start (String.length source - start))
    | [ source; start; stop ] ->
        let source =
          match source.payload with
          | String value -> value
          | _ -> invalid_arg "subs expects a string"
        in
        let integer = function
          | { payload = Int value; _ } -> Int64.to_int value
          | _ -> invalid_arg "subs expects integer indexes"
        in
        let start = integer start in
        let stop = integer stop in
        string (String.sub source start (stop - start))
    | _ -> invalid_arg "subs expects two or three arguments")

let get_function =
  function_ (function
    | [ value; key ] -> get value key
    | [ value; key; default ] -> get_default value key default
    | _ -> invalid_arg "get expects two or three arguments")

let joined_string ~pr arguments =
  arguments |> List.map (to_string ~pr) |> String.concat " "

let pr_str_function =
  function_ (fun arguments -> string (joined_string ~pr:true arguments))

let print_str_function =
  function_ (fun arguments -> string (joined_string ~pr:false arguments))

let println_str_function =
  function_ (fun arguments ->
      string (joined_string ~pr:false arguments ^ "\n"))

let prn_str_function =
  function_ (fun arguments ->
      string (joined_string ~pr:true arguments ^ "\n"))

let escape_function =
  binary_function "clojure.string/escape" (fun source replacements ->
      let source =
        match source.payload with
        | String source -> source
        | _ -> invalid_arg "clojure.string/escape expects a string"
      in
      let buffer = Buffer.create (String.length source) in
      String.iter
        (fun character ->
          let replacement = get replacements (char character) in
          if is_nil replacement then Buffer.add_char buffer character
          else Buffer.add_string buffer (str replacement))
        source;
      string (Buffer.contents buffer))

let dynamic_string_value = function
  | { payload = String value; _ } -> value
  | _ -> invalid_arg "expected a string"

let dynamic_string_unary name fn =
  unary_function name (fun value -> string (fn (dynamic_string_value value)))

let dynamic_string_predicate name fn =
  unary_function name (fun value -> bool (fn (dynamic_string_value value)))

let dynamic_string_binary name fn =
  binary_function name (fun left right ->
      string (fn (dynamic_string_value left) (dynamic_string_value right)))

let dynamic_string_binary_predicate name fn =
  binary_function name (fun left right ->
      bool (fn (dynamic_string_value left) (dynamic_string_value right)))

let string_blank_function =
  dynamic_string_predicate "clojure.string/blank?" Runtime_string.blank

let string_includes_function =
  dynamic_string_binary_predicate "clojure.string/includes?"
    Runtime_string.includes

let string_starts_with_function =
  dynamic_string_binary_predicate "clojure.string/starts-with?"
    Runtime_string.starts_with

let string_ends_with_function =
  dynamic_string_binary_predicate "clojure.string/ends-with?"
    Runtime_string.ends_with

let string_lower_case_function =
  dynamic_string_unary "clojure.string/lower-case" String.lowercase_ascii

let string_upper_case_function =
  dynamic_string_unary "clojure.string/upper-case" String.uppercase_ascii

let string_capitalize_function =
  dynamic_string_unary "clojure.string/capitalize" Runtime_string.capitalize

let string_join_function =
  function_ (function
    | [ values ] ->
        values |> to_seq |> List.of_seq
        |> List.map dynamic_string_value |> String.concat "" |> string
    | [ separator; values ] ->
        values |> to_seq |> List.of_seq
        |> List.map dynamic_string_value
        |> String.concat (dynamic_string_value separator)
        |> string
    | _ -> invalid_arg "clojure.string/join expects one or two arguments")

let string_index_of_function =
  binary_function "clojure.string/index-of" (fun source needle ->
      int
        (Int64.of_int
           (Runtime_string.index_of (dynamic_string_value source)
              (dynamic_string_value needle))))

let string_last_index_of_function =
  binary_function "clojure.string/last-index-of" (fun source needle ->
      int
        (Int64.of_int
           (Runtime_string.last_index_of (dynamic_string_value source)
              (dynamic_string_value needle))))

let dynamic_string_ternary name fn =
  function_ (function
    | [ first; second; third ] ->
        string
          (fn (dynamic_string_value first) (dynamic_string_value second)
             (dynamic_string_value third))
    | _ -> invalid_arg (name ^ " expects three arguments"))

let string_replace_function =
  dynamic_string_ternary "clojure.string/replace" Runtime_string.replace

let string_replace_first_function =
  dynamic_string_ternary "clojure.string/replace-first"
    Runtime_string.replace_first

let string_reverse_function =
  dynamic_string_unary "clojure.string/reverse" Runtime_string.reverse

let string_split_function =
  binary_function "clojure.string/split" (fun source separator ->
      Runtime_string.split (dynamic_string_value source)
        (dynamic_string_value separator)
      |> Rrbvec.map string |> vector)

let string_split_lines_function =
  unary_function "clojure.string/split-lines" (fun source ->
      Runtime_string.split_lines (dynamic_string_value source)
      |> Rrbvec.map string |> vector)

let string_trim_function =
  dynamic_string_unary "clojure.string/trim" String.trim

let string_trim_newline_function =
  dynamic_string_unary "clojure.string/trim-newline"
    Runtime_string.trim_newline

let string_triml_function =
  dynamic_string_unary "clojure.string/triml" Runtime_string.triml

let string_trimr_function =
  dynamic_string_unary "clojure.string/trimr" Runtime_string.trimr

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
  let hash_method =
    match value.payload with
    | Opaque _ -> find_protocol_method value "IHash" "-hash"
    | _ -> None
  in
  match hash_method with
  | Some hash_method -> (
      match (hash_method []).payload with
      | Int result -> Int64.to_int result
      | _ -> invalid_arg "IHash/-hash must return int")
  | None -> (
  match value.payload with
  | Nil -> 0
  | Bool true -> 1231
  | Bool false -> 1237
  | Int value -> Runtime_hash.hash_int64 value
  | Float value -> Runtime_hash.hash_float value
  | Char value -> Char.code value
  | String value -> Runtime_hash.hash_string value
  | Symbol value -> Runtime_hash.hash_symbol value
  | Keyword value -> Runtime_hash.hash_keyword value
  | Array _ | List | Vector | Seq ->
      value |> to_seq |> Seq.map hash |> Runtime_hash.hash_ordered
  | Set values ->
      values |> List.to_seq |> Seq.map hash |> Runtime_hash.hash_unordered
  | Map entries when Option.is_some value.type_name ->
      let name = Option.get value.type_name in
      let entry_hashes =
        entries |> expand_record_extension_entries |> List.to_seq
        |> Seq.map (fun (key, entry_value) ->
               [ hash key; hash entry_value ] |> List.to_seq
               |> Runtime_hash.hash_ordered)
      in
      Runtime_hash.hash_combine (Runtime_hash.hash_string name)
        (Runtime_hash.hash_unordered entry_hashes)
  | Map entries ->
      entries |> List.to_seq
      |> Seq.map (fun (key, value) ->
          [ hash key; hash value ] |> List.to_seq |> Runtime_hash.hash_ordered)
      |> Runtime_hash.hash_unordered
  | Record (name, fields, extensions) ->
      let field_hashes =
        List.map
          (fun (key, project) ->
            [ Runtime_hash.hash_keyword key; hash (project ()) ]
            |> List.to_seq |> Runtime_hash.hash_ordered)
          fields
      in
      let extension_hashes =
        List.map
          (fun (key, value) ->
            [ Runtime_hash.hash_keyword key; hash value ]
            |> List.to_seq |> Runtime_hash.hash_ordered)
          extensions
      in
      Runtime_hash.hash_combine (Runtime_hash.hash_string name)
        (Runtime_hash.hash_unordered
           (List.to_seq (field_hashes @ extension_hashes)))
  | Function _ -> 0
  | Reference _ -> 0
  | Opaque (name, _) -> Runtime_hash.hash_string name)

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
  Option.is_some (find_protocol value protocol_id)

let invoke value protocol_id method_name arguments =
  match find_protocol_method value protocol_id method_name with
  | None ->
      let receiver_type = Option.value value.type_name ~default:"<dynamic>" in
      invalid_arg
        ("missing protocol method " ^ protocol_id ^ "/" ^ method_name
       ^ " for " ^ receiver_type)
  | Some method_ -> method_ arguments

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
let dissoc_bang collection key = dissoc collection key

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

let invoke_function = call

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

let update target key update_fn arguments =
  assoc target key (invoke_function update_fn (get target key :: arguments))

let update_function =
  function_ (function
    | target :: key :: update_fn :: arguments ->
        update target key update_fn arguments
    | _ ->
        invalid_arg
          "update expects collection, key, function, and optional arguments")

let as_int value =
  match value.payload with
  | Int value -> value
  | _ -> invalid_arg "expected int"

let to_int value =
  match value.payload with
  | Int value -> value
  | Float value -> Int64.of_float value
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
