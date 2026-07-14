type t = {
  payload : payload;
  sequence : (unit -> t Seq.t) option;
  sequential : bool;
  protocols : protocol list;
  metadata : t option;
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
  | Map of (t * t) list
  | Opaque of string

and protocol = {
  id : string;
  methods : (string * (t list -> t)) list;
}

let protocol id methods = { id; methods }

let make ?sequence ?(sequential = false) ?(protocols = []) ?metadata payload =
  { payload; sequence; sequential; protocols; metadata }

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
let opaque name protocols = make ~protocols (Opaque name)
let metadata value = Option.value value.metadata ~default:nil

let list values =
  make ~sequential:true ~sequence:(fun () -> List.to_seq values) List

let vector values =
  make ~sequential:true
    ~sequence:(fun () -> values |> Rrbvec.to_list |> List.to_seq)
    Vector

let seq values =
  let values = Seq.memoize values in
  make ~sequential:true ~sequence:(fun () -> values) Seq

let map entries =
  make
    ~sequence:(fun () ->
      entries
      |> List.to_seq
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
  | Function _, Function _ -> false
  | _ -> false

and to_seq value =
  match value.sequence with
  | Some sequence -> sequence ()
  | None -> invalid_arg "dynamic value is not seqable"

let assoc value key replacement =
  match value.payload with
  | Map entries ->
      let rec replace acc = function
        | [] -> List.rev ((key, replacement) :: acc)
        | (existing_key, _) :: rest when equal key existing_key ->
            List.rev_append acc ((key, replacement) :: rest)
        | entry :: rest -> replace (entry :: acc) rest
      in
      map (replace [] entries)
  | _ -> invalid_arg "dynamic value is not associative"

let get value key =
  match value.payload with
  | Map entries -> (
      match List.find_opt (fun (entry_key, _) -> equal key entry_key) entries with
      | Some (_, value) -> value
      | None -> nil)
  | _ -> nil

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
           (List.of_seq (to_seq target)) source)
  | Vector ->
      vector
        (Rrbvec.of_list
           (List.of_seq (Seq.append (to_seq target) source)))
  | Seq -> seq (Seq.append (to_seq target) source)
  | Map _ -> Seq.fold_left (fun map entry -> let key, value = pair entry in assoc map key value) target source
  | _ -> invalid_arg "dynamic into target is not a collection"

let is_sequential value = value.sequential
let is_seqable value = Option.is_some value.sequence
let truthy value = match value.payload with Nil | Bool false -> false | _ -> true
let is_symbol value = match value.payload with Symbol _ -> true | _ -> false
let is_keyword value = match value.payload with Keyword _ -> true | _ -> false
let is_string value = match value.payload with String _ -> true | _ -> false
let is_int value = match value.payload with Int _ -> true | _ -> false
let is_float value = match value.payload with Float _ -> true | _ -> false
let is_number value = match value.payload with Int _ | Float _ -> true | _ -> false
let is_bool value = match value.payload with Bool _ -> true | _ -> false
let is_list value = match value.payload with List -> true | _ -> false
let is_vector value = match value.payload with Vector -> true | _ -> false
let is_seq value = match value.payload with List | Seq -> true | _ -> false
let is_map value = match value.payload with Map _ -> true | _ -> false
let is_coll value = is_seqable value

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

let invoke_function value arguments =
  match value.payload with
  | Function function_ -> function_ arguments
  | _ -> invalid_arg "dynamic value is not a function"

let as_int value = match value.payload with Int value -> value | _ -> invalid_arg "expected int"
let as_float value = match value.payload with Float value -> value | _ -> invalid_arg "expected float"
let as_char value = match value.payload with Char value -> value | _ -> invalid_arg "expected char"
let as_string value =
  match value.payload with String value -> value | _ -> invalid_arg "expected string"

let as_symbol value =
  match value.payload with Symbol value -> value | _ -> invalid_arg "expected symbol"

let as_keyword value =
  match value.payload with Keyword value -> value | _ -> invalid_arg "expected keyword"

let as_bool value = match value.payload with Bool value -> value | _ -> invalid_arg "expected bool"

let as_identifier value =
  match value.payload with
  | String value | Symbol value | Keyword value -> value
  | _ -> invalid_arg "expected string, symbol, or keyword"
