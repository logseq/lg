type entity_ref =
  | Entity_id of int
  | Temp_id of string
  | Auto_tempid of int
  | Current_tx
  | Ident of string
  | Lookup_ref of string * t

and t =
  | Nil
  | Int of int
  | Wide_int of int64
  | Float of float
  | String of string
  | Symbol of string
  | Bool of bool
  | Keyword of string
  | Uuid of string
  | Instant of int
  | Regex of string
  | Ref of int
  | List of t list
  | Vector of t list
  | Map of (t * t) list
  | Set of t list
  | Tuple of t option list
  | Tx_ref
  | Ref_to of entity_ref

let quoted value = Printf.sprintf "%S" value

let float_to_edn_string value =
  if Float.is_nan value then "##NaN"
  else if Float.is_infinite value then
    if value > 0. then "##Inf" else "##-Inf"
  else
    let rendered = string_of_float value in
    if String.ends_with ~suffix:"." rendered then rendered ^ "0" else rendered

let float_to_javascript_string value =
  if Float.is_nan value then "NaN"
  else if Float.is_infinite value then
    if value > 0. then "Infinity" else "-Infinity"
  else
    let rendered = string_of_float value in
    if String.ends_with ~suffix:"." rendered then
      String.sub rendered 0 (String.length rendered - 1)
    else rendered

let render_sequence opening closing values =
  opening ^ String.concat " " values ^ closing

let rec to_edn_string = function
  | Nil -> "nil"
  | Int value -> string_of_int value
  | Wide_int value -> Int64.to_string value
  | Float value -> float_to_edn_string value
  | String value -> quoted value
  | Symbol value -> value
  | Bool value -> string_of_bool value
  | Keyword value -> value
  | Uuid value -> "#uuid " ^ quoted value
  | Instant value -> "#inst " ^ quoted (string_of_int value)
  | Regex value -> "#\"" ^ String.escaped value ^ "\""
  | Ref value -> string_of_int value
  | List values ->
      render_sequence "(" ")" (List.map to_edn_string values)
  | Vector values ->
      render_sequence "[" "]" (List.map to_edn_string values)
  | Map entries ->
      render_sequence
        "{" "}"
        (List.map
           (fun (key, value) ->
             to_edn_string key ^ " " ^ to_edn_string value)
           entries)
  | Set values ->
      render_sequence "#{" "}" (List.map to_edn_string values)
  | Tuple values ->
      render_sequence
        "[" "]"
        (List.map
           (function None -> "nil" | Some value -> to_edn_string value)
           values)
  | Tx_ref -> ":db/current-tx"
  | Ref_to entity_ref -> entity_ref_to_edn_string entity_ref

and entity_ref_to_edn_string = function
  | Entity_id value -> string_of_int value
  | Temp_id value -> quoted value
  | Auto_tempid value ->
      "#datascript/AutoTempid [" ^ string_of_int value ^ "]"
  | Current_tx -> ":db/current-tx"
  | Ident value -> value
  | Lookup_ref (attr, value) ->
      "[" ^ attr ^ " " ^ to_edn_string value ^ "]"

let to_clojure_string = function
  | Nil -> ""
  | String value | Symbol value | Keyword value | Uuid value -> value
  | Int value | Ref value | Instant value -> string_of_int value
  | Wide_int value -> Int64.to_string value
  | Float value -> float_to_javascript_string value
  | Bool value -> string_of_bool value
  | Regex value -> value
  | (List _ | Vector _ | Map _ | Set _ | Tuple _ | Tx_ref | Ref_to _) as value ->
      to_edn_string value

let tuple_of_vector values = Tuple (Rrbvec.to_list values)
let set_of_vector values = Set (Rrbvec.to_list values)
let vector_of_vector values = Vector (Rrbvec.to_list values)
let list_of_vector values = List (Rrbvec.to_list values)
let vector_of_vector_with convert values =
  Vector
    (Rrbvec.fold_right
       (fun value converted -> convert value :: converted)
       values [])

let is_nil = function Nil -> true | _ -> false

let add values =
  let total =
    Rrbvec.fold_left
      (fun total value ->
        match (total, value) with
        | None, _ -> None
        | Some (`Int left), Int right -> Some (`Int (left + right))
        | Some (`Int left), Wide_int right ->
            Some (`Wide (Int64.add (Int64.of_int left) right))
        | Some (`Int left), Float right ->
            Some (`Float (float_of_int left +. right))
        | Some (`Wide left), Int right ->
            Some (`Wide (Int64.add left (Int64.of_int right)))
        | Some (`Wide left), Wide_int right ->
            Some (`Wide (Int64.add left right))
        | Some (`Wide left), Float right ->
            Some (`Float (Int64.to_float left +. right))
        | Some (`Float left), Int right ->
            Some (`Float (left +. float_of_int right))
        | Some (`Float left), Wide_int right ->
            Some (`Float (left +. Int64.to_float right))
        | Some (`Float left), Float right -> Some (`Float (left +. right))
        | Some _, _ -> None)
      (Some (`Int 0)) values
  in
  match total with
  | None -> None
  | Some (`Int value) -> Some (Int value)
  | Some (`Wide value) -> Some (Wide_int value)
  | Some (`Float value) -> Some (Float value)

let subtract_pair left right =
  match (left, right) with
  | Int left, Int right -> Some (Int (left - right))
  | Int left, Wide_int right ->
      Some (Wide_int (Int64.sub (Int64.of_int left) right))
  | Wide_int left, Int right ->
      Some (Wide_int (Int64.sub left (Int64.of_int right)))
  | Wide_int left, Wide_int right -> Some (Wide_int (Int64.sub left right))
  | Int left, Float right -> Some (Float (float_of_int left -. right))
  | Wide_int left, Float right ->
      Some (Float (Int64.to_float left -. right))
  | Float left, Int right -> Some (Float (left -. float_of_int right))
  | Float left, Wide_int right ->
      Some (Float (left -. Int64.to_float right))
  | Float left, Float right -> Some (Float (left -. right))
  | _ -> None

let negate = function
  | Int value -> Some (Int (-value))
  | Wide_int value -> Some (Wide_int (Int64.neg value))
  | Float value -> Some (Float (-.value))
  | _ -> None

let subtract values =
  match Rrbvec.to_list values with
  | [] -> None
  | [ value ] -> negate value
  | first :: rest ->
      List.fold_left
        (fun result value -> Option.bind result (fun left -> subtract_pair left value))
        (Some first) rest

let multiply_pair left right =
  match (left, right) with
  | Int left, Int right -> Some (Int (left * right))
  | Int left, Wide_int right ->
      Some (Wide_int (Int64.mul (Int64.of_int left) right))
  | Wide_int left, Int right ->
      Some (Wide_int (Int64.mul left (Int64.of_int right)))
  | Wide_int left, Wide_int right -> Some (Wide_int (Int64.mul left right))
  | Int left, Float right -> Some (Float (float_of_int left *. right))
  | Wide_int left, Float right ->
      Some (Float (Int64.to_float left *. right))
  | Float left, Int right -> Some (Float (left *. float_of_int right))
  | Float left, Wide_int right ->
      Some (Float (left *. Int64.to_float right))
  | Float left, Float right -> Some (Float (left *. right))
  | _ -> None

let multiply values =
  Rrbvec.fold_left
    (fun result value ->
      Option.bind result (fun left -> multiply_pair left value))
    (Some (Int 1)) values

let numeric_float = function
  | Int value | Ref value -> Some (float_of_int value)
  | Wide_int value -> Some (Int64.to_float value)
  | Float value -> Some value
  | _ -> None

let divide values =
  match Rrbvec.to_list values with
  | [] -> Some (Float Float.nan)
  | first :: rest ->
      Option.bind (numeric_float first) (fun first_number ->
          match rest with
          | [] -> Some (Float (1.0 /. first_number))
          | _ ->
              List.fold_left
                (fun result value ->
                  Option.bind result (fun result ->
                      Option.map
                        (fun value -> result /. value)
                        (numeric_float value)))
                (Some first_number) rest
              |> Option.map (fun value -> Float value))

let integral_numeric_result value =
  if Float.is_finite value && Float.equal value (Float.trunc value) then
    let minimum = float_of_int min_int and maximum = float_of_int max_int in
    if value >= minimum && value < maximum then Int (int_of_float value)
    else Float value
  else Float value

let binary_numeric operation values =
  match Rrbvec.to_list values with
  | [ left; right ] ->
      Option.bind (numeric_float left) (fun left ->
          Option.map
            (fun right -> integral_numeric_result (operation left right))
            (numeric_float right))
  | _ -> None

let quotient values =
  binary_numeric
    (fun left right ->
      if Float.equal right 0.0 then Float.nan
      else Float.trunc (left /. right))
    values

let remainder values = binary_numeric Float.rem values

let modulo values =
  binary_numeric
    (fun left right ->
      let remainder = Float.rem left right in
      if
        Float.is_nan remainder
        || Float.equal remainder 0.0
        || (remainder > 0.0) = (right > 0.0)
      then remainder
      else remainder +. right)
    values

let numeric_extreme better values =
  match Rrbvec.to_list values with
  | [] -> Some Nil
  | first :: rest ->
      Option.bind (numeric_float first) (fun _ ->
          List.fold_left
            (fun result value ->
              Option.bind result (fun current ->
                  Option.bind (numeric_float current) (fun current_number ->
                      Option.map
                        (fun value_number ->
                          if better value_number current_number then value
                          else current)
                        (numeric_float value))))
            (Some first) rest)

let maximum values = numeric_extreme ( > ) values
let minimum values = numeric_extreme ( < ) values

let int_argument = function Int value | Ref value -> Some value | _ -> None

let range_value values =
  let build start finish step =
    if step = 0 then None
    else
      let before_finish value = if step > 0 then value < finish else value > finish in
      let rec loop value result =
        if before_finish value then loop (value + step) (Int value :: result)
        else Some (List (List.rev result))
      in
      loop start []
  in
  match Rrbvec.to_list values with
  | [ finish ] -> Option.bind (int_argument finish) (fun finish -> build 0 finish 1)
  | [ start; finish ] ->
      Option.bind (int_argument start) (fun start ->
          Option.bind (int_argument finish) (fun finish -> build start finish 1))
  | [ start; finish; step ] ->
      Option.bind (int_argument start) (fun start ->
          Option.bind (int_argument finish) (fun finish ->
              Option.bind (int_argument step) (fun step ->
                  build start finish step)))
  | _ -> None

let string_value values =
  Some
    (String
       (Rrbvec.fold_left
          (fun result value -> result ^ to_clojure_string value)
          "" values))

let substring values =
  let clamp length index = max 0 (min length index) in
  let extract source start finish =
    let length = String.length source in
    let start = clamp length start and finish = clamp length finish in
    let start, finish = if start <= finish then (start, finish) else (finish, start) in
    Some (String (String.sub source start (finish - start)))
  in
  match Rrbvec.to_list values with
  | [ String source; start ] ->
      Option.bind (int_argument start) (fun start ->
          extract source start (String.length source))
  | [ String source; start; finish ] ->
      Option.bind (int_argument start) (fun start ->
          Option.bind (int_argument finish) (fun finish ->
              extract source start finish))
  | _ -> None

let increment = function
  | Int value -> Some (Int (value + 1))
  | Wide_int value -> Some (Wide_int (Int64.succ value))
  | Float value -> Some (Float (value +. 1.0))
  | _ -> None

let decrement = function
  | Int value -> Some (Int (value - 1))
  | Wide_int value -> Some (Wide_int (Int64.pred value))
  | Float value -> Some (Float (value -. 1.0))
  | _ -> None

let map_of_keyword_map values =
  Map
    (Lg_runtime.Runtime_map.to_list values
    |> List.map (fun (key, value) -> (Keyword key, value)))

let map_of_keyword_entries entries =
  Map
    (Rrbvec.to_list entries
    |> List.map (fun (key, value) -> (Keyword key, value)))

let map_of_keyword_map_with convert values =
  Map
    (Lg_runtime.Runtime_map.to_list values
    |> List.map (fun (key, value) -> (Keyword key, convert value)))

let map_of_data_map values = Map (Lg_runtime.Runtime_map.to_list values)

let map_of_data_map_with convert values =
  Map
    (Lg_runtime.Runtime_map.to_list values
    |> List.map (fun (key, value) -> (key, convert value)))

let regex_pattern = function
  | String pattern ->
      let _ = Lg_edn_backend.regex_valid pattern in
      Some (Regex pattern)
  | _ -> None

let regex_find pattern source =
  match (pattern, source) with
  | Regex pattern, String source ->
      Some (Lg_edn_backend.regex_find pattern source)
  | _ -> None

let javascript_whitespace =
  [
    " ";
    "\t";
    "\n";
    "\011";
    "\012";
    "\r";
    "\194\160";
    "\225\154\128";
    "\226\128\128";
    "\226\128\129";
    "\226\128\130";
    "\226\128\131";
    "\226\128\132";
    "\226\128\133";
    "\226\128\134";
    "\226\128\135";
    "\226\128\136";
    "\226\128\137";
    "\226\128\138";
    "\226\128\168";
    "\226\128\169";
    "\226\128\175";
    "\226\129\159";
    "\227\128\128";
    "\239\187\191";
  ]

let whitespace_length_at source index =
  List.find_map
    (fun whitespace ->
      let length = String.length whitespace in
      if
        index + length <= String.length source
        && String.equal (String.sub source index length) whitespace
      then Some length
      else None)
    javascript_whitespace

let string_blank = function
  | Nil -> true
  | String source ->
      let rec loop index =
        if index = String.length source then true
        else
          match whitespace_length_at source index with
          | Some length -> loop (index + length)
          | None -> false
      in
      loop 0
  | _ -> false

let query_search_text = function
  | Nil -> Some "null"
  | String value | Symbol value | Keyword value | Uuid value -> Some value
  | Int value | Ref value -> Some (string_of_int value)
  | Wide_int value -> Some (Int64.to_string value)
  | Float value -> Some (float_to_javascript_string value)
  | Bool value -> Some (string_of_bool value)
  | Regex value -> Some ("/" ^ value ^ "/")
  | (List _ | Vector _ | Map _ | Set _ | Tuple _ | Tx_ref | Ref_to _) as value ->
      Some (to_edn_string value)
  | Instant _ -> None

let query_search_text_or_undefined = function
  | None -> Some "undefined"
  | Some value -> query_search_text value

let string_includes source needle =
  match source with
  | String source ->
      Option.map
        (fun needle -> Lg_runtime.Runtime_string.index_of source needle >= 0)
        (query_search_text_or_undefined needle)
  | _ -> None

let string_starts_with source prefix =
  match source with
  | String source ->
      Option.map
        (fun prefix -> String.starts_with ~prefix source)
        (query_search_text_or_undefined prefix)
  | _ -> None

let string_ends_with source suffix =
  match (source, suffix) with
  | String source, Some (String suffix) ->
      Some (String.ends_with ~suffix source)
  | _, None | _, Some Nil -> None
  | String _, Some _ -> Some false
  | Nil, Some _ -> None
  | _, Some _ -> Some false

let unary_string operation values =
  match Rrbvec.to_list values with
  | [ String source ] -> Some (String (operation source))
  | _ -> None

let string_lower_case values = unary_string String.lowercase_ascii values
let string_upper_case values = unary_string String.uppercase_ascii values
let string_capitalize values = unary_string Lg_runtime.Runtime_string.capitalize values
let string_reverse values = unary_string Lg_runtime.Runtime_string.reverse values
let string_trim values = unary_string String.trim values
let string_trim_newline values =
  unary_string Lg_runtime.Runtime_string.trim_newline values

let string_trim_left values = unary_string Lg_runtime.Runtime_string.triml values
let string_trim_right values = unary_string Lg_runtime.Runtime_string.trimr values

let join_items = function
  | Nil -> Some []
  | List values | Vector values | Set values -> Some values
  | Tuple values ->
      Some (List.map (function None -> Nil | Some value -> value) values)
  | Map entries ->
      Some (List.map (fun (key, value) -> Vector [ key; value ]) entries)
  | _ -> None

let string_join values =
  let join separator collection =
    Option.map
      (fun values ->
        String
          (String.concat separator
             (List.map
                (function Nil -> "" | value -> to_clojure_string value)
                values)))
      (join_items collection)
  in
  match Rrbvec.to_list values with
  | [ collection ] -> join "" collection
  | [ separator; collection ] ->
      let separator =
        match separator with Nil -> "null" | value -> to_clojure_string value
      in
      join separator collection
  | _ -> None

let index_result index = Some (if index < 0 then Nil else Int index)

let string_index_of values =
  match Rrbvec.to_list values with
  | [ String source; String needle ] ->
      index_result (Lg_runtime.Runtime_string.index_of source needle)
  | [ String source; String needle; start ] ->
      Option.bind (int_argument start) (fun start ->
          index_result
            (Lg_runtime.Runtime_string.index_of_from source needle start))
  | _ -> None

let string_last_index_of values =
  match Rrbvec.to_list values with
  | [ String source; String needle ] ->
      index_result (Lg_runtime.Runtime_string.last_index_of source needle)
  | [ String source; String needle; start ] ->
      Option.bind (int_argument start) (fun start ->
          index_result
            (Lg_runtime.Runtime_string.last_index_of_from source needle start))
  | _ -> None

let string_split_lines values =
  match Rrbvec.to_list values with
  | [ String source ] ->
      Some
        (Vector
           (Lg_runtime.Runtime_string.split_lines source |> Rrbvec.to_list
          |> List.map (fun value -> String value)))
  | _ -> None

let string_replace all values =
  match Rrbvec.to_list values with
  | [ String source; String matched; String replacement ] ->
      Some
        (String
           ((if all then Lg_runtime.Runtime_string.replace
             else Lg_runtime.Runtime_string.replace_first)
              source matched replacement))
  | [ String source; Regex pattern; String replacement ] ->
      Some
        (String
           (Lg_edn_backend.regex_replace ~all ~pattern ~replacement source))
  | _ -> None

let string_split values =
  let split source pattern limit =
    Some
      (Vector
         (Lg_edn_backend.regex_split ~pattern ~limit source |> Array.to_list
        |> List.map (function None -> Nil | Some value -> String value)))
  in
  match Rrbvec.to_list values with
  | [ String source; Regex pattern ] -> split source pattern None
  | [ String source; Regex pattern; limit ] ->
      Option.bind (int_argument limit) (fun limit ->
          split source pattern (Some limit))
  | _ -> None

let keyword_map_get key = function
  | Map entries ->
      List.find_map
        (function
          | Keyword candidate, value when String.equal candidate key -> Some value
          | _ -> None)
        entries
  | _ -> None

let keyword_map_value = function
  | Map entries ->
      List.fold_left
        (fun result (key, value) ->
          match (result, key) with
          | Some values, Keyword key ->
              Some (Lg_runtime.Runtime_map.assoc values key value)
          | _ -> None)
        (Some Lg_runtime.Runtime_map.empty)
        entries
  | _ -> None

let keyword_map_entries = function
  | Map entries ->
      let rec collect result = function
        | [] -> Some (Rrbvec.of_list (List.rev result))
        | (Keyword key, value) :: rest ->
            collect ((key, value) :: result) rest
        | _ :: _ -> None
      in
      collect [] entries
  | _ -> None

let map_entries = function
  | Map entries -> Some (Rrbvec.of_list entries)
  | _ -> None

let string_vector values =
  Vector (values |> Rrbvec.to_list |> List.map (fun value -> String value))

let temp_id_vector values =
  Vector
    (values |> Rrbvec.to_list
    |> List.map (fun value -> Ref_to (Temp_id value)))

let tuple_items = function
  | Tuple values -> Some (Rrbvec.of_list values)
  | _ -> None

let keyword_value = function
  | Keyword value -> Some value
  | _ -> None

let keyword_text value =
  if String.length value > 0 && value.[0] = ':' then
    String.sub value 1 (String.length value - 1)
  else value

let named_text = function
  | Keyword value -> Some (keyword_text value)
  | Symbol value -> Some value
  | _ -> None

let name_from_text value =
  match String.index_opt value '/' with
  | Some index ->
      String.sub value (index + 1) (String.length value - index - 1)
  | None -> value

let keyword_from_values values =
  match Rrbvec.to_list values with
  | [ Keyword _ as value ] -> Some value
  | [ String value ] | [ Symbol value ] -> Some (Keyword (":" ^ value))
  | [ Nil ] -> Some Nil
  | [ _ ] -> Some Nil
  | [ String namespace; String name ] ->
      Some (Keyword (":" ^ namespace ^ "/" ^ name))
  | [ Nil; String name ] -> Some (Keyword (":" ^ name))
  | [ _; _ ] -> None
  | _ -> None

let name_value = function
  | String value -> Some (String value)
  | (Keyword _ | Symbol _) as value ->
      Option.map (fun text -> String (name_from_text text)) (named_text value)
  | _ -> None

let namespace_value value =
  Option.map
    (fun text ->
      match String.index_opt text '/' with
      | Some index -> String (String.sub text 0 index)
      | None -> Nil)
    (named_text value)

let bool_value = function Bool value -> Some value | _ -> None

let sequential_items = function
  | List values | Vector values -> Some (Rrbvec.of_list values)
  | _ -> None

let set_items = function
  | Set values -> Some (Rrbvec.of_list values)
  | _ -> None

let count_value = function
  | Nil -> Some 0
  | String value -> Some (String.length value)
  | List values | Vector values | Set values -> Some (List.length values)
  | Map entries -> Some (List.length entries)
  | Tuple values -> Some (List.length values)
  | _ -> None

let entity_ref_value = function
  | Ref_to entity_ref -> Some entity_ref
  | Int eid | Ref eid -> Some (Entity_id eid)
  | _ -> None

let lookup_ref_value = function
  | List [ Keyword attr; value ] | Vector [ Keyword attr; value ] ->
      Some (attr, value)
  | _ -> None

let ref_value = function Ref eid -> Some eid | _ -> None

let tuple_parts attrs ref_attrs value =
  match value with
  | Tuple items ->
      let attrs = Rrbvec.to_list attrs in
      let ref_attrs = Rrbvec.to_list ref_attrs in
      if List.length attrs <> List.length items then
        invalid_arg "tuple value has the wrong arity"
      else (attrs, ref_attrs, items)
  | _ -> invalid_arg "expected a DataScript tuple value"

let tuple_entity_refs attrs ref_attrs value =
  let attrs, ref_attrs, items = tuple_parts attrs ref_attrs value in
  List.fold_left2
    (fun refs attr item ->
      if List.mem attr ref_attrs then
        match item with
        | None -> refs
        | Some value -> (
            match entity_ref_value value with
            | Some entity_ref -> entity_ref :: refs
            | None -> (
                match lookup_ref_value value with
                | Some (attr, lookup_value) ->
                    Lookup_ref (attr, lookup_value) :: refs
                | None ->
                    invalid_arg "tuple ref item must be an entity reference"))
      else refs)
    [] attrs items
  |> List.rev |> Rrbvec.of_list

let resolve_tuple_refs attrs ref_attrs eids value =
  let attrs, ref_attrs, items = tuple_parts attrs ref_attrs value in
  let eids = ref (Rrbvec.to_list eids) in
  let resolve_item attr item =
    if List.mem attr ref_attrs then
      match (!eids, item) with
      | eid :: rest, Some _ ->
          eids := rest;
          Some (Ref eid)
      | [], Some _ -> invalid_arg "tuple ref resolution is missing an entity id"
      | _, None -> None
    else item
  in
  let resolved = Tuple (List.map2 resolve_item attrs items) in
  if !eids <> [] then invalid_arg "tuple ref resolution has extra entity ids";
  resolved

let keyword_items value =
  let values =
    match value with
    | List values | Vector values -> Some values
    | _ -> None
  in
  Option.bind values (fun values ->
      let rec collect keywords = function
        | [] -> Some (Rrbvec.of_list (List.rev keywords))
        | Keyword value :: rest -> collect (value :: keywords) rest
        | _ -> None
      in
      collect [] values)

let rec list_equal equal left right =
  match (left, right) with
  | [], [] -> true
  | left :: left_rest, right :: right_rest ->
      equal left right && list_equal equal left_rest right_rest
  | [], _ | _, [] -> false

let option_equal equal left right =
  match (left, right) with
  | None, None -> true
  | Some left, Some right -> equal left right
  | None, Some _ | Some _, None -> false

let sequence = function
  | List values | Vector values ->
      Some (List.map (fun value -> Some value) values)
  | Tuple values -> Some values
  | _ -> None

let rec equal left right =
  match (left, right) with
  | Nil, Nil | Tx_ref, Tx_ref -> true
  | Int left, Int right | Ref left, Ref right | Instant left, Instant right ->
      left = right
  | Int left, Ref right | Ref left, Int right -> left = right
  | Wide_int left, Wide_int right -> Int64.equal left right
  | (Int left | Ref left), Wide_int right ->
      Int64.equal (Int64.of_int left) right
  | Wide_int left, (Int right | Ref right) ->
      Int64.equal left (Int64.of_int right)
  | Float left, Float right -> Float.equal left right
  | Int left, Float right | Ref left, Float right ->
      Float.equal (float_of_int left) right
  | Float left, Int right | Float left, Ref right ->
      Float.equal left (float_of_int right)
  | Wide_int left, Float right -> Float.equal (Int64.to_float left) right
  | Float left, Wide_int right -> Float.equal left (Int64.to_float right)
  | String left, String right
  | Symbol left, Symbol right
  | Keyword left, Keyword right
  | Uuid left, Uuid right
  | Regex left, Regex right ->
      String.equal left right
  | Bool left, Bool right -> Bool.equal left right
  | Map left, Map right -> map_equal left right
  | Set left, Set right -> set_equal left right
  | Ref_to left, Ref_to right -> entity_ref_equal left right
  | _ -> (
      match (sequence left, sequence right) with
      | Some left, Some right -> list_equal (option_equal equal) left right
      | _ -> false)

and map_equal left right =
  List.length left = List.length right
  && List.for_all
       (fun (left_key, left_value) ->
         List.exists
           (fun (right_key, right_value) ->
             equal left_key right_key && equal left_value right_value)
           right)
       left

and set_equal left right =
  List.length left = List.length right
  && List.for_all
       (fun left_value -> List.exists (equal left_value) right)
       left

and entity_ref_equal left right =
  match (left, right) with
  | Entity_id left, Entity_id right -> left = right
  | Auto_tempid left, Auto_tempid right -> left = right
  | Temp_id left, Temp_id right | Ident left, Ident right -> String.equal left right
  | Current_tx, Current_tx -> true
  | Lookup_ref (left_attr, left_value), Lookup_ref (right_attr, right_value) ->
      String.equal left_attr right_attr && equal left_value right_value
  | _ -> false

let set_value = function
  | Nil -> Some (Set [])
  | Set values -> Some (Set values)
  | List values | Vector values ->
      Some
        (Set
           (List.fold_left
              (fun unique value ->
                if List.exists (equal value) unique then unique
                else unique @ [ value ])
              [] values))
  | Tuple values ->
      Some
        (Set
           (List.fold_left
              (fun unique value ->
                let value = Option.value ~default:Nil value in
                if List.exists (equal value) unique then unique
                else unique @ [ value ])
              [] values))
  | _ -> None

let index_in_bounds length = function
  | Int index | Ref index -> index >= 0 && index < length
  | Wide_int index ->
      Int64.compare index 0L >= 0
      && Int64.compare index (Int64.of_int length) < 0
  | _ -> false

let contains_key collection key =
  match collection with
  | Nil -> Some false
  | String value -> Some (index_in_bounds (String.length value) key)
  | Vector values -> Some (index_in_bounds (List.length values) key)
  | Tuple values -> Some (index_in_bounds (List.length values) key)
  | Map entries ->
      Some (List.exists (fun (candidate, _) -> equal candidate key) entries)
  | Set values -> Some (List.exists (equal key) values)
  | List _ -> None
  | _ -> None

let get_or_default collection key default =
  let indexed values =
    match key with
    | Int index | Ref index when index >= 0 ->
        Option.value ~default (List.nth_opt values index)
    | _ -> default
  in
  match collection with
  | Nil -> Some default
  | Map entries ->
      Some
        (Option.value ~default
           (List.find_map
              (fun (candidate, value) ->
                if equal candidate key then Some value else None)
              entries))
  | Vector values -> Some (indexed values)
  | Tuple values ->
      let values = List.map (Option.value ~default:Nil) values in
      Some (indexed values)
  | Set values ->
      Some
        (Option.value ~default
           (List.find_opt (fun candidate -> equal candidate key) values))
  | List _ | String _ | Symbol _ | Bool _ | Keyword _ | Uuid _ | Instant _
  | Regex _ | Int _ | Wide_int _ | Float _ | Ref _ | Tx_ref | Ref_to _ ->
      Some default

let map_get map key = get_or_default map key Nil

let combine_hash seed value = (seed * 33) lxor value

let ordered_hash values =
  List.fold_left (fun result value -> combine_hash result value) 1 values

let unordered_hash values =
  List.fold_left (fun result value -> result + (value lxor (value lsl 16))) 0 values

let rec hash = function
  | Nil -> 0
  | Int value | Ref value -> Hashtbl.hash (float_of_int value)
  | Wide_int value -> Hashtbl.hash (Int64.to_float value)
  | Float value -> Hashtbl.hash value
  | String value -> Hashtbl.hash (0, value)
  | Symbol value -> Hashtbl.hash (1, value)
  | Bool value -> Hashtbl.hash (2, value)
  | Keyword value -> Hashtbl.hash (3, value)
  | Uuid value -> Hashtbl.hash (4, value)
  | Instant value -> Hashtbl.hash (5, value)
  | Regex value -> Hashtbl.hash (6, value)
  | List values | Vector values -> ordered_hash (List.map hash values)
  | Tuple values ->
      ordered_hash
        (List.map (function None -> 0 | Some value -> hash value) values)
  | Map entries ->
      unordered_hash
        (List.map
           (fun (key, value) -> ordered_hash [ hash key; hash value ])
           entries)
  | Set values -> unordered_hash (List.map hash values)
  | Tx_ref -> Hashtbl.hash 7
  | Ref_to entity_ref -> Hashtbl.hash (8, hash_entity_ref entity_ref)

and hash_entity_ref = function
  | Entity_id value -> Hashtbl.hash (0, value)
  | Temp_id value -> Hashtbl.hash (1, value)
  | Auto_tempid value -> Hashtbl.hash (2, value)
  | Current_tx -> Hashtbl.hash 3
  | Ident value -> Hashtbl.hash (4, value)
  | Lookup_ref (attr, value) -> Hashtbl.hash (5, attr, hash value)

let identifier_offset value =
  if
    String.length value > 0
    && (String.unsafe_get value 0 = ':' || String.unsafe_get value 0 = '\'')
  then 1
  else 0

let compare_string_slice left left_start left_length right right_start
    right_length =
  let shared_length = min left_length right_length in
  let rec loop index =
    if index = shared_length then Int.compare left_length right_length
    else
      let compared =
        Char.compare
          (String.unsafe_get left (left_start + index))
          (String.unsafe_get right (right_start + index))
      in
      if compared = 0 then loop (index + 1) else compared
  in
  loop 0

let compare_identifier left right =
  if String.equal left right then 0
  else
    let left_offset = identifier_offset left in
    let right_offset = identifier_offset right in
    let left_separator =
      String.index_from_opt left left_offset '/'
    in
    let right_separator =
      String.index_from_opt right right_offset '/'
    in
    let left_namespace_length =
      match left_separator with
      | None -> 0
      | Some separator -> separator - left_offset
    in
    let right_namespace_length =
      match right_separator with
      | None -> 0
      | Some separator -> separator - right_offset
    in
    let namespace =
      compare_string_slice left left_offset left_namespace_length right
        right_offset right_namespace_length
    in
    if namespace <> 0 then namespace
    else
      let left_name_start =
        match left_separator with
        | None -> left_offset
        | Some separator -> separator + 1
      in
      let right_name_start =
        match right_separator with
        | None -> right_offset
        | Some separator -> separator + 1
      in
      compare_string_slice left left_name_start
        (String.length left - left_name_start)
        right right_name_start
        (String.length right - right_name_start)

let rec compare_list compare left right =
  let length = Int.compare (List.length left) (List.length right) in
  if length <> 0 then length
  else
    match (left, right) with
    | [], [] -> 0
    | left :: left_rest, right :: right_rest ->
        let current = compare left right in
        if current <> 0 then current else compare_list compare left_rest right_rest
    | [], _ | _, [] -> assert false

let compare_option left right =
  match (left, right) with
  | None, None -> 0
  | None, Some _ -> -1
  | Some _, None -> 1
  | Some left, Some right -> compare left right

let rank = function
  | Nil -> 0
  | Keyword _ -> 1
  | Symbol _ -> 2
  | Map _ -> 3
  | Set _ -> 4
  | List _ | Vector _ | Tuple _ -> 5
  | Bool _ -> 6
  | Int _ | Wide_int _ | Float _ | Ref _ -> 7
  | String _ -> 8
  | Regex _ -> 9
  | Instant _ -> 10
  | Uuid _ -> 11
  | Tx_ref -> 12
  | Ref_to _ -> 13

let compare left right =
  match (left, right) with
  | Int left, Int right | Ref left, Ref right | Instant left, Instant right ->
      Int.compare left right
  | Int left, Ref right | Ref left, Int right -> Int.compare left right
  | Wide_int left, Wide_int right -> Int64.compare left right
  | (Int left | Ref left), Wide_int right ->
      Int64.compare (Int64.of_int left) right
  | Wide_int left, (Int right | Ref right) ->
      Int64.compare left (Int64.of_int right)
  | Float left, Float right -> Float.compare left right
  | Int left, Float right | Ref left, Float right ->
      Float.compare (float_of_int left) right
  | Float left, Int right | Float left, Ref right ->
      Float.compare left (float_of_int right)
  | Wide_int left, Float right -> Float.compare (Int64.to_float left) right
  | Float left, Wide_int right -> Float.compare left (Int64.to_float right)
  | String left, String right
  | Uuid left, Uuid right
  | Regex left, Regex right ->
      String.compare left right
  | Symbol left, Symbol right | Keyword left, Keyword right ->
      compare_identifier left right
  | Bool left, Bool right -> Bool.compare left right
  | Map _, Map _ | Set _, Set _ -> Int.compare (hash left) (hash right)
  | Ref_to left, Ref_to right -> Stdlib.compare left right
  | _ -> (
      match (sequence left, sequence right) with
      | Some left, Some right -> compare_list compare_option left right
      | _ -> Int.compare (rank left) (rank right))

let numeric_float = function
  | Int value | Ref value -> Some (float_of_int value)
  | Wide_int value -> Some (Int64.to_float value)
  | Float value -> Some value
  | _ -> None

let default_number_compare left right =
  if left > right then 1 else if left < right then -1 else 0

let comparable_vector_items = function
  | Vector values -> Some (List.map Option.some values)
  | Tuple values -> Some values
  | _ -> None

let rec compare_query_values left right =
  if left == right then Some 0
  else
    match (left, right) with
    | Nil, _ -> Some (-1)
    | _, Nil -> Some 1
    | (Int _ | Wide_int _ | Float _ | Ref _),
      (Int _ | Wide_int _ | Float _ | Ref _) ->
        Option.bind (numeric_float left) (fun left ->
            Option.map
              (fun right -> default_number_compare left right)
              (numeric_float right))
    | String left, String right | Uuid left, Uuid right ->
        Some (String.compare left right)
    | Keyword left, Keyword right | Symbol left, Symbol right ->
        Some (compare_identifier left right)
    | Bool left, Bool right -> Some (Bool.compare left right)
    | Instant left, Instant right -> Some (Int.compare left right)
    | (Vector _ | Tuple _), (Vector _ | Tuple _) ->
        Option.bind (comparable_vector_items left) (fun left ->
            Option.bind (comparable_vector_items right) (fun right ->
                compare_query_vectors left right))
    | _ -> None

and compare_query_vectors left right =
  let length = Int.compare (List.length left) (List.length right) in
  if length <> 0 then Some length
  else
    match (left, right) with
    | [], [] -> Some 0
    | left :: left_rest, right :: right_rest ->
        Option.bind
          (match (left, right) with
          | None, None -> Some 0
          | None, Some _ -> Some (-1)
          | Some _, None -> Some 1
          | Some left, Some right -> compare_query_values left right)
          (fun current ->
            if current <> 0 then Some current
            else compare_query_vectors left_rest right_rest)
    | [], _ | _, [] -> assert false
