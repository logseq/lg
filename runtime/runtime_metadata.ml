let nil = Lg_edn_backend.Nil
let of_bool value = Lg_edn_backend.Bool value
let of_int value = Lg_edn_backend.Small_int value
let of_float value = Lg_edn_backend.Float value
let of_string value = Lg_edn_backend.String value
let of_char value = Lg_edn_backend.Char (Uchar.of_char value)
let of_symbol value = Lg_edn_backend.Symbol value
let of_regex value = Lg_edn_backend.Regex value

let of_keyword value =
  let value =
    if String.length value > 0 && value.[0] = ':' then
      String.sub value 1 (String.length value - 1)
    else value
  in
  Lg_edn_backend.Keyword value

let of_list convert values =
  Lg_edn_backend.List (Array.of_list (List.map convert values))

let of_seq convert values =
  Lg_edn_backend.List (Array.of_seq (Seq.map convert values))

let of_vector convert values =
  Lg_edn_backend.Vector (Array.map convert (Rrbvec.to_array values))

let of_array convert values =
  Lg_edn_backend.Vector (Array.map convert values)

let of_set convert values =
  Lg_edn_backend.Set (Array.of_list (List.map convert values))

let of_map convert_key convert_value values =
  values
  |> Runtime_map.to_list
  |> List.map (fun (key, value) ->
         (convert_key key, convert_value value))
  |> Array.of_list
  |> fun entries -> Lg_edn_backend.Map entries

let of_entries entries = Lg_edn_backend.Map (Array.of_list entries)

let bool_value = function
  | Lg_edn_backend.Bool value -> value
  | _ -> invalid_arg "expected boolean metadata"

let int_value = function
  | Lg_edn_backend.Small_int value -> value
  | _ -> invalid_arg "expected integer metadata"

let float_value = function
  | Lg_edn_backend.Float value -> value
  | _ -> invalid_arg "expected floating-point metadata"

let string_value = function
  | Lg_edn_backend.String value -> value
  | _ -> invalid_arg "expected string metadata"

let char_value = function
  | Lg_edn_backend.Char value -> Uchar.to_char value
  | _ -> invalid_arg "expected character metadata"

let symbol_value = function
  | Lg_edn_backend.Symbol value -> value
  | _ -> invalid_arg "expected symbol metadata"

let keyword_value = function
  | Lg_edn_backend.Keyword value -> ":" ^ value
  | _ -> invalid_arg "expected keyword metadata"
