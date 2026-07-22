let tag_parsers = Hashtbl.create 16
let data_readers = ref (Runtime_dynamic.map [])

let dynamic_function function_ =
  Runtime_dynamic.function_ (function
    | [ value ] -> function_ value
    | _ -> invalid_arg "tag parser expects one argument")

let register_tag_parser tag function_ =
  let previous = Hashtbl.find_opt tag_parsers tag in
  Hashtbl.replace tag_parsers tag function_;
  Option.fold ~none:Runtime_dynamic.nil ~some:dynamic_function previous

let keyword_name value =
  if String.starts_with ~prefix:":" value then
    String.sub value 1 (String.length value - 1)
  else value

let rec of_dynamic value =
  let open Runtime_dynamic in
  match value.payload with
  | Nil -> Lg_edn_backend.Nil
  | Bool value -> Lg_edn_backend.Bool value
  | String value -> Lg_edn_backend.String value
  | Char value -> Lg_edn_backend.Char (Uchar.of_char value)
  | Symbol value -> Lg_edn_backend.Symbol value
  | Keyword value -> Lg_edn_backend.Keyword (keyword_name value)
  | Int value -> Lg_edn_backend.Int value
  | Float value -> Lg_edn_backend.Float value
  | Regex value -> Lg_edn_backend.Regex value
  | Array values ->
      Lg_edn_backend.Vector (Array.map of_dynamic values)
  | List ->
      Lg_edn_backend.List
        (value |> to_seq |> Seq.map of_dynamic |> Array.of_seq)
  | Vector _ ->
      Lg_edn_backend.Vector
        (value |> to_seq |> Seq.map of_dynamic |> Array.of_seq)
  | Seq ->
      Lg_edn_backend.List
        (value |> to_seq |> Seq.map of_dynamic |> Array.of_seq)
  | Set set ->
      Lg_edn_backend.Set (set.values |> List.map of_dynamic |> Array.of_list)
  | Map map ->
      Lg_edn_backend.Map
        (Runtime_dynamic.map_entries map
        |> List.map (fun (key, value) -> (of_dynamic key, of_dynamic value))
        |> Array.of_list)
  | Record (_, fields, extensions) ->
      let fields =
        List.map
          (fun (key, project) ->
            (Lg_edn_backend.Keyword (keyword_name key), of_dynamic (project ())))
          fields
      in
      let extensions =
        List.map
          (fun (key, value) ->
            (Lg_edn_backend.Keyword (keyword_name key), of_dynamic value))
          extensions
      in
      Lg_edn_backend.Map (Array.of_list (fields @ extensions))
  | Function _ | Reference _ | Opaque _ ->
      invalid_arg "value cannot be represented as EDN"

let int64_of_number kind value =
  match Int64.of_string_opt value with
  | Some value -> Runtime_dynamic.int value
  | None -> invalid_arg (kind ^ " is outside CljML's int64 range")

type readers = {
  readers : (string * (Runtime_dynamic.t -> Runtime_dynamic.t)) list;
  default : (string -> Runtime_dynamic.t -> Runtime_dynamic.t) option;
}

let no_readers = { readers = []; default = None }

let rec to_dynamic_with readers value =
  match value with
  | Lg_edn_backend.Nil -> Runtime_dynamic.nil
  | Lg_edn_backend.Bool value -> Runtime_dynamic.bool value
  | Lg_edn_backend.String value -> Runtime_dynamic.string value
  | Lg_edn_backend.Char value -> (
      match Uchar.to_char value with
      | value -> Runtime_dynamic.char value
      | exception Invalid_argument _ ->
          invalid_arg "CljML characters currently require a single byte")
  | Lg_edn_backend.Symbol value -> Runtime_dynamic.symbol value
  | Lg_edn_backend.Keyword value -> Runtime_dynamic.keyword (":" ^ value)
  | Lg_edn_backend.Int value -> Runtime_dynamic.int value
  | Lg_edn_backend.Bigint value -> int64_of_number "EDN bigint" value
  | Lg_edn_backend.Float value -> Runtime_dynamic.float value
  | Lg_edn_backend.Decimal value -> (
      match float_of_string_opt value with
      | Some value -> Runtime_dynamic.float value
      | None -> invalid_arg "invalid EDN decimal")
  | Lg_edn_backend.Ratio _ ->
      invalid_arg "EDN ratios are not supported by CljML numeric types"
  | Lg_edn_backend.Regex value -> Runtime_dynamic.regex value
  | Lg_edn_backend.List values ->
      values |> Array.to_list |> List.map (to_dynamic_with readers)
      |> Runtime_dynamic.list
  | Lg_edn_backend.Vector values ->
      values |> Array.to_list |> List.map (to_dynamic_with readers)
      |> Rrbvec.of_list
      |> Runtime_dynamic.vector
  | Lg_edn_backend.Map entries ->
      entries |> Array.to_list
      |> List.map (fun (key, value) ->
             (to_dynamic_with readers key, to_dynamic_with readers value))
      |> Runtime_dynamic.map
  | Lg_edn_backend.Set values ->
      values |> Array.to_seq |> Seq.map (to_dynamic_with readers)
      |> Runtime_dynamic.set
  | Lg_edn_backend.Tagged (tag, value) -> (
      let value = to_dynamic_with readers value in
      match List.assoc_opt tag readers.readers with
      | Some parser -> parser value
      | None -> (
          match Hashtbl.find_opt tag_parsers tag with
          | Some parser -> parser value
          | None -> (
              match readers.default with
              | Some default -> default tag value
              | None -> invalid_arg ("no reader function for tag " ^ tag))))

let to_dynamic value = to_dynamic_with no_readers value

let dynamic_reader function_ value = Runtime_dynamic.call function_ [ value ]

let readers_from_map reader_map =
  let readers =
    if Runtime_dynamic.is_nil reader_map then []
    else
      Runtime_dynamic.entries reader_map
      |> List.map (fun (tag, function_) ->
             ( Runtime_dynamic.as_symbol tag,
               dynamic_reader function_ ))
  in
  readers

let readers_from_options options =
  let reader_map =
    Runtime_dynamic.get options (Runtime_dynamic.keyword ":readers")
  in
  let readers = readers_from_map reader_map in
  let default =
    let function_ =
      Runtime_dynamic.get options (Runtime_dynamic.keyword ":default")
    in
    if Runtime_dynamic.is_nil function_ then None
    else
      Some
        (fun tag value ->
          Runtime_dynamic.call function_
            [ Runtime_dynamic.symbol tag; value ])
  in
  { readers; default }

let read_string source =
  Lg_edn_backend.of_edn_string source
  |> to_dynamic_with { no_readers with readers = readers_from_map !data_readers }

let read_string_with_options options source =
  Lg_edn_backend.of_edn_string source
  |> to_dynamic_with (readers_from_options options)

let write_string value = value |> of_dynamic |> Lg_edn_backend.to_edn_string
