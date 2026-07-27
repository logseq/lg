type 'db result =
  | Entity of int
  | Attr of string
  | Value of Data_value.t
  | Database of 'db
  | Pull of Data_value.t
  | Added of bool
  | Callable of 'db callable

and 'db callable = 'db result Rrbvec.t -> Data_value.t option

type 'db source =
  | Database_source of 'db
  | Relation_source of 'db result array Rrbvec.t

type 'db binding_value =
  | Scalar_binding of 'db result
  | Collection_binding of 'db binding_value Rrbvec.t

type ('db, 'rules) input =
  | Source_input of 'db source
  | Rules_input of 'rules
  | Binding_input of 'db binding_value

type 'db output =
  | Relation_output of 'db result array Rrbvec.t
  | Collection_output of 'db result Rrbvec.t
  | Scalar_output of 'db result option
  | Tuple_output of 'db result array option
  | Keyword_relation_output of
      (string, 'db result) Lg_runtime.Lg_map.t Rrbvec.t
  | Symbol_relation_output of
      (string, 'db result) Lg_runtime.Lg_map.t Rrbvec.t
  | String_relation_output of
      (string, 'db result) Lg_runtime.Lg_map.t Rrbvec.t
  | Keyword_tuple_output of
      (string, 'db result) Lg_runtime.Lg_map.t option
  | Symbol_tuple_output of
      (string, 'db result) Lg_runtime.Lg_map.t option
  | String_tuple_output of
      (string, 'db result) Lg_runtime.Lg_map.t option

type 'db relation = {
  attrs : (string, int) Lg_runtime.Lg_map.t;
  rows : 'db result array Rrbvec.t;
  lookup_databases : (string, 'db) Lg_runtime.Lg_map.t;
}

type ('db, 'rules) context = {
  relations : 'db relation Rrbvec.t;
  sources : (string, 'db source) Lg_runtime.Lg_map.t;
  rules : 'rules;
}

let database_source database = Database_source database
let relation_source rows = Relation_source rows
let entity entity_id = Entity entity_id
let attr attribute = Attr attribute
let value value = Value value
let database database = Database database
let pull result = Pull result
let added added = Added added
let callable invoke = invoke
let callable_result callable = Callable callable
let result_callable = function Callable callable -> Some callable | _ -> None
let invoke_callable callable arguments = callable arguments
let scalar_binding result = Scalar_binding result
let collection_binding values = Collection_binding values
let source_input source = Source_input source
let rules_input rules = Rules_input rules
let binding_input binding = Binding_input binding

let source_database = function
  | Database_source database -> Some database
  | Relation_source _ -> None

let source_rows = function
  | Database_source _ -> None
  | Relation_source rows -> Some rows

let result_value = function Value value -> Some value | _ -> None

let binding_result = function
  | Scalar_binding result -> Some result
  | Collection_binding _ -> None

let binding_items = function
  | Scalar_binding _ -> None
  | Collection_binding values -> Some values

let input_source = function
  | Source_input source -> Some source
  | Rules_input _ | Binding_input _ -> None

let input_rules = function
  | Rules_input rules -> Some rules
  | Source_input _ | Binding_input _ -> None

let input_binding = function
  | Binding_input binding -> Some binding
  | Source_input _ | Rules_input _ -> None

let relation_output rows = Relation_output rows
let collection_output values = Collection_output values
let scalar_output value = Scalar_output value
let tuple_output row = Tuple_output row

let output_relation = function
  | Relation_output rows -> Some rows
  | Collection_output _ | Scalar_output _ | Tuple_output _
  | Keyword_relation_output _ | Symbol_relation_output _
  | String_relation_output _ | Keyword_tuple_output _ | Symbol_tuple_output _
  | String_tuple_output _ ->
      None

let output_collection = function
  | Collection_output values -> Some values
  | Relation_output _ | Scalar_output _ | Tuple_output _
  | Keyword_relation_output _ | Symbol_relation_output _
  | String_relation_output _ | Keyword_tuple_output _ | Symbol_tuple_output _
  | String_tuple_output _ ->
      None

let output_scalar = function
  | Scalar_output value -> Some value
  | Relation_output _ | Collection_output _ | Tuple_output _
  | Keyword_relation_output _ | Symbol_relation_output _
  | String_relation_output _ | Keyword_tuple_output _ | Symbol_tuple_output _
  | String_tuple_output _ ->
      None

let output_tuple = function
  | Tuple_output row -> Some row
  | Relation_output _ | Collection_output _ | Scalar_output _
  | Keyword_relation_output _ | Symbol_relation_output _
  | String_relation_output _ | Keyword_tuple_output _ | Symbol_tuple_output _
  | String_tuple_output _ ->
      None

let output_keyword_relation = function
  | Keyword_relation_output rows -> Some rows
  | _ -> None

let output_symbol_relation = function
  | Symbol_relation_output rows -> Some rows
  | _ -> None

let output_string_relation = function
  | String_relation_output rows -> Some rows
  | _ -> None

let output_keyword_tuple = function
  | Keyword_tuple_output row -> Some row
  | _ -> None

let output_symbol_tuple = function
  | Symbol_tuple_output row -> Some row
  | _ -> None

let output_string_tuple = function
  | String_tuple_output row -> Some row
  | _ -> None

let empty_row () = [||]

let row_get row index =
  if index < 0 || index >= Array.length row then None else Some row.(index)

let project_row row indexes =
  Array.map (fun index -> row.(index)) indexes

let join_rows left left_indexes right right_indexes =
  let left_length = Array.length left_indexes in
  Array.init (left_length + Array.length right_indexes) (fun index ->
      if index < left_length then left.(left_indexes.(index))
      else right.(right_indexes.(index - left_length)))

let concat_rows = Array.append

let product_rows left_rows right_rows =
  if Rrbvec.length left_rows = 1 then
    let left = Rrbvec.nth left_rows 0 in
    right_rows |> Rrbvec.to_array
    |> Array.map (concat_rows left)
    |> Rrbvec.of_array
  else if Rrbvec.length right_rows = 1 then
    let right = Rrbvec.nth right_rows 0 in
    left_rows |> Rrbvec.to_array
    |> Array.map (fun left -> concat_rows left right)
    |> Rrbvec.of_array
  else
    Rrbvec.fold_left
      (fun rows left ->
        Rrbvec.fold_left
          (fun rows right -> Rrbvec.push_back rows (concat_rows left right))
          rows right_rows)
      Rrbvec.empty left_rows

let collect_tuples acc relation len copy_map =
  Rrbvec.fold_left
    (fun collected seed ->
      Rrbvec.fold_left
        (fun collected row ->
          let result = Array.copy seed in
          for index = 0 to len - 1 do
            match copy_map.(index) with
            | Some source_index -> result.(index) <- Some row.(source_index)
            | None -> ()
          done;
          Rrbvec.push_back collected result)
        collected relation.rows)
    Rrbvec.empty acc

let relation attrs rows lookup_databases =
  { attrs; rows; lookup_databases }

let index_attrs variables =
  Rrbvec.fold_left
    (fun (attrs, index) variable ->
      (Lg_runtime.Lg_map.assoc attrs variable index, index + 1))
    (Lg_runtime.Lg_map.empty, 0) variables
  |> fst

let relation_attrs relation = relation.attrs
let relation_rows relation = relation.rows
let relation_lookup_databases relation = relation.lookup_databases

let relation_result relation variable row =
  match Lg_runtime.Lg_map.get_option relation.attrs variable with
  | None -> None
  | Some index -> row_get row index

let relation_lookup_database relation variable =
  Lg_runtime.Lg_map.get_option relation.lookup_databases variable

let relation_with_rows relation rows = { relation with rows }

let relation_append_rows left right =
  { left with rows = Rrbvec.append left.rows right.rows }

let join_key_value = function
  | Entity entity -> Value (Data_value.Int entity)
  | Attr attribute -> Value (Data_value.Keyword attribute)
  | result -> result

let equal_result left right =
  match (left, right) with
  | Entity left, Entity right -> left = right
  | Attr left, Attr right -> String.equal left right
  | Value left, Value right -> Data_value.equal left right
  | Database left, Database right -> left == right
  | Pull left, Pull right -> Data_value.equal left right
  | Added left, Added right -> Bool.equal left right
  | Callable left, Callable right -> left == right
  | _ -> false

let hash_result = function
  | Entity entity -> Hashtbl.hash (0, entity)
  | Attr attribute -> Hashtbl.hash (1, attribute)
  | Value value -> Hashtbl.hash (2, Data_value.hash value)
  | Database database -> Hashtbl.hash (3, database)
  | Pull value -> Hashtbl.hash (4, Data_value.hash value)
  | Added added -> Hashtbl.hash (5, added)
  | Callable callable -> Hashtbl.hash (6, callable)

let equal_result_map left right =
  Lg_runtime.Lg_map.count left = Lg_runtime.Lg_map.count right
  && Lg_runtime.Lg_map.fold_left
       (fun equal (key, value) ->
         equal
         &&
         match Lg_runtime.Lg_map.get_option right key with
         | Some candidate -> equal_result value candidate
         | None -> false)
       true left

let hash_result_map map =
  Lg_runtime.Lg_map.fold_left
    (fun hash (key, value) ->
      hash lxor Hashtbl.hash (key, hash_result value))
    0 map

let distinct_by equal hash_value values =
  let buckets = Hashtbl.create (max 16 (Rrbvec.length values)) in
  Rrbvec.fold_left
    (fun unique_values value ->
      let hash = hash_value value in
      let bucket = Option.value (Hashtbl.find_opt buckets hash) ~default:[] in
      if List.exists (equal value) bucket then unique_values
      else (
        Hashtbl.replace buckets hash (value :: bucket);
        Rrbvec.push_back unique_values value))
    Rrbvec.empty values

let distinct_result_maps maps =
  distinct_by equal_result_map hash_result_map maps

let equal_key left right =
  let length = Array.length left in
  length = Array.length right
  &&
  let rec loop index =
    index = length
    || (equal_result left.(index) right.(index) && loop (index + 1))
  in
  loop 0

let hash_key key =
  Array.fold_left
    (fun hash result -> ((hash lsl 5) - hash) lxor hash_result result)
    0 key

let distinct_rows rows = distinct_by equal_key hash_key rows

let equal_optional_key left right =
  let length = Array.length left in
  length = Array.length right
  &&
  let rec loop index =
    index = length
    ||
    match (left.(index), right.(index)) with
    | None, None -> loop (index + 1)
    | Some left, Some right ->
        equal_result left right && loop (index + 1)
    | None, Some _ | Some _, None -> false
  in
  loop 0

let hash_optional_key key =
  Array.fold_left
    (fun hash value ->
      let value_hash =
        match value with None -> 0x4f1bbcdc | Some result -> hash_result result
      in
      ((hash lsl 5) - hash) lxor value_hash)
    0 key

let distinct_optional_rows rows =
  distinct_by equal_optional_key hash_optional_key rows

let group_rows rows indexes =
  let rec add_row key row = function
    | [] -> [ (key, [ row ]) ]
    | (candidate, grouped_rows) :: rest when equal_key key candidate ->
        (candidate, row :: grouped_rows) :: rest
    | group :: rest -> group :: add_row key row rest
  in
  Rrbvec.fold_left
    (fun groups row -> add_row (project_row row indexes) row groups)
    [] rows
  |> List.map (fun (_, grouped_rows) ->
         Rrbvec.of_list (List.rev grouped_rows))
  |> Rrbvec.of_list

let subtract_relation left right =
  let shared_indexes =
    Lg_runtime.Lg_map.fold_left
      (fun indexes (variable, left_index) ->
        match Lg_runtime.Lg_map.get_option right.attrs variable with
        | Some right_index -> (left_index, right_index) :: indexes
        | None -> indexes)
      [] left.attrs
    |> List.rev
  in
  let left_indexes =
    shared_indexes |> List.map fst |> Array.of_list
  in
  let right_indexes =
    shared_indexes |> List.map snd |> Array.of_list
  in
  let buckets =
    Hashtbl.create (max 16 (Rrbvec.length right.rows))
  in
  Rrbvec.iter
    (fun row ->
      let key = project_row row right_indexes in
      let hash = hash_key key in
      let bucket =
        Option.value (Hashtbl.find_opt buckets hash) ~default:[]
      in
      Hashtbl.replace buckets hash (key :: bucket))
    right.rows;
  let rows =
    Rrbvec.fold_left
      (fun rows row ->
        let key = project_row row left_indexes in
        let bucket =
          Option.value
            (Hashtbl.find_opt buckets (hash_key key))
            ~default:[]
        in
        if List.exists (equal_key key) bucket then rows
        else Rrbvec.push_back rows row)
      Rrbvec.empty left.rows
  in
  { left with rows }

let aggregate_values rows index =
  rows |> Rrbvec.to_list
  |> List.map (fun row ->
         match row_get row index with
         | Some (Value value) -> value
         | Some _ -> invalid_arg "aggregate input must be a DataScript value"
         | None -> invalid_arg "aggregate row index is out of bounds")

let numeric_value = function
  | Data_value.Int value -> float_of_int value
  | Data_value.Float value -> value
  | _ -> invalid_arg "aggregate expects numeric values"

let aggregate_sum rows index =
  let values = aggregate_values rows index in
  let all_ints =
    List.for_all
      (function Data_value.Int _ -> true | Data_value.Float _ -> false | _ -> false)
      values
  in
  if all_ints then
    Value
      (Data_value.Int
         (List.fold_left
            (fun sum -> function
              | Data_value.Int value -> sum + value
              | _ -> assert false)
            0 values))
  else
    Value
      (Data_value.Float
         (List.fold_left
            (fun sum value -> sum +. numeric_value value)
            0.0 values))

let aggregate_average rows index =
  let values = aggregate_values rows index in
  let total =
    List.fold_left (fun sum value -> sum +. numeric_value value) 0.0 values
  in
  Value (Data_value.Float (total /. float_of_int (List.length values)))

let aggregate_median rows index =
  let values =
    aggregate_values rows index |> List.sort Data_value.compare
    |> Array.of_list
  in
  let length = Array.length values in
  let middle = length / 2 in
  if length mod 2 = 1 then Value values.(middle)
  else
    Value
      (Data_value.Float
         ((numeric_value values.(middle - 1) +. numeric_value values.(middle))
         /. 2.0))

let aggregate_variance rows index =
  let values = aggregate_values rows index |> List.map numeric_value in
  let length = List.length values in
  let mean =
    List.fold_left ( +. ) 0.0 values /. float_of_int length
  in
  let squared_deviations =
    List.fold_left
      (fun sum value ->
        let difference = value -. mean in
        sum +. (difference *. difference))
      0.0 values
  in
  Value (Data_value.Float (squared_deviations /. float_of_int length))

let aggregate_standard_deviation rows index =
  match aggregate_variance rows index with
  | Value (Data_value.Float variance) ->
      Value (Data_value.Float (Float.sqrt variance))
  | _ -> assert false

let aggregate_minimum rows index =
  match aggregate_values rows index with
  | first :: rest ->
      Value
        (List.fold_left
           (fun minimum value ->
             if Data_value.compare value minimum < 0 then value else minimum)
           first rest)
  | [] -> invalid_arg "min aggregate requires at least one value"

let aggregate_maximum rows index =
  match aggregate_values rows index with
  | first :: rest ->
      Value
        (List.fold_left
           (fun maximum value ->
             if Data_value.compare value maximum > 0 then value else maximum)
           first rest)
  | [] -> invalid_arg "max aggregate requires at least one value"

let rec take count values =
  if count <= 0 then []
  else
    match values with
    | [] -> []
    | value :: rest -> value :: take (count - 1) rest

let aggregate_minimum_n rows index count =
  let values =
    aggregate_values rows index |> List.sort Data_value.compare
    |> take count
  in
  Value (Data_value.Vector values)

let aggregate_maximum_n rows index count =
  let values =
    aggregate_values rows index |> List.sort Data_value.compare
    |> List.rev |> take count |> List.rev
  in
  Value (Data_value.Vector values)

let random_value values =
  let values = Array.of_list values in
  let length = Array.length values in
  if length = 0 then invalid_arg "rand aggregate requires at least one value";
  values.(Random.int length)

let aggregate_random rows index =
  Value (random_value (aggregate_values rows index))

let aggregate_random_n rows index count =
  let values = aggregate_values rows index in
  Value
    (Data_value.Vector
       (List.init (max 0 count) (fun _ -> random_value values)))

let aggregate_sample rows index count =
  let values = aggregate_values rows index |> Array.of_list in
  for index = Array.length values - 1 downto 1 do
    let swap_index = Random.int (index + 1) in
    let value = values.(index) in
    values.(index) <- values.(swap_index);
    values.(swap_index) <- value
  done;
  Value
    (Data_value.Vector
       (values |> Array.to_list |> take (max 0 count)))

let aggregate_distinct rows index =
  let values =
    List.fold_left
      (fun distinct value ->
        if List.exists (Data_value.equal value) distinct then distinct
        else value :: distinct)
      [] (aggregate_values rows index)
    |> List.rev
  in
  Value (Data_value.Set values)

let append_right_only left right right_indexes =
  let left_length = Array.length left in
  Array.init (left_length + Array.length right_indexes) (fun index ->
      if index < left_length then left.(index)
      else right.(right_indexes.(index - left_length)))

let merge_lookup_databases left right =
  Lg_runtime.Lg_map.fold_left
    (fun databases (variable, database) ->
      Lg_runtime.Lg_map.assoc databases variable database)
    left right

let hash_join resolve_lookup left right =
  let left_attrs = Lg_runtime.Lg_map.to_list left.attrs in
  let right_attrs = Lg_runtime.Lg_map.to_list right.attrs in
  let common =
    List.filter_map
      (fun (variable, left_index) ->
        match Lg_runtime.Lg_map.get_option right.attrs variable with
        | None -> None
        | Some right_index -> Some (variable, left_index, right_index))
      left_attrs
  in
  let right_only =
    List.filter
      (fun (variable, _) ->
        not (Lg_runtime.Lg_map.mem left.attrs variable))
      right_attrs
  in
  let attrs =
    List.fold_left
      (fun attrs (variable, _) ->
        Lg_runtime.Lg_map.assoc attrs variable
          (Lg_runtime.Lg_map.count attrs))
      left.attrs right_only
  in
  let lookup_databases =
    merge_lookup_databases left.lookup_databases right.lookup_databases
  in
  if common = [] then
    { attrs; rows = product_rows left.rows right.rows; lookup_databases }
  else
    let lookup_database variable =
      match
        Lg_runtime.Lg_map.get_option right.lookup_databases variable
      with
      | Some _ as database -> database
      | None ->
          Lg_runtime.Lg_map.get_option left.lookup_databases variable
    in
    let result_at variable left_index right_index row side =
      let index = if side = `Left then left_index else right_index in
      let result = row.(index) in
      match lookup_database variable with
      | None -> join_key_value result
      | Some database -> join_key_value (resolve_lookup database result)
    in
    let key row side =
      Array.of_list
        (List.map
           (fun (variable, left_index, right_index) ->
             result_at variable left_index right_index row side)
           common)
    in
    let right_only_indexes = Array.of_list (List.map snd right_only) in
    let rows =
      match common with
      | [ (variable, left_index, right_index) ] ->
          let buckets =
            Hashtbl.create (max 16 (Rrbvec.length left.rows))
          in
          Rrbvec.fold_left
            (fun () row ->
              let row_key =
                result_at variable left_index right_index row `Left
              in
              let hash = hash_result row_key in
              let bucket =
                Hashtbl.find_opt buckets hash |> Option.value ~default:[]
              in
              Hashtbl.replace buckets hash ((row_key, row) :: bucket))
            () left.rows;
          Rrbvec.fold_left
            (fun rows right_row ->
              let right_key =
                result_at variable left_index right_index right_row `Right
              in
              let candidates =
                Hashtbl.find_opt buckets (hash_result right_key)
                |> Option.value ~default:[]
              in
              List.fold_left
                (fun rows (left_key, left_row) ->
                  if equal_result left_key right_key then
                    Rrbvec.push_back rows
                      (append_right_only left_row right_row right_only_indexes)
                  else rows)
                rows candidates)
            Rrbvec.empty right.rows
      | _ ->
          let buckets =
            Hashtbl.create (max 16 (Rrbvec.length left.rows))
          in
          Rrbvec.fold_left
            (fun () row ->
              let row_key = key row `Left in
              let hash = hash_key row_key in
              let bucket =
                Hashtbl.find_opt buckets hash |> Option.value ~default:[]
              in
              Hashtbl.replace buckets hash ((row_key, row) :: bucket))
            () left.rows;
          Rrbvec.fold_left
            (fun rows right_row ->
              let right_key = key right_row `Right in
              let candidates =
                Hashtbl.find_opt buckets (hash_key right_key)
                |> Option.value ~default:[]
              in
              List.fold_left
                (fun rows (left_key, left_row) ->
                  if equal_key left_key right_key then
                    Rrbvec.push_back rows
                      (append_right_only left_row right_row right_only_indexes)
                  else rows)
                rows candidates)
            Rrbvec.empty right.rows
    in
    { attrs; rows; lookup_databases }

let context relations sources rules = { relations; sources; rules }
let context_relations context = context.relations
let context_sources context = context.sources
let context_rules context = context.rules
