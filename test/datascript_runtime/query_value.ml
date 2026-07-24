type 'db result =
  | Entity of int
  | Attr of string
  | Value of Data_value.t
  | Database of 'db
  | Pull of Data_value.t
  | Added of bool

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
  | Collection_output _ | Scalar_output _ | Tuple_output _ -> None

let output_collection = function
  | Collection_output values -> Some values
  | Relation_output _ | Scalar_output _ | Tuple_output _ -> None

let output_scalar = function
  | Scalar_output value -> Some value
  | Relation_output _ | Collection_output _ | Tuple_output _ -> None

let output_tuple = function
  | Tuple_output row -> Some row
  | Relation_output _ | Collection_output _ | Scalar_output _ -> None

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
  Rrbvec.fold_left
    (fun rows left ->
      Rrbvec.fold_left
        (fun rows right -> Rrbvec.push_back rows (concat_rows left right))
        rows right_rows)
    Rrbvec.empty left_rows

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
  | _ -> false

let hash_result = function
  | Entity entity -> Hashtbl.hash (0, entity)
  | Attr attribute -> Hashtbl.hash (1, attribute)
  | Value value -> Hashtbl.hash (2, Data_value.hash value)
  | Database database -> Hashtbl.hash (3, database)
  | Pull value -> Hashtbl.hash (4, Data_value.hash value)
  | Added added -> Hashtbl.hash (5, added)

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
