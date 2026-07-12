open Types

let validate_unique_keywords pairs =
  let rec loop seen = function
    | [] -> Ok ()
    | (keyword, _) :: rest ->
        if List.mem keyword seen then Error.error ("duplicate field " ^ keyword)
        else loop (keyword :: seen) rest
  in
  loop [] pairs

let field_expr target field =
  match target.record_values with
  | Some values -> (
      match List.assoc_opt field values with
      | Some expression -> expression
      | None -> Semantic_ir.Field (target.semantic_expr, field.ocaml_name))
  | None -> Semantic_ir.Field (target.semantic_expr, field.ocaml_name)

let values_for target fields =
  List.map (fun (field : field) -> (field, field_expr target field)) fields

let record_expr fields values =
  {
    ty = TRecord fields;
    semantic_expr =
      Semantic_ir.Record
        (List.map (fun ((field : field), value) -> (field.ocaml_name, value)) values, None);
    record_values = Some values;
    return_param_index = None;
  }

let named_record_expr record values =
  let type_name =
    match record.type_parameters with
    | [] -> record.type_name
    | [ _ ] -> "_ " ^ record.type_name
    | parameters ->
        "(" ^ String.concat ", " (List.map (fun _ -> "_") parameters) ^ ") "
        ^ record.type_name
  in
  {
    ty = TNamed_record record;
    semantic_expr =
      Semantic_ir.Record
        ( List.map
            (fun ((field : field), value) -> (field.ocaml_name, value))
            values,
          Some type_name );
    record_values = Some values;
    return_param_index = None;
  }

let assoc target fields keyword value =
  match find_field keyword fields with
  | Some field when not (Types.equal field.ty value.ty) ->
      Error.error
        (Printf.sprintf "cannot assoc %s as %s because it is already %s" keyword
           (source_name value.ty) (source_name field.ty))
  | Some _ ->
      let values =
        fields
        |> List.map (fun (field : field) ->
               let expression =
                 if field.keyword = keyword then value.semantic_expr
                 else field_expr target field
               in
               (field, expression))
      in
      (match target.ty with
      | TNamed_record record -> Ok (named_record_expr record values)
      | _ -> Ok (record_expr fields values))
  | None ->
      let new_field = make_field keyword value.ty in
      let old_fields = fields in
      let fields = old_fields @ [ new_field ] in
      let values = values_for target old_fields in
      let values = values @ [ (new_field, value.semantic_expr) ] in
      Ok (record_expr fields values)

let rec assoc_many target pairs =
  match (target.ty, pairs) with
  | (TRecord _fields | TNamed_record { fields = _fields; _ }), [] -> Ok target
  | (TRecord fields | TNamed_record { fields; _ }), (keyword, value) :: rest -> (
      match assoc target fields keyword value with
      | Error _ as err -> err
      | Ok target -> assoc_many target rest)
  | _ -> Error.error "assoc expects a map"

let dissoc target fields keyword =
  match find_field keyword fields with
  | None -> Error.error ("cannot dissoc unknown field " ^ keyword)
  | Some _ ->
      let fields = List.filter (fun (field : field) -> field.keyword <> keyword) fields in
      let values = values_for target fields in
      Ok (record_expr fields values)

let rec dissoc_many target keywords =
  match (target.ty, keywords) with
  | (TRecord _fields | TNamed_record { fields = _fields; _ }), [] -> Ok target
  | (TRecord fields | TNamed_record { fields; _ }), keyword :: rest -> (
      match dissoc target fields keyword with
      | Error _ as err -> err
      | Ok target -> dissoc_many target rest)
  | _ -> Error.error "dissoc expects a map"

let merge maps =
  let merge_one fields values right =
    match right.ty with
    | TRecord right_fields | TNamed_record { fields = right_fields; _ } ->
        let right_values = values_for right right_fields in
        let add_field (fields, values) (right_field : field) =
          match find_field right_field.keyword fields with
          | Some existing when not (Types.equal existing.ty right_field.ty) ->
              Error.error
                (Printf.sprintf "cannot merge %s as %s because it is already %s"
                   right_field.keyword (source_name right_field.ty)
                   (source_name existing.ty))
          | Some existing ->
              let values =
                values
                |> List.map (fun (field, expression) ->
                       if field.keyword = existing.keyword then
                         (field, List.assoc right_field right_values)
                       else (field, expression))
              in
              Ok (fields, values)
          | None ->
              Ok
                ( fields @ [ right_field ],
                  values @ [ (right_field, List.assoc right_field right_values) ] )
        in
        List.fold_left
          (fun acc field ->
            match acc with
            | Error _ as err -> err
            | Ok acc -> add_field acc field)
          (Ok (fields, values)) right_fields
    | _ -> Error.error "merge expects maps"
  in
  match maps with
  | [] -> Error.error "merge expects at least 1 map"
  | first :: rest -> (
      match first.ty with
      | TRecord fields | TNamed_record { fields; _ } -> (
          let values = values_for first fields in
          let result =
            List.fold_left
              (fun acc right ->
                match acc with
                | Error _ as err -> err
                | Ok (fields, values) -> merge_one fields values right)
              (Ok (fields, values)) rest
          in
          match result with
          | Error _ as err -> err
          | Ok (fields, values) ->
              Ok (record_expr fields values))
      | _ -> Error.error "merge expects maps")

let update_value target fields keyword value_ty value_expr =
  match find_field keyword fields with
  | None -> Error.error ("cannot update unknown field " ^ keyword)
  | Some field when not (Types.equal field.ty value_ty) ->
      Error.error
        (Printf.sprintf "cannot update %s as %s because it is already %s" keyword
           (source_name value_ty) (source_name field.ty))
  | Some _ ->
      let values =
        fields
        |> List.map (fun (field : field) ->
               if field.keyword = keyword then (field, value_expr)
               else (field, field_expr target field))
      in
      Ok (record_expr fields values)

let select_keys target fields keywords =
  if keywords = [] then Error.error "select-keys requires at least one key"
  else
    let rec collect acc = function
      | [] ->
          let selected = List.rev acc in
          let keyword_pairs =
            selected |> List.map (fun (field : field) -> (field.keyword, field))
          in
          validate_unique_keywords keyword_pairs
          |> Result.map (fun () ->
                 let values = values_for target selected in
                 record_expr selected values)
      | keyword :: rest -> (
          match find_field keyword fields with
          | Some field -> collect (field :: acc) rest
          | None -> Error.error ("cannot select unknown field " ^ keyword))
    in
    collect [] keywords
