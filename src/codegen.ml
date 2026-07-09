open Types

let ocaml_string_literal value = Printf.sprintf "%S" value

let rec stringify_expr ?(pr = false) expr =
  match expr.ty with
  | TInt -> "string_of_int (" ^ expr.code ^ ")"
  | TString -> if pr then "Printf.sprintf \"%S\" (" ^ expr.code ^ ")" else expr.code
  | TKeyword -> expr.code
  | TBool -> "string_of_bool (" ^ expr.code ^ ")"
  | TNil -> {|"nil"|}
  | TUnit -> {|""|}
  | TAny -> expr.code
  | TList inner ->
      let mapper =
        match inner with
        | TInt -> "string_of_int"
        | TKeyword -> "(fun x -> x)"
        | TString ->
            if pr then "Printf.sprintf \"%S\""
            else Printf.sprintf "(fun x -> %S ^ x ^ %S)" "\"" "\""
        | TBool -> "string_of_bool"
        | TNil -> {|(fun _ -> "nil")|}
        | TAny -> "(fun _ -> \"<value>\")"
        | _ -> {|(fun _ -> "<value>")|}
      in
      {|("(" ^ String.concat " " (List.map |}
      ^ mapper ^ " (" ^ expr.code ^ {|)) ^ ")")|}
  | TVector inner ->
      let mapper =
        match inner with
        | TInt -> "string_of_int"
        | TKeyword -> "(fun x -> x)"
        | TString ->
            if pr then "Printf.sprintf \"%S\""
            else Printf.sprintf "(fun x -> %S ^ x ^ %S)" "\"" "\""
        | TBool -> "string_of_bool"
        | TNil -> {|(fun _ -> "nil")|}
        | TAny -> "(fun _ -> \"<value>\")"
        | _ -> {|(fun _ -> "<value>")|}
      in
      {|("[" ^ String.concat " " (List.map |}
      ^ mapper ^ " (Rrbvec.to_list (" ^ expr.code ^ {|))) ^ "]")|}
  | TSet inner ->
      let mapper =
        match inner with
        | TInt -> "string_of_int"
        | TKeyword -> "(fun x -> x)"
        | TString ->
            if pr then "Printf.sprintf \"%S\""
            else Printf.sprintf "(fun x -> %S ^ x ^ %S)" "\"" "\""
        | TBool -> "string_of_bool"
        | TNil -> {|(fun _ -> "nil")|}
        | _ -> {|(fun _ -> "<value>")|}
      in
      {|("#{" ^ String.concat " " (List.map |} ^ mapper ^ " (" ^ expr.code ^ {|)) ^ "}")|}
  | TFn _ -> {|"<function>"|}
  | TRecord fields -> (
      match expr.record_values with
      | Some values ->
          let parts =
            values
            |> List.map (fun ((field : field), code) ->
                   let part =
                     stringify_expr ~pr:true
                       { ty = field.ty; code; record_values = None }
                   in
                   Printf.sprintf "%S ^ %s" (field.keyword ^ " ") part)
            |> String.concat {| ^ ", " ^ |}
          in
          {|("{" ^ |} ^ parts ^ {| ^ "}")|}
      | None ->
          let parts =
            fields
            |> List.map (fun (field : field) ->
                   let code = expr.code ^ "." ^ field.ocaml_name in
                   let part =
                     stringify_expr ~pr:true
                       { ty = field.ty; code; record_values = None }
                   in
                   Printf.sprintf "%S ^ %s" (field.keyword ^ " ") part)
            |> String.concat {| ^ ", " ^ |}
          in
          {|("{" ^ |} ^ parts ^ {| ^ "}")|})

let print_expr expr =
  match expr.ty with
  | TString -> stringify_expr ~pr:false expr
  | _ -> stringify_expr ~pr:true expr

let emit_type type_name (fields : field list) =
  let fields =
    fields
    |> List.map (fun (field : field) ->
           Printf.sprintf "  %s : %s;" field.ocaml_name (Types.ocaml_name field.ty))
    |> String.concat "\n"
  in
  Printf.sprintf "type %s = {\n%s\n}" type_name fields

let emit_record_def var_name type_name (fields : field list) values =
  let values =
    values
    |> List.map (fun ((field : field), code) ->
           Printf.sprintf "  %s = %s;" field.ocaml_name code)
    |> String.concat "\n"
  in
  Printf.sprintf "%s\n\nlet %s : %s = {\n%s\n}" (emit_type type_name fields)
    var_name type_name values

let emit_item = function
  | Emit code -> code
  | Record_def { var_name; type_name; fields; values } ->
      emit_record_def var_name type_name fields values

let emit_program items =
  items |> List.map emit_item |> String.concat "\n\n" |> fun body -> body ^ "\n"
