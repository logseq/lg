open Types

let ocaml_string_literal value = Printf.sprintf "%S" value

let rec stringify_expr ?(pr = false) expr =
  match expr.ty with
  | TInt -> "string_of_int (" ^ expr.code ^ ")"
  | TString -> if pr then "Printf.sprintf \"%S\" (" ^ expr.code ^ ")" else expr.code
  | TSymbol -> expr.code
  | TKeyword -> expr.code
  | TBool -> "string_of_bool (" ^ expr.code ^ ")"
  | TNil -> {|"nil"|}
  | TUnit -> {|""|}
  | TAny -> expr.code
  | TList inner ->
      let mapper =
        match inner with
        | TInt -> "string_of_int"
        | TSymbol -> "(fun x -> x)"
        | TKeyword -> "(fun x -> x)"
        | TString ->
            if pr then "(fun x -> Printf.sprintf \"%S\" x)"
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
        | TSymbol -> "(fun x -> x)"
        | TKeyword -> "(fun x -> x)"
        | TString ->
            if pr then "(fun x -> Printf.sprintf \"%S\" x)"
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
        let value =
          stringify_expr ~pr
            { ty = inner;
              code = "value";
              ocaml_expr = Ocaml_ir.Ident "value";
              record_values = None }
        in
        "(fun value -> " ^ value ^ ")"
      in
      let values =
        match Types.set_module_name inner with
        | Ok set_module -> set_module ^ ".elements (" ^ expr.code ^ ")"
        | Error _ -> "[]"
      in
      {|("#{" ^ String.concat " " (List.map |} ^ mapper ^ " (" ^ values ^ {|)) ^ "}")|}
  | TFn _ -> {|"<function>"|}
  | (TRecord fields | TNamed_record { fields; _ }) -> (
      match expr.record_values with
      | Some values ->
          let parts =
            values
            |> List.map (fun ((field : field), expression) ->
                   let part =
                     stringify_expr ~pr:true
                       {
                         ty = field.ty;
                         code = Ocaml_ir.to_source expression;
                         ocaml_expr = expression;
                         record_values = None;
                       }
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
                       {
                         ty = field.ty;
                         code;
                         ocaml_expr = Ocaml_ir.Raw code;
                         record_values = None;
                       }
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

let emit_set_module module_name type_name =
  Printf.sprintf
    "module %s = Set.Make (struct\n  type t = %s\n\n  let compare = Stdlib.compare\nend)"
    module_name type_name

let emit_record_def var_name type_name set_module_name (fields : field list) values =
  let values =
    values
    |> List.map (fun ((field : field), expression) ->
           Printf.sprintf "  %s = %s;" field.ocaml_name
             (Ocaml_ir.to_source expression))
    |> String.concat "\n"
  in
  Printf.sprintf "%s\n\n%s\n\nlet %s : %s = {\n%s\n}"
    (emit_type type_name fields)
    (emit_set_module set_module_name type_name)
    var_name type_name values

let rec emit_item = function
  | Value_binding { pattern; expression } ->
      let pattern =
        match pattern with
        | Named name -> name
        | Unit_pattern -> "()"
        | Ignore_pattern -> "_"
      in
      "let " ^ pattern ^ " = " ^ Ocaml_ir.to_source expression
  | Comment text -> "(* " ^ text ^ " *)"
  | Type_def { type_name; fields } -> emit_type type_name fields
  | Group items -> items |> List.map emit_item |> String.concat "\n\n"
  | Module_def { module_name; items } ->
      let body = items |> List.map emit_item |> String.concat "\n\n" in
      "module " ^ module_name ^ " = struct\n" ^ body ^ "\nend"
  | Record_def { var_name; type_name; set_module_name; fields; values } ->
      emit_record_def var_name type_name set_module_name fields values

let emit_program items =
  items |> List.map emit_item |> String.concat "\n\n" |> fun body -> body ^ "\n"
