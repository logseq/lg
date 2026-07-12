open Types

let apply name args = Ocaml_ir.Apply (Ocaml_ir.Ident name, args)

let concat_expr = function
  | [] -> Ocaml_ir.String ""
  | first :: rest ->
      List.fold_left
        (fun acc part -> Ocaml_ir.Infix ("^", acc, part))
        first rest

let wrap_expr prefix value suffix =
  concat_expr [ Ocaml_ir.String prefix; value; Ocaml_ir.String suffix ]

let rec stringify_expr_ir ?(pr = false) expr =
  let scalar_mapper ty =
    match ty with
    | TInt -> Ocaml_ir.Ident "string_of_int"
    | TSymbol | TKeyword -> Ocaml_ir.Fun ([ Ocaml_ir.PVar "x" ], Ocaml_ir.Ident "x")
    | TString ->
        if pr then
          Ocaml_ir.Fun
            ( [ Ocaml_ir.PVar "x" ],
              apply "Printf.sprintf" [ Ocaml_ir.String "%S"; Ocaml_ir.Ident "x" ] )
        else
          Ocaml_ir.Fun
            ( [ Ocaml_ir.PVar "x" ],
              wrap_expr "\"" (Ocaml_ir.Ident "x") "\"" )
    | TBool -> Ocaml_ir.Ident "string_of_bool"
    | TAny | TVar _ -> Ocaml_ir.Fun ([ Ocaml_ir.PAny ], Ocaml_ir.String "<value>")
    | _ -> Ocaml_ir.Fun ([ Ocaml_ir.PAny ], Ocaml_ir.String "<value>")
  in
  match expr.ty with
  | TInt -> apply "string_of_int" [ expr.ocaml_expr ]
  | TFloat -> apply "string_of_float" [ expr.ocaml_expr ]
  | TChar -> apply "String.make" [ Ocaml_ir.Int 1; expr.ocaml_expr ]
  | TString ->
      if pr then apply "Printf.sprintf" [ Ocaml_ir.String "%S"; expr.ocaml_expr ]
      else expr.ocaml_expr
  | TSymbol | TKeyword -> expr.ocaml_expr
  | TBool -> apply "string_of_bool" [ expr.ocaml_expr ]
  | TUnit -> Ocaml_ir.String ""
  | TAny -> expr.ocaml_expr
  | TVar _ -> Ocaml_ir.String "<value>"
  | TArray _ | TRef _ | TOcaml _ | TOcaml_app _ | TTuple _ ->
      Ocaml_ir.String "<value>"
  | TList inner ->
      wrap_expr "("
        (apply "String.concat"
           [ Ocaml_ir.String " ";
             apply "List.map" [ scalar_mapper inner; expr.ocaml_expr ] ])
        ")"
  | TVector inner ->
      wrap_expr "["
        (apply "String.concat"
           [ Ocaml_ir.String " ";
             apply "List.map"
               [ scalar_mapper inner; apply "Rrbvec.to_list" [ expr.ocaml_expr ] ] ])
        "]"
  | TSet inner ->
      let mapper =
        Ocaml_ir.Fun
          ( [ Ocaml_ir.PVar "value" ],
            stringify_expr_ir ~pr
              { ty = inner;
                ocaml_expr = Ocaml_ir.Ident "value";
                record_values = None;
                return_param_index = None } )
      in
      let values =
        match Types.set_module_name inner with
        | Ok set_module -> apply (set_module ^ ".elements") [ expr.ocaml_expr ]
        | Error _ -> Ocaml_ir.List []
      in
      wrap_expr "#{"
        (apply "String.concat"
           [ Ocaml_ir.String " "; apply "List.map" [ mapper; values ] ])
        "}"
  | TFn _ -> Ocaml_ir.String "<function>"
  | (TRecord fields | TNamed_record { fields; _ }) ->
      let field_part field expression =
        concat_expr
          [ Ocaml_ir.String (field.keyword ^ " ");
            stringify_expr_ir ~pr:true
              { ty = field.ty;
                ocaml_expr = expression;
                record_values = None;
                return_param_index = None } ]
      in
      let parts =
        match expr.record_values with
        | Some values ->
            values
            |> List.map (fun ((field : field), expression) -> field_part field expression)
        | None ->
            fields
            |> List.map (fun (field : field) ->
                   field_part field (Ocaml_ir.Field (expr.ocaml_expr, field.ocaml_name)))
      in
      wrap_expr "{"
        (apply "String.concat" [ Ocaml_ir.String ", "; Ocaml_ir.List parts ])
        "}"

let print_expr_ir expr =
  match expr.ty with
  | TString -> stringify_expr_ir ~pr:false expr
  | _ -> stringify_expr_ir ~pr:true expr
