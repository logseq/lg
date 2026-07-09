open Asttypes
open Parsetree

let parse_implementation ?(filename = "<cljml-generated>") source =
  let lexbuf = Lexing.from_string source in
  Location.init lexbuf filename;
  try Ok (Parse.implementation lexbuf)
  with exn ->
    Error.error ("generated OCaml did not parse: " ^ Printexc.to_string exn)

let parse_expression ~context source =
  let lexbuf = Lexing.from_string source in
  Location.init lexbuf context;
  try Ok (Parse.expression lexbuf)
  with exn ->
    Error.error
      ("generated OCaml expression did not parse in " ^ context ^ ": "
     ^ Printexc.to_string exn)

let loc = Location.none
let str value = Location.mkloc value loc
let lid value = Location.mkloc value loc

let type_constructor name args =
  Ast_helper.Typ.constr ~loc (lid (Longident.Lident name)) args

let rec core_type = function
  | Types.TInt -> type_constructor "int" []
  | Types.TString | Types.TSymbol | Types.TKeyword -> type_constructor "string" []
  | Types.TBool -> type_constructor "bool" []
  | Types.TNil | Types.TUnit -> type_constructor "unit" []
  | Types.TAny -> Ast_helper.Typ.var ~loc "a"
  | Types.TList inner | Types.TSet inner -> type_constructor "list" [ core_type inner ]
  | Types.TVector inner ->
      Ast_helper.Typ.constr ~loc
        (lid
           (Longident.Ldot
              (lid (Longident.Lident "Rrbvec"), str "t")))
        [ core_type inner ]
  | Types.TFn (args, ret) ->
      let args = match args with [] -> [ Types.TUnit ] | _ -> args in
      List.fold_right
        (fun arg result -> Ast_helper.Typ.arrow ~loc Nolabel (core_type arg) result)
        args (core_type ret)
  | Types.TRecord _ -> type_constructor "record" []

let parse_record_values var_name values =
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | ((field : Types.field), code) :: rest -> (
        let context = "record " ^ var_name ^ " field " ^ field.keyword in
        match parse_expression ~context code with
        | Error _ as err -> err
        | Ok expr ->
            loop ((lid (Longident.Lident field.ocaml_name), expr) :: acc) rest)
  in
  loop [] values

let record_definition var_name type_name fields values =
  let label_declarations =
    fields
    |> List.map (fun (field : Types.field) ->
           Ast_helper.Type.field ~loc (str field.ocaml_name) (core_type field.ty))
  in
  let type_declaration =
    Ast_helper.Type.mk ~loc ~kind:(Ptype_record label_declarations) (str type_name)
  in
  match parse_record_values var_name values with
  | Error _ as err -> err
  | Ok record_fields ->
      let record_expr = Ast_helper.Exp.record ~loc record_fields None in
      let annotated_expr =
        Ast_helper.Exp.constraint_ ~loc record_expr (type_constructor type_name [])
      in
      let value_binding =
        Ast_helper.Vb.mk ~loc
          (Ast_helper.Pat.var ~loc (str var_name))
          annotated_expr
      in
      Ok
        [ Ast_helper.Str.type_ ~loc Nonrecursive [ type_declaration ];
          Ast_helper.Str.value ~loc Nonrecursive [ value_binding ] ]

let structure_of_item index = function
  | Types.Emit source ->
      parse_implementation
        ~filename:("<cljml-generated-item-" ^ string_of_int index ^ ">")
        source
  | Types.Record_def { var_name; type_name; fields; values } ->
      record_definition var_name type_name fields values

let structure_of_items items =
  let rec loop index acc = function
    | [] -> Ok (List.concat (List.rev acc))
    | item :: rest -> (
        match structure_of_item index item with
        | Error _ as err -> err
        | Ok structure -> loop (index + 1) (structure :: acc) rest)
  in
  loop 1 [] items

let print_implementation structure =
  Format.asprintf "%a@." Pprintast.structure structure
