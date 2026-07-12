open Asttypes
open Parsetree
open Lowered

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

let longident_of_string name =
  match String.split_on_char '.' name with
  | [] -> Longident.Lident name
  | first :: rest ->
      List.fold_left
        (fun path segment ->
          Longident.Ldot (lid path, str segment))
        (Longident.Lident first) rest

let type_constructor name args =
  Ast_helper.Typ.constr ~loc (lid (Longident.Lident name)) args

let rec core_type = function
  | Types.TInt -> type_constructor "int" []
  | Types.TFloat -> type_constructor "float" []
  | Types.TChar -> type_constructor "char" []
  | Types.TString | Types.TSymbol | Types.TKeyword -> type_constructor "string" []
  | Types.TBool -> type_constructor "bool" []
  | Types.TUnit -> type_constructor "unit" []
  | Types.TAny -> Ast_helper.Typ.var ~loc "a"
  | Types.TVar name -> Ast_helper.Typ.var ~loc name
  | Types.TOcaml name ->
      Ast_helper.Typ.constr ~loc (lid (longident_of_string name)) []
  | Types.TOcaml_app (name, args) ->
      Ast_helper.Typ.constr ~loc (lid (longident_of_string name))
        (List.map core_type args)
  | Types.TTuple args ->
      Ast_helper.Typ.tuple ~loc
        (List.map (fun arg -> (None, core_type arg)) args)
  | Types.TArray inner -> type_constructor "array" [ core_type inner ]
  | Types.TRef inner -> type_constructor "ref" [ core_type inner ]
  | Types.TList inner -> type_constructor "list" [ core_type inner ]
  | Types.TSet inner -> (
      match Types.set_module_name inner with
      | Ok set_module ->
          Ast_helper.Typ.constr ~loc
            (lid (longident_of_string (set_module ^ ".t"))) []
      | Error _ -> type_constructor "unsupported_set" [ core_type inner ])
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
  | Types.TNamed_record record ->
      Ast_helper.Typ.constr ~loc
        (lid (longident_of_string record.type_name))
        (List.map (fun _ -> Ast_helper.Typ.any ~loc ()) record.type_parameters)

let record_values_to_parsetree var_name values =
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | ((field : Types.field), expression) :: rest -> (
        let context = "record " ^ var_name ^ " field " ^ field.keyword in
        match Ocaml_ir.to_parsetree ~context expression with
        | Error _ as err -> err
        | Ok expr ->
            loop ((lid (Longident.Lident field.ocaml_name), expr) :: acc) rest)
  in
  loop [] values

let type_parameters parameters =
  List.map
    (fun name ->
      ( Ast_helper.Typ.var ~loc name,
        (Asttypes.NoVariance, Asttypes.NoInjectivity) ))
    parameters

let record_type_definition type_name parameters fields =
  let label_declarations =
    fields
    |> List.map (fun (field : Types.field) ->
           Ast_helper.Type.field ~loc (str field.ocaml_name) (core_type field.ty))
  in
  let type_declaration =
    Ast_helper.Type.mk ~loc ~params:(type_parameters parameters)
      ~kind:(Ptype_record label_declarations) (str type_name)
  in
  Ast_helper.Str.type_ ~loc Nonrecursive [ type_declaration ]

let type_alias_definition type_name parameters manifest =
  let type_declaration =
    Ast_helper.Type.mk ~loc ~params:(type_parameters parameters)
      ~manifest:(core_type manifest) (str type_name)
  in
  Ast_helper.Str.type_ ~loc Nonrecursive [ type_declaration ]

let type_variant_definition type_name parameters constructors =
  let constructor_declarations =
    constructors
    |> List.map (fun constructor ->
           Ast_helper.Type.constructor ~loc
             ~args:(Pcstr_tuple (List.map core_type constructor.payload_types))
             (str constructor.constructor_name))
  in
  let type_declaration =
    Ast_helper.Type.mk ~loc ~params:(type_parameters parameters)
      ~kind:(Ptype_variant constructor_declarations)
      (str type_name)
  in
  Ast_helper.Str.type_ ~loc Nonrecursive [ type_declaration ]

let signature_item = function
  | Signature_value { value_name; value_type; _ } ->
      Ast_helper.Sig.value ~loc
        (Ast_helper.Val.mk ~loc (str value_name) (core_type value_type))
  | Signature_type { type_name; type_parameters = parameters; manifest } ->
      let type_declaration =
        match manifest with
        | None ->
            Ast_helper.Type.mk ~loc ~params:(type_parameters parameters)
              (str type_name)
        | Some manifest ->
            Ast_helper.Type.mk ~loc ~params:(type_parameters parameters)
              ~manifest:(core_type manifest) (str type_name)
      in
      Ast_helper.Sig.type_ ~loc Nonrecursive [ type_declaration ]
  | Signature_module { module_name; module_signature; _ } ->
      let module_type =
        Ast_helper.Mty.ident ~loc (lid (longident_of_string module_signature))
      in
      Ast_helper.Sig.module_ ~loc
        (Ast_helper.Md.mk ~loc (Location.mkloc (Some module_name) loc) module_type)
  | Signature_include { module_signature } ->
      let module_type =
        Ast_helper.Mty.ident ~loc (lid (longident_of_string module_signature))
      in
      Ast_helper.Sig.include_ ~loc (Ast_helper.Incl.mk ~loc module_type)

let module_signature_definition signature_name items =
  let module_type =
    Ast_helper.Mty.signature ~loc (List.map signature_item items)
  in
  Ast_helper.Str.modtype ~loc
    (Ast_helper.Mtd.mk ~loc ~typ:module_type (str signature_name))

let set_module_definition module_name element_ty =
  let type_declaration =
    Ast_helper.Type.mk ~loc ~manifest:(core_type element_ty) (str "t")
  in
  let compare_binding =
    Ast_helper.Vb.mk ~loc (Ast_helper.Pat.var ~loc (str "compare"))
      (Ast_helper.Exp.ident ~loc (lid (longident_of_string "Stdlib.compare")))
  in
  let comparator =
    Ast_helper.Mod.structure ~loc
      [ Ast_helper.Str.type_ ~loc Nonrecursive [ type_declaration ];
        Ast_helper.Str.value ~loc Nonrecursive [ compare_binding ] ]
  in
  let set_make =
    Ast_helper.Mod.ident ~loc (lid (longident_of_string "Set.Make"))
  in
  let module_expr = Ast_helper.Mod.apply ~loc set_make comparator in
  let module_binding =
    Ast_helper.Mb.mk ~loc (Location.mkloc (Some module_name) loc) module_expr
  in
  Ast_helper.Str.module_ ~loc module_binding

let record_definition var_name type_name set_module_name fields values =
  let type_item = record_type_definition type_name [] fields in
  let set_item =
    set_module_definition set_module_name
      (Types.named_record ~type_name ~set_module_name fields)
  in
  match record_values_to_parsetree var_name values with
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
        [ type_item;
          set_item;
          Ast_helper.Str.value ~loc Nonrecursive [ value_binding ] ]

let value_pattern = function
  | Named name -> Ast_helper.Pat.var ~loc (str name)
  | Unit_pattern ->
      Ast_helper.Pat.construct ~loc (lid (Longident.Lident "()")) None
  | Ignore_pattern -> Ast_helper.Pat.any ~loc ()

let value_binding pattern expression =
  let context =
    match pattern with
    | Named name -> "value " ^ name
    | Unit_pattern -> "top-level effect"
    | Ignore_pattern -> "top-level expression"
  in
  match Ocaml_ir.to_parsetree ~context expression with
  | Error _ as err -> err
  | Ok expression ->
      let binding =
        Ast_helper.Vb.mk ~loc (value_pattern pattern) expression
      in
      Ok [ Ast_helper.Str.value ~loc Nonrecursive [ binding ] ]

let rec structure_of_item = function
  | Value_binding { pattern; expression } ->
      value_binding pattern expression
  | Comment _ -> Ok []
  | Type_def { type_name; type_parameters; fields } ->
      Ok [ record_type_definition type_name type_parameters fields ]
  | Type_alias { type_name; type_parameters; manifest } ->
      Ok [ type_alias_definition type_name type_parameters manifest ]
  | Type_variant { type_name; type_parameters; constructors } ->
      Ok [ type_variant_definition type_name type_parameters constructors ]
  | Group items -> structure_of_items items
  | Module_def { module_name; signature_name; items } -> (
      match structure_of_items items with
      | Error _ as err -> err
      | Ok body ->
          let module_expr =
            let structure = Ast_helper.Mod.structure ~loc body in
            match signature_name with
            | None -> structure
            | Some signature_name ->
                Ast_helper.Mod.constraint_ ~loc structure
                  (Ast_helper.Mty.ident ~loc
                     (lid (longident_of_string signature_name)))
          in
          let module_binding =
            Ast_helper.Mb.mk ~loc (Location.mkloc (Some module_name) loc) module_expr
          in
          Ok [ Ast_helper.Str.module_ ~loc module_binding ])
  | Module_alias { alias_name; target_name } ->
      let module_expr =
        Ast_helper.Mod.ident ~loc (lid (longident_of_string target_name))
      in
      let module_binding =
        Ast_helper.Mb.mk ~loc (Location.mkloc (Some alias_name) loc) module_expr
      in
      Ok [ Ast_helper.Str.module_ ~loc module_binding ]
  | Module_functor { functor_name; parameters; items } -> (
      match structure_of_items items with
      | Error _ as err -> err
      | Ok body ->
          let module_expr =
            List.fold_right
              (fun (parameter_name, parameter_signature) body ->
                let parameter =
                  Parsetree.Named
                    ( Location.mkloc (Some parameter_name) loc,
                      Ast_helper.Mty.ident ~loc
                        (lid (longident_of_string parameter_signature)) )
                in
                Ast_helper.Mod.functor_ ~loc parameter body)
              parameters (Ast_helper.Mod.structure ~loc body)
          in
          let module_binding =
            Ast_helper.Mb.mk ~loc (Location.mkloc (Some functor_name) loc)
              module_expr
          in
          Ok [ Ast_helper.Str.module_ ~loc module_binding ])
  | Module_apply { module_name; functor_name; argument_names } ->
      let module_expr =
        List.fold_left
          (fun applied_functor argument_name ->
            Ast_helper.Mod.apply ~loc applied_functor
              (Ast_helper.Mod.ident ~loc
                 (lid (longident_of_string argument_name))))
          (Ast_helper.Mod.ident ~loc (lid (longident_of_string functor_name)))
          argument_names
      in
      let module_binding =
        Ast_helper.Mb.mk ~loc (Location.mkloc (Some module_name) loc) module_expr
      in
      Ok [ Ast_helper.Str.module_ ~loc module_binding ]
  | Module_signature { signature_name; items } ->
      Ok [ module_signature_definition signature_name items ]
  | Open_module module_name ->
      let module_expr =
        Ast_helper.Mod.ident ~loc (lid (longident_of_string module_name))
      in
      Ok [ Ast_helper.Str.open_ ~loc (Ast_helper.Opn.mk ~loc module_expr) ]
  | Include_module module_name ->
      let module_expr =
        Ast_helper.Mod.ident ~loc (lid (longident_of_string module_name))
      in
      Ok [ Ast_helper.Str.include_ ~loc (Ast_helper.Incl.mk ~loc module_expr) ]
  | Record_def { var_name; type_name; set_module_name; fields; values } ->
      record_definition var_name type_name set_module_name fields values

and structure_of_items items =
  let rec loop acc = function
    | [] -> Ok (List.concat (List.rev acc))
    | item :: rest -> (
        match structure_of_item item with
        | Error _ as err -> err
        | Ok structure -> loop (structure :: acc) rest)
  in
  loop [] items

let relocate_structure location structure =
  let mapper =
    { Ast_mapper.default_mapper with
      location =
        (fun _mapper current -> if current.loc_ghost then location else current);
    }
  in
  mapper.structure mapper structure

let structure_of_located_items items =
  let rec loop acc = function
    | [] -> Ok (List.concat (List.rev acc))
    | (location, item) :: rest -> (
        match structure_of_item item with
        | Error _ as err -> err
        | Ok structure ->
            loop (relocate_structure location structure :: acc) rest)
  in
  loop [] items

let print_implementation structure =
  Format.asprintf "%a@." Pprintast.structure structure
