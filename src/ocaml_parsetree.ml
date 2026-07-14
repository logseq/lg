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
  | Types.TUnknown -> Ast_helper.Typ.var ~loc "a"
  | Types.TVar name -> Ast_helper.Typ.var ~loc name
  | Types.TOcaml name ->
      Ast_helper.Typ.constr ~loc (lid (longident_of_string name)) []
  | Types.TOcaml_app (name, [ inner; container ])
    when name = Types.seqable_constraint_name ->
      let element = core_type inner in
      let container = core_type container in
      let adapter =
        Ast_helper.Typ.arrow ~loc Nolabel container
          (type_constructor "Seq.t" [ element ])
      in
      Ast_helper.Typ.tuple ~loc [ (None, adapter); (None, container) ]
  | Types.TOcaml_app (name, args) ->
      Ast_helper.Typ.constr ~loc (lid (longident_of_string name))
        (List.map core_type args)
  | Types.TTuple args ->
      Ast_helper.Typ.tuple ~loc
        (List.map (fun arg -> (None, core_type arg)) args)
  | Types.TArray inner -> type_constructor "array" [ core_type inner ]
  | Types.TRef inner -> type_constructor "ref" [ core_type inner ]
  | Types.TList inner -> type_constructor "list" [ core_type inner ]
  | Types.TSeq inner -> type_constructor "Seq.t" [ core_type inner ]
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
  | Types.TOverloaded_fn arities ->
      core_type (Types.overloaded_storage_type arities)
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
        match
          Ocaml_ir.to_parsetree ~context
            (Semantic_lowering.expression expression)
        with
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

let declaration_location = Option.value ~default:loc

let record_type_definition type_name parameters fields location =
  let declaration_loc = declaration_location location in
  let label_declarations =
    fields
    |> List.map (fun (field : Types.field) ->
           let field_loc = declaration_location field.location in
           Ast_helper.Type.field ~loc:field_loc
             (Location.mkloc field.ocaml_name field_loc)
             (core_type field.ty))
  in
  let type_declaration =
    Ast_helper.Type.mk ~loc:declaration_loc ~params:(type_parameters parameters)
      ~kind:(Ptype_record label_declarations)
      (Location.mkloc type_name declaration_loc)
  in
  Ast_helper.Str.type_ ~loc Nonrecursive [ type_declaration ]

let type_alias_definition type_name parameters manifest location =
  let declaration_loc = declaration_location location in
  let type_declaration =
    Ast_helper.Type.mk ~loc:declaration_loc ~params:(type_parameters parameters)
      ~manifest:(core_type manifest) (Location.mkloc type_name declaration_loc)
  in
  Ast_helper.Str.type_ ~loc Nonrecursive [ type_declaration ]

let type_variant_definition type_name parameters constructors location =
  let declaration_loc = declaration_location location in
  let constructor_declarations =
    constructors
    |> List.map (fun (constructor : variant_constructor) ->
           let constructor_loc =
             Option.value constructor.location ~default:loc
           in
           Ast_helper.Type.constructor ~loc:constructor_loc
             ~args:(Pcstr_tuple (List.map core_type constructor.payload_types))
             (Location.mkloc constructor.constructor_name constructor_loc))
  in
  let type_declaration =
    Ast_helper.Type.mk ~loc:declaration_loc ~params:(type_parameters parameters)
      ~kind:(Ptype_variant constructor_declarations)
      (Location.mkloc type_name declaration_loc)
  in
  Ast_helper.Str.type_ ~loc Recursive [ type_declaration ]

let signature_item = function
  | Signature_value { value_name; value_type; location; _ } ->
      let item_loc = declaration_location location in
      Ast_helper.Sig.value ~loc:item_loc
        (Ast_helper.Val.mk ~loc:item_loc (Location.mkloc value_name item_loc)
           (core_type value_type))
  | Signature_type
      { type_name; type_parameters = parameters; manifest; location } ->
      let item_loc = declaration_location location in
      let type_declaration =
        match manifest with
        | None ->
            Ast_helper.Type.mk ~loc:item_loc ~params:(type_parameters parameters)
              (Location.mkloc type_name item_loc)
        | Some manifest ->
            Ast_helper.Type.mk ~loc:item_loc ~params:(type_parameters parameters)
              ~manifest:(core_type manifest) (Location.mkloc type_name item_loc)
      in
      Ast_helper.Sig.type_ ~loc:item_loc Nonrecursive [ type_declaration ]
  | Signature_module
      { module_name; module_signature; location; signature_location; _ } ->
      let item_loc = declaration_location location in
      let signature_loc = declaration_location signature_location in
      let module_type =
        Ast_helper.Mty.ident ~loc:signature_loc
          (Location.mkloc (longident_of_string module_signature) signature_loc)
      in
      Ast_helper.Sig.module_ ~loc:item_loc
        (Ast_helper.Md.mk ~loc:item_loc
           (Location.mkloc (Some module_name) item_loc) module_type)
  | Signature_include { module_signature; signature_location } ->
      let signature_loc = declaration_location signature_location in
      let module_type =
        Ast_helper.Mty.ident ~loc:signature_loc
          (Location.mkloc (longident_of_string module_signature) signature_loc)
      in
      Ast_helper.Sig.include_ ~loc:signature_loc
        (Ast_helper.Incl.mk ~loc:signature_loc module_type)

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

let node_id_attribute node_id =
  let payload =
    Parsetree.PStr
      [ Ast_helper.Str.eval
          (Ast_helper.Exp.constant
             (Ast_helper.Const.string (Source_node_id.to_string node_id))) ]
  in
  Ast_helper.Attr.mk (str "lg.node_id") payload

let record_definition var_name identity type_name set_module_name fields values =
  let type_item = record_type_definition type_name [] fields None in
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
        let pattern = Ast_helper.Pat.var ~loc (str var_name) in
        let pattern =
          match identity with
          | None -> pattern
          | Some (node_id, location) ->
              { pattern with
                ppat_loc = location;
                ppat_attributes =
                  node_id_attribute node_id :: pattern.ppat_attributes;
              }
        in
        Ast_helper.Vb.mk ~loc
          pattern
          annotated_expr
      in
      Ok
        [ type_item;
          set_item;
          Ast_helper.Str.value ~loc Nonrecursive [ value_binding ] ]

let projected_record_definition var_name identity type_name set_module_name fields
    source =
  let type_item = record_type_definition type_name [] fields None in
  let set_item =
    set_module_definition set_module_name
      (Types.named_record ~type_name ~set_module_name fields)
  in
  match
    Ocaml_ir.to_parsetree ~context:("record source " ^ var_name)
      (Semantic_lowering.expression source)
  with
  | Error _ as err -> err
  | Ok source_expr ->
      let source_name = "__lg_record_source" in
      let source_ident = Ast_helper.Exp.ident ~loc (lid (Longident.Lident source_name)) in
      let projected_fields =
        List.map
          (fun (field : Types.field) ->
            let label = lid (Longident.Lident field.ocaml_name) in
            (label, Ast_helper.Exp.field ~loc source_ident label))
          fields
      in
      let record_expr = Ast_helper.Exp.record ~loc projected_fields None in
      let projected_expr =
        Ast_helper.Exp.let_ ~loc Nonrecursive
          [ Ast_helper.Vb.mk ~loc
              (Ast_helper.Pat.var ~loc (str source_name))
              source_expr ]
          record_expr
        |> fun expression ->
        Ast_helper.Exp.constraint_ ~loc expression (type_constructor type_name [])
      in
      let pattern = Ast_helper.Pat.var ~loc (str var_name) in
      let pattern =
        match identity with
        | None -> pattern
        | Some (node_id, location) ->
            { pattern with
              ppat_loc = location;
              ppat_attributes = node_id_attribute node_id :: pattern.ppat_attributes;
            }
      in
      let value_binding = Ast_helper.Vb.mk ~loc pattern projected_expr in
      Ok
        [ type_item;
          set_item;
          Ast_helper.Str.value ~loc Nonrecursive [ value_binding ] ]

let rec value_pattern = function
  | Named name -> Ast_helper.Pat.var ~loc (str name)
  | Unit_pattern ->
      Ast_helper.Pat.construct ~loc (lid (Longident.Lident "()")) None
  | Ignore_pattern -> Ast_helper.Pat.any ~loc ()
  | Located_value (node_id, location, pattern) ->
      let pattern = value_pattern pattern in
      {
        pattern with
        ppat_loc = location;
        ppat_attributes = node_id_attribute node_id :: pattern.ppat_attributes;
      }

let rec value_pattern_context = function
  | Named name -> "value " ^ name
  | Unit_pattern -> "top-level effect"
  | Ignore_pattern -> "top-level expression"
  | Located_value (_, _, pattern) -> value_pattern_context pattern

let value_binding pattern expression =
  let context =
    value_pattern_context pattern
  in
  match
    Ocaml_ir.to_parsetree ~context (Semantic_lowering.expression expression)
  with
  | Error _ as err -> err
  | Ok expression ->
      let binding =
        Ast_helper.Vb.mk ~loc (value_pattern pattern) expression
      in
      Ok [ Ast_helper.Str.value ~loc Nonrecursive [ binding ] ]

let recursive_value_binding name identity expression =
  match
    Ocaml_ir.to_parsetree ~context:("recursive value " ^ name)
      (Semantic_lowering.expression expression)
  with
  | Error _ as err -> err
  | Ok expression ->
      let pattern =
        match identity with
        | None -> Named name
        | Some (node_id, location) ->
            Located_value (node_id, location, Named name)
      in
      let binding =
        Ast_helper.Vb.mk ~loc (value_pattern pattern) expression
      in
      Ok [ Ast_helper.Str.value ~loc Recursive [ binding ] ]

let recursive_value_bindings bindings =
  let rec compile acc = function
    | [] -> Ok (List.rev acc)
    | (binding : recursive_value) :: rest -> (
        match
          Ocaml_ir.to_parsetree ~context:("recursive value " ^ binding.name)
            (Semantic_lowering.expression binding.expression)
        with
        | Error _ as err -> err
        | Ok expression ->
            let pattern =
              match binding.identity with
              | None -> Named binding.name
              | Some (node_id, location) ->
                  Located_value (node_id, location, Named binding.name)
            in
            compile
              (Ast_helper.Vb.mk ~loc (value_pattern pattern) expression :: acc)
              rest)
  in
  match compile [] bindings with
  | Error _ as err -> err
  | Ok bindings -> Ok [ Ast_helper.Str.value ~loc Recursive bindings ]

let rec structure_of_item = function
  | Value_binding { pattern; expression } ->
      value_binding pattern expression
  | Recursive_value_binding { name; identity; expression } ->
      recursive_value_binding name identity expression
  | Recursive_value_bindings bindings -> recursive_value_bindings bindings
  | Comment _ -> Ok []
  | Type_def { type_name; type_parameters; fields; location } ->
      Ok [ record_type_definition type_name type_parameters fields location ]
  | Type_alias { type_name; type_parameters; manifest; location } ->
      Ok [ type_alias_definition type_name type_parameters manifest location ]
  | Type_variant { type_name; type_parameters; constructors; location } ->
      Ok
        [ type_variant_definition type_name type_parameters constructors location ]
  | Group items -> structure_of_items items
  | Module_def
      { module_name; location; signature_name; signature_location; items } -> (
      match structure_of_items items with
      | Error _ as err -> err
      | Ok body ->
          let module_expr =
            let structure = Ast_helper.Mod.structure ~loc body in
            match signature_name with
            | None -> structure
            | Some signature_name ->
                let signature_loc = declaration_location signature_location in
                Ast_helper.Mod.constraint_ ~loc structure
                  (Ast_helper.Mty.ident ~loc:signature_loc
                     (Location.mkloc (longident_of_string signature_name)
                        signature_loc))
          in
          let module_loc = declaration_location location in
          let module_binding =
            Ast_helper.Mb.mk ~loc:module_loc
              (Location.mkloc (Some module_name) module_loc) module_expr
          in
          Ok [ Ast_helper.Str.module_ ~loc module_binding ])
  | Module_alias { alias_name; location; target_name; target_location } ->
      let alias_loc = declaration_location location in
      let target_loc = declaration_location target_location in
      let module_expr =
        Ast_helper.Mod.ident ~loc:target_loc
          (Location.mkloc (longident_of_string target_name) target_loc)
      in
      let module_binding =
        Ast_helper.Mb.mk ~loc:alias_loc
          (Location.mkloc (Some alias_name) alias_loc) module_expr
      in
      Ok [ Ast_helper.Str.module_ ~loc:alias_loc module_binding ]
  | Module_functor { functor_name; location; parameters; items } -> (
      match structure_of_items items with
      | Error _ as err -> err
      | Ok body ->
          let module_expr =
            List.fold_right
              (fun parameter body ->
                let parameter_loc =
                  declaration_location parameter.parameter_location
                in
                let signature_loc =
                  declaration_location parameter.signature_location
                in
                let parameter =
                  Parsetree.Named
                    ( Location.mkloc (Some parameter.parameter_name) parameter_loc,
                      Ast_helper.Mty.ident ~loc:signature_loc
                        (Location.mkloc
                           (longident_of_string parameter.signature_name)
                           signature_loc) )
                in
                Ast_helper.Mod.functor_ ~loc:parameter_loc parameter body)
              parameters (Ast_helper.Mod.structure ~loc body)
          in
          let functor_loc = declaration_location location in
          let module_binding =
            Ast_helper.Mb.mk ~loc:functor_loc
              (Location.mkloc (Some functor_name) functor_loc) module_expr
          in
          Ok [ Ast_helper.Str.module_ ~loc:functor_loc module_binding ])
  | Module_apply
      { module_name; location; functor_name; functor_location; arguments } ->
      let functor_loc = declaration_location functor_location in
      let module_expr =
        List.fold_left
          (fun applied_functor argument ->
            let argument_loc = declaration_location argument.location in
            Ast_helper.Mod.apply ~loc:argument_loc applied_functor
              (Ast_helper.Mod.ident ~loc:argument_loc
                 (Location.mkloc (longident_of_string argument.module_name)
                    argument_loc)))
          (Ast_helper.Mod.ident ~loc:functor_loc
             (Location.mkloc (longident_of_string functor_name) functor_loc))
          arguments
      in
      let module_loc = declaration_location location in
      let module_binding =
        Ast_helper.Mb.mk ~loc:module_loc
          (Location.mkloc (Some module_name) module_loc) module_expr
      in
      Ok [ Ast_helper.Str.module_ ~loc:module_loc module_binding ]
  | Module_signature { signature_name; location; items } ->
      let signature_loc = declaration_location location in
      let module_type =
        Ast_helper.Mty.signature ~loc (List.map signature_item items)
      in
      Ok
        [ Ast_helper.Str.modtype ~loc:signature_loc
            (Ast_helper.Mtd.mk ~loc:signature_loc ~typ:module_type
               (Location.mkloc signature_name signature_loc)) ]
  | Open_module { module_name; location } ->
      let module_loc = declaration_location location in
      let module_expr =
        Ast_helper.Mod.ident ~loc:module_loc
          (Location.mkloc (longident_of_string module_name) module_loc)
      in
      Ok
        [ Ast_helper.Str.open_ ~loc:module_loc
            (Ast_helper.Opn.mk ~loc:module_loc module_expr) ]
  | Include_module { module_name; location } ->
      let module_loc = declaration_location location in
      let module_expr =
        Ast_helper.Mod.ident ~loc:module_loc
          (Location.mkloc (longident_of_string module_name) module_loc)
      in
      Ok
        [ Ast_helper.Str.include_ ~loc:module_loc
            (Ast_helper.Incl.mk ~loc:module_loc module_expr) ]
  | Record_def { var_name; identity; type_name; set_module_name; fields; values } ->
      record_definition var_name identity type_name set_module_name fields values
  | Projected_record_def
      { var_name; identity; type_name; set_module_name; fields; source } ->
      projected_record_definition var_name identity type_name set_module_name fields
        source

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
