let uid (declaration : Types.module_declaration) = declaration.md_uid
let location (declaration : Types.module_declaration) = declaration.md_loc
let type_uid (declaration : Types.modtype_declaration) = declaration.mtd_uid

let type_location (declaration : Types.modtype_declaration) =
  declaration.mtd_loc

let private_attribute = "lg.private"

let mark_private_values ~is_private structure =
  let path = ref [] in
  let attribute loc =
    Ast_helper.Attr.mk ~loc
      (Location.mkloc private_attribute loc)
      (Parsetree.PStr [])
  in
  let rec pattern_names pattern =
    match pattern.Parsetree.ppat_desc with
    | Ppat_var name -> [ name.txt ]
    | Ppat_constraint (pattern, _) | Ppat_alias (pattern, _) ->
        pattern_names pattern
    | _ -> []
  in
  let mapper =
    {
      Ast_mapper.default_mapper with
      module_binding =
        (fun mapper binding ->
          let previous = !path in
          path := previous @ Option.to_list binding.Parsetree.pmb_name.txt;
          Fun.protect
            ~finally:(fun () -> path := previous)
            (fun () -> Ast_mapper.default_mapper.module_binding mapper binding));
      value_binding =
        (fun mapper binding ->
          let binding =
            Ast_mapper.default_mapper.value_binding mapper binding
          in
          if
            List.exists
              (fun name ->
                let full = String.concat "." (!path @ [ name ]) in
                is_private full)
              (pattern_names binding.Parsetree.pvb_pat)
          then
            let rec mark pattern =
              let pattern =
                {
                  pattern with
                  Parsetree.ppat_attributes =
                    attribute pattern.Parsetree.ppat_loc
                    :: pattern.ppat_attributes;
                }
              in
              match pattern.ppat_desc with
              | Ppat_constraint (inner, ty) ->
                  { pattern with ppat_desc = Ppat_constraint (mark inner, ty) }
              | _ -> pattern
            in
            { binding with pvb_pat = mark binding.pvb_pat }
          else binding);
    }
  in
  mapper.structure mapper structure

let public_signature ?compiler_env ?(is_private = fun _ -> false) signature =
  let rec filter path signature =
    let changed = ref false in
    let items = List.filter_map (fun value ->
      let filtered, item_changed = item path value in
      changed := !changed || item_changed;
      filtered) signature in
    (items, !changed)
  and module_type path ty = match ty with
    | Types.Mty_signature signature ->
        let signature, changed = filter path signature in
        (Types.Mty_signature signature, changed)
    | Types.Mty_functor (parameter, result) ->
        let result, changed = module_type path result in
        (Types.Mty_functor (parameter, result), changed)
    | Types.Mty_ident _ ->
        (match compiler_env with
         | None -> (ty, false)
         | Some env ->
             let expanded = Mtype.scrape env ty in
             (match expanded with
              | Types.Mty_ident _ -> (ty, false)
              | _ ->
                  let expanded, changed = module_type path expanded in
                  (if changed then expanded else ty), changed))
    | Types.Mty_alias _ -> (ty, false)
  and item path = function
    | Types.Sig_value (id, description, _) as value ->
        if is_private (String.concat "." (path @ [Ident.name id]))
           || List.exists (fun attribute -> attribute.Parsetree.attr_name.txt = private_attribute)
                description.Types.val_attributes
        then (None, true) else (Some value, false)
    | Types.Sig_module (id, presence, declaration, recursion, visibility) ->
        let md_type, changed = module_type (path @ [Ident.name id]) declaration.md_type in
        (Some (Types.Sig_module (id, presence, {declaration with md_type}, recursion, visibility)), changed)
    | item -> (Some item, false)
  in
  fst (filter [] signature)
