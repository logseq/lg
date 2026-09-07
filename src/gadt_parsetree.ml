open Parsetree

let marker attribute = attribute.attr_name.txt = "lg.gadt_scope"

let binding binding =
  let found = ref false in
  let scan =
    {
      Ast_iterator.default_iterator with
      expr =
        (fun self expression ->
          if List.exists marker expression.pexp_attributes then found := true;
          Ast_iterator.default_iterator.expr self expression);
    }
  in
  scan.expr scan binding.pvb_expr;
  match (!found, binding.pvb_constraint) with
  | ( true,
      Some
        (Pvc_constraint
           {
             locally_abstract_univars = [];
             typ = { ptyp_desc = Ptyp_poly (variables, body); _ };
           }) )
    when variables <> [] ->
      let names = List.map (fun variable -> variable.Location.txt) variables in
      let rec mapper names =
        {
          Ast_mapper.default_mapper with
          typ =
            (fun self ty ->
              match ty.ptyp_desc with
              | Ptyp_var name when List.mem name names ->
                  {
                    ty with
                    ptyp_desc =
                      Ptyp_constr
                        (Location.mkloc (Longident.Lident name) ty.ptyp_loc, []);
                  }
              | Ptyp_poly (bound, _) ->
                  let nested =
                    mapper
                      (List.filter
                         (fun name ->
                           not
                             (List.exists
                                (fun variable -> variable.Location.txt = name)
                                bound))
                         names)
                  in
                  Ast_mapper.default_mapper.typ nested ty
              | _ -> Ast_mapper.default_mapper.typ self ty);
          expr =
            (fun self expression ->
              let expression = Ast_mapper.default_mapper.expr self expression in
              {
                expression with
                pexp_attributes =
                  List.filter
                    (fun attribute -> not (marker attribute))
                    expression.pexp_attributes;
              });
        }
      in
      let mapper = mapper names in
      {
        binding with
        pvb_constraint =
          Some
            (Pvc_constraint
               {
                 locally_abstract_univars = variables;
                 typ = mapper.typ mapper body;
               });
        pvb_expr = mapper.expr mapper binding.pvb_expr;
      }
  | _ ->
      let cleanup =
        {
          Ast_mapper.default_mapper with
          expr =
            (fun self expression ->
              let expression = Ast_mapper.default_mapper.expr self expression in
              {
                expression with
                pexp_attributes =
                  List.filter
                    (fun attribute -> not (marker attribute))
                    expression.pexp_attributes;
              });
        }
      in
      { binding with pvb_expr = cleanup.expr cleanup binding.pvb_expr }
