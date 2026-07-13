let uid (declaration : Types.type_declaration) = declaration.type_uid

let location (declaration : Types.type_declaration) = declaration.type_loc

let rec arrow_parts ty =
  match Types.get_desc ty with
  | Tarrow (_, parameter, result, _) ->
      let parameters, return_type = arrow_parts result in
      (parameter :: parameters, return_type)
  | _ -> ([], ty)
