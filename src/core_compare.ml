open Types

let rec equality_code left right =
  match left.ty with
  | TRecord fields ->
      let parts =
        fields
        |> List.map (fun (field : field) ->
               let left_field =
                 {
                   ty = field.ty;
                   code = Structural_map.field_code left field;
                   ocaml_expr =
                     Ocaml_ir.Raw (Structural_map.field_code left field);
                   record_values = None;
                 }
               in
               let right_field =
                 {
                   ty = field.ty;
                   code = Structural_map.field_code right field;
                   ocaml_expr =
                     Ocaml_ir.Raw (Structural_map.field_code right field);
                   record_values = None;
                 }
               in
               equality_code left_field right_field)
      in
      if parts = [] then "true" else "(" ^ String.concat " && " parts ^ ")"
  | _ -> "(" ^ left.code ^ " = " ^ right.code ^ ")"

let pairwise_codes op args =
  let rec loop acc = function
    | left :: ((right :: _) as rest) ->
        loop (("(" ^ left.code ^ " " ^ op ^ " " ^ right.code ^ ")") :: acc) rest
    | _ -> List.rev acc
  in
  loop [] args

let pairwise_equality_codes args =
  let rec loop acc = function
    | left :: ((right :: _) as rest) -> loop (equality_code left right :: acc) rest
    | _ -> List.rev acc
  in
  loop [] args

let compile name args =
  match args with
  | [] | [ _ ] -> Ok (typed TBool (if name = "not=" then "false" else "true"))
  | first :: _ ->
      if name = "=" || name = "not=" then
        if List.for_all (fun arg -> Types.equal first.ty arg.ty) args then
          let equal_code = String.concat " && " (pairwise_equality_codes args) in
          let code = if name = "not=" then "not (" ^ equal_code ^ ")" else equal_code in
          Ok (typed TBool code)
        else Error.error (name ^ " arguments must have the same type")
      else if List.for_all (fun arg -> Types.equal arg.ty TInt) args then
        Ok (typed TBool (String.concat " && " (pairwise_codes name args)))
      else Error.error ("expected int arguments for " ^ name)
