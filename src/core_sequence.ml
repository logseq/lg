open Types

let one_arg name args =
  match args with
  | [ arg ] -> Ok arg
  | _ -> Error.error (name ^ " expects 1 arguments")

let two_args name args =
  match args with
  | [ left; right ] -> Ok (left, right)
  | _ -> Error.error (name ^ " expects collection and count")

let drop_list_code count_code list_code =
  "(let rec drop n xs = if n <= 0 then xs else match xs with [] -> [] | _ :: rest -> drop (n - 1) rest in drop ("
  ^ count_code ^ ") (" ^ list_code ^ "))"

let first_expr name collection =
  match collection.ty with
  | TList inner -> Ok (typed inner ("List.hd (" ^ collection.code ^ ")"))
  | TVector inner -> Ok (typed inner ("Rrbvec.nth (" ^ collection.code ^ ") 0"))
  | _ -> Error.error (name ^ " expects a list or vector")

let next_expr name collection =
  match collection.ty with
  | TList _ ->
      Ok
        (typed collection.ty
           ("(match " ^ collection.code ^ " with [] -> [] | _ :: rest -> rest)"))
  | TVector _ ->
      Ok
        (typed collection.ty
           ("Rrbvec.of_list (match Rrbvec.to_list (" ^ collection.code
          ^ ") with [] -> [] | _ :: rest -> rest)"))
  | _ -> Error.error (name ^ " expects a list or vector")

let nth_next_expr name collection count =
  if not (Types.equal count.ty TInt) then Error.error (name ^ " count must be int")
  else
    match collection.ty with
    | TList _ ->
        Ok (typed collection.ty (drop_list_code count.code collection.code))
    | TVector _ ->
        let list_code = "Rrbvec.to_list (" ^ collection.code ^ ")" in
        Ok
          (typed collection.ty
             ("Rrbvec.of_list (" ^ drop_list_code count.code list_code ^ ")"))
    | _ -> Error.error (name ^ " expects a list or vector")

let reverse_expr name collection =
  match collection.ty with
  | TList _ -> Ok (typed collection.ty ("List.rev (" ^ collection.code ^ ")"))
  | TVector _ -> Ok (typed collection.ty ("Rrbvec.rev (" ^ collection.code ^ ")"))
  | _ -> Error.error (name ^ " expects a list or vector")

let compile name args =
  match name with
  | "next" -> (
      match one_arg name args with
      | Error _ as err -> err
      | Ok collection -> next_expr name collection)
  | "nthnext" | "nthrest" -> (
      match two_args name args with
      | Error _ as err -> err
      | Ok (collection, count) -> nth_next_expr name collection count)
  | "ffirst" -> (
      match one_arg name args with
      | Error _ as err -> err
      | Ok collection -> (
          match first_expr name collection with
          | Error _ as err -> err
          | Ok first -> first_expr name first))
  | "fnext" -> (
      match one_arg name args with
      | Error _ as err -> err
      | Ok collection -> (
          match next_expr name collection with
          | Error _ as err -> err
          | Ok next -> first_expr name next))
  | "nfirst" -> (
      match one_arg name args with
      | Error _ as err -> err
      | Ok collection -> (
          match first_expr name collection with
          | Error _ as err -> err
          | Ok first -> next_expr name first))
  | "nnext" -> (
      match one_arg name args with
      | Error _ as err -> err
      | Ok collection -> (
          match next_expr name collection with
          | Error _ as err -> err
          | Ok next -> next_expr name next))
  | "rseq" -> (
      match one_arg name args with
      | Error _ as err -> err
      | Ok collection -> reverse_expr name collection)
  | _ -> Error.error ("unknown function " ^ name)
