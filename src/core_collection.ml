open Types

let one_arg name args =
  match args with
  | [ arg ] -> Ok arg
  | _ -> Error.error (name ^ " expects 1 arguments")

let two_args name args =
  match args with
  | [ left; right ] -> Ok (left, right)
  | _ -> Error.error (name ^ " expects count and collection")

let count collection =
  match collection.ty with
  | TList _ | TSet _ -> Ok (typed TInt ("List.length (" ^ collection.code ^ ")"))
  | TVector _ -> Ok (typed TInt ("Rrbvec.length (" ^ collection.code ^ ")"))
  | TRecord fields -> Ok (typed TInt (string_of_int (List.length fields)))
  | TString -> Ok (typed TInt ("String.length (" ^ collection.code ^ ")"))
  | _ -> Error.error "count expects a collection or string"

let first collection =
  match collection.ty with
  | TList inner | TSet inner -> Ok (typed inner ("List.hd (" ^ collection.code ^ ")"))
  | TVector inner -> Ok (typed inner ("Rrbvec.nth (" ^ collection.code ^ ") 0"))
  | _ -> Error.error "first expects a list, vector, or set"

let second collection =
  match collection.ty with
  | TList inner | TSet inner -> Ok (typed inner ("List.nth (" ^ collection.code ^ ") 1"))
  | TVector inner -> Ok (typed inner ("Rrbvec.nth (" ^ collection.code ^ ") 1"))
  | _ -> Error.error "second expects a list, vector, or set"

let last collection =
  match collection.ty with
  | TList inner | TSet inner ->
      Ok (typed inner ("List.hd (List.rev (" ^ collection.code ^ "))"))
  | TVector inner ->
      Ok (typed inner ("Option.get (Rrbvec.peek_back (" ^ collection.code ^ "))"))
  | _ -> Error.error "last expects a list, vector, or set"

let peek collection =
  match collection.ty with
  | TList inner -> Ok (typed inner ("List.hd (" ^ collection.code ^ ")"))
  | TVector inner ->
      Ok (typed inner ("Option.get (Rrbvec.peek_back (" ^ collection.code ^ "))"))
  | _ -> Error.error "peek expects a list or vector"

let pop collection =
  match collection.ty with
  | TList _ -> Ok (typed collection.ty ("List.tl (" ^ collection.code ^ ")"))
  | TVector _ ->
      Ok
        (typed collection.ty
           ("snd (Option.get (Rrbvec.pop_back (" ^ collection.code ^ ")))"))
  | _ -> Error.error "pop expects a list or vector"

let rest collection =
  match collection.ty with
  | TList _ | TSet _ ->
      Ok
        (typed collection.ty
           ("(match " ^ collection.code ^ " with [] -> [] | _ :: rest -> rest)"))
  | TVector _ ->
      Ok
        (typed collection.ty
           ("Rrbvec.of_list (match Rrbvec.to_list " ^ collection.code
          ^ " with [] -> [] | _ :: rest -> rest)"))
  | _ -> Error.error "rest expects a list, vector, or set"

let seq collection =
  match collection.ty with
  | TList _ | TVector _ | TSet _ -> Ok collection
  | _ -> Error.error "seq expects a collection"

let empty_question collection =
  match collection.ty with
  | TList _ | TSet _ -> Ok (typed TBool ("((" ^ collection.code ^ ") = [])"))
  | TVector _ -> Ok (typed TBool ("Rrbvec.is_empty " ^ collection.code))
  | TString -> Ok (typed TBool ("(" ^ collection.code ^ " = \"\")"))
  | _ -> Error.error "empty? expects a collection or string"

let empty collection =
  match collection.ty with
  | TList _ | TSet _ -> Ok (typed collection.ty "[]")
  | TVector _ -> Ok (typed collection.ty "Rrbvec.empty")
  | TString -> Ok (typed TString {|""|})
  | _ -> Error.error "empty expects a collection or string"

let take_list_code count_code list_code =
  "(let rec take n xs = if n <= 0 then [] else match xs with [] -> [] | x :: rest -> x :: take (n - 1) rest in take ("
  ^ count_code ^ ") (" ^ list_code ^ "))"

let drop_list_code count_code list_code =
  "(let rec drop n xs = if n <= 0 then xs else match xs with [] -> [] | _ :: rest -> drop (n - 1) rest in drop ("
  ^ count_code ^ ") (" ^ list_code ^ "))"

let take_drop name count collection =
  if not (Types.equal count.ty TInt) then Error.error (name ^ " count must be int")
  else
    match collection.ty with
    | TList _ ->
        let code =
          if name = "take" then take_list_code count.code collection.code
          else drop_list_code count.code collection.code
        in
        Ok (typed collection.ty code)
    | TVector _ ->
        let list_code = "Rrbvec.to_list (" ^ collection.code ^ ")" in
        let code =
          if name = "take" then take_list_code count.code list_code
          else drop_list_code count.code list_code
        in
        Ok (typed collection.ty ("Rrbvec.of_list (" ^ code ^ ")"))
    | _ -> Error.error (name ^ " expects a list or vector")

let reverse collection =
  match collection.ty with
  | TList _ -> Ok (typed collection.ty ("List.rev (" ^ collection.code ^ ")"))
  | TVector _ -> Ok (typed collection.ty ("Rrbvec.rev (" ^ collection.code ^ ")"))
  | _ -> Error.error "reverse expects a list or vector"

let compile name args =
  match name with
  | "count" | "first" | "second" | "last" | "peek" | "pop" | "rest" | "seq"
  | "empty?" | "empty" | "reverse" -> (
      match one_arg name args with
      | Error _ as err -> err
      | Ok collection -> (
          match name with
          | "count" -> count collection
          | "first" -> first collection
          | "second" -> second collection
          | "last" -> last collection
          | "peek" -> peek collection
          | "pop" -> pop collection
          | "rest" -> rest collection
          | "seq" -> seq collection
          | "empty?" -> empty_question collection
          | "empty" -> empty collection
          | "reverse" -> reverse collection
          | _ -> assert false))
  | "take" | "drop" -> (
      match two_args name args with
      | Error _ as err -> err
      | Ok (count, collection) -> take_drop name count collection)
  | _ -> Error.error ("unknown function " ^ name)
