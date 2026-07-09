open Ast

let rec parse_one = function
  | Symbol value :: rest -> Ok (FSymbol value, rest)
  | Keyword value :: rest -> Ok (FKeyword value, rest)
  | String value :: rest -> Ok (FString value, rest)
  | Int value :: rest -> Ok (FInt value, rest)
  | Bool value :: rest -> Ok (FBool value, rest)
  | Nil :: rest -> Ok (FNil, rest)
  | Lparen :: rest ->
      parse_until Rparen [] rest
      |> Result.map (fun (forms, rest) -> (FList forms, rest))
  | Lbracket :: rest ->
      parse_until Rbracket [] rest
      |> Result.map (fun (forms, rest) -> (FVector forms, rest))
  | Lbrace :: rest -> parse_map [] rest
  | [] -> Error.error "expected form"
  | Rparen :: _ -> Error.error "unexpected ')'"
  | Rbracket :: _ -> Error.error "unexpected ']'"
  | Rbrace :: _ -> Error.error "unexpected '}'"

and parse_until closing acc = function
  | [] -> Error.error "unterminated collection"
  | token :: rest when token = closing -> Ok (List.rev acc, rest)
  | tokens -> (
      match parse_one tokens with
      | Ok (form, rest) -> parse_until closing (form :: acc) rest
      | Error _ as err -> err)

and parse_map acc = function
  | Rbrace :: rest -> Ok (FMap (List.rev acc), rest)
  | [] -> Error.error "unterminated map"
  | tokens -> (
      match parse_one tokens with
      | Error _ as err -> err
      | Ok (key, rest) -> (
          match parse_one rest with
          | Error _ -> Error.error "map literal requires an even number of forms"
          | Ok (value, rest) -> parse_map ((key, value) :: acc) rest))

let parse tokens =
  let rec loop forms = function
    | [] -> Ok (List.rev forms)
    | tokens -> (
        match parse_one tokens with
        | Ok (form, rest) -> loop (form :: forms) rest
        | Error _ as err -> err)
  in
  loop [] tokens
