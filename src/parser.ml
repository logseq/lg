open Ast

let located ?(children = []) form span = { form; span; children }

let rec parse_one = function
  | { desc = Symbol value; span } :: rest -> Ok (located (FSymbol value) span, rest)
  | { desc = Keyword value; span } :: rest -> Ok (located (FKeyword value) span, rest)
  | { desc = String value; span } :: rest -> Ok (located (FString value) span, rest)
  | { desc = Int value; span } :: rest -> Ok (located (FInt value) span, rest)
  | { desc = Float value; span } :: rest -> Ok (located (FFloat value) span, rest)
  | { desc = Char value; span } :: rest -> Ok (located (FChar value) span, rest)
  | { desc = Bool value; span } :: rest -> Ok (located (FBool value) span, rest)
  | { desc = Lparen; span = open_span } :: rest ->
      parse_until Rparen [] rest
      |> Result.map (fun (forms, close_span, rest) ->
             ( located ~children:forms
                 (FList (List.map (fun form -> form.form) forms))
                 { start_offset = open_span.start_offset;
                   end_offset = close_span.end_offset },
               rest ))
  | { desc = Lbracket; span = open_span } :: rest ->
      parse_until Rbracket [] rest
      |> Result.map (fun (forms, close_span, rest) ->
             ( located ~children:forms
                 (FVector (List.map (fun form -> form.form) forms))
                 { start_offset = open_span.start_offset;
                   end_offset = close_span.end_offset },
               rest ))
  | { desc = Lbrace; span = open_span } :: rest -> parse_map open_span [] rest
  | [] -> Error.error "expected form"
  | { desc = Rparen; _ } :: _ -> Error.error "unexpected ')'"
  | { desc = Rbracket; _ } :: _ -> Error.error "unexpected ']'"
  | { desc = Rbrace; _ } :: _ -> Error.error "unexpected '}'"

and parse_until closing acc = function
  | [] -> Error.error "unterminated collection"
  | { desc; span } :: rest when desc = closing -> Ok (List.rev acc, span, rest)
  | tokens -> (
      match parse_one tokens with
      | Ok (form, rest) -> parse_until closing (form :: acc) rest
      | Error _ as err -> err)

and parse_map open_span acc = function
  | { desc = Rbrace; span = close_span } :: rest ->
      let pairs = List.rev acc in
      Ok
        ( located
            ~children:
              (pairs
              |> List.concat_map (fun (key, value) -> [ key; value ]))
            (FMap
               (pairs
               |> List.map (fun (key, value) -> (key.form, value.form))))
            { start_offset = open_span.start_offset;
              end_offset = close_span.end_offset },
          rest )
  | [] -> Error.error "unterminated map"
  | tokens -> (
      match parse_one tokens with
      | Error _ as err -> err
      | Ok (key, rest) -> (
          match parse_one rest with
          | Error _ -> Error.error "map literal requires an even number of forms"
          | Ok (value, rest) -> parse_map open_span ((key, value) :: acc) rest))

let parse_located tokens =
  let rec loop forms = function
    | [] -> Ok (List.rev forms)
    | tokens -> (
        match parse_one tokens with
        | Ok (form, rest) -> loop (form :: forms) rest
        | Error _ as err -> err)
  in
  loop [] tokens

let parse tokens =
  parse_located tokens |> Result.map (List.map (fun located -> located.form))
