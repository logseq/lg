open Ast

let located ?(children = []) form span = { form; span; children }

let error_at span message =
  let position offset =
    { Lexing.pos_fname = ""; pos_lnum = 1; pos_bol = 0; pos_cnum = offset }
  in
  Error.error
    ~location:
      {
        Location.loc_start = position span.start_offset;
        loc_end = position span.end_offset;
        loc_ghost = false;
      }
    message

let rec parse_one ~target = function
  | { desc = Symbol "#?"; span = reader_span }
    :: { desc = Lparen; span = open_span }
    :: rest ->
      Result.bind
        (parse_until ~target Rparen open_span "reader conditional; expected ')'"
           [] rest) (fun (forms, close_span, rest) ->
          select_reader_conditional target reader_span close_span forms
          |> Result.map (fun selected -> (selected, rest)))
  | { desc = Symbol value; span } :: rest ->
      Ok (located (FSymbol value) span, rest)
  | { desc = Keyword value; span } :: rest ->
      Ok (located (FKeyword value) span, rest)
  | { desc = String value; span } :: rest ->
      Ok (located (FString value) span, rest)
  | { desc = Int value; span } :: rest -> Ok (located (FInt value) span, rest)
  | { desc = Float value; span } :: rest ->
      Ok (located (FFloat value) span, rest)
  | { desc = Char value; span } :: rest -> Ok (located (FChar value) span, rest)
  | { desc = Bool value; span } :: rest -> Ok (located (FBool value) span, rest)
  | { desc = Lparen; span = open_span } :: rest ->
      parse_until ~target Rparen open_span "list; expected ')'" [] rest
      |> Result.map (fun (forms, close_span, rest) ->
          ( located ~children:forms
              (FList (List.map (fun form -> form.form) forms))
              {
                start_offset = open_span.start_offset;
                end_offset = close_span.end_offset;
              },
            rest ))
  | { desc = Lbracket; span = open_span } :: rest ->
      parse_until ~target Rbracket open_span "vector; expected ']'" [] rest
      |> Result.map (fun (forms, close_span, rest) ->
          ( located ~children:forms
              (FVector (List.map (fun form -> form.form) forms))
              {
                start_offset = open_span.start_offset;
                end_offset = close_span.end_offset;
              },
            rest ))
  | { desc = Lbrace; span = open_span } :: rest ->
      parse_map ~target open_span [] rest
  | { desc = Set_lbrace; span = open_span } :: rest ->
      parse_until ~target Rbrace open_span "set; expected '}'" [] rest
      |> Result.map (fun (forms, close_span, rest) ->
          let head = located (FSymbol "hash-set") open_span in
          let children = head :: forms in
          ( located ~children
              (FList (List.map (fun form -> form.form) children))
              {
                start_offset = open_span.start_offset;
                end_offset = close_span.end_offset;
              },
            rest ))
  | [] -> Error.error "expected form"
  | { desc = Rparen; span } :: _ -> error_at span "unexpected ')'"
  | { desc = Rbracket; span } :: _ -> error_at span "unexpected ']'"
  | { desc = Rbrace; span } :: _ -> error_at span "unexpected '}'"

and parse_until ~target closing open_span description acc = function
  | [] -> error_at open_span ("unterminated " ^ description)
  | { desc; span } :: rest when desc = closing -> Ok (List.rev acc, span, rest)
  | tokens -> (
      match parse_one ~target tokens with
      | Ok (form, rest) ->
          parse_until ~target closing open_span description (form :: acc) rest
      | Error _ as err -> err)

and parse_map ~target open_span acc = function
  | { desc = Rbrace; span = close_span } :: rest ->
      let pairs = List.rev acc in
      Ok
        ( located
            ~children:
              (pairs |> List.concat_map (fun (key, value) -> [ key; value ]))
            (FMap
               (pairs |> List.map (fun (key, value) -> (key.form, value.form))))
            {
              start_offset = open_span.start_offset;
              end_offset = close_span.end_offset;
            },
          rest )
  | [] -> error_at open_span "unterminated map; expected '}'"
  | tokens -> (
      match parse_one ~target tokens with
      | Error _ as err -> err
      | Ok (key, rest) -> (
          match parse_one ~target rest with
          | Error _ ->
              Error.error "map literal requires an even number of forms"
          | Ok (value, rest) ->
              parse_map ~target open_span ((key, value) :: acc) rest))

and select_reader_conditional target reader_span close_span forms =
  let conditional_span =
    {
      start_offset = reader_span.start_offset;
      end_offset = close_span.end_offset;
    }
  in
  let rec collect seen branches = function
    | [] -> Ok (List.rev branches)
    | [ _ ] ->
        error_at conditional_span
          "reader conditional requires feature/form pairs"
    | feature :: value :: rest -> (
        match feature.form with
        | FKeyword name when List.mem name seen ->
            error_at feature.span
              ("duplicate reader conditional feature " ^ name)
        | FKeyword name ->
            collect (name :: seen) ((name, value) :: branches) rest
        | _ ->
            error_at feature.span "reader conditional feature must be a keyword"
        )
  in
  Result.bind (collect [] [] forms) (fun branches ->
      let selected_feature = Target.feature target in
      match List.assoc_opt selected_feature branches with
      | Some selected -> Ok selected
      | None -> (
          match List.assoc_opt ":default" branches with
          | Some selected -> Ok selected
          | None ->
              error_at conditional_span
                (Printf.sprintf
                   "reader conditional has no %s or :default branch"
                   selected_feature)))

let parse_located ?(target = Target.default) tokens =
  let rec loop forms = function
    | [] -> Ok (List.rev forms)
    | tokens -> (
        match parse_one ~target tokens with
        | Ok (form, rest) -> loop (form :: forms) rest
        | Error _ as err -> err)
  in
  loop [] tokens

let parse_located_recovering ?(target = Target.default) tokens =
  let rec loop forms = function
    | [] -> (List.rev forms, None)
    | tokens -> (
        match parse_one ~target tokens with
        | Ok (form, rest) -> loop (form :: forms) rest
        | Error error -> (List.rev forms, Some error))
  in
  loop [] tokens

let parse ?(target = Target.default) tokens =
  parse_located ~target tokens
  |> Result.map (List.map (fun located -> located.form))
