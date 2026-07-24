open Ast

let omitted_reader_form = "\000lg-reader-omitted"
let spliced_reader_form = "\000lg-reader-spliced"

let is_omitted_reader_form located =
  located.form = FSymbol omitted_reader_form

let spliced_reader_forms located =
  match located.form with
  | FList (FSymbol marker :: _) when marker = spliced_reader_form ->
      Some located.children
  | _ -> None

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
  | { desc = Symbol "#_"; span = reader_span } :: rest -> (
      match parse_present ~target rest with
      | Error _ -> error_at reader_span "reader discard expects a form"
      | Ok (discarded, rest) ->
          Ok
            ( located (FSymbol omitted_reader_form)
                { start_offset = reader_span.start_offset;
                  end_offset = discarded.span.end_offset;
                },
              rest ))
  | { desc = Symbol "#?"; span = reader_span }
    :: { desc = Lparen; span = open_span }
    :: rest ->
      Result.bind
        (parse_until ~target Rparen open_span "reader conditional; expected ')'"
           [] rest) (fun (forms, close_span, rest) ->
          select_reader_conditional target reader_span close_span forms
          |> Result.map (fun selected -> (selected, rest)))
  | { desc = Symbol "#?@"; span = reader_span }
    :: { desc = Lparen; span = open_span }
    :: rest ->
      Result.bind
        (parse_until ~target Rparen open_span
           "splicing reader conditional; expected ')'" [] rest)
        (fun (forms, close_span, rest) ->
          Result.bind
            (select_reader_conditional target reader_span close_span forms)
            (fun selected ->
                 let forms =
                   match selected.form with
                   | FSymbol omitted when omitted = omitted_reader_form -> Ok []
                   | FList _ | FVector _ -> Ok selected.children
                   | _ ->
                       error_at selected.span
                         "splicing reader conditional must select a list or vector"
                 in
                 Result.map
                   (fun forms ->
                     ( located ~children:forms
                         (FList
                            (FSymbol spliced_reader_form
                            :: List.map (fun form -> form.form) forms))
                         {
                           start_offset = reader_span.start_offset;
                           end_offset = close_span.end_offset;
                         },
                       rest ))
                   forms))
  | { desc = Quote; span } :: rest ->
      parse_reader_prefix ~target span "quote" rest
  | { desc = Syntax_quote; span } :: rest ->
      parse_reader_prefix ~target span "syntax-quote" rest
  | { desc = Unquote; span } :: rest ->
      parse_reader_prefix ~target span "unquote" rest
  | { desc = Unquote_splicing; span } :: rest ->
      parse_reader_prefix ~target span "unquote-splicing" rest
  | { desc = Deref; span } :: rest ->
      parse_reader_prefix ~target span "deref" rest
  | { desc = Var_quote value; span } :: rest ->
      let symbol = located (FSymbol value) span in
      let head = located (FSymbol "__lg-var-quote") span in
      let children = [ head; symbol ] in
      Ok
        ( located ~children
            (FList (List.map (fun child -> child.form) children))
            span,
          rest )
  | { desc = Symbol value; span } :: rest ->
      Ok (located (FSymbol value) span, rest)
  | { desc = Keyword value; span } :: rest ->
      Ok (located (FKeyword value) span, rest)
  | { desc = String value; span } :: rest ->
      Ok (located (FString value) span, rest)
  | { desc = Regex value; span } :: rest ->
      Ok (located (FRegex value) span, rest)
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
  | { desc = Anon_lparen; span = open_span } :: rest ->
      Result.bind
        (parse_until ~target Rparen open_span
           "anonymous function; expected ')'" [] rest)
        (fun (forms, close_span, rest) ->
          anonymous_function open_span close_span forms
          |> Result.map (fun form -> (form, rest)))
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
      Result.bind
        (parse_until ~target Rbrace open_span "map; expected '}'" [] rest)
        (fun (forms, close_span, rest) ->
          map_of_forms open_span close_span forms
          |> Result.map (fun form -> (form, rest)))
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

and parse_reader_prefix ~target prefix_span name tokens =
  match parse_one ~target tokens with
  | Error _ as err -> err
  | Ok (value, rest) ->
      let head = located (FSymbol name) prefix_span in
      let children = [ head; value ] in
      let span =
        {
          start_offset = prefix_span.start_offset;
          end_offset = value.span.end_offset;
        }
      in
      Ok
        ( located ~children
            (FList (List.map (fun child -> child.form) children))
            span,
          rest )

and anonymous_function open_span close_span forms =
  let span =
    {
      start_offset = open_span.start_offset;
      end_offset = close_span.end_offset;
    }
  in
  let rec highest_parameter acc (form : located_form) =
    let own =
      match form.form with
      | FSymbol "%" -> max acc 1
      | FSymbol name
        when String.length name > 1 && name.[0] = '%' -> (
          match
            int_of_string_opt (String.sub name 1 (String.length name - 1))
          with
          | Some index when index > 0 -> max acc index
          | _ -> acc)
      | _ -> acc
    in
    List.fold_left highest_parameter own form.children
  in
  let parameter_count = List.fold_left highest_parameter 0 forms in
  let rec parameters index acc =
    if index = 0 then acc
    else parameters (index - 1) (FSymbol ("%" ^ string_of_int index) :: acc)
  in
  let rewrite_symbol = function
    | FSymbol "%" -> FSymbol "%1"
    | form -> form
  in
  let rec rewrite (form : located_form) =
    let children = List.map rewrite form.children in
    let rec map_pairs pairs = function
      | key :: value :: rest ->
          map_pairs ((key.form, value.form) :: pairs) rest
      | [] -> List.rev pairs
      | [ _ ] -> assert false
    in
    let rewritten_form =
      match rewrite_symbol form.form with
      | FList _ -> FList (List.map (fun child -> child.form) children)
      | FVector _ -> FVector (List.map (fun child -> child.form) children)
      | FMap _ -> FMap (map_pairs [] children)
      | rewritten -> rewritten
    in
    { form with form = rewritten_form; children }
  in
  let forms = List.map rewrite forms in
  let body = located ~children:forms (FList (List.map (fun form -> form.form) forms)) span in
  let params = located (FVector (parameters parameter_count [])) span in
  let head = located (FSymbol "fn") open_span in
  let children = [ head; params; body ] in
  Ok (located ~children (FList (List.map (fun form -> form.form) children)) span)

and parse_until ~target closing open_span description acc = function
  | [] -> error_at open_span ("unterminated " ^ description)
  | { desc; span } :: rest when desc = closing -> Ok (List.rev acc, span, rest)
  | tokens -> (
      match parse_one ~target tokens with
      | Ok (form, rest) when is_omitted_reader_form form ->
          parse_until ~target closing open_span description acc rest
      | Ok (form, rest) -> (
          match spliced_reader_forms form with
          | Some forms ->
              parse_until ~target closing open_span description
                (List.rev_append forms acc) rest
          | None ->
              parse_until ~target closing open_span description (form :: acc)
                rest)
      | Error _ as err -> err)

and map_of_forms open_span close_span forms =
  let rec pairs acc = function
    | [] -> Ok (List.rev acc)
    | key :: value :: rest -> pairs ((key, value) :: acc) rest
    | [ _ ] -> Error.error "map literal requires an even number of forms"
  in
  Result.map
    (fun pairs ->
      located
        ~children:(pairs |> List.concat_map (fun (key, value) -> [ key; value ]))
        (FMap (pairs |> List.map (fun (key, value) -> (key.form, value.form))))
        {
          start_offset = open_span.start_offset;
          end_offset = close_span.end_offset;
        })
    (pairs [] forms)

and parse_present ~target tokens =
  match parse_one ~target tokens with
  | Ok (form, rest) when is_omitted_reader_form form ->
      parse_present ~target rest
  | result -> result

and select_reader_conditional target reader_span close_span forms =
  let conditional_span =
    {
      start_offset = reader_span.start_offset;
      end_offset = close_span.end_offset;
    }
  in
  let metadata_annotation metadata =
    match metadata.form with
    | FMap entries -> (
        match List.assoc_opt (FKeyword ":tag") entries with
        | Some (FString tag | FSymbol tag | FKeyword tag) ->
            Some (located (FSymbol ("^" ^ tag)) metadata.span)
        | Some _ | None -> None)
    | _ -> None
  in
  let branch_value value rest =
    match (value.form, rest) with
    | FSymbol "^", metadata :: actual :: rest ->
        let forms =
          match metadata_annotation metadata with
          | Some annotation -> [ annotation; actual ]
          | None -> [ actual ]
        in
        Ok (forms, rest)
    | FSymbol metadata, actual :: rest
      when String.starts_with ~prefix:"^" metadata ->
        Ok ([ value; actual ], rest)
    | FSymbol "^", _ ->
        error_at value.span "reader conditional metadata expects a form"
    | _ -> Ok ([ value ], rest)
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
            Result.bind (branch_value value rest) (fun (value, rest) ->
                collect (name :: seen) ((name, value) :: branches) rest)
        | _ ->
            error_at feature.span "reader conditional feature must be a keyword"
        )
  in
  Result.bind (collect [] [] forms) (fun branches ->
      let selected_form = function
        | [ selected ] -> Ok selected
        | selected ->
            Ok
              (located ~children:selected
                 (FList
                    (FSymbol spliced_reader_form
                    :: List.map (fun form -> form.form) selected))
                 conditional_span)
      in
      let selected_features = Target.reader_features target in
      let selected =
        List.find_map
          (fun feature -> List.assoc_opt feature branches)
          selected_features
      in
      match selected with
      | Some selected -> selected_form selected
      | None -> (
          match List.assoc_opt ":default" branches with
          | Some selected -> selected_form selected
          | None -> Ok (located (FSymbol omitted_reader_form) conditional_span)))

let parse_located ?(target = Target.default) tokens =
  let rec loop forms = function
    | [] -> Ok (List.rev forms)
    | tokens -> (
        match parse_one ~target tokens with
        | Ok (form, rest) when is_omitted_reader_form form -> loop forms rest
        | Ok (form, rest) -> loop (form :: forms) rest
        | Error _ as err -> err)
  in
  loop [] tokens

let parse_located_recovering ?(target = Target.default) tokens =
  let rec loop forms = function
    | [] -> (List.rev forms, None)
    | tokens -> (
        match parse_one ~target tokens with
        | Ok (form, rest) when is_omitted_reader_form form -> loop forms rest
        | Ok (form, rest) -> loop (form :: forms) rest
        | Error error -> (List.rev forms, Some error))
  in
  loop [] tokens

let parse ?(target = Target.default) tokens =
  parse_located ~target tokens
  |> Result.map (List.map (fun located -> located.form))
