type complete = {
  source : string;
  remaining : string;
}

type result =
  | Empty
  | Complete of complete
  | Incomplete
  | Invalid of Lg.Compiler.compile_error

let incomplete_message message =
  String.starts_with ~prefix:"unterminated" message
  || String.ends_with ~suffix:"expects a form" message

let classify_error (error : Lg.Compiler.compile_error) =
  let unfinished_syntax =
    match error.phase with
    | `Lexing | `Parsing ->
        String.starts_with ~prefix:"UNFINISHED " error.title
        || error.title = "MISSING FORM"
    | _ -> false
  in
  if unfinished_syntax || incomplete_message error.message then Incomplete
  else Invalid error

let read source =
  match Lg.Lexer.tokenize source with
  | Error error -> classify_error error
  | Ok [] -> Empty
  | Ok tokens -> (
      match
        Lg.Parser.parse_present ~target:Lg.Target.Native
          ~reader_features:(Lg.Target.reader_features Lg.Target.Native)
          tokens
      with
      | Error error -> classify_error error
      | Ok (form, remaining_tokens) ->
          let first_token = List.hd tokens in
          let start_offset = first_token.Lg.Ast.span.start_offset in
          let end_offset = form.Lg.Ast.span.end_offset in
          let remaining_offset =
            match remaining_tokens with
            | [] -> String.length source
            | token :: _ -> token.Lg.Ast.span.start_offset
          in
          Complete
            {
              source =
                String.sub source start_offset (end_offset - start_offset);
              remaining =
                String.sub source remaining_offset
                  (String.length source - remaining_offset);
            })
