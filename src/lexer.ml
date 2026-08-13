open Ast

let is_space = function ' ' | '\n' | '\r' | '\t' | ',' -> true | _ -> false

let is_delimiter ch =
  is_space ch
  ||
  match ch with
  | '(' | ')' | '[' | ']' | '{' | '}' -> true
  | _ -> false

let rec skip_ignored source i =
  if i >= String.length source then i
  else if is_space source.[i] then skip_ignored source (i + 1)
  else if source.[i] = ';' then
    let rec skip_comment j =
      if j >= String.length source || source.[j] = '\n' then j
      else skip_comment (j + 1)
    in
    skip_ignored source (skip_comment i)
  else i

let read_string source start =
  let buffer = Buffer.create 16 in
  let rec loop i =
    if i >= String.length source then Error.error "unterminated string"
    else
      match source.[i] with
      | '"' -> Ok (Buffer.contents buffer, i + 1)
      | '\\' when i + 1 < String.length source -> (
          match source.[i + 1] with
          | '"' ->
              Buffer.add_char buffer '"';
              loop (i + 2)
          | '\\' ->
              Buffer.add_char buffer '\\';
              loop (i + 2)
          | 'n' ->
              Buffer.add_char buffer '\n';
              loop (i + 2)
          | 'r' ->
              Buffer.add_char buffer '\r';
              loop (i + 2)
          | ch ->
              Buffer.add_char buffer ch;
              loop (i + 2))
      | ch ->
          Buffer.add_char buffer ch;
          loop (i + 1)
  in
  loop start

let read_regex source start =
  let buffer = Buffer.create 16 in
  let rec loop i =
    if i >= String.length source then Error.error "unterminated regex"
    else
      match source.[i] with
      | '"' -> Ok (Buffer.contents buffer, i + 1)
      | '\\' when i + 1 < String.length source ->
          Buffer.add_char buffer '\\';
          Buffer.add_char buffer source.[i + 1];
          loop (i + 2)
      | ch ->
          Buffer.add_char buffer ch;
          loop (i + 1)
  in
  loop start

let read_atom source start =
  let rec loop i =
    if i >= String.length source || is_delimiter source.[i] then i
    else loop (i + 1)
  in
  let finish = loop start in
  (String.sub source start (finish - start), finish)

let char_of_atom = function
  | "\\newline" -> Some '\n'
  | "\\space" -> Some ' '
  | "\\tab" -> Some '\t'
  | atom when String.length atom = 2 && atom.[0] = '\\' -> Some atom.[1]
  | _ -> None

let looks_like_float atom =
  String.contains atom '.' || String.contains atom 'e' || String.contains atom 'E'

let strip_numeric_suffix suffix atom =
  if String.length atom > 1 && atom.[String.length atom - 1] = suffix then
    Some (String.sub atom 0 (String.length atom - 1))
  else None

let ratio_float_literal atom =
  match String.split_on_char '/' atom with
  | [ numerator; denominator ] -> (
      match (int_of_string_opt numerator, int_of_string_opt denominator) with
      | Some numerator, Some denominator when denominator <> 0 ->
          Some (string_of_float (float_of_int numerator /. float_of_int denominator))
      | _ -> None)
  | _ -> None

let tokenize source =
  let token desc start_offset end_offset =
    { desc; span = { start_offset; end_offset } }
  in
  let rec loop i tokens =
    let i = skip_ignored source i in
    if i >= String.length source then Ok (List.rev tokens)
    else
      match source.[i] with
      | '(' -> loop (i + 1) (token Lparen i (i + 1) :: tokens)
      | '\'' -> loop (i + 1) (token Quote i (i + 1) :: tokens)
      | '`' -> loop (i + 1) (token Syntax_quote i (i + 1) :: tokens)
      | '~' when i + 1 < String.length source && source.[i + 1] = '@' ->
          loop (i + 2) (token Unquote_splicing i (i + 2) :: tokens)
      | '~' -> loop (i + 1) (token Unquote i (i + 1) :: tokens)
      | '@' -> loop (i + 1) (token Deref i (i + 1) :: tokens)
      | '#' when i + 1 < String.length source && source.[i + 1] = '_' ->
          loop (i + 2) (token (Symbol "#_") i (i + 2) :: tokens)
      | '#' when i + 1 < String.length source && source.[i + 1] = '\'' ->
          let value, next = read_atom source (i + 2) in
          if value = "" then Error.error "var quote expects a symbol"
          else loop next (token (Var_quote value) i next :: tokens)
      | '#' when i + 1 < String.length source && source.[i + 1] = '(' ->
          loop (i + 2) (token Anon_lparen i (i + 2) :: tokens)
      | ')' -> loop (i + 1) (token Rparen i (i + 1) :: tokens)
      | '[' -> loop (i + 1) (token Lbracket i (i + 1) :: tokens)
      | ']' -> loop (i + 1) (token Rbracket i (i + 1) :: tokens)
      | '#' when i + 1 < String.length source && source.[i + 1] = '{' ->
          loop (i + 2) (token Set_lbrace i (i + 2) :: tokens)
      | '#' when i + 1 < String.length source && source.[i + 1] = '"' -> (
          match read_regex source (i + 2) with
          | Ok (value, next) -> loop next (token (Regex value) i next :: tokens)
          | Error _ as err -> err)
      | '{' -> loop (i + 1) (token Lbrace i (i + 1) :: tokens)
      | '}' -> loop (i + 1) (token Rbrace i (i + 1) :: tokens)
      | '"' -> (
          match read_string source (i + 1) with
          | Ok (value, next) -> loop next (token (String value) i next :: tokens)
          | Error _ as err -> err)
      | ':' ->
          let value, next = read_atom source i in
          loop next (token (Keyword value) i next :: tokens)
      | _ ->
          let atom, next = read_atom source i in
          let token_result =
            match (atom, int_of_string_opt atom) with
            | "true", _ -> Ok (Bool true)
            | "false", _ -> Ok (Bool false)
            | "nil", _ -> Ok (Symbol atom)
            | ("##Inf" | "##-Inf" | "##NaN"), _ -> Ok (Float atom)
            | _, Some value -> Ok (Int value)
            | _ -> (
                match strip_numeric_suffix 'N' atom with
                | Some integer -> (
                    match int_of_string_opt integer with
                    | Some value -> Ok (Int value)
                    | None -> (
                        match float_of_string_opt integer with
                        | Some value -> Ok (Float (string_of_float value))
                        | None -> Ok (Symbol atom)))
                | None -> (
                match strip_numeric_suffix 'M' atom with
                | Some decimal when looks_like_float decimal -> (
                    match float_of_string_opt decimal with
                    | Some _ -> Ok (Float decimal)
                    | None -> Ok (Symbol atom))
                | Some decimal -> (
                    match int_of_string_opt decimal with
                    | Some value -> Ok (Int value)
                    | None -> (
                        match float_of_string_opt decimal with
                        | Some value -> Ok (Float (string_of_float value))
                        | None -> Ok (Symbol atom)))
                | None -> (
                match ratio_float_literal atom with
                | Some value -> Ok (Float value)
                | None -> (
                match char_of_atom atom with
                | Some value -> Ok (Char value)
                | None when looks_like_float atom -> (
                    match float_of_string_opt atom with
                    | Some _ -> Ok (Float atom)
                    | None -> Ok (Symbol atom))
                | None -> Ok (Symbol atom)))))
          in
          (match token_result with
          | Error _ as err -> err
          | Ok desc -> loop next (token desc i next :: tokens))
  in
  loop 0 []
