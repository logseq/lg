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
          | ch ->
              Buffer.add_char buffer ch;
              loop (i + 2))
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

let tokenize source =
  let rec loop i tokens =
    let i = skip_ignored source i in
    if i >= String.length source then Ok (List.rev tokens)
    else
      match source.[i] with
      | '(' -> loop (i + 1) (Lparen :: tokens)
      | ')' -> loop (i + 1) (Rparen :: tokens)
      | '[' -> loop (i + 1) (Lbracket :: tokens)
      | ']' -> loop (i + 1) (Rbracket :: tokens)
      | '{' -> loop (i + 1) (Lbrace :: tokens)
      | '}' -> loop (i + 1) (Rbrace :: tokens)
      | '"' -> (
          match read_string source (i + 1) with
          | Ok (value, next) -> loop next (String value :: tokens)
          | Error _ as err -> err)
      | ':' ->
          let value, next = read_atom source i in
          loop next (Keyword value :: tokens)
      | _ ->
          let atom, next = read_atom source i in
          let token =
            match (atom, int_of_string_opt atom) with
            | "true", _ -> Bool true
            | "false", _ -> Bool false
            | "nil", _ -> Nil
            | _, Some value -> Int value
            | _ -> Symbol atom
          in
          loop next (token :: tokens)
  in
  loop 0 []
