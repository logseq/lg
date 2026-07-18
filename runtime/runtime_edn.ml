type state = {
  source : string;
  mutable index : int;
}

let length state = String.length state.source
let at_end state = state.index >= length state
let peek state = if at_end state then None else Some state.source.[state.index]

let fail state message =
  invalid_arg (Printf.sprintf "EDN reader error at offset %d: %s" state.index message)

let is_separator = function
  | ' ' | '\n' | '\r' | '\t' | ',' -> true
  | _ -> false

let is_delimiter = function
  | '(' | ')' | '[' | ']' | '{' | '}' | '"' | ';' -> true
  | ch -> is_separator ch

let rec skip_ignored state =
  match peek state with
  | Some ch when is_separator ch ->
      state.index <- state.index + 1;
      skip_ignored state
  | Some ';' ->
      while not (at_end state) && state.source.[state.index] <> '\n' do
        state.index <- state.index + 1
      done;
      skip_ignored state
  | _ -> ()

let take state =
  match peek state with
  | Some ch ->
      state.index <- state.index + 1;
      ch
  | None -> fail state "unexpected end of input"

let expect state expected =
  let actual = take state in
  if actual <> expected then
    fail state (Printf.sprintf "expected %C, got %C" expected actual)

let read_hex_digit state =
  match take state with
  | '0' .. '9' as ch -> Char.code ch - Char.code '0'
  | 'a' .. 'f' as ch -> 10 + Char.code ch - Char.code 'a'
  | 'A' .. 'F' as ch -> 10 + Char.code ch - Char.code 'A'
  | _ -> fail state "invalid unicode escape"

let read_unicode_escape state =
  let code = ref 0 in
  for _ = 1 to 4 do
    code := (!code * 16) + read_hex_digit state
  done;
  if !code <= 0x7f then String.make 1 (Char.chr !code)
  else if !code <= 0x7ff then
    String.init 2 (function
      | 0 -> Char.chr (0xc0 lor (!code lsr 6))
      | _ -> Char.chr (0x80 lor (!code land 0x3f)))
  else
    String.init 3 (function
      | 0 -> Char.chr (0xe0 lor (!code lsr 12))
      | 1 -> Char.chr (0x80 lor ((!code lsr 6) land 0x3f))
      | _ -> Char.chr (0x80 lor (!code land 0x3f)))

let read_quoted_string state =
  expect state '"';
  let buffer = Buffer.create 16 in
  let rec loop () =
    match take state with
    | '"' -> Buffer.contents buffer
    | '\\' -> (
        match take state with
        | '"' -> Buffer.add_char buffer '"'
        | '\\' -> Buffer.add_char buffer '\\'
        | 'n' -> Buffer.add_char buffer '\n'
        | 'r' -> Buffer.add_char buffer '\r'
        | 't' -> Buffer.add_char buffer '\t'
        | 'b' -> Buffer.add_char buffer '\b'
        | 'f' -> Buffer.add_char buffer '\012'
        | 'u' -> Buffer.add_string buffer (read_unicode_escape state)
        | _ -> fail state "unsupported string escape");
        loop ()
    | ch ->
        Buffer.add_char buffer ch;
        loop ()
  in
  loop ()

let read_token state =
  let start = state.index in
  while
    not (at_end state) && not (is_delimiter state.source.[state.index])
  do
    state.index <- state.index + 1
  done;
  if state.index = start then fail state "expected a value";
  String.sub state.source start (state.index - start)

let parse_number token =
  match int_of_string_opt token with
  | Some value -> Some (Runtime_dynamic.int value)
  | None -> Option.map Runtime_dynamic.float (float_of_string_opt token)

let parse_atom token =
  match token with
  | "nil" -> Runtime_dynamic.nil
  | "true" -> Runtime_dynamic.bool true
  | "false" -> Runtime_dynamic.bool false
  | token when String.length token > 0 && token.[0] = ':' ->
      Runtime_dynamic.keyword token
  | token -> (
      match parse_number token with
      | Some value -> value
      | None -> Runtime_dynamic.symbol token)

let parse_character state =
  expect state '\\';
  let token = read_token state in
  let value =
    match token with
    | "newline" -> '\n'
    | "return" -> '\r'
    | "space" -> ' '
    | "tab" -> '\t'
    | "backspace" -> '\b'
    | "formfeed" -> '\012'
    | token when String.length token = 1 -> token.[0]
    | token
      when String.length token = 5 && token.[0] = 'u' -> (
        match int_of_string_opt ("0x" ^ String.sub token 1 4) with
        | Some code when code <= 0xff -> Char.chr code
        | _ -> fail state "unsupported character literal")
    | _ -> fail state "unsupported character literal"
  in
  Runtime_dynamic.char value

let rec parse_value state =
  skip_ignored state;
  match peek state with
  | None -> fail state "unexpected end of input"
  | Some '"' -> Runtime_dynamic.string (read_quoted_string state)
  | Some '\\' -> parse_character state
  | Some '(' ->
      state.index <- state.index + 1;
      Runtime_dynamic.list (parse_values_until state ')')
  | Some '[' ->
      state.index <- state.index + 1;
      Runtime_dynamic.vector (Rrbvec.of_list (parse_values_until state ']'))
  | Some '{' ->
      state.index <- state.index + 1;
      parse_map state
  | Some '#' -> parse_dispatch state
  | Some (')' | ']' | '}') -> fail state "unmatched delimiter"
  | Some _ -> parse_atom (read_token state)

and parse_values_until state closing =
  let rec loop values =
    skip_ignored state;
    match peek state with
    | Some ch when ch = closing ->
        state.index <- state.index + 1;
        List.rev values
    | None -> fail state "unexpected end of collection"
    | _ -> loop (parse_value state :: values)
  in
  loop []

and parse_map state =
  let values = parse_values_until state '}' in
  let rec pairs entries = function
    | [] -> Runtime_dynamic.map (List.rev entries)
    | [ _ ] -> fail state "map literal must contain an even number of forms"
    | key :: value :: rest ->
        if List.exists (fun (existing, _) -> Runtime_dynamic.equal key existing) entries
        then fail state "duplicate map key"
        else pairs ((key, value) :: entries) rest
  in
  pairs [] values

and parse_dispatch state =
  expect state '#';
  match peek state with
  | Some '{' ->
      state.index <- state.index + 1;
      let values = parse_values_until state '}' in
      let rec reject_duplicates seen = function
        | [] -> Runtime_dynamic.set (List.to_seq (List.rev seen))
        | value :: rest ->
            if List.exists (Runtime_dynamic.equal value) seen then
              fail state "duplicate set element"
            else reject_duplicates (value :: seen) rest
      in
      reject_duplicates [] values
  | Some '_' ->
      state.index <- state.index + 1;
      ignore (parse_value state);
      parse_value state
  | _ -> fail state "unsupported dispatch macro"

let read_string source =
  let state = { source; index = 0 } in
  skip_ignored state;
  if at_end state then Runtime_dynamic.nil else parse_value state
