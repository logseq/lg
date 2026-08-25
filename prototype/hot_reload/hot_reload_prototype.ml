type reload_status = Unchanged | Reloaded

type reload_error = {
  line : int;
  column : int;
  message : string;
}

type model = { mutable count : int }
type segment = Literal of string | Count
type renderer = model -> string

type t = {
  model : model;
  mutable renderer : renderer;
  mutable source : string;
  mutable generation : int;
}

let error column message = Error { line = 1; column; message }

let parse source =
  if String.length source > 256 then error 1 "view source exceeds 256 bytes"
  else
    let length = String.length source in
    let flush_literal source start finish segments =
      if finish = start then segments
      else Literal (String.sub source start (finish - start)) :: segments
    in
    let rec loop index literal_start has_count segments =
      if index = length then
        let segments = flush_literal source literal_start index segments in
        if has_count then Ok (List.rev segments)
        else error 1 "the prototype view must reference {count}"
      else
        match source.[index] with
        | '}' -> error (index + 1) "unexpected closing brace"
        | '{' -> (
            match String.index_from_opt source (index + 1) '}' with
            | None -> error (index + 1) "unterminated view binding"
            | Some closing ->
                let name =
                  String.sub source (index + 1) (closing - index - 1)
                in
                if not (String.equal name "count") then
                  error (index + 1) ("unknown view binding {" ^ name ^ "}")
                else
                  let segments =
                    Count :: flush_literal source literal_start index segments
                  in
                  loop (closing + 1) (closing + 1) true segments)
        | _ -> loop (index + 1) literal_start has_count segments
    in
    loop 0 0 false []

let compile source =
  Result.map
    (fun segments model ->
      let output = Buffer.create (String.length source + 16) in
      List.iter
        (function
          | Literal text -> Buffer.add_string output text
          | Count -> Buffer.add_string output (string_of_int model.count))
        segments;
      Buffer.contents output)
    (parse source)

let create ~source =
  Result.map
    (fun renderer ->
      { model = { count = 0 }; renderer; source; generation = 0 })
    (compile source)

let increment session = session.model.count <- session.model.count + 1
let render session = session.renderer session.model

let reload session ~source =
  if String.equal source session.source then Ok Unchanged
  else
    Result.map
      (fun renderer ->
        session.renderer <- renderer;
        session.source <- source;
        session.generation <- session.generation + 1;
        Reloaded)
      (compile source)

let generation session = session.generation
let source session = session.source
