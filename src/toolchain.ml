type parser_result = {
  ast : Ast.form list;
  parsed_as : [ `Cljml ];
}

type typed_result = {
  ast : Ast.form list;
  items : Types.compiled_item list;
}

type state = Typecheck.state

module type FRONTEND = sig
  val implementation : string -> (parser_result, Error.t) result
end

module type BACKEND = sig
  val implementation : typed_result -> string
end

module Cljml_frontend : FRONTEND = struct
  let implementation source =
    match Lexer.tokenize source with
    | Error _ as err -> err
    | Ok tokens -> (
        match Parser.parse tokens with
        | Error _ as err -> err
        | Ok ast -> Ok { ast; parsed_as = `Cljml })
end

module Ocaml_backend : BACKEND = struct
  let implementation typed = Codegen.emit_program typed.items
end

let empty_state = Typecheck.empty_state

let typecheck (parsed : parser_result) =
  match Typecheck.compile_forms parsed.ast with
  | Error _ as err -> err
  | Ok items -> Ok { ast = parsed.ast; items }

let typecheck_incremental state (parsed : parser_result) =
  match Typecheck.compile_forms_incremental state parsed.ast with
  | Error _ as err -> err
  | Ok (state, items) -> Ok (state, { ast = parsed.ast; items })

let implementation source =
  match Cljml_frontend.implementation source with
  | Error _ as err -> err
  | Ok parsed -> (
      match typecheck parsed with
      | Error _ as err -> err
      | Ok typed -> Ok (Ocaml_backend.implementation typed))

let compile_chunk state source =
  match Cljml_frontend.implementation source with
  | Error _ as err -> err
  | Ok parsed -> (
      match typecheck_incremental state parsed with
      | Error _ as err -> err
      | Ok (state, typed) -> Ok (state, Ocaml_backend.implementation typed))
