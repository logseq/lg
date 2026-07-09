type parser_result = {
  ast : Ast.form list;
  parsed_as : [ `Cljml ];
}

type typed_result = {
  ast : Ast.form list;
  items : Types.compiled_item list;
}

type parsetree_result = {
  ast : Ast.form list;
  items : Types.compiled_item list;
  structure : Parsetree.structure;
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
  let implementation (typed : typed_result) = Codegen.emit_program typed.items
end

module Ocaml_parsetree_backend = struct
  let implementation typed =
    let source = Ocaml_backend.implementation typed in
    match Ocaml_parsetree.parse_implementation source with
    | Error _ as err -> err
    | Ok structure -> Ok { ast = typed.ast; items = typed.items; structure }

  let print = Ocaml_parsetree.print_implementation
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

let implementation_parsetree source =
  match Cljml_frontend.implementation source with
  | Error _ as err -> err
  | Ok parsed -> (
      match typecheck parsed with
      | Error _ as err -> err
      | Ok typed -> (
          match Ocaml_parsetree_backend.implementation typed with
          | Error _ as err -> err
          | Ok result -> Ok result.structure))

let print_parsetree = Ocaml_parsetree_backend.print

let compile_chunk state source =
  match Cljml_frontend.implementation source with
  | Error _ as err -> err
  | Ok parsed -> (
      match typecheck_incremental state parsed with
      | Error _ as err -> err
      | Ok (state, typed) -> Ok (state, Ocaml_backend.implementation typed))

let compile_chunk_parsetree state source =
  match Cljml_frontend.implementation source with
  | Error _ as err -> err
  | Ok parsed -> (
      match typecheck_incremental state parsed with
      | Error _ as err -> err
      | Ok (state, typed) -> (
          match Ocaml_parsetree_backend.implementation typed with
          | Error _ as err -> err
          | Ok result -> Ok (state, result.structure)))
