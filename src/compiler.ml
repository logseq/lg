type compile_error = Error.t = { message : string }

type state = Toolchain.state

let empty_state = Toolchain.empty_state

let compile_string source = Toolchain.implementation source

let compile_string_with_filename ~filename source =
  Toolchain.implementation ~filename source

let required_ocaml_packages source = Toolchain.required_ocaml_packages source

let compile_parsetree source = Toolchain.implementation_parsetree source

let compile_parsetree_with_filename ~filename source =
  Toolchain.implementation_parsetree ~filename source

let typecheck_parsetree source = Toolchain.typecheck_parsetree source

let print_parsetree structure = Toolchain.print_parsetree structure

let compile_chunk state source = Toolchain.compile_chunk state source

let compile_chunk_with_filename ~filename state source =
  Toolchain.compile_chunk ~filename state source

let compile_chunk_parsetree state source =
  Toolchain.compile_chunk_parsetree state source

let compile_chunk_parsetree_with_filename ~filename state source =
  Toolchain.compile_chunk_parsetree ~filename state source
