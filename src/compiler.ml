type compile_error = Error.t = {
  message : string;
  location : Location.t option;
}

type diagnostic_severity = Toolchain.diagnostic_severity

type diagnostic = Toolchain.diagnostic = {
  message : string;
  severity : diagnostic_severity;
  location : Location.t option;
}

type compilation = Toolchain.compilation = {
  ocaml_source : string;
  diagnostics : diagnostic list;
}

type state = Toolchain.state

let empty_state = Toolchain.empty_state

let compile_string source = Toolchain.implementation source

let compile_string_with_filename ~filename source =
  Toolchain.implementation ~filename source

let compile_string_with_diagnostics source =
  Toolchain.implementation_with_diagnostics source

let compile_string_with_filename_and_diagnostics ~filename source =
  Toolchain.implementation_with_diagnostics ~filename source

let required_ocaml_packages source = Toolchain.required_ocaml_packages source

let compile_parsetree source = Toolchain.implementation_parsetree source

let compile_parsetree_with_filename ~filename source =
  Toolchain.implementation_parsetree ~filename source

let typecheck_parsetree source = Toolchain.typecheck_parsetree source

let print_parsetree structure = Toolchain.print_parsetree structure

let compile_chunk state source = Toolchain.compile_chunk state source

let compile_chunk_with_filename ~filename state source =
  Toolchain.compile_chunk ~filename state source

let compile_chunk_with_filename_and_diagnostics ~filename state source =
  Toolchain.compile_chunk_with_diagnostics ~filename state source

let compile_chunk_parsetree state source =
  Toolchain.compile_chunk_parsetree state source

let compile_chunk_parsetree_with_filename ~filename state source =
  Toolchain.compile_chunk_parsetree ~filename state source
