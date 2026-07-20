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
let cacheable_state = Toolchain.cacheable_state

let restore_ocaml_environment ?(target = Target.default) ~packages state
    sources =
  Toolchain.restore_ocaml_environment ~target ~packages state sources

let compile_string ?(target = Target.default) source =
  Toolchain.implementation ~target source

let compile_string_with_filename ?(target = Target.default) ~filename source =
  Toolchain.implementation ~target ~filename source

let compile_string_with_diagnostics ?(target = Target.default) source =
  Toolchain.implementation_with_diagnostics ~target source

let compile_string_with_filename_and_diagnostics ?(target = Target.default)
    ~filename source =
  Toolchain.implementation_with_diagnostics ~target ~filename source

let required_ocaml_packages ?(target = Target.default) source =
  Toolchain.required_ocaml_packages ~target source

let infer_interface ?(target = Target.default) source =
  Toolchain.interface ~target source

let infer_interface_with_filename ?(target = Target.default) ~filename source =
  Toolchain.interface ~target ~filename source

let compile_parsetree ?(target = Target.default) source =
  Toolchain.implementation_parsetree ~target source

let compile_parsetree_with_filename ?(target = Target.default) ~filename source
    =
  Toolchain.implementation_parsetree ~target ~filename source

let typecheck_parsetree ?(target = Target.default) source =
  Toolchain.typecheck_parsetree ~target source

let print_parsetree structure = Toolchain.print_parsetree structure

let compile_chunk ?(target = Target.default) state source =
  Toolchain.compile_chunk ~target state source

let compile_chunk_with_filename ?(target = Target.default) ~filename state
    source =
  Toolchain.compile_chunk ~target ~filename state source

let compile_chunk_with_filename_and_diagnostics ?(target = Target.default)
    ?(check_ocaml = true) ~filename state source =
  Toolchain.compile_chunk_with_diagnostics ~target ~filename ~check_ocaml state
    source

let compile_chunk_parsetree ?(target = Target.default) state source =
  Toolchain.compile_chunk_parsetree ~target state source

let compile_chunk_parsetree_with_filename ?(target = Target.default) ~filename
    state source =
  Toolchain.compile_chunk_parsetree ~target ~filename state source
