type compile_error = Error.t = { message : string }

type state = Toolchain.state

let empty_state = Toolchain.empty_state

let compile_string source = Toolchain.implementation source

let compile_parsetree source = Toolchain.implementation_parsetree source

let print_parsetree structure = Toolchain.print_parsetree structure

let compile_chunk state source = Toolchain.compile_chunk state source

let compile_chunk_parsetree state source =
  Toolchain.compile_chunk_parsetree state source
