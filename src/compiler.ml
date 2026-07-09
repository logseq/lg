type compile_error = Error.t = { message : string }

type state = Toolchain.state

let empty_state = Toolchain.empty_state

let compile_string source = Toolchain.implementation source

let compile_chunk state source = Toolchain.compile_chunk state source
