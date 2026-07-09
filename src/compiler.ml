type compile_error = Error.t = { message : string }

let compile_string source = Toolchain.implementation source
