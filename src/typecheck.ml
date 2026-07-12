type state = Compiler_state.t

let empty_state = Compiler_state.empty
let compile_expr = Elaborator.compile_expr
let compile_forms_incremental = Elaborator.compile_forms_incremental
let compile_forms = Elaborator.compile_forms
