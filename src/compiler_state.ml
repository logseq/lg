type t = {
  scope : string;
  env : Compiler_environment.t;
  next_type : int;
  items : Lowered.compiled_item list;
}

let empty =
  { scope = ""; env = Compiler_environment.empty; next_type = 1; items = [] }

let with_target target state =
  { state with env = Compiler_environment.with_target target state.env }
