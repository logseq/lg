type t = {
  scope : string;
  env : Compiler_environment.t;
  next_type : int;
  items : Lowered.compiled_item list;
}

let empty =
  { scope = ""; env = Compiler_environment.empty; next_type = 1; items = [] }
