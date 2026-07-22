type t = {
  scope : string;
  env : Compiler_environment.t;
  next_type : int;
  items : Lowered.compiled_item list;
  dynamic_packers : Type_id.t list;
  shared_values : string list;
  runtime_var_reflection : bool;
  runtime_definitions : string list;
  runtime_var_requests : string list;
  runtime_vars : string list;
}

let empty =
  {
    scope = "";
    env = Compiler_environment.empty;
    next_type = 1;
    items = [];
    dynamic_packers = [];
    shared_values = [];
    runtime_var_reflection = false;
    runtime_definitions = [];
    runtime_var_requests = [];
    runtime_vars = [];
  }

let with_target target state =
  { state with env = Compiler_environment.with_target target state.env }
