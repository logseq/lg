type t = {
  special_forms : Special_form_elaborator.t;
  calls : Call_elaborator.t;
}

let create ~compile_expr =
  {
    special_forms = Special_form_elaborator.create ~compile_expr;
    calls = Call_elaborator.create ~compile_expr;
  }
