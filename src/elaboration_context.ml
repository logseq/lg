type t = {
  special_forms : Special_form_elaborator.t;
  calls : Call_elaborator.t;
}

let create ~compile_expr =
  {
    special_forms =
      Special_form_elaborator.create ~compile_expr
        ~dynamic_unpack:Call_elaborator.dynamic_unpack
        ~pack_dynamic_value:Call_elaborator.pack_dynamic_value;
    calls = Call_elaborator.create ~compile_expr;
  }
