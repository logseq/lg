type t = { special_forms : Special_form_elaborator.t }

let create ~compile_expr =
  { special_forms = Special_form_elaborator.create ~compile_expr }

