open Types

let bindings =
  let dynamic = dynamic_constraint TUnknown in
  let arity fixed_params =
    { fixed_params; rest_param = None; return_ty = dynamic }
  in
  [
    ( "read-string",
      Types.binding
        ~overload_targets:
          [
            "Lg_runtime.Runtime_edn.read_string";
            "Lg_runtime.Runtime_edn.read_string_with_options";
          ]
        "Lg_runtime.Runtime_edn.read_string"
        (TOverloaded_fn
           [ arity [ TString ]; arity [ dynamic; TString ] ]) );
    ( "register-tag-parser!",
      Types.binding "Lg_runtime.Runtime_edn.register_tag_parser"
        (TFn ([ TSymbol; TFn ([ dynamic ], dynamic) ], dynamic)) );
  ]
