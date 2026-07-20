open Types

let bindings =
  let dynamic = dynamic_constraint TUnknown in
  [
    ( "read-string",
      Types.binding "Lg_runtime.Runtime_edn.read_string"
        (TFn ([ TString ], dynamic)) );
    ( "register-tag-parser!",
      Types.binding "Lg_runtime.Runtime_edn.register_tag_parser"
        (TFn ([ TSymbol; TFn ([ dynamic ], dynamic) ], dynamic)) );
  ]
