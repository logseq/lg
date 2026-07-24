open Types

let bindings =
  let edn = TOcaml "Lg_edn_backend.t" in
  [
    ( "read-string",
      Types.binding "Lg_runtime.Runtime_edn.read_string"
        (TFn ([ TString ], edn)) );
    ( "register-tag-parser!",
      Types.binding "Lg_runtime.Runtime_edn.register_tag_parser"
        (TFn
           ( [ TSymbol; TFn ([ edn ], edn) ],
             TOcaml_app ("option", [ TFn ([ edn ], edn) ]) )) );
  ]
