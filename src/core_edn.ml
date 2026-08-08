open Types

let bindings =
  let edn = TOcaml "Lg_edn_backend.t" in
  [
    ( "register-tag-parser!",
      Types.binding "Lg_runtime.Runtime_edn.register_tag_parser"
        (TFn
           ( [ TSymbol; TFn ([ edn ], edn) ],
             TOcaml_app ("option", [ TFn ([ edn ], edn) ]) )) );
  ]

let read_string_binding =
  Types.binding "Lg_runtime.Runtime_edn.read_string"
    (TFn ([ TString ], TOcaml "Lg_edn_backend.t"))
