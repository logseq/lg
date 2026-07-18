open Types

let bindings =
  [
    ( "read-string",
      Types.binding "Lg_runtime.Runtime_edn.read_string"
        (TFn ([ TString ], dynamic_constraint TUnknown)) );
  ]
