open Types

let read_string_binding =
  Types.binding "Lg_runtime.Runtime_edn.read_string"
    (TFn ([ TString ], TOcaml "Lg_edn_backend.t"))
