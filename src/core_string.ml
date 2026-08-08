open Types

let bindings =
  [
    ( "split",
      Types.binding "Lg_runtime.Runtime_string.split"
        (TFn ([ TString; TUnknown ], TVector TString)) );
  ]
