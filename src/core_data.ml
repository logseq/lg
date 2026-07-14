open Types

let dynamic = Types.dynamic_constraint TUnknown

let bindings =
  [
    ( "diff",
      Types.binding "Lg_runtime.Runtime_data.diff"
        (TFn ([ dynamic; dynamic ], dynamic)) );
  ]
