open Types

let runtime name = "Lg_runtime.Runtime_walk." ^ name
let dynamic = Types.dynamic_constraint TUnknown
let transform = TFn ([ dynamic ], dynamic)

let binding name =
  (name, Types.binding (runtime name) (TFn ([ transform; dynamic ], dynamic)))

let bindings =
  [
    ( "walk",
      Types.binding (runtime "walk")
        (TFn ([ transform; transform; dynamic ], dynamic)) );
    binding "prewalk";
    binding "postwalk";
  ]
