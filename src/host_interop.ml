let imported_module ~package ~class_name =
  match (package, class_name) with
  | "java.util", "UUID" -> Some "Lg_runtime.Runtime_uuid"
  | _ -> None

let type_annotation = function
  | "UUID" -> Some "Lg_runtime.Runtime_uuid.t"
  | _ -> None

let implicit_module = function
  | _ -> None

let instance_method ~receiver_type ~method_name =
  match (receiver_type, method_name) with
  | "Lg_runtime.Runtime_uuid.t", ".getMostSignificantBits" ->
      Some "Lg_runtime.Runtime_uuid.getmostsignificantbits"
  | "Lg_runtime.Runtime_uuid.t", ".getLeastSignificantBits" ->
      Some "Lg_runtime.Runtime_uuid.getleastsignificantbits"
  | _ -> None
