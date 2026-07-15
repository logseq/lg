let type_annotation = function
  | "java.io.Writer" -> Some "Buffer.t"
  | _ -> None

let dynamic_instance_property ~receiver_type:_ ~method_name:_ = None

let implicit_module = function _ -> None

let instance_method ~receiver_type ~method_name =
  match (receiver_type, method_name) with
  | "Lg_runtime.Runtime_uuid.t", ".getMostSignificantBits" ->
      Some "Lg_runtime.Runtime_uuid.getmostsignificantbits"
  | "Lg_runtime.Runtime_uuid.t", ".getLeastSignificantBits" ->
      Some "Lg_runtime.Runtime_uuid.getleastsignificantbits"
  | _ -> None
