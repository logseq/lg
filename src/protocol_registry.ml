module Protocol_map = Map.Make (Protocol_id)
module Method_map = Map.Make (Method_id)

type method_signature = {
  method_id : Method_id.t;
  param_tys : Types.ty list;
  return_ty : Types.ty;
}

type declaration = {
  protocol_id : Protocol_id.t;
  methods : method_signature Method_map.t;
}

type receiver_id =
  | Int_receiver
  | String_receiver
  | Keyword_receiver
  | Bool_receiver
  | Record_receiver of Type_id.t

module Implementation_key = struct
  type t = Protocol_id.t * Method_id.t * receiver_id

  let compare = Stdlib.compare
end

module Implementation_map = Map.Make (Implementation_key)
module Emitted_name_map = Map.Make (String)

type t = {
  declarations : declaration Protocol_map.t;
  implementations : Types.binding Implementation_map.t;
  implementation_names : Implementation_key.t Emitted_name_map.t;
}

let empty =
  {
    declarations = Protocol_map.empty;
    implementations = Implementation_map.empty;
    implementation_names = Emitted_name_map.empty;
  }

let declare protocol_id signatures registry =
  if Protocol_map.mem protocol_id registry.declarations then
    Error.error
      ("duplicate protocol declaration " ^ Protocol_id.to_string protocol_id)
  else
    let rec methods acc = function
      | [] -> Ok acc
      | signature :: rest ->
          if Method_map.mem signature.method_id acc then
            Error.error
              ("duplicate protocol method "
             ^ Method_id.to_string signature.method_id)
          else methods (Method_map.add signature.method_id signature acc) rest
    in
    match methods Method_map.empty signatures with
    | Error _ as err -> err
    | Ok methods ->
        let declaration = { protocol_id; methods } in
        Ok
          {
            registry with
            declarations =
              Protocol_map.add protocol_id declaration registry.declarations;
          }

let find_protocol protocol_id registry =
  Protocol_map.find_opt protocol_id registry.declarations

let find_method protocol_id method_id registry =
  match find_protocol protocol_id registry with
  | None -> None
  | Some declaration -> Method_map.find_opt method_id declaration.methods

let protocols_for_method ~owner ~method_name registry =
  Protocol_map.fold
    (fun protocol_id declaration protocols ->
      if Protocol_id.owner protocol_id <> owner then protocols
      else
        let has_method =
          Method_map.exists
            (fun method_id _ -> Method_id.name method_id = method_name)
            declaration.methods
        in
        if has_method then protocol_id :: protocols else protocols)
    registry.declarations []
  |> List.rev

let add_implementation protocol_id method_id receiver_id binding registry =
  let key = (protocol_id, method_id, receiver_id) in
  if Implementation_map.mem key registry.implementations then
    Error.error
      ("duplicate protocol implementation " ^ Protocol_id.to_string protocol_id
     ^ "/" ^ Method_id.name method_id)
  else
    match
      Emitted_name_map.find_opt binding.Types.ocaml_name
        registry.implementation_names
    with
    | Some (existing_protocol, existing_method, _existing_receiver) ->
        Error.error
          ("OCaml protocol implementation name collision: "
         ^ Protocol_id.to_string existing_protocol ^ "/"
         ^ Method_id.name existing_method ^ " and "
         ^ Protocol_id.to_string protocol_id ^ "/" ^ Method_id.name method_id
         ^ " both emit " ^ binding.ocaml_name)
    | None ->
        Ok
          {
            registry with
            implementations =
              Implementation_map.add key binding registry.implementations;
            implementation_names =
              Emitted_name_map.add binding.ocaml_name key
                registry.implementation_names;
          }

let find_implementation protocol_id method_id receiver_id registry =
  Implementation_map.find_opt (protocol_id, method_id, receiver_id)
    registry.implementations

let export_owner ~from_owner ~to_owner ~from_module ~to_module source target =
  let replace_prefix value =
    let prefix = from_module ^ "." in
    if String.starts_with ~prefix value then
      to_module ^ String.sub value (String.length from_module)
        (String.length value - String.length from_module)
    else value
  in
  let remap_protocol protocol_id =
    if Protocol_id.owner protocol_id = from_owner then
      Protocol_id.create ~owner:to_owner ~name:(Protocol_id.name protocol_id)
    else protocol_id
  in
  let remap_method protocol_id method_id =
    Method_id.create
      ~owner:(Protocol_id.owner protocol_id @ [ Protocol_id.name protocol_id ])
      ~name:(Method_id.name method_id)
  in
  let declarations =
    Protocol_map.fold
      (fun protocol_id declaration declarations ->
        if Protocol_id.owner protocol_id <> from_owner then declarations
        else
          let protocol_id = remap_protocol protocol_id in
          let methods =
            Method_map.fold
              (fun _ signature methods ->
                let method_id = remap_method protocol_id signature.method_id in
                Method_map.add method_id { signature with method_id } methods)
              declaration.methods Method_map.empty
          in
          Protocol_map.add protocol_id { protocol_id; methods } declarations)
      source.declarations target.declarations
  in
  let implementations =
    Implementation_map.fold
      (fun (protocol_id, method_id, receiver_id) binding implementations ->
        if Protocol_id.owner protocol_id <> from_owner then implementations
        else
          let protocol_id = remap_protocol protocol_id in
          let method_id = remap_method protocol_id method_id in
          let binding =
            {
              binding with
              Types.ocaml_name = replace_prefix binding.Types.ocaml_name;
              protocol_id = Option.map remap_protocol binding.protocol_id;
            }
          in
          Implementation_map.add (protocol_id, method_id, receiver_id) binding
            implementations)
      source.implementations target.implementations
  in
  let implementation_names =
    Implementation_map.fold
      (fun key binding names ->
        Emitted_name_map.add binding.Types.ocaml_name key names)
      implementations Emitted_name_map.empty
  in
  { declarations; implementations; implementation_names }

let qualify_implementations ~owner ~module_name registry =
  let implementations =
    Implementation_map.fold
      (fun (protocol_id, method_id, receiver_id) (binding : Types.binding) result ->
        if Protocol_id.owner protocol_id = owner then
          let receiver_id =
            match receiver_id with
            | Record_receiver type_id -> Record_receiver type_id
            | receiver_id -> receiver_id
          in
          let binding =
            {
              binding with
              ocaml_name = module_name ^ "." ^ binding.ocaml_name;
              ty = Types.qualify_module_type module_name binding.ty;
            }
          in
          Implementation_map.add (protocol_id, method_id, receiver_id) binding result
        else Implementation_map.add (protocol_id, method_id, receiver_id) binding result)
      registry.implementations Implementation_map.empty
  in
  let implementation_names =
    Implementation_map.fold
      (fun key (binding : Types.binding) names ->
        Emitted_name_map.add binding.ocaml_name key names)
      implementations Emitted_name_map.empty
  in
  { registry with implementations; implementation_names }
