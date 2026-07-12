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

type t = {
  declarations : declaration Protocol_map.t;
  implementations : Types.binding Implementation_map.t;
}

let empty =
  {
    declarations = Protocol_map.empty;
    implementations = Implementation_map.empty;
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
    Ok
      {
        registry with
        implementations =
          Implementation_map.add key binding registry.implementations;
      }

let find_implementation protocol_id method_id receiver_id registry =
  Implementation_map.find_opt (protocol_id, method_id, receiver_id)
    registry.implementations

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
  { registry with implementations }
