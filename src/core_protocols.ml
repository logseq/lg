open Types

let seqable_id = Protocol_id.create ~owner:[] ~name:"Seqable"
let seq_method_id = Method_id.create ~owner:[ "Seqable" ] ~name:"-seq"

let add_or_fail result =
  match result with
  | Ok registry -> registry
  | Error error -> failwith error.Error.message

let declare_seqable registry =
  Protocol_registry.declare seqable_id
    [ { Protocol_registry.method_id = seq_method_id;
        param_tys = [ TUnknown ];
        return_ty = TUnknown } ]
    registry
  |> add_or_fail

let add_seqable receiver ocaml_name registry =
  let binding =
    Types.binding ~protocol_id:seqable_id ocaml_name
      (TFn ([ TUnknown ], TUnknown))
  in
  Protocol_registry.add_implementation seqable_id seq_method_id receiver binding
    registry
  |> add_or_fail

let initial_registry =
  Protocol_registry.empty
  |> declare_seqable
  |> add_seqable Receiver_id.List_receiver
       "Cljml.Runtime_seq.of_list"
  |> add_seqable Receiver_id.Vector_receiver
       "Cljml.Runtime_seq.of_vector"
  |> add_seqable Receiver_id.Set_receiver "Cljml.Core_protocols.seq_of_set"
  |> add_seqable Receiver_id.Seq_receiver "Cljml.Runtime_seq.memoize"
  |> add_seqable Receiver_id.Array_receiver
       "Cljml.Runtime_seq.of_array"
  |> add_seqable Receiver_id.String_receiver
       "Cljml.Runtime_seq.of_string"
  |> add_seqable (Receiver_id.Host_receiver "list")
       "Cljml.Runtime_seq.of_host_list"
  |> add_seqable (Receiver_id.Host_receiver "array")
       "Cljml.Runtime_seq.of_host_array"
  |> add_seqable (Receiver_id.Host_receiver "Seq.t")
       "Cljml.Runtime_seq.of_host_seq"
  |> add_seqable (Receiver_id.Host_receiver "Seq")
       "Cljml.Runtime_seq.of_host_seq_alias"

let find_seqable receiver_ty registry =
  match Receiver_id.of_type receiver_ty with
  | None -> None
  | Some receiver ->
      Protocol_registry.find_implementation seqable_id seq_method_id receiver
        registry
