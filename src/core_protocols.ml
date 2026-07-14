open Types

let seqable_id = Protocol_id.create ~owner:[] ~name:"Seqable"
let seq_method_id = Method_id.create ~owner:[ "Seqable" ] ~name:"-seq"
let reducible_id = Protocol_id.create ~owner:[] ~name:"Reducible"
let reduce_method_id = Method_id.create ~owner:[ "Reducible" ] ~name:"-reduce"
let counted_id = Protocol_id.create ~owner:[] ~name:"Counted"
let count_method_id = Method_id.create ~owner:[ "Counted" ] ~name:"-count"
let indexed_id = Protocol_id.create ~owner:[] ~name:"Indexed"
let nth_method_id = Method_id.create ~owner:[ "Indexed" ] ~name:"-nth"

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

let declare_reducible registry =
  Protocol_registry.declare reducible_id
    [ { Protocol_registry.method_id = reduce_method_id;
        param_tys = [ TUnknown; TUnknown; TUnknown ];
        return_ty = TUnknown } ]
    registry
  |> add_or_fail

let add_reducible receiver ocaml_name registry =
  let binding =
    Types.binding ~protocol_id:reducible_id ocaml_name
      (TFn ([ TUnknown; TUnknown; TUnknown ], TUnknown))
  in
  Protocol_registry.add_implementation reducible_id reduce_method_id receiver
    binding registry
  |> add_or_fail

let declare_counted registry =
  Protocol_registry.declare counted_id
    [ { Protocol_registry.method_id = count_method_id;
        param_tys = [ TUnknown ];
        return_ty = TInt } ]
    registry
  |> add_or_fail

let add_counted receiver ocaml_name registry =
  let binding =
    Types.binding ~protocol_id:counted_id ocaml_name
      (TFn ([ TUnknown ], TInt))
  in
  Protocol_registry.add_implementation counted_id count_method_id receiver binding
    registry
  |> add_or_fail

let declare_indexed registry =
  Protocol_registry.declare indexed_id
    [ { Protocol_registry.method_id = nth_method_id;
        param_tys = [ TUnknown; TInt ];
        return_ty = TUnknown } ]
    registry
  |> add_or_fail

let add_indexed receiver ocaml_name registry =
  let binding =
    Types.binding ~protocol_id:indexed_id ocaml_name
      (TFn ([ TUnknown; TInt ], TUnknown))
  in
  Protocol_registry.add_implementation indexed_id nth_method_id receiver binding
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
  |> declare_reducible
  |> add_reducible Receiver_id.List_receiver "Cljml.Core_protocols.reduce_list"
  |> add_reducible Receiver_id.Vector_receiver
       "Cljml.Core_protocols.reduce_vector"
  |> add_reducible Receiver_id.Set_receiver "Cljml.Core_protocols.reduce_set"
  |> add_reducible Receiver_id.Seq_receiver "Cljml.Core_protocols.reduce_seq"
  |> add_reducible Receiver_id.Array_receiver "Cljml.Core_protocols.reduce_array"
  |> add_reducible Receiver_id.String_receiver "Cljml.Core_protocols.reduce_string"
  |> add_reducible (Receiver_id.Host_receiver "list")
       "Cljml.Core_protocols.reduce_host_list"
  |> add_reducible (Receiver_id.Host_receiver "array")
       "Cljml.Core_protocols.reduce_host_array"
  |> add_reducible (Receiver_id.Host_receiver "Seq.t")
       "Cljml.Core_protocols.reduce_host_seq"
  |> add_reducible (Receiver_id.Host_receiver "Seq")
       "Cljml.Core_protocols.reduce_host_seq_alias"
  |> declare_counted
  |> add_counted Receiver_id.List_receiver "Cljml.Core_protocols.count_list"
  |> add_counted Receiver_id.Vector_receiver "Cljml.Core_protocols.count_vector"
  |> add_counted Receiver_id.Set_receiver "Cljml.Core_protocols.count_set"
  |> add_counted Receiver_id.Array_receiver "Cljml.Core_protocols.count_array"
  |> add_counted Receiver_id.String_receiver "Cljml.Core_protocols.count_string"
  |> add_counted (Receiver_id.Host_receiver "list")
       "Cljml.Core_protocols.count_host_list"
  |> add_counted (Receiver_id.Host_receiver "array")
       "Cljml.Core_protocols.count_host_array"
  |> declare_indexed
  |> add_indexed Receiver_id.List_receiver "Cljml.Core_protocols.nth_list"
  |> add_indexed Receiver_id.Vector_receiver "Cljml.Core_protocols.nth_vector"
  |> add_indexed Receiver_id.Array_receiver "Cljml.Core_protocols.nth_array"
  |> add_indexed Receiver_id.String_receiver "Cljml.Core_protocols.nth_string"
  |> add_indexed (Receiver_id.Host_receiver "list")
       "Cljml.Core_protocols.nth_host_list"
  |> add_indexed (Receiver_id.Host_receiver "array")
       "Cljml.Core_protocols.nth_host_array"

let find_seqable receiver_ty registry =
  match Receiver_id.of_type receiver_ty with
  | None -> None
  | Some receiver ->
      Protocol_registry.find_implementation seqable_id seq_method_id receiver
        registry

let find_reducible receiver_ty registry =
  match Receiver_id.of_type receiver_ty with
  | None -> None
  | Some receiver ->
      Protocol_registry.find_implementation reducible_id reduce_method_id receiver
        registry

let find_counted receiver_ty registry =
  match Receiver_id.of_type receiver_ty with
  | None -> None
  | Some receiver ->
      Protocol_registry.find_implementation counted_id count_method_id receiver
        registry

let find_indexed receiver_ty registry =
  match Receiver_id.of_type receiver_ty with
  | None -> None
  | Some receiver ->
      Protocol_registry.find_implementation indexed_id nth_method_id receiver
        registry
