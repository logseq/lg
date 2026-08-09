open Types

let method_id protocol_id name =
  Method_id.create
    ~owner:(Protocol_id.owner protocol_id @ [ Protocol_id.name protocol_id ])
    ~name

let seqable_id = Protocol_id.create ~owner:[] ~name:"Seqable"
let seq_method_id = Method_id.create ~owner:[ "Seqable" ] ~name:"-seq"
let reducible_id = Protocol_id.create ~owner:[] ~name:"Reducible"
let reduce_method_id = Method_id.create ~owner:[ "Reducible" ] ~name:"-reduce"
let counted_id = Protocol_id.create ~owner:[] ~name:"Counted"
let count_method_id = Method_id.create ~owner:[ "Counted" ] ~name:"-count"
let indexed_id = Protocol_id.create ~owner:[] ~name:"Indexed"
let nth_method_id = Method_id.create ~owner:[ "Indexed" ] ~name:"-nth"
let emptyable_id = Protocol_id.create ~owner:[] ~name:"Emptyable"
let empty_method_id = Method_id.create ~owner:[ "Emptyable" ] ~name:"-empty"
let runtime_map_receiver =
  Receiver_id.Host_receiver "Lg_runtime.Runtime_map.t"
let reversible_id = Protocol_id.create ~owner:[] ~name:"IReversible"
let editable_id = Protocol_id.create ~owner:[] ~name:"IEditableCollection"

let transient_collection_id =
  Protocol_id.create ~owner:[] ~name:"ITransientCollection"

let transient_set_id = Protocol_id.create ~owner:[] ~name:"ITransientSet"
let equiv_id = Protocol_id.create ~owner:[] ~name:"IEquiv"
let hash_id = Protocol_id.create ~owner:[] ~name:"IHash"
let deref_id = Protocol_id.create ~owner:[] ~name:"IDeref"
let atom_id = Protocol_id.create ~owner:[] ~name:"IAtom"
let reset_id = Protocol_id.create ~owner:[] ~name:"IReset"
let swap_id = Protocol_id.create ~owner:[] ~name:"ISwap"
let comparable_id = Protocol_id.create ~owner:[] ~name:"IComparable"
let lookup_id = Protocol_id.create ~owner:[] ~name:"ILookup"
let collection_id = Protocol_id.create ~owner:[] ~name:"ICollection"
let associative_id = Protocol_id.create ~owner:[] ~name:"IAssociative"
let find_id = Protocol_id.create ~owner:[] ~name:"IFind"
let map_id = Protocol_id.create ~owner:[] ~name:"IMap"
let vector_id = Protocol_id.create ~owner:[] ~name:"IVector"
let kv_reduce_id = Protocol_id.create ~owner:[] ~name:"IKVReduce"
let meta_id = Protocol_id.create ~owner:[] ~name:"IMeta"
let with_meta_id = Protocol_id.create ~owner:[] ~name:"IWithMeta"

let data_owner = [ "clojure.data" ]

let equality_partition_id =
  Protocol_id.create ~owner:data_owner ~name:"EqualityPartition"

let equality_partition_method_id =
  Method_id.create
    ~owner:(data_owner @ [ "EqualityPartition" ])
    ~name:"equality-partition"

let diff_id = Protocol_id.create ~owner:data_owner ~name:"Diff"

let diff_method_id =
  Method_id.create ~owner:(data_owner @ [ "Diff" ]) ~name:"diff-similar"

let add_or_fail result =
  match result with
  | Ok registry -> registry
  | Error error -> failwith error.Error.message

let signature method_id param_tys return_ty =
  { Protocol_registry.method_id; method_ty = TFn (param_tys, return_ty) }

let declare_seqable registry =
  Protocol_registry.declare seqable_id
    [ signature seq_method_id [ TUnknown ] TUnknown ]
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

let add_edn_seqable registry =
  let value_ty = TOcaml "Lg_edn_backend.t" in
  let binding =
    Types.binding ~protocol_id:seqable_id "Lg_runtime.Runtime_edn.to_seq"
      (TFn ([ value_ty ], TSeq value_ty))
  in
  Protocol_registry.add_implementation seqable_id seq_method_id
    (Receiver_id.Host_receiver "Lg_edn_backend.t")
    binding registry
  |> add_or_fail

let declare_reducible registry =
  Protocol_registry.declare reducible_id
    [ signature reduce_method_id [ TUnknown; TUnknown; TUnknown ] TUnknown ]
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
    [ signature count_method_id [ TUnknown ] TInt ]
    registry
  |> add_or_fail

let add_counted receiver ocaml_name registry =
  let binding =
    Types.binding ~protocol_id:counted_id ocaml_name (TFn ([ TUnknown ], TInt))
  in
  Protocol_registry.add_implementation counted_id count_method_id receiver
    binding registry
  |> add_or_fail

let declare_indexed registry =
  Protocol_registry.declare indexed_id
    [ signature nth_method_id [ TUnknown; TInt ] TUnknown ]
    registry
  |> add_or_fail

let declare_emptyable registry =
  let receiver = TVar "emptyable_receiver" in
  Protocol_registry.declare emptyable_id
    [ signature empty_method_id [ receiver ] receiver ]
    registry
  |> add_or_fail

let add_emptyable receiver ocaml_name registry =
  let binding =
    Types.binding ~protocol_id:emptyable_id ocaml_name
      (TFn ([ TUnknown ], TUnknown))
  in
  Protocol_registry.add_implementation emptyable_id empty_method_id receiver
    binding registry
  |> add_or_fail

let declare_collection_lifecycle_protocols registry =
  let receiver = TVar "equiv_receiver" in
  registry
  |> Protocol_registry.declare equiv_id
       [ signature (method_id equiv_id "-equiv") [ receiver; receiver ] TBool ]
  |> add_or_fail
  |> Protocol_registry.declare hash_id
       [ signature (method_id hash_id "-hash") [ TUnknown ] TInt ]
  |> add_or_fail
  |> Protocol_registry.declare reversible_id
       [ signature (method_id reversible_id "-rseq") [ TUnknown ] TUnknown ]
  |> add_or_fail
  |> Protocol_registry.declare editable_id
       [
         signature (method_id editable_id "-as-transient") [ TUnknown ]
           TUnknown;
       ]
  |> add_or_fail
  |> Protocol_registry.declare transient_collection_id
       [
         signature (method_id transient_collection_id "-conj!")
           [ TUnknown; TUnknown ] TUnknown;
         signature (method_id transient_collection_id "-persistent!")
           [ TUnknown ] TUnknown;
       ]
  |> add_or_fail
  |> Protocol_registry.declare transient_set_id
       [
         signature (method_id transient_set_id "-disjoin!")
           [ TUnknown; TUnknown ] TUnknown;
       ]
  |> add_or_fail

let declare_deref registry =
  Protocol_registry.declare deref_id
    [ signature (method_id deref_id "-deref") [ TUnknown ] TUnknown ]
    registry
  |> add_or_fail

let declare_compare_and_set registry =
  let value = TVar "atom_value" in
  Protocol_registry.declare atom_id
    [
      signature (method_id atom_id "-compare-and-set!")
        [ TUnknown; value; value ] TBool;
    ]
    registry
  |> add_or_fail

let declare_reset registry =
  let value = TVar "reset_value" in
  Protocol_registry.declare reset_id
    [ signature (method_id reset_id "-reset!") [ TUnknown; value ] value ]
    registry
  |> add_or_fail

let declare_swap registry =
  let value = TVar "swap_value" in
  Protocol_registry.declare swap_id
    [
      signature (method_id swap_id "-swap!")
        [ TUnknown; TFn ([ value ], value) ] value;
    ]
    registry
  |> add_or_fail

let declare_data_protocols registry =
  let dynamic = Types.dynamic_constraint TUnknown in
  registry
  |> Protocol_registry.declare equality_partition_id
       [ signature equality_partition_method_id [ dynamic ] TKeyword ]
  |> add_or_fail
  |> Protocol_registry.declare diff_id
       [ signature diff_method_id [ dynamic; dynamic ] dynamic ]
  |> add_or_fail

let declare_comparable_protocol registry =
  registry
  |> Protocol_registry.declare comparable_id
       [
         signature (method_id comparable_id "-compare")
           [ TUnknown; TUnknown ] TInt;
       ]
  |> add_or_fail

let declare_map_protocols registry =
  let key = TVar "map_key" in
  let value = TVar "map_value" in
  let accumulator = TVar "map_accumulator" in
  let map_ty = TOcaml_app ("Lg_runtime.Runtime_map.t", [ key; value ]) in
  let metadata_ty = TOcaml "Lg_edn_backend.t" in
  registry
  |> Protocol_registry.declare lookup_id
       [
         {
           Protocol_registry.method_id = method_id lookup_id "-lookup";
           method_ty =
             TOverloaded_fn
               [
                 {
                   fixed_params = [ map_ty; key ];
                   rest_param = None;
                   return_ty = TOcaml_app ("option", [ value ]);
                 };
                 {
                   fixed_params = [ map_ty; key; value ];
                   rest_param = None;
                   return_ty = value;
                 };
               ];
         };
       ]
  |> add_or_fail
  |> Protocol_registry.declare collection_id
       [
         signature (method_id collection_id "-conj")
           [ map_ty; TTuple [ key; value ] ] map_ty;
       ]
  |> add_or_fail
  |> Protocol_registry.declare associative_id
       [
         signature (method_id associative_id "-contains-key?")
           [ map_ty; key ] TBool;
         signature (method_id associative_id "-assoc")
           [ map_ty; key; value ] map_ty;
       ]
  |> add_or_fail
  |> Protocol_registry.declare find_id
       [
         signature (method_id find_id "-find") [ map_ty; key ]
           (TOcaml_app ("option", [ TTuple [ key; value ] ]));
       ]
  |> add_or_fail
  |> Protocol_registry.declare map_id
       [ signature (method_id map_id "-dissoc") [ map_ty; key ] map_ty ]
  |> add_or_fail
  |> Protocol_registry.declare kv_reduce_id
       [
         signature (method_id kv_reduce_id "-kv-reduce")
           [
             map_ty;
             TFn ([ accumulator; key; value ], accumulator);
             accumulator;
           ]
           accumulator;
       ]
  |> add_or_fail
  |> Protocol_registry.declare meta_id
       [ signature (method_id meta_id "-meta") [ map_ty ] metadata_ty ]
  |> add_or_fail
  |> Protocol_registry.declare with_meta_id
       [
         signature (method_id with_meta_id "-with-meta")
           [ map_ty; metadata_ty ] map_ty;
       ]
  |> add_or_fail

let add_runtime_map_protocols registry =
  let key = TVar "map_key" in
  let value = TVar "map_value" in
  let accumulator = TVar "map_accumulator" in
  let map_ty = TOcaml_app ("Lg_runtime.Runtime_map.t", [ key; value ]) in
  let metadata_ty = TOcaml "Lg_edn_backend.t" in
  let add ?(overload_targets = []) protocol_id method_name ocaml_name ty
      registry =
    let binding =
      Types.binding ~protocol_id ~overload_targets ocaml_name ty
    in
    Protocol_registry.add_implementation protocol_id
      (method_id protocol_id method_name)
      runtime_map_receiver binding registry
    |> add_or_fail
  in
  registry
  |> add
       ~overload_targets:
         [ "Lg_runtime.Runtime_map.lookup"; "Lg_runtime.Runtime_map.lookup_default" ]
       lookup_id "-lookup" "Lg_runtime.Runtime_map.lookup"
       (TOverloaded_fn
          [
            {
              fixed_params = [ map_ty; key ];
              rest_param = None;
              return_ty = TOcaml_app ("option", [ value ]);
            };
            {
              fixed_params = [ map_ty; key; value ];
              rest_param = None;
              return_ty = value;
            };
          ])
  |> add collection_id "-conj" "Lg_runtime.Runtime_map.conj_entry"
       (TFn ([ map_ty; TTuple [ key; value ] ], map_ty))
  |> add associative_id "-contains-key?" "Lg_runtime.Runtime_map.contains_key"
       (TFn ([ map_ty; key ], TBool))
  |> add associative_id "-assoc" "Lg_runtime.Runtime_map.assoc"
       (TFn ([ map_ty; key; value ], map_ty))
  |> add find_id "-find" "Lg_runtime.Runtime_map.find_entry"
       (TFn
          ([ map_ty; key ], TOcaml_app ("option", [ TTuple [ key; value ] ])))
  |> add map_id "-dissoc" "Lg_runtime.Runtime_map.dissoc"
       (TFn ([ map_ty; key ], map_ty))
  |> add kv_reduce_id "-kv-reduce" "Lg_runtime.Runtime_map.kv_reduce_protocol"
       (TFn
          ( [
              map_ty;
              TFn ([ accumulator; key; value ], accumulator);
              accumulator;
            ],
            accumulator ))
  |> add meta_id "-meta" "Lg_runtime.Runtime_map.metadata"
       (TFn ([ map_ty ], metadata_ty))
  |> add with_meta_id "-with-meta" "Lg_runtime.Runtime_map.with_metadata"
       (TFn ([ map_ty; metadata_ty ], map_ty))

let declare_vector_protocol registry =
  let element = TVar "vector_element" in
  let vector = TVector element in
  let assoc_n_id = method_id vector_id "-assoc-n" in
  let implementation =
    Types.binding ~protocol_id:vector_id "Rrbvec.set"
      (TFn ([ vector; TInt; element ], vector))
  in
  registry
  |> Protocol_registry.declare vector_id
       [ signature assoc_n_id [ vector; TInt; element ] vector ]
  |> add_or_fail
  |> Protocol_registry.add_implementation vector_id assoc_n_id
       Receiver_id.Vector_receiver implementation
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
  Protocol_registry.empty |> declare_seqable
  |> add_seqable Receiver_id.List_receiver "Lg_runtime.Runtime_seq.of_list"
  |> add_seqable Receiver_id.Vector_receiver "Lg_runtime.Runtime_seq.of_vector"
  |> add_seqable Receiver_id.Set_receiver "Lg.Core_protocols.seq_of_set"
  |> add_seqable Receiver_id.Seq_receiver "Lg_runtime.Runtime_seq.memoize"
  |> add_seqable Receiver_id.Array_receiver "Lg_runtime.Runtime_seq.of_array"
  |> add_seqable Receiver_id.String_receiver "Lg_runtime.Runtime_seq.of_string"
  |> add_seqable (Receiver_id.Host_receiver "list")
       "Lg_runtime.Runtime_seq.of_host_list"
  |> add_seqable (Receiver_id.Host_receiver "array")
       "Lg_runtime.Runtime_seq.of_host_array"
  |> add_seqable (Receiver_id.Host_receiver "Seq.t")
       "Lg_runtime.Runtime_seq.of_host_seq"
  |> add_seqable (Receiver_id.Host_receiver "Seq")
       "Lg_runtime.Runtime_seq.of_host_seq_alias"
  |> add_seqable runtime_map_receiver "Lg_runtime.Runtime_map.to_seq"
  |> add_edn_seqable
  |> declare_reducible
  |> add_reducible Receiver_id.List_receiver "Lg.Core_protocols.reduce_list"
  |> add_reducible Receiver_id.Vector_receiver "Lg.Core_protocols.reduce_vector"
  |> add_reducible Receiver_id.Set_receiver "Lg.Core_protocols.reduce_set"
  |> add_reducible Receiver_id.Seq_receiver "Lg.Core_protocols.reduce_seq"
  |> add_reducible Receiver_id.Array_receiver "Lg.Core_protocols.reduce_array"
  |> add_reducible Receiver_id.String_receiver "Lg.Core_protocols.reduce_string"
  |> add_reducible (Receiver_id.Host_receiver "list")
       "Lg.Core_protocols.reduce_host_list"
  |> add_reducible (Receiver_id.Host_receiver "array")
       "Lg.Core_protocols.reduce_host_array"
  |> add_reducible (Receiver_id.Host_receiver "Seq.t")
       "Lg.Core_protocols.reduce_host_seq"
  |> add_reducible (Receiver_id.Host_receiver "Seq")
       "Lg.Core_protocols.reduce_host_seq_alias"
  |> declare_counted
  |> add_counted Receiver_id.List_receiver "Lg.Core_protocols.count_list"
  |> add_counted Receiver_id.Vector_receiver "Lg.Core_protocols.count_vector"
  |> add_counted Receiver_id.Set_receiver "Lg.Core_protocols.count_set"
  |> add_counted Receiver_id.Array_receiver "Lg.Core_protocols.count_array"
  |> add_counted Receiver_id.String_receiver "Lg.Core_protocols.count_string"
  |> add_counted (Receiver_id.Host_receiver "list")
       "Lg.Core_protocols.count_host_list"
  |> add_counted (Receiver_id.Host_receiver "array")
       "Lg.Core_protocols.count_host_array"
  |> add_counted runtime_map_receiver "Lg_runtime.Runtime_map.count"
  |> declare_indexed
  |> add_indexed Receiver_id.List_receiver "Lg.Core_protocols.nth_list"
  |> add_indexed Receiver_id.Vector_receiver "Lg.Core_protocols.nth_vector"
  |> add_indexed Receiver_id.Array_receiver "Lg.Core_protocols.nth_array"
  |> add_indexed Receiver_id.String_receiver "Lg.Core_protocols.nth_string"
  |> add_indexed (Receiver_id.Host_receiver "list")
       "Lg.Core_protocols.nth_host_list"
  |> add_indexed (Receiver_id.Host_receiver "array")
       "Lg.Core_protocols.nth_host_array"
  |> declare_emptyable
  |> add_emptyable runtime_map_receiver "Lg_runtime.Runtime_map.empty_like"
  |> declare_collection_lifecycle_protocols |> declare_deref
  |> declare_compare_and_set |> declare_reset |> declare_swap
  |> declare_comparable_protocol
  |> declare_map_protocols |> add_runtime_map_protocols
  |> declare_vector_protocol
  |> declare_data_protocols

let find_seqable receiver_ty registry =
  match Receiver_id.of_type receiver_ty with
  | None -> None
  | Some receiver ->
      Protocol_registry.find_implementation seqable_id seq_method_id receiver
        registry
      |> Option.map (fun (implementation : binding) ->
             {
               implementation with
               ty =
                 instantiate_receiver_method_type receiver_ty
                   implementation.ty;
             })

let find_reducible receiver_ty registry =
  match Receiver_id.of_type receiver_ty with
  | None -> None
  | Some receiver ->
      Protocol_registry.find_implementation reducible_id reduce_method_id
        receiver registry
      |> Option.map (fun (implementation : binding) ->
             {
               implementation with
               ty =
                 instantiate_receiver_method_type receiver_ty
                   implementation.ty;
             })

let find_counted receiver_ty registry =
  match Receiver_id.of_type receiver_ty with
  | None -> None
  | Some receiver ->
      Protocol_registry.find_implementation counted_id count_method_id receiver
        registry
      |> Option.map (fun (implementation : binding) ->
             {
               implementation with
               ty =
                 instantiate_receiver_method_type receiver_ty
                   implementation.ty;
             })

let find_indexed receiver_ty registry =
  match Receiver_id.of_type receiver_ty with
  | None -> None
  | Some receiver ->
      Protocol_registry.find_implementation indexed_id nth_method_id receiver
        registry
      |> Option.map (fun (implementation : binding) ->
             {
               implementation with
               ty =
                 instantiate_receiver_method_type receiver_ty
                   implementation.ty;
             })

let find_emptyable receiver_ty registry =
  match Receiver_id.of_type receiver_ty with
  | None -> None
  | Some receiver ->
      Protocol_registry.find_implementation emptyable_id empty_method_id
        receiver registry

let find_comparable receiver_ty registry =
  match Receiver_id.of_type receiver_ty with
  | None -> None
  | Some receiver ->
      Protocol_registry.find_implementation comparable_id
        (method_id comparable_id "-compare") receiver registry
      |> Option.map (fun (implementation : binding) ->
             {
               implementation with
               ty =
                 instantiate_receiver_method_type receiver_ty
                   implementation.ty;
             })
