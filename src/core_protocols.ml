open Types

let method_id protocol_id name =
  Method_id.create
    ~owner:(Protocol_id.owner protocol_id @ [ Protocol_id.name protocol_id ])
    ~name

let seqable_id = Protocol_id.create ~owner:[] ~name:"Seqable"
let seq_method_id = Method_id.create ~owner:[ "Seqable" ] ~name:"-seq"
let iseq_id = Protocol_id.create ~owner:[] ~name:"ISeq"
let inext_id = Protocol_id.create ~owner:[] ~name:"INext"
let drop_id = Protocol_id.create ~owner:[] ~name:"IDrop"
let reducible_id = Protocol_id.create ~owner:[] ~name:"Reducible"
let reduce_method_id = Method_id.create ~owner:[ "Reducible" ] ~name:"-reduce"
let counted_id = Protocol_id.create ~owner:[] ~name:"Counted"
let count_method_id = Method_id.create ~owner:[ "Counted" ] ~name:"-count"
let indexed_id = Protocol_id.create ~owner:[] ~name:"Indexed"
let nth_method_id = Method_id.create ~owner:[ "Indexed" ] ~name:"-nth"
let emptyable_id = Protocol_id.create ~owner:[] ~name:"Emptyable"
let empty_method_id = Method_id.create ~owner:[ "Emptyable" ] ~name:"-empty"
let stack_id = Protocol_id.create ~owner:[] ~name:"IStack"
let iindexed_id = Protocol_id.create ~owner:[] ~name:"IIndexed"
let sequential_id = Protocol_id.create ~owner:[] ~name:"ISequential"
let sorted_id = Protocol_id.create ~owner:[] ~name:"ISorted"
let runtime_map_receiver =
  Receiver_id.Host_receiver "Lg_runtime.Runtime_map.t"
let reversible_id = Protocol_id.create ~owner:[] ~name:"IReversible"
let editable_id = Protocol_id.create ~owner:[] ~name:"IEditableCollection"

let transient_collection_id =
  Protocol_id.create ~owner:[] ~name:"ITransientCollection"

let transient_associative_id =
  Protocol_id.create ~owner:[] ~name:"ITransientAssociative"

let transient_map_id = Protocol_id.create ~owner:[] ~name:"ITransientMap"
let transient_vector_id = Protocol_id.create ~owner:[] ~name:"ITransientVector"
let transient_set_id = Protocol_id.create ~owner:[] ~name:"ITransientSet"
let equiv_id = Protocol_id.create ~owner:[] ~name:"IEquiv"
let hash_id = Protocol_id.create ~owner:[] ~name:"IHash"
let deref_id = Protocol_id.create ~owner:[] ~name:"IDeref"
let pending_id = Protocol_id.create ~owner:[] ~name:"IPending"
let atom_id = Protocol_id.create ~owner:[] ~name:"IAtom"
let reset_id = Protocol_id.create ~owner:[] ~name:"IReset"
let volatile_id = Protocol_id.create ~owner:[] ~name:"IVolatile"
let swap_id = Protocol_id.create ~owner:[] ~name:"ISwap"
let watchable_id = Protocol_id.create ~owner:[] ~name:"IWatchable"
let comparable_id = Protocol_id.create ~owner:[] ~name:"IComparable"
let lookup_id = Protocol_id.create ~owner:[] ~name:"ILookup"
let collection_id = Protocol_id.create ~owner:[] ~name:"ICollection"
let set_id = Protocol_id.create ~owner:[] ~name:"ISet"
let associative_id = Protocol_id.create ~owner:[] ~name:"IAssociative"
let find_id = Protocol_id.create ~owner:[] ~name:"IFind"
let map_id = Protocol_id.create ~owner:[] ~name:"IMap"
let vector_id = Protocol_id.create ~owner:[] ~name:"IVector"
let map_entry_id = Protocol_id.create ~owner:[] ~name:"IMapEntry"
let kv_reduce_id = Protocol_id.create ~owner:[] ~name:"IKVReduce"
let meta_id = Protocol_id.create ~owner:[] ~name:"IMeta"
let with_meta_id = Protocol_id.create ~owner:[] ~name:"IWithMeta"

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

let declare_sequence_protocols registry =
  let element = TVar "sequence_element" in
  let sequence = TSeq element in
  registry
  |> Protocol_registry.declare iseq_id
       [
         signature (method_id iseq_id "-first") [ sequence ]
           (TOcaml_app ("option", [ element ]));
         signature (method_id iseq_id "-rest") [ sequence ] sequence;
       ]
  |> add_or_fail
  |> Protocol_registry.declare inext_id
       [
         signature (method_id inext_id "-next") [ sequence ]
           (Types.next_seq element);
       ]
  |> add_or_fail

let add_sequence_protocols registry =
  let element = TVar "sequence_element" in
  let sequence = TSeq element in
  let add protocol_id method_name ocaml_name method_ty registry =
    let binding = Types.binding ~protocol_id ocaml_name method_ty in
    Protocol_registry.add_implementation protocol_id
      (method_id protocol_id method_name)
      Receiver_id.Seq_receiver binding registry
    |> add_or_fail
  in
  registry
  |> add iseq_id "-first" "Lg_runtime.Runtime_seq.first_opt"
       (TFn ([ sequence ], TOcaml_app ("option", [ element ])))
  |> add iseq_id "-rest" "Lg_runtime.Runtime_seq.rest"
       (TFn ([ sequence ], sequence))
  |> add inext_id "-next" "Lg_runtime.Runtime_seq.next"
       (TFn ([ sequence ], Types.next_seq element))

let declare_drop_protocol registry =
  Protocol_registry.declare drop_id
    [
      signature (method_id drop_id "-drop") [ TUnknown; TInt ]
        (Types.next_seq TUnknown);
    ]
    registry
  |> add_or_fail

let add_drop_protocol registry =
  let element = TVar "drop_element" in
  let sequence = TSeq element in
  let vector = TVector element in
  let result = Types.next_seq element in
  let add receiver ocaml_name method_ty registry =
    let binding = Types.binding ~protocol_id:drop_id ocaml_name method_ty in
    Protocol_registry.add_implementation drop_id (method_id drop_id "-drop")
      receiver binding registry
    |> add_or_fail
  in
  registry
  |> add Receiver_id.Seq_receiver
       "Lg_runtime.Runtime_seq.drop_from_sequence"
       (TFn ([ sequence; TInt ], result))
  |> add Receiver_id.Vector_receiver "Lg_runtime.Runtime_seq.drop_from_vector"
       (TFn ([ vector; TInt ], result))

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
  let receiver = TVar "counted_receiver" in
  Protocol_registry.declare counted_id
    [ signature count_method_id [ receiver ] TInt ]
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

let add_emptyable receiver ocaml_name collection_ty registry =
  let binding =
    Types.binding ~protocol_id:emptyable_id ocaml_name
      (TFn ([ collection_ty ], collection_ty))
  in
  Protocol_registry.add_implementation emptyable_id empty_method_id receiver
    binding registry
  |> add_or_fail

let declare_stack registry =
  Protocol_registry.declare stack_id
    [
      signature (method_id stack_id "-peek") [ TUnknown ] TUnknown;
      signature (method_id stack_id "-pop") [ TUnknown ] TUnknown;
    ]
    registry
  |> add_or_fail

let add_stack receiver peek_name pop_name collection_ty element_ty registry =
  let add method_name ocaml_name method_ty registry =
    let binding = Types.binding ~protocol_id:stack_id ocaml_name method_ty in
    Protocol_registry.add_implementation stack_id
      (method_id stack_id method_name)
      receiver binding registry
    |> add_or_fail
  in
  registry
  |> add "-peek" peek_name (TFn ([ collection_ty ], element_ty))
  |> add "-pop" pop_name (TFn ([ collection_ty ], collection_ty))

let declare_collection_lifecycle_protocols registry =
  let receiver = TVar "equiv_receiver" in
  let persistent = TVar "persistent_collection" in
  let transient = TVar "transient_collection" in
  let element = TVar "transient_element" in
  let key = TVar "transient_key" in
  let value = TVar "transient_value" in
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
         signature (method_id editable_id "-as-transient") [ persistent ]
           transient;
       ]
  |> add_or_fail
  |> Protocol_registry.declare transient_collection_id
       [
         signature (method_id transient_collection_id "-conj!")
           [ transient; element ] transient;
         signature (method_id transient_collection_id "-persistent!")
           [ transient ] persistent;
       ]
  |> add_or_fail
  |> Protocol_registry.declare transient_associative_id
       [
         signature (method_id transient_associative_id "-assoc!")
           [ transient; key; value ] transient;
       ]
  |> add_or_fail
  |> Protocol_registry.declare transient_map_id
       [
         signature (method_id transient_map_id "-dissoc!")
           [ transient; key ] transient;
       ]
  |> add_or_fail
  |> Protocol_registry.declare transient_vector_id
       [
         signature (method_id transient_vector_id "-assoc-n!")
           [ transient; TInt; element ] transient;
         signature (method_id transient_vector_id "-pop!") [ transient ]
           transient;
       ]
  |> add_or_fail
  |> Protocol_registry.declare transient_set_id
       [
         signature (method_id transient_set_id "-disjoin!")
           [ transient; element ] transient;
       ]
  |> add_or_fail

let add_transient_protocols registry =
  let element = TVar "transient_element" in
  let key = TVar "transient_key" in
  let value = TVar "transient_value" in
  let vector = TVector element in
  let transient_vector =
    TOcaml_app ("Lg_runtime.Runtime_transient.vector", [ element ])
  in
  let map = TOcaml_app ("Lg_runtime.Runtime_map.t", [ key; value ]) in
  let transient_map =
    TOcaml_app ("Lg_runtime.Runtime_transient.map", [ key; value ])
  in
  let set = TSet element in
  let transient_set =
    TOcaml_app ("Lg_runtime.Runtime_transient.set", [ element ])
  in
  let add protocol_id method_name receiver ocaml_name ty registry =
    let binding = Types.binding ~protocol_id ocaml_name ty in
    Protocol_registry.add_implementation protocol_id
      (method_id protocol_id method_name)
      receiver binding registry
    |> add_or_fail
  in
  registry
  |> add editable_id "-as-transient" Receiver_id.Vector_receiver
       "Lg_runtime.Runtime_transient.vector_of_vector"
       (TFn ([ vector ], transient_vector))
  |> add editable_id "-as-transient" runtime_map_receiver
       "Lg_runtime.Runtime_transient.map_of_persistent"
       (TFn ([ map ], transient_map))
  |> add editable_id "-as-transient" Receiver_id.Set_receiver
       "Lg.Core_protocols.transient_set" (TFn ([ set ], transient_set))
  |> add transient_collection_id "-conj!"
       (Receiver_id.Host_receiver "Lg_runtime.Runtime_transient.vector")
       "Lg_runtime.Runtime_transient.vector_add"
       (TFn ([ transient_vector; element ], transient_vector))
  |> add transient_collection_id "-persistent!"
       (Receiver_id.Host_receiver "Lg_runtime.Runtime_transient.vector")
       "Lg_runtime.Runtime_transient.vector_persistent"
       (TFn ([ transient_vector ], vector))
  |> add transient_collection_id "-conj!"
       (Receiver_id.Host_receiver "Lg_runtime.Runtime_transient.map")
       "Lg_runtime.Runtime_transient.map_conj_entry"
       (TFn ([ transient_map; TTuple [ key; value ] ], transient_map))
  |> add transient_collection_id "-persistent!"
       (Receiver_id.Host_receiver "Lg_runtime.Runtime_transient.map")
       "Lg_runtime.Runtime_transient.map_persistent"
       (TFn ([ transient_map ], map))
  |> add transient_collection_id "-conj!"
       (Receiver_id.Host_receiver "Lg_runtime.Runtime_transient.set")
       "Lg_runtime.Runtime_transient.set_add"
       (TFn ([ transient_set; element ], transient_set))
  |> add transient_collection_id "-persistent!"
       (Receiver_id.Host_receiver "Lg_runtime.Runtime_transient.set")
       "Lg.Core_protocols.persistent_set" (TFn ([ transient_set ], set))
  |> add transient_associative_id "-assoc!"
       (Receiver_id.Host_receiver "Lg_runtime.Runtime_transient.vector")
       "Lg_runtime.Runtime_transient.vector_assoc"
       (TFn ([ transient_vector; TInt; element ], transient_vector))
  |> add transient_associative_id "-assoc!"
       (Receiver_id.Host_receiver "Lg_runtime.Runtime_transient.map")
       "Lg_runtime.Runtime_transient.map_assoc"
       (TFn ([ transient_map; key; value ], transient_map))
  |> add transient_map_id "-dissoc!"
       (Receiver_id.Host_receiver "Lg_runtime.Runtime_transient.map")
       "Lg_runtime.Runtime_transient.map_dissoc"
       (TFn ([ transient_map; key ], transient_map))
  |> add transient_vector_id "-assoc-n!"
       (Receiver_id.Host_receiver "Lg_runtime.Runtime_transient.vector")
       "Lg_runtime.Runtime_transient.vector_assoc_n"
       (TFn ([ transient_vector; TInt; element ], transient_vector))
  |> add transient_vector_id "-pop!"
       (Receiver_id.Host_receiver "Lg_runtime.Runtime_transient.vector")
       "Lg_runtime.Runtime_transient.vector_pop"
       (TFn ([ transient_vector ], transient_vector))
  |> add transient_set_id "-disjoin!"
       (Receiver_id.Host_receiver "Lg_runtime.Runtime_transient.set")
       "Lg_runtime.Runtime_transient.set_disjoin"
       (TFn ([ transient_set; element ], transient_set))

let declare_protocol_predicate_family registry =
  let indexed_element = TVar "indexed_element" in
  let indexed_receiver = TVar "indexed_receiver" in
  let map_entry_key = TVar "map_entry_key" in
  let map_entry_value = TVar "map_entry_value" in
  let map_entry = TVar "map_entry_receiver" in
  registry
  |> Protocol_registry.declare iindexed_id
       [
         {
           Protocol_registry.method_id = method_id iindexed_id "-nth";
           method_ty =
             TOverloaded_fn
               [
                 {
                   fixed_params = [ indexed_receiver; TInt ];
                   rest_param = None;
                   return_ty = indexed_element;
                 };
                 {
                   fixed_params = [ indexed_receiver; TInt; indexed_element ];
                   rest_param = None;
                   return_ty = indexed_element;
                 };
               ];
         };
       ]
  |> add_or_fail
  |> Protocol_registry.declare sequential_id []
  |> add_or_fail
  |> Protocol_registry.declare map_entry_id
       [
         signature (method_id map_entry_id "-key") [ map_entry ] map_entry_key;
         signature (method_id map_entry_id "-val") [ map_entry ]
           map_entry_value;
       ]
  |> add_or_fail
  |> Protocol_registry.declare sorted_id
       [
         signature (method_id sorted_id "-sorted-seq")
           [ TUnknown; TBool ] TUnknown;
         signature (method_id sorted_id "-sorted-seq-from")
           [ TUnknown; TUnknown; TBool ] TUnknown;
         signature (method_id sorted_id "-entry-key")
           [ TUnknown; TUnknown ] TUnknown;
         signature (method_id sorted_id "-comparator") [ TUnknown ] TUnknown;
       ]
  |> add_or_fail

let add_protocol_predicate_family registry =
  let element = TVar "indexed_element" in
  let vector = TVector element in
  let map_entry_key = TVar "map_entry_key" in
  let map_entry_value = TVar "map_entry_value" in
  let map_entry = TTuple [ map_entry_key; map_entry_value ] in
  let indexed_binding =
    Types.binding ~protocol_id:iindexed_id
      ~overload_targets:
        [ "Lg_runtime.Runtime_vector.nth"; "Lg_runtime.Runtime_vector.nth_default" ]
      "Lg_runtime.Runtime_vector.nth"
      (TOverloaded_fn
         [
           {
             fixed_params = [ vector; TInt ];
             rest_param = None;
             return_ty = element;
           };
           {
             fixed_params = [ vector; TInt; element ];
             rest_param = None;
             return_ty = element;
           };
         ])
  in
  let add_marker receiver registry =
    Protocol_registry.add_marker_implementation sequential_id receiver registry
  in
  registry
  |> Protocol_registry.add_implementation iindexed_id
       (method_id iindexed_id "-nth") Receiver_id.Vector_receiver indexed_binding
  |> add_or_fail
  |> add_marker Receiver_id.List_receiver
  |> add_marker Receiver_id.Vector_receiver
  |> add_marker Receiver_id.Seq_receiver
  |> Protocol_registry.add_implementation map_entry_id
       (method_id map_entry_id "-key") Receiver_id.Tuple_receiver
       (Types.binding ~protocol_id:map_entry_id "Stdlib.fst"
          (TFn ([ map_entry ], map_entry_key)))
  |> add_or_fail
  |> Protocol_registry.add_implementation map_entry_id
       (method_id map_entry_id "-val") Receiver_id.Tuple_receiver
       (Types.binding ~protocol_id:map_entry_id "Stdlib.snd"
          (TFn ([ map_entry ], map_entry_value)))
  |> add_or_fail

let add_vector_reversible_protocol registry =
  let element = TVar "reversible_element" in
  let vector = TVector element in
  let binding =
    Types.binding ~protocol_id:reversible_id "Lg_runtime.Runtime_vector.rseq"
      (TFn ([ vector ], vector))
  in
  Protocol_registry.add_implementation reversible_id
    (method_id reversible_id "-rseq")
    Receiver_id.Vector_receiver binding registry
  |> add_or_fail

let declare_deref registry =
  Protocol_registry.declare deref_id
    [ signature (method_id deref_id "-deref") [ TUnknown ] TUnknown ]
    registry
  |> add_or_fail

let declare_pending registry =
  Protocol_registry.declare pending_id
    [ signature (method_id pending_id "-realized?") [ TUnknown ] TBool ]
    registry
  |> add_or_fail

let add_reference_protocols registry =
  let value = TVar "reference_value" in
  let result = TVar "reference_watch_result" in
  let reference = TRef value in
  let lazy_value = TOcaml_app ("Lazy.t", [ value ]) in
  let reduced_value = Types.reduced value in
  let future = TOcaml_app ("Lg_runtime.Runtime_future.t", [ value ]) in
  let slot = TOcaml_app ("Lg_runtime.Runtime_slot.t", [ value ]) in
  let add receiver protocol_id method_name ocaml_name method_ty registry =
    let binding = Types.binding ~protocol_id ocaml_name method_ty in
    Protocol_registry.add_implementation protocol_id
      (method_id protocol_id method_name)
      receiver binding registry
    |> add_or_fail
  in
  registry
  |> add Receiver_id.Ref_receiver deref_id "-deref"
       "Lg_runtime.Runtime_reference.deref"
       (TFn ([ reference ], value))
  |> add (Receiver_id.Host_receiver "Lazy.t") deref_id "-deref" "Lazy.force"
       (TFn ([ lazy_value ], value))
  |> add
       (Receiver_id.Host_receiver Types.reduced_type_name)
       deref_id "-deref" "Lg_runtime.Runtime_reduced.unreduced"
       (TFn ([ reduced_value ], value))
  |> add (Receiver_id.Host_receiver "Lg_runtime.Runtime_future.t") deref_id
       "-deref" "Lg_runtime.Runtime_future.get" (TFn ([ future ], value))
  |> add (Receiver_id.Host_receiver "Lg_runtime.Runtime_slot.t") deref_id
       "-deref" "Lg_runtime.Runtime_slot.get" (TFn ([ slot ], value))
  |> add (Receiver_id.Host_receiver "Lazy.t") pending_id "-realized?"
       "Lazy.is_val" (TFn ([ lazy_value ], TBool))
  |> add (Receiver_id.Host_receiver "Lg_runtime.Runtime_future.t") pending_id
       "-realized?" "Lg_runtime.Runtime_future.realized"
       (TFn ([ future ], TBool))
  |> add Receiver_id.Seq_receiver pending_id "-realized?"
       "Lg_runtime.Runtime_seq.realized" (TFn ([ TSeq value ], TBool))
  |> add Receiver_id.Ref_receiver reset_id "-reset!"
       "Lg_runtime.Runtime_reference.reset"
       (TFn ([ reference; value ], value))
  |> add (Receiver_id.Host_receiver "Lg_runtime.Runtime_slot.t") reset_id
       "-reset!" "Lg_runtime.Runtime_slot.set"
       (TFn ([ slot; value ], value))
  |> add Receiver_id.Ref_receiver volatile_id "-vreset!"
       "Lg_runtime.Runtime_reference.vreset"
       (TFn ([ reference; value ], value))
  |> add (Receiver_id.Host_receiver "Lg_runtime.Runtime_slot.t") volatile_id
       "-vreset!" "Lg_runtime.Runtime_slot.vreset"
       (TFn ([ slot; value ], value))
  |> add Receiver_id.Ref_receiver swap_id "-swap!"
       "Lg_runtime.Runtime_reference.swap"
       (TFn ([ reference; TFn ([ value ], value) ], value))
  |> add Receiver_id.Ref_receiver atom_id "-compare-and-set!"
       "Lg_runtime.Runtime_reference.compare_and_set"
       (TFn ([ reference; value; value ], TBool))
  |> add (Receiver_id.Host_receiver "Lg_runtime.Runtime_slot.t") swap_id
       "-swap!" "Lg_runtime.Runtime_slot.swap"
       (TFn ([ slot; TFn ([ value ], value) ], value))
  |> add Receiver_id.Ref_receiver watchable_id "-notify-watches"
       "Lg_runtime.Runtime_reference.notify_watches"
       (TFn ([ reference; value; value ], TUnit))
  |> add Receiver_id.Ref_receiver watchable_id "-add-watch"
       "Lg_runtime.Runtime_reference.add_watch"
       (TFn
          ( [
              reference;
              TKeyword;
              TFn ([ TKeyword; reference; value; value ], result);
            ],
            reference ))
  |> add Receiver_id.Ref_receiver watchable_id "-remove-watch"
       "Lg_runtime.Runtime_reference.remove_watch"
       (TFn ([ reference; TKeyword ], reference))
  |> add Receiver_id.Ref_receiver meta_id "-meta"
       "Lg_runtime.Runtime_reference.metadata"
       (TFn ([ reference ], TOcaml "Lg_edn_backend.t"))

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

let declare_volatile registry =
  let value = TVar "volatile_value" in
  Protocol_registry.declare volatile_id
    [ signature (method_id volatile_id "-vreset!") [ TUnknown; value ] value ]
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

let declare_watchable registry =
  let value = TVar "watch_value" in
  let result = TVar "watch_callback_result" in
  Protocol_registry.declare watchable_id
    [
      signature (method_id watchable_id "-notify-watches")
        [ TUnknown; value; value ] TUnit;
      signature (method_id watchable_id "-add-watch")
        [
          TUnknown;
          TKeyword;
          TFn ([ TKeyword; TUnknown; value; value ], result);
        ]
        TUnknown;
      signature (method_id watchable_id "-remove-watch")
        [ TUnknown; TKeyword ] TUnknown;
    ]
    registry
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
  let collection = TVar "conj_collection" in
  let collection_element = TVar "conj_element" in
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
           [ collection; collection_element ] collection;
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

let declare_set_protocol registry =
  let element = TVar "set_element" in
  let set = TVar "set_collection" in
  Protocol_registry.declare set_id
    [ signature (method_id set_id "-disjoin") [ set; element ] set ]
    registry
  |> add_or_fail

let add_static_set_protocol registry =
  let element = TVar "set_element" in
  let set = TSet element in
  let binding =
    Types.binding ~protocol_id:set_id
      "Lg_runtime.Runtime_collection.disjoin_poly_set"
      (TFn ([ set; element ], set))
  in
  Protocol_registry.add_implementation set_id (method_id set_id "-disjoin")
    Receiver_id.Set_receiver binding registry
  |> add_or_fail
  |> Protocol_registry.add_implementation set_id (method_id set_id "-disjoin")
       Receiver_id.Nil_receiver
       (Types.binding ~protocol_id:set_id
          "Lg_runtime.Runtime_collection.disjoin_nil"
          (TFn ([ TNil; element ], TNil)))
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

let add_static_collection_protocols registry =
  let element = TVar "collection_element" in
  let add receiver ocaml_name collection_ty registry =
    let method_ty = TFn ([ collection_ty; element ], collection_ty) in
    let binding = Types.binding ~protocol_id:collection_id ocaml_name method_ty in
    Protocol_registry.add_implementation collection_id
      (method_id collection_id "-conj")
      receiver binding registry
    |> add_or_fail
  in
  registry
  |> add Receiver_id.List_receiver "Lg_runtime.Runtime_collection.conj_list"
       (TList element)
  |> add Receiver_id.Seq_receiver "Lg_runtime.Runtime_collection.conj_seq"
       (TSeq element)
  |> add Receiver_id.Vector_receiver
       "Lg_runtime.Runtime_collection.conj_vector" (TVector element)
  |> add Receiver_id.Set_receiver "Lg_runtime.Runtime_collection.conj_set"
       (TSet element)

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

let add_vector_associative_protocols registry =
  let element = TVar "vector_element" in
  let vector = TVector element in
  let add protocol_id method_name ocaml_name ty registry =
    let binding = Types.binding ~protocol_id ocaml_name ty in
    Protocol_registry.add_implementation protocol_id
      (method_id protocol_id method_name)
      Receiver_id.Vector_receiver binding registry
    |> add_or_fail
  in
  registry
  |> add associative_id "-contains-key?" "Lg_runtime.Runtime_vector.contains_index"
       (TFn ([ vector; TInt ], TBool))
  |> add associative_id "-assoc" "Lg_runtime.Runtime_vector.assoc"
       (TFn ([ vector; TInt; element ], vector))
  |> add find_id "-find" "Lg_runtime.Runtime_vector.find_entry"
       (TFn
          ( [ vector; TInt ],
            TOcaml_app ("option", [ TTuple [ TInt; element ] ]) ))

let add_vector_kv_reduce_protocol registry =
  let element = TVar "vector_kv_element" in
  let accumulator = TVar "vector_kv_accumulator" in
  let vector = TVector element in
  let binding =
    Types.binding ~protocol_id:kv_reduce_id
      "Lg_runtime.Runtime_vector.kv_reduce_protocol"
      (TFn
         ( [
             vector;
             TFn ([ accumulator; TInt; element ], accumulator);
             accumulator;
           ],
           accumulator ))
  in
  Protocol_registry.add_implementation kv_reduce_id
    (method_id kv_reduce_id "-kv-reduce") Receiver_id.Vector_receiver binding
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
  |> declare_sequence_protocols |> add_sequence_protocols
  |> declare_drop_protocol |> add_drop_protocol
  |> declare_reducible
  |> add_reducible Receiver_id.List_receiver "Lg.Core_protocols.reduce_list"
  |> add_reducible Receiver_id.Vector_receiver "Lg.Core_protocols.reduce_vector"
  |> add_reducible Receiver_id.Set_receiver "Lg.Core_protocols.reduce_set"
  |> add_reducible Receiver_id.Seq_receiver "Lg.Core_protocols.reduce_seq"
  |> add_reducible Receiver_id.Array_receiver "Lg.Core_protocols.reduce_array"
  |> add_reducible Receiver_id.String_receiver "Lg.Core_protocols.reduce_string"
  |> add_reducible runtime_map_receiver "Lg_runtime.Runtime_map.reduce_protocol"
  |> add_reducible (Receiver_id.Host_receiver "list")
       "Lg.Core_protocols.reduce_host_list"
  |> add_reducible (Receiver_id.Host_receiver "array")
       "Lg.Core_protocols.reduce_host_array"
  |> add_reducible (Receiver_id.Host_receiver "Seq.t")
       "Lg.Core_protocols.reduce_host_seq"
  |> add_reducible (Receiver_id.Host_receiver "Seq")
       "Lg.Core_protocols.reduce_host_seq_alias"
  |> declare_counted
  |> add_counted Receiver_id.List_receiver
       "Lg_runtime.Runtime_collection.count_list"
  |> add_counted Receiver_id.Vector_receiver
       "Lg_runtime.Runtime_collection.count_vector"
  |> add_counted Receiver_id.Set_receiver
       "Lg_runtime.Runtime_collection.count_set"
  |> add_counted Receiver_id.Array_receiver
       "Lg_runtime.Runtime_collection.count_array"
  |> add_counted Receiver_id.String_receiver
       "Lg_runtime.Runtime_collection.count_string"
  |> add_counted (Receiver_id.Host_receiver "list")
       "Lg_runtime.Runtime_collection.count_host_list"
  |> add_counted (Receiver_id.Host_receiver "array")
       "Lg_runtime.Runtime_collection.count_host_array"
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
  |> add_emptyable Receiver_id.List_receiver
       "Lg_runtime.Runtime_collection.empty_list" (TList (TVar "empty_element"))
  |> add_emptyable Receiver_id.Vector_receiver
       "Lg_runtime.Runtime_collection.empty_vector"
       (TVector (TVar "empty_element"))
  |> add_emptyable Receiver_id.String_receiver
       "Lg_runtime.Runtime_collection.empty_string" TString
  |> add_emptyable Receiver_id.Set_receiver
       "Lg_runtime.Runtime_collection.empty_poly_set"
       (TSet (TVar "empty_element"))
  |> add_emptyable Receiver_id.Seq_receiver "Lg_runtime.Runtime_seq.empty"
       (TSeq (TVar "empty_element"))
  |> add_emptyable runtime_map_receiver "Lg_runtime.Runtime_map.empty_like"
       (TOcaml_app
          ( "Lg_runtime.Runtime_map.t",
            [ TVar "empty_key"; TVar "empty_value" ] ))
  |> declare_stack
  |> add_stack Receiver_id.List_receiver "List.hd" "List.tl"
       (TList (TVar "stack_element")) (TVar "stack_element")
  |> add_stack Receiver_id.Vector_receiver
       "Lg_runtime.Runtime_collection.peek_vector"
       "Lg_runtime.Runtime_collection.pop_vector"
       (TVector (TVar "stack_element")) (TVar "stack_element")
  |> add_stack Receiver_id.Nil_receiver "Lg_runtime.Runtime_collection.peek_nil"
       "Lg_runtime.Runtime_collection.pop_nil" TNil TNil
  |> declare_collection_lifecycle_protocols |> add_transient_protocols
  |> add_vector_reversible_protocol
  |> declare_protocol_predicate_family |> add_protocol_predicate_family
  |> declare_deref |> declare_pending
  |> declare_compare_and_set |> declare_reset |> declare_volatile |> declare_swap
  |> declare_watchable
  |> add_reference_protocols
  |> declare_comparable_protocol
  |> declare_set_protocol |> add_static_set_protocol
  |> declare_map_protocols |> add_runtime_map_protocols
  |> add_static_collection_protocols
  |> declare_vector_protocol |> add_vector_associative_protocols
  |> add_vector_kv_reduce_protocol

let find_seqable receiver_ty registry =
  match Receiver_id.of_type receiver_ty with
  | None -> None
  | Some receiver ->
      Protocol_registry.find_implementation seqable_id seq_method_id receiver
        registry
      |> Option.map (fun (implementation : binding) ->
             let ty =
               instantiate_receiver_method_type receiver_ty implementation.ty
             in
             let ty =
               match (receiver_ty, ty) with
               | ( TNamed_record { type_arguments = [ element_ty ]; _ },
                   TFn (parameters, (TUnknown | TMeta _ | TVar _)) ) ->
                   TFn (parameters, TSeq element_ty)
               | _ -> ty
             in
             {
               implementation with
               ty;
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

let find_hash receiver_ty registry =
  match Receiver_id.of_type receiver_ty with
  | None -> None
  | Some receiver ->
      Protocol_registry.find_implementation hash_id
        (method_id hash_id "-hash") receiver registry
      |> Option.map (fun (implementation : binding) ->
             {
               implementation with
               ty =
                 instantiate_receiver_method_type receiver_ty
                   implementation.ty;
             })
