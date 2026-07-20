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
let reversible_id = Protocol_id.create ~owner:[] ~name:"IReversible"
let editable_id = Protocol_id.create ~owner:[] ~name:"IEditableCollection"

let transient_collection_id =
  Protocol_id.create ~owner:[] ~name:"ITransientCollection"

let transient_set_id = Protocol_id.create ~owner:[] ~name:"ITransientSet"
let equiv_id = Protocol_id.create ~owner:[] ~name:"IEquiv"
let hash_id = Protocol_id.create ~owner:[] ~name:"IHash"
let comparable_id = Protocol_id.create ~owner:[] ~name:"IComparable"
let object_id = Protocol_id.create ~owner:[] ~name:"Object"
let clojure_hash_id = Protocol_id.create ~owner:[] ~name:"clojure.lang.IHashEq"

let clojure_collection_id =
  Protocol_id.create ~owner:[] ~name:"clojure.lang.IPersistentCollection"

let clojure_transient_collection_id =
  Protocol_id.create ~owner:[] ~name:"clojure.lang.ITransientCollection"

let clojure_editable_collection_id =
  Protocol_id.create ~owner:[] ~name:"clojure.lang.IEditableCollection"

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

let declare_seqable registry =
  Protocol_registry.declare seqable_id
    [
      {
        Protocol_registry.method_id = seq_method_id;
        param_tys = [ TUnknown ];
        return_ty = TUnknown;
      };
    ]
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
    [
      {
        Protocol_registry.method_id = reduce_method_id;
        param_tys = [ TUnknown; TUnknown; TUnknown ];
        return_ty = TUnknown;
      };
    ]
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
    [
      {
        Protocol_registry.method_id = count_method_id;
        param_tys = [ TUnknown ];
        return_ty = TInt;
      };
    ]
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
    [
      {
        Protocol_registry.method_id = nth_method_id;
        param_tys = [ TUnknown; TInt ];
        return_ty = TUnknown;
      };
    ]
    registry
  |> add_or_fail

let declare_emptyable registry =
  Protocol_registry.declare emptyable_id
    [
      {
        Protocol_registry.method_id = empty_method_id;
        param_tys = [ TUnknown ];
        return_ty = TUnknown;
      };
    ]
    registry
  |> add_or_fail

let declare_collection_lifecycle_protocols registry =
  let receiver = TVar "equiv_receiver" in
  registry
  |> Protocol_registry.declare equiv_id
       [
         {
           Protocol_registry.method_id = method_id equiv_id "-equiv";
           param_tys = [ receiver; receiver ];
           return_ty = TBool;
         };
       ]
  |> add_or_fail
  |> Protocol_registry.declare hash_id
       [
         {
           Protocol_registry.method_id = method_id hash_id "-hash";
           param_tys = [ TUnknown ];
           return_ty = TInt;
         };
       ]
  |> add_or_fail
  |> Protocol_registry.declare reversible_id
       [
         {
           Protocol_registry.method_id = method_id reversible_id "-rseq";
           param_tys = [ TUnknown ];
           return_ty = TUnknown;
         };
       ]
  |> add_or_fail
  |> Protocol_registry.declare editable_id
       [
         {
           Protocol_registry.method_id = method_id editable_id "-as-transient";
           param_tys = [ TUnknown ];
           return_ty = TUnknown;
         };
       ]
  |> add_or_fail
  |> Protocol_registry.declare transient_collection_id
       [
         {
           Protocol_registry.method_id =
             method_id transient_collection_id "-conj!";
           param_tys = [ TUnknown; TUnknown ];
           return_ty = TUnknown;
         };
         {
           Protocol_registry.method_id =
             method_id transient_collection_id "-persistent!";
           param_tys = [ TUnknown ];
           return_ty = TUnknown;
         };
       ]
  |> add_or_fail
  |> Protocol_registry.declare transient_set_id
       [
         {
           Protocol_registry.method_id = method_id transient_set_id "-disjoin!";
           param_tys = [ TUnknown; TUnknown ];
           return_ty = TUnknown;
         };
       ]
  |> add_or_fail

let declare_data_protocols registry =
  let dynamic = Types.dynamic_constraint TUnknown in
  registry
  |> Protocol_registry.declare equality_partition_id
       [
         {
           Protocol_registry.method_id = equality_partition_method_id;
           param_tys = [ dynamic ];
           return_ty = TKeyword;
         };
       ]
  |> add_or_fail
  |> Protocol_registry.declare diff_id
       [
         {
           Protocol_registry.method_id = diff_method_id;
           param_tys = [ dynamic; dynamic ];
           return_ty = dynamic;
         };
       ]
  |> add_or_fail

let declare_clojure_host_protocols registry =
  let dynamic = Types.dynamic_constraint TUnknown in
  registry
  |> Protocol_registry.declare comparable_id
       [
         {
           Protocol_registry.method_id = method_id comparable_id "-compare";
           param_tys = [ TUnknown; TUnknown ];
           return_ty = TInt;
         };
       ]
  |> add_or_fail
  |> Protocol_registry.declare object_id
       [
         {
           Protocol_registry.method_id = method_id object_id "hashCode";
           param_tys = [ dynamic ];
           return_ty = TInt;
         };
         {
           Protocol_registry.method_id = method_id object_id "toString";
           param_tys = [ dynamic ];
           return_ty = TString;
         };
         {
           Protocol_registry.method_id = method_id object_id "equals";
           param_tys = [ dynamic; dynamic ];
           return_ty = TBool;
         };
       ]
  |> add_or_fail
  |> Protocol_registry.declare clojure_hash_id
       [
         {
           Protocol_registry.method_id = method_id clojure_hash_id "hasheq";
           param_tys = [ dynamic ];
           return_ty = TInt;
         };
       ]
  |> add_or_fail
  |> Protocol_registry.declare clojure_collection_id
       [
         {
           Protocol_registry.method_id = method_id clojure_collection_id "count";
           param_tys = [ dynamic ];
           return_ty = TInt;
         };
         {
           Protocol_registry.method_id = method_id clojure_collection_id "equiv";
           param_tys = [ dynamic; dynamic ];
           return_ty = TBool;
         };
         {
           Protocol_registry.method_id = method_id clojure_collection_id "empty";
           param_tys = [ dynamic ];
           return_ty = dynamic;
         };
         {
           Protocol_registry.method_id = method_id clojure_collection_id "cons";
           param_tys = [ dynamic; dynamic ];
           return_ty = dynamic;
         };
       ]
  |> add_or_fail
  |> Protocol_registry.declare clojure_editable_collection_id
       [
         {
           Protocol_registry.method_id =
             method_id clojure_editable_collection_id "empty";
           param_tys = [ dynamic ];
           return_ty = dynamic;
         };
         {
           Protocol_registry.method_id =
             method_id clojure_editable_collection_id "asTransient";
           param_tys = [ dynamic ];
           return_ty = dynamic;
         };
       ]
  |> add_or_fail
  |> Protocol_registry.declare clojure_transient_collection_id
       [
         {
           Protocol_registry.method_id =
             method_id clojure_transient_collection_id "conj";
           param_tys = [ dynamic; dynamic ];
           return_ty = dynamic;
         };
         {
           Protocol_registry.method_id =
             method_id clojure_transient_collection_id "persistent";
           param_tys = [ dynamic ];
           return_ty = dynamic;
         };
       ]
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
  |> declare_indexed
  |> add_indexed Receiver_id.List_receiver "Lg.Core_protocols.nth_list"
  |> add_indexed Receiver_id.Vector_receiver "Lg.Core_protocols.nth_vector"
  |> add_indexed Receiver_id.Array_receiver "Lg.Core_protocols.nth_array"
  |> add_indexed Receiver_id.String_receiver "Lg.Core_protocols.nth_string"
  |> add_indexed (Receiver_id.Host_receiver "list")
       "Lg.Core_protocols.nth_host_list"
  |> add_indexed (Receiver_id.Host_receiver "array")
       "Lg.Core_protocols.nth_host_array"
  |> declare_emptyable |> declare_collection_lifecycle_protocols
  |> declare_clojure_host_protocols |> declare_data_protocols

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
