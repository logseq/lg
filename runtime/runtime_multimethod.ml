module Dynamic = Runtime_dynamic

type dispatch_fn = Dynamic.t list -> Dynamic.t
type method_fn = Dynamic.t list -> Dynamic.t

type method_entry = { dispatch : Dynamic.t; fn : method_fn }

type multifn = {
  id : string;
  dispatch_fn : dispatch_fn;
  default_dispatch : Dynamic.t;
  mutable methods : method_entry list;
}

let registry : (string, multifn) Hashtbl.t = Hashtbl.create 64

let dynamic_nil () = Dynamic.nil
let dynamic_int value = Dynamic.int value
let dynamic_float value = Dynamic.float value
let dynamic_char value = Dynamic.char value
let dynamic_string value = Dynamic.string value
let dynamic_keyword value = Dynamic.keyword value
let dynamic_symbol value = Dynamic.symbol value
let dynamic_bool value = Dynamic.bool value
let dynamic_regex value = Dynamic.regex value

let dynamic_vector values = Dynamic.vector (Rrbvec.of_list values)
let dynamic_list values = Dynamic.list values
let dynamic_map entries = Dynamic.map entries

let dynamic_map_of_runtime_map key_mapper value_mapper map =
  map |> Runtime_map.to_seq
  |> Seq.map (fun (key, value) -> (key_mapper key, value_mapper value))
  |> List.of_seq |> Dynamic.map

let register id dispatch_fn default_dispatch =
  let methods =
    match Hashtbl.find_opt registry id with
    | Some existing -> existing.methods
    | None -> []
  in
  Hashtbl.replace registry id { id; dispatch_fn; default_dispatch; methods }

let keyword_or_symbol_to_edn value =
  match value.Dynamic.payload with
  | Dynamic.Keyword name -> Some (Lg_edn_backend.Keyword name)
  | Dynamic.Symbol name -> Some (Lg_edn_backend.Symbol name)
  | _ -> None

let method_matches actual registered =
  Dynamic.equal actual registered
  ||
  match (keyword_or_symbol_to_edn actual, keyword_or_symbol_to_edn registered) with
  | Some actual, Some registered -> Runtime_hierarchy.global_isa actual registered
  | _ -> false

let find_multifn id =
  match Hashtbl.find_opt registry id with
  | Some multifn -> multifn
  | None -> invalid_arg ("unknown multimethod " ^ id)

let register_method id dispatch fn =
  let multifn = find_multifn id in
  multifn.methods <-
    { dispatch; fn }
    :: List.filter
         (fun method_ -> not (Dynamic.equal method_.dispatch dispatch))
         multifn.methods

let remove_method id dispatch =
  let multifn = find_multifn id in
  multifn.methods <-
    List.filter
      (fun method_ -> not (Dynamic.equal method_.dispatch dispatch))
      multifn.methods;
  Dynamic.opaque ("multimethod:" ^ id)

let remove_all_methods id =
  let multifn = find_multifn id in
  multifn.methods <- [];
  Dynamic.opaque ("multimethod:" ^ id)

let find_method_entry multifn dispatch =
  match
    List.find_opt
      (fun method_ -> method_matches dispatch method_.dispatch)
      multifn.methods
  with
  | Some _ as method_ -> method_
  | None ->
      List.find_opt
        (fun method_ -> Dynamic.equal multifn.default_dispatch method_.dispatch)
        multifn.methods

let invoke id args =
  let multifn = find_multifn id in
  let dispatch = multifn.dispatch_fn args in
  match find_method_entry multifn dispatch with
  | Some method_ -> method_.fn args
  | None -> Dynamic.nil

let methods id =
  let multifn = find_multifn id in
  multifn.methods
  |> List.rev
  |> List.map (fun method_ ->
         (method_.dispatch, Dynamic.opaque ("multimethod-method:" ^ multifn.id)))
  |> Dynamic.map

let get_method id dispatch =
  let multifn = find_multifn id in
  match find_method_entry multifn dispatch with
  | Some _ -> Dynamic.opaque ("multimethod-method:" ^ multifn.id)
  | None -> Dynamic.nil

let dispatch_fn id =
  ignore (find_multifn id);
  Dynamic.opaque ("multimethod-dispatch-fn:" ^ id)

let default_dispatch_val id =
  let multifn = find_multifn id in
  multifn.default_dispatch
