module Dynamic = Runtime_dynamic

type method_fn = Dynamic.t -> Dynamic.t

let methods : (Dynamic.t * method_fn) list ref = ref []

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

let register dispatch fn =
  methods :=
    (dispatch, fn)
    :: List.filter
         (fun (registered, _fn) -> not (Dynamic.equal dispatch registered))
         !methods;
  ()

let keyword_or_symbol_to_edn value =
  match value.Dynamic.payload with
  | Dynamic.Keyword name -> Some (Lg_edn_backend.Keyword name)
  | Dynamic.Symbol name -> Some (Lg_edn_backend.Symbol name)
  | _ -> None

let vector2 value =
  match value.Dynamic.payload with
  | Dynamic.Vector values when Rrbvec.length values = 2 ->
      Some (Rrbvec.nth values 0, Rrbvec.nth values 1)
  | _ -> None

let isa_dispatch actual registered =
  match (vector2 actual, vector2 registered) with
  | Some (actual_reporter, actual_type), Some (registered_reporter, registered_type)
    when Dynamic.equal actual_type registered_type -> (
      match
        ( keyword_or_symbol_to_edn actual_reporter,
          keyword_or_symbol_to_edn registered_reporter )
      with
      | Some actual, Some registered -> Runtime_hierarchy.global_isa actual registered
      | _ -> false)
  | _ -> false

let method_matches actual (registered, _fn) =
  Dynamic.equal actual registered || isa_dispatch actual registered

let dispatch_for reporter event =
  let event_type = Dynamic.get event (Dynamic.keyword ":type") in
  Dynamic.vector (Rrbvec.of_list [ reporter; event_type ])

let default_dispatch_for event =
  let event_type = Dynamic.get event (Dynamic.keyword ":type") in
  Dynamic.vector (Rrbvec.of_list [ Dynamic.keyword ":cljs.test/default"; event_type ])

let report reporter event =
  let dispatch = dispatch_for reporter event in
  let default_dispatch = default_dispatch_for event in
  match
    List.find_opt
      (fun method_ ->
        method_matches dispatch method_
        || Dynamic.equal default_dispatch (fst method_))
      !methods
  with
  | Some (_dispatch, fn) -> fn event
  | None -> Dynamic.nil
