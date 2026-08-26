type 'value watch =
  | Watch :
      {
        key : Runtime_keyword.t;
        callback :
          Runtime_keyword.t -> 'value t -> 'value -> 'value -> 'result;
      }
      -> 'value watch

and 'value replacement_observer = {
  replacement_observer_id : int;
  replacement_callback : 'value -> 'value -> bool;
}

and 'value t = {
  mutable value : 'value;
  mutable watches : 'value watch list;
  mutable replacement_observers : 'value replacement_observer list;
  mutable next_replacement_observer_id : int;
  mutable validator : 'value validator;
  mutable metadata : Lg_edn_backend.t option;
  mutable binding_depth : int;
}
and 'value validator = ('value -> bool) option

let of_value value =
  {
    value;
    watches = [];
    replacement_observers = [];
    next_replacement_observer_id = 0;
    validator = None;
    metadata = None;
    binding_depth = 0;
  }

let deref reference = reference.value
let is_bound reference = reference.binding_depth > 0
let enter_binding reference = reference.binding_depth <- reference.binding_depth + 1
let leave_binding reference = reference.binding_depth <- reference.binding_depth - 1

let metadata reference =
  Option.value reference.metadata ~default:Lg_edn_backend.Nil

let reset_metadata reference metadata =
  reference.metadata <-
    (match metadata with Lg_edn_backend.Nil -> None | metadata -> Some metadata);
  metadata

let validate reference value =
  match reference.validator with
  | None -> ()
  | Some validator ->
      if validator value then () else invalid_arg "Invalid reference state"

let get_validator reference = reference.validator

let set_validator reference validator =
  Option.iter
    (fun validate ->
      if validate reference.value then () else invalid_arg "Invalid reference state")
    validator;
  reference.validator <- validator

let notify reference old_value new_value =
  List.iter
    (fun (Watch { key; callback }) ->
      ignore (callback key reference old_value new_value))
    reference.watches

let notify_watches reference old_value new_value =
  notify reference old_value new_value

let reset reference value =
  validate reference value;
  let old_value = reference.value in
  reference.value <- value;
  notify reference old_value value;
  value

let add_replacement_observer reference callback =
  reference.next_replacement_observer_id <-
    reference.next_replacement_observer_id + 1;
  let observer =
    {
      replacement_observer_id = reference.next_replacement_observer_id;
      replacement_callback = callback;
    }
  in
  reference.replacement_observers <-
    reference.replacement_observers @ [ observer ];
  observer.replacement_observer_id

let observe_replacements reference callback =
  let observer_id = add_replacement_observer reference callback in
  fun () ->
    reference.replacement_observers <-
      List.filter
        (fun observer -> observer.replacement_observer_id <> observer_id)
        reference.replacement_observers;
    true

let observe_replacement_notifications reference callback =
  observe_replacements reference (fun _old_value _new_value -> callback ())

let replace reference value =
  validate reference value;
  let old_value = reference.value in
  reference.value <- value;
  match
    List.iter
      (fun observer ->
        if observer.replacement_callback old_value value then ()
        else invalid_arg "replacement observer rejected candidate")
      reference.replacement_observers;
    notify reference old_value value
  with
  | () -> value
  | exception exn ->
      reference.value <- old_value;
      raise exn

let vreset reference value =
  reference.value <- value;
  value

let swap reference update = reset reference (update reference.value)

let compare_and_set reference old_value new_value =
  if reference.value = old_value then (
    ignore (reset reference new_value);
    true)
  else false

let same_key key (Watch watch) = String.equal key watch.key

let add_watch reference key callback =
  reference.watches <-
    List.filter (fun watch -> not (same_key key watch)) reference.watches
    @ [ Watch { key; callback } ];
  reference

let remove_watch reference key =
  reference.watches <-
    List.filter (fun watch -> not (same_key key watch)) reference.watches;
  reference
