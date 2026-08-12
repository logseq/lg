type 'value watch =
  | Watch :
      {
        key : Runtime_keyword.t;
        callback :
          Runtime_keyword.t -> 'value t -> 'value -> 'value -> 'result;
      }
      -> 'value watch

and 'value t = { mutable value : 'value; mutable watches : 'value watch list }

let of_value value = { value; watches = [] }

let deref reference = reference.value

let notify reference old_value new_value =
  List.iter
    (fun (Watch { key; callback }) ->
      ignore (callback key reference old_value new_value))
    reference.watches

let notify_watches reference old_value new_value =
  notify reference old_value new_value

let reset reference value =
  let old_value = reference.value in
  reference.value <- value;
  notify reference old_value value;
  value

let vreset reference value = reset reference value

let swap reference update = reset reference (update reference.value)

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
