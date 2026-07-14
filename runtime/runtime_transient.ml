let ensure_active active =
  if not active then invalid_arg "transient used after persistent!"

type 'value set = {
  values : ('value, unit) Hashtbl.t;
  mutable active : bool;
}

let set_empty () = { values = Hashtbl.create 16; active = true }

let set_of_list values =
  let set = set_empty () in
  List.iter (fun value -> Hashtbl.replace set.values value ()) values;
  set

let set_mem set value =
  ensure_active set.active;
  Hashtbl.mem set.values value

let set_count set =
  ensure_active set.active;
  Hashtbl.length set.values

let set_add set value =
  ensure_active set.active;
  Hashtbl.replace set.values value ();
  set

let set_to_seq set =
  ensure_active set.active;
  let values = Hashtbl.to_seq_keys set.values |> List.of_seq in
  set.active <- false;
  List.to_seq values

type 'value vector = {
  mutable reversed : 'value list;
  mutable active : bool;
}

let vector_empty () = { reversed = []; active = true }

let vector_of_list values = { reversed = List.rev values; active = true }

let vector_add vector value =
  ensure_active vector.active;
  vector.reversed <- value :: vector.reversed;
  vector

let vector_assoc vector index value =
  ensure_active vector.active;
  let length = List.length vector.reversed in
  if index < 0 || index > length then invalid_arg "assoc! index out of bounds"
  else if index = length then vector_add vector value
  else (
    let reversed_index = length - index - 1 in
    vector.reversed <-
      List.mapi
        (fun current existing ->
          if current = reversed_index then value else existing)
        vector.reversed;
    vector)

let vector_persistent vector =
  ensure_active vector.active;
  let result = Rrbvec.of_list (List.rev vector.reversed) in
  vector.active <- false;
  result

type ('key, 'value) map = {
  entries : ('key, 'value) Hashtbl.t;
  mutable active : bool;
}

let map_empty () = { entries = Hashtbl.create 16; active = true }

let map_of_list entries =
  let map = map_empty () in
  List.iter (fun (key, value) -> Hashtbl.replace map.entries key value) entries;
  map

let map_assoc map key value =
  ensure_active map.active;
  Hashtbl.replace map.entries key value;
  map

let map_persistent map =
  ensure_active map.active;
  let result =
    Hashtbl.fold
      (fun key value result -> Runtime_map.assoc result key value)
      map.entries Runtime_map.empty
  in
  map.active <- false;
  result
