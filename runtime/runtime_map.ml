type ('key, 'value) t = ('key * 'value) list

let empty = []

let dynamic_key_equal left right =
  Runtime_dynamic.equal left right || Runtime_dynamic.equal right left

let assoc_by compare map key value =
  let rec insert acc = function
    | [] -> List.rev ((key, value) :: acc)
    | ((existing_key, _) as entry) :: rest ->
        let comparison = compare key existing_key in
        if comparison = 0 then List.rev_append acc ((key, value) :: rest)
        else if comparison < 0 then List.rev_append acc ((key, value) :: entry :: rest)
        else insert (entry :: acc) rest
  in
  insert [] map

let assoc map key value = assoc_by Stdlib.compare map key value

let assoc_dynamic map key value =
  let rec insert accumulated = function
    | [] -> List.rev ((key, value) :: accumulated)
    | ((existing_key, _) as entry) :: rest ->
        if dynamic_key_equal key existing_key then
          List.rev_append accumulated ((key, value) :: rest)
        else
          match
            try Some (Runtime_dynamic.compare key existing_key)
            with Invalid_argument _ -> None
          with
          | Some comparison when comparison < 0 ->
              List.rev_append accumulated ((key, value) :: entry :: rest)
          | Some _ | None -> insert (entry :: accumulated) rest
  in
  insert [] map

let zipmap_by assoc keys values =
  let rec build map keys values =
    match (keys (), values ()) with
    | Seq.Cons (key, remaining_keys), Seq.Cons (value, remaining_values) ->
        build (assoc map key value) remaining_keys remaining_values
    | Seq.Nil, _ | _, Seq.Nil -> map
  in
  build empty keys values

let zipmap keys values = zipmap_by assoc keys values
let zipmap_dynamic keys values = zipmap_by assoc_dynamic keys values

let dissoc_by compare map key =
  List.filter (fun (existing_key, _) -> compare key existing_key <> 0) map

let dissoc map key = dissoc_by Stdlib.compare map key

let dissoc_dynamic map key =
  List.filter
    (fun (existing_key, _) -> not (dynamic_key_equal key existing_key))
    map

let get_option_by compare map key =
  map
  |> List.find_opt (fun (existing_key, _) -> compare key existing_key = 0)
  |> Option.map snd

let get_option map key = get_option_by Stdlib.compare map key

let get_option_dynamic map key =
  map
  |> List.find_opt (fun (existing_key, _) -> dynamic_key_equal key existing_key)
  |> Option.map snd

let get_default map key default =
  match get_option map key with Some value -> value | None -> default

let get_default_dynamic map key default =
  match get_option_dynamic map key with Some value -> value | None -> default

let get_option_default map key default =
  match get_option map key with Some value -> Some value | None -> default

let get_option_default_dynamic map key default =
  match get_option_dynamic map key with Some value -> Some value | None -> default

let mem map key = Option.is_some (get_option map key)
let mem_dynamic map key = Option.is_some (get_option_dynamic map key)

let find map key =
  List.find_opt (fun (existing_key, _) -> Stdlib.compare key existing_key = 0) map

let find_dynamic map key =
  List.find_opt
    (fun (existing_key, _) -> dynamic_key_equal key existing_key)
    map

let select_keys_by find assoc map keys =
  Seq.fold_left
    (fun selected key ->
      match find map key with
      | Some (existing_key, value) -> assoc selected existing_key value
      | None -> selected)
    empty keys

let select_keys map keys = select_keys_by find assoc map keys

let select_keys_dynamic map keys =
  select_keys_by find_dynamic assoc_dynamic map keys

let count = List.length

let first_exn = function
  | first :: _ -> first
  | [] -> invalid_arg "first called on an empty map"
