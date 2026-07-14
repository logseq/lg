type ('key, 'value) t = ('key * 'value) list

let empty = []

let assoc map key value =
  let rec insert acc = function
    | [] -> List.rev ((key, value) :: acc)
    | ((existing_key, _) as entry) :: rest ->
        let comparison = Stdlib.compare key existing_key in
        if comparison = 0 then List.rev_append acc ((key, value) :: rest)
        else if comparison < 0 then List.rev_append acc ((key, value) :: entry :: rest)
        else insert (entry :: acc) rest
  in
  insert [] map

let dissoc map key =
  List.filter (fun (existing_key, _) -> Stdlib.compare key existing_key <> 0) map

let get_option map key =
  map
  |> List.find_opt (fun (existing_key, _) -> Stdlib.compare key existing_key = 0)
  |> Option.map snd

let get_default map key default =
  match get_option map key with Some value -> value | None -> default

let get_option_default map key default =
  match get_option map key with Some value -> Some value | None -> default

let mem map key = Option.is_some (get_option map key)

let find map key =
  List.find_opt (fun (existing_key, _) -> Stdlib.compare key existing_key = 0) map

let count = List.length

let first_exn = function
  | first :: _ -> first
  | [] -> invalid_arg "first called on an empty map"
