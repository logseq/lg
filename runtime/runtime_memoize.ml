let memoize0 f =
  let cache = ref None in
  fun () ->
    match !cache with
    | Some value -> value
    | None ->
        let value = f () in
        cache := Some value;
        value

let memoize1 f =
  let cache = Hashtbl.create 16 in
  fun a ->
    match Hashtbl.find_opt cache a with
    | Some value -> value
    | None ->
        let value = f a in
        Hashtbl.replace cache a value;
        value

let memoize2 f =
  let cache = Hashtbl.create 16 in
  fun a b ->
    let key = (a, b) in
    match Hashtbl.find_opt cache key with
    | Some value -> value
    | None ->
        let value = f a b in
        Hashtbl.replace cache key value;
        value

let memoize3 f =
  let cache = Hashtbl.create 16 in
  fun a b c ->
    let key = (a, b, c) in
    match Hashtbl.find_opt cache key with
    | Some value -> value
    | None ->
        let value = f a b c in
        Hashtbl.replace cache key value;
        value
