open Types

let parenthesize code = "(" ^ code ^ ")"

let apply_code fn_code arg_codes =
  match arg_codes with
  | [] -> parenthesize (fn_code ^ " ()")
  | _ ->
      parenthesize
        (fn_code ^ " " ^ (arg_codes |> List.map parenthesize |> String.concat " "))

let collection_to_list_code collection =
  match collection.ty with
  | TList inner -> Ok (inner, collection.code)
  | TVector inner -> Ok (inner, "Rrbvec.to_list (" ^ collection.code ^ ")")
  | TSet inner ->
      Types.set_module_name inner
      |> Result.map (fun set_module ->
             (inner, set_module ^ ".elements (" ^ collection.code ^ ")"))
  | _ -> Error.error "collection value is not sequenceable"

let collection_from_list_code collection_ty list_code =
  match collection_ty with
  | TList _ -> list_code
  | TVector _ -> "Rrbvec.of_list (" ^ list_code ^ ")"
  | TSet inner -> (
      match Types.set_module_name inner with
      | Ok set_module -> set_module ^ ".of_list (" ^ list_code ^ ")"
      | Error _ -> list_code)
  | _ -> list_code

let remove fn collection =
  match (fn.ty, collection_to_list_code collection) with
  | TFn ([ param_ty ], TBool), Ok (inner, list_code) when Types.equal param_ty inner ->
      let code =
        "List.filter (fun item -> not ("
        ^ apply_code fn.code [ "item" ]
        ^ ")) (" ^ list_code ^ ")"
      in
      Ok (typed collection.ty (collection_from_list_code collection.ty code))
  | TFn _, Ok _ -> Error.error "remove expects a predicate matching collection elements"
  | _, Ok _ -> Error.error "remove expects a function"
  | _, Error _ -> Error.error "remove expects a list, vector, or set"

let take_drop_while name fn collection =
  match (fn.ty, collection_to_list_code collection) with
  | TFn ([ param_ty ], TBool), Ok (inner, list_code) when Types.equal param_ty inner ->
      let list_code =
        if name = "take-while" then
          "(let rec take_while xs = match xs with item :: rest when "
          ^ apply_code fn.code [ "item" ]
          ^ " -> item :: take_while rest | _ -> [] in take_while (" ^ list_code ^ "))"
        else
          "(let rec drop_while xs = match xs with item :: rest when "
          ^ apply_code fn.code [ "item" ]
          ^ " -> drop_while rest | rest -> rest in drop_while (" ^ list_code ^ "))"
      in
      Ok (typed collection.ty (collection_from_list_code collection.ty list_code))
  | TFn _, Ok _ ->
      Error.error (name ^ " expects a predicate matching collection elements")
  | _, Ok _ -> Error.error (name ^ " expects a function")
  | _, Error _ -> Error.error (name ^ " expects a list, vector, or set")

let distinct collection =
  match collection_to_list_code collection with
  | Error _ -> Error.error "distinct expects a list, vector, or set"
  | Ok (_inner, list_code) ->
      let code =
        "(let rec distinct seen acc xs = match xs with [] -> List.rev acc | item :: rest -> if List.mem item seen then distinct seen acc rest else distinct (item :: seen) (item :: acc) rest in distinct [] [] ("
        ^ list_code ^ "))"
      in
      Ok (typed collection.ty (collection_from_list_code collection.ty code))

let dedupe collection =
  match collection_to_list_code collection with
  | Error _ -> Error.error "dedupe expects a list, vector, or set"
  | Ok (_inner, list_code) ->
      let code =
        "(let rec dedupe acc xs = match xs with [] -> List.rev acc | item :: rest -> (match acc with previous :: _ when previous = item -> dedupe acc rest | _ -> dedupe (item :: acc) rest) in dedupe [] ("
        ^ list_code ^ "))"
      in
      Ok (typed collection.ty (collection_from_list_code collection.ty code))

let sort collection =
  match collection_to_list_code collection with
  | Error _ -> Error.error "sort expects a list, vector, or set"
  | Ok (inner, list_code) ->
      Ok (typed (TList inner) ("List.sort compare (" ^ list_code ^ ")"))

let concat collections =
  let rec loop element_ty codes = function
    | [] -> Ok (element_ty, List.rev codes)
    | collection :: rest -> (
        match collection_to_list_code collection with
        | Error _ -> Error.error "concat expects collections"
        | Ok (inner, code) -> (
            match element_ty with
            | None -> loop (Some inner) (code :: codes) rest
            | Some element_ty ->
                if Types.equal element_ty inner then
                  loop (Some element_ty) (code :: codes) rest
                else Error.error "concat element types must match"))
  in
  match loop None [] collections with
  | Error _ as err -> err
  | Ok (None, _) -> Error.error "concat expects at least 1 collection"
  | Ok (Some inner, codes) ->
      Ok (typed (TList inner) ("List.concat [" ^ String.concat "; " codes ^ "]"))

let vec collection =
  match collection_to_list_code collection with
  | Error _ -> Error.error "vec expects a list, vector, or set"
  | Ok (inner, list_code) ->
      Ok (typed (TVector inner) ("Rrbvec.of_list (" ^ list_code ^ ")"))

let set collection =
  match collection_to_list_code collection with
  | Error _ -> Error.error "set expects a list, vector, or set"
  | Ok (inner, list_code) ->
      Types.set_module_name inner
      |> Result.map (fun set_module ->
             typed (TSet inner) (set_module ^ ".of_list (" ^ list_code ^ ")"))

let repeat count value =
  if Types.equal count.ty TInt then
    Ok
      (typed (TList value.ty)
         ("(let rec repeat acc n = if n <= 0 then acc else repeat ("
        ^ value.code ^ " :: acc) (n - 1) in repeat [] (" ^ count.code ^ "))"))
  else Error.error "repeat count must be int"

let interpose separator collection =
  match collection_to_list_code collection with
  | Error _ -> Error.error "interpose expects a collection"
  | Ok (inner, list_code) ->
      if Types.equal separator.ty inner then
        Ok
          (typed (TList inner)
             ("(let rec interpose acc xs = match xs with [] -> List.rev acc | [item] -> List.rev (item :: acc) | item :: rest -> interpose ("
            ^ separator.code
            ^ " :: item :: acc) rest in interpose [] (" ^ list_code ^ "))"))
      else Error.error "interpose separator type must match collection elements"

let interleave collections =
  if List.length collections < 2 then
    Error.error "interleave expects at least two collections"
  else
    let rec loop element_ty codes = function
      | [] -> Ok (element_ty, List.rev codes)
      | collection :: rest -> (
          match collection_to_list_code collection with
          | Error _ -> Error.error "interleave expects collections"
          | Ok (inner, code) -> (
              match element_ty with
              | None -> loop (Some inner) (code :: codes) rest
              | Some element_ty ->
                  if Types.equal element_ty inner then
                    loop (Some element_ty) (code :: codes) rest
                  else Error.error "interleave element types must match"))
    in
    match loop None [] collections with
    | Error _ as err -> err
    | Ok (None, _) -> Error.error "interleave expects at least two collections"
    | Ok (Some inner, codes) ->
        Ok
          (typed (TList inner)
             ("(let rec interleave acc collections = if List.exists (function [] -> true | _ -> false) collections then List.rev acc else let heads = List.map List.hd collections in let tails = List.map List.tl collections in interleave (List.rev_append heads acc) tails in interleave [] ["
            ^ String.concat "; " codes ^ "])"))

let partition name size collection =
  if not (Types.equal size.ty TInt) then Error.error (name ^ " size must be int")
  else
    match collection_to_list_code collection with
    | Error _ -> Error.error (name ^ " expects a collection")
    | Ok (inner, list_code) ->
        let code =
          if name = "partition-all" then
            "(let rec take n acc xs = if n = 0 then (List.rev acc, xs) else match xs with [] -> (List.rev acc, []) | item :: rest -> take (n - 1) (item :: acc) rest in let rec partition_all acc xs = match xs with [] -> List.rev acc | _ -> let chunk, rest = take "
            ^ size.code
            ^ " [] xs in partition_all (chunk :: acc) rest in partition_all [] ("
            ^ list_code ^ "))"
          else
            "(let rec take n acc xs = if n = 0 then Some (List.rev acc, xs) else match xs with [] -> None | item :: rest -> take (n - 1) (item :: acc) rest in let rec partition acc xs = match take "
            ^ size.code
            ^ " [] xs with Some (chunk, rest) -> partition (chunk :: acc) rest | None -> List.rev acc in partition [] ("
            ^ list_code ^ "))"
        in
        Ok (typed (TList (TList inner)) code)

let butlast collection =
  match collection_to_list_code collection with
  | Error _ -> Error.error "butlast expects a collection"
  | Ok (_inner, list_code) ->
      let code =
        "(let rec butlast acc xs = match xs with [] | [_] -> List.rev acc | item :: rest -> butlast (item :: acc) rest in butlast [] ("
        ^ list_code ^ "))"
      in
      Ok (typed collection.ty (collection_from_list_code collection.ty code))

let take_drop_last name count collection =
  if not (Types.equal count.ty TInt) then Error.error (name ^ " count must be int")
  else
    match collection_to_list_code collection with
    | Error _ -> Error.error (name ^ " expects a collection")
    | Ok (_inner, list_code) ->
        let length_code = "List.length source" in
        let code =
          if name = "take-last" then
            "(let source = " ^ list_code ^ " in let drop_count = max 0 ("
            ^ length_code ^ " - (" ^ count.code ^ ")) in "
            ^ Core_collection.drop_list_code "drop_count" "source" ^ ")"
          else
            "(let source = " ^ list_code ^ " in let keep_count = max 0 ("
            ^ length_code ^ " - (" ^ count.code ^ ")) in "
            ^ Core_collection.take_list_code "keep_count" "source" ^ ")"
        in
        Ok (typed collection.ty (collection_from_list_code collection.ty code))

let take_nth count collection =
  if not (Types.equal count.ty TInt) then Error.error "take-nth n must be int"
  else
    match collection_to_list_code collection with
    | Error _ -> Error.error "take-nth expects a collection"
    | Ok (_inner, list_code) ->
        let code =
          "(let rec take_nth index acc xs = match xs with [] -> List.rev acc | item :: rest -> if index mod ("
          ^ count.code
          ^ ") = 0 then take_nth (index + 1) (item :: acc) rest else take_nth (index + 1) acc rest in take_nth 0 [] ("
          ^ list_code ^ "))"
        in
        Ok (typed collection.ty (collection_from_list_code collection.ty code))

let split_at count collection =
  if not (Types.equal count.ty TInt) then Error.error "split-at count must be int"
  else
    match collection_to_list_code collection with
    | Error _ -> Error.error "split-at expects a collection"
    | Ok (_inner, list_code) ->
        let left =
          collection_from_list_code collection.ty
            (Core_collection.take_list_code count.code list_code)
        in
        let right =
          collection_from_list_code collection.ty
            (Core_collection.drop_list_code count.code list_code)
        in
        Ok
          (typed (TVector collection.ty)
             ("Rrbvec.of_list [" ^ left ^ "; " ^ right ^ "]"))

let bounded_count limit collection =
  if not (Types.equal limit.ty TInt) then Error.error "bounded-count limit must be int"
  else
    match collection_to_list_code collection with
    | Error _ -> Error.error "bounded-count expects a collection"
    | Ok (_inner, list_code) ->
        Ok (typed TInt ("min (" ^ limit.code ^ ") (List.length (" ^ list_code ^ "))"))

let dorun collection =
  match collection_to_list_code collection with
  | Error _ -> Error.error "dorun expects a collection"
  | Ok _ -> Ok (typed TNil "()")

let doall collection =
  match collection_to_list_code collection with
  | Error _ -> Error.error "doall expects a collection"
  | Ok _ -> Ok collection

let into target source =
  match collection_to_list_code source with
  | Error _ -> Error.error "into source must be a collection"
  | Ok (source_inner, source_list_code) -> (
      match target.ty with
      | TVector target_inner when Types.equal target_inner source_inner -> (
          match source.ty with
          | TVector _ ->
              Ok
                (typed target.ty
                   ("Rrbvec.append (" ^ target.code ^ ") (" ^ source.code ^ ")"))
          | _ ->
              Ok
                (typed target.ty
                   ("Rrbvec.append_list (" ^ target.code ^ ") (" ^ source_list_code
                  ^ ")")))
      | TList target_inner when Types.equal target_inner source_inner ->
          Ok
            (typed target.ty
               ("List.fold_left (fun acc item -> item :: acc) (" ^ target.code ^ ") ("
              ^ source_list_code ^ ")"))
      | TSet target_inner when Types.equal target_inner source_inner ->
          Types.set_module_name target_inner
          |> Result.map (fun set_module ->
                 typed target.ty
                   (set_module ^ ".of_list (" ^ set_module ^ ".elements "
                  ^ target.code ^ " @ " ^ source_list_code ^ ")"))
      | TVector _ | TList _ | TSet _ ->
          Error.error "into source element type must match target element type"
      | _ -> Error.error "into target must be a collection")

let compile name args =
  match (name, args) with
  | "remove", [ fn; collection ] -> remove fn collection
  | ("take-while" | "drop-while"), [ fn; collection ] ->
      take_drop_while name fn collection
  | "distinct", [ collection ] -> distinct collection
  | "dedupe", [ collection ] -> dedupe collection
  | "sort", [ collection ] -> sort collection
  | "concat", [] -> Error.error "concat expects at least 1 collection"
  | "concat", collections -> concat collections
  | "vec", [ collection ] -> vec collection
  | "set", [ collection ] -> set collection
  | "repeat", [ count; value ] -> repeat count value
  | "interpose", [ separator; collection ] -> interpose separator collection
  | "interleave", collections -> interleave collections
  | ("partition" | "partition-all"), [ size; collection ] ->
      partition name size collection
  | "butlast", [ collection ] -> butlast collection
  | ("take-last" | "drop-last"), [ count; collection ] ->
      take_drop_last name count collection
  | "take-nth", [ count; collection ] -> take_nth count collection
  | "split-at", [ count; collection ] -> split_at count collection
  | "bounded-count", [ limit; collection ] -> bounded_count limit collection
  | "dorun", [ collection ] -> dorun collection
  | "doall", [ collection ] -> doall collection
  | "into", [ target; source ] -> into target source
  | "remove", _ -> Error.error "remove expects function and collection"
  | ("take-while" | "drop-while"), _ ->
      Error.error (name ^ " expects function and collection")
  | ("distinct" | "dedupe" | "sort" | "vec" | "set" | "butlast" | "dorun"
    | "doall"),
    _ -> Error.error (name ^ " expects 1 arguments")
  | "repeat", _ -> Error.error "repeat expects count and value"
  | "interpose", _ -> Error.error "interpose expects separator and collection"
  | ("partition" | "partition-all"), _ ->
      Error.error (name ^ " expects size and collection")
  | ("take-last" | "drop-last"), _ ->
      Error.error (name ^ " expects count and collection")
  | "take-nth", _ -> Error.error "take-nth expects n and collection"
  | "split-at", _ -> Error.error "split-at expects count and collection"
  | "bounded-count", _ -> Error.error "bounded-count expects limit and collection"
  | "into", _ -> Error.error "into expects target and source collections"
  | _ -> Error.error ("unknown function " ^ name)
