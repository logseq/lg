open Ast

let core_call symbol arguments = FList (FCoreSymbol symbol :: arguments)

let get_in target keys default_form =
  let target_name = "__lg_get_in_target" in
  let key_bindings =
    List.mapi
      (fun index key ->
        match key with
        | FKeyword _ | FInt _ | FFloat _ | FString _ | FChar _ | FBool _ ->
            ([], key)
        | _ ->
            let name = "__lg_get_in_key_" ^ string_of_int index in
            ([ FSymbol name; key ], FSymbol name))
      keys
  in
  let default_name = "__lg_get_in_default" in
  let rec expand default target = function
    | [] -> target
    | key :: rest ->
        let get =
          match (rest, default) with
          | [], Some default ->
              FList [ FSymbol "__lg_get-in-step"; target; key; default ]
          | _ -> FList [ FSymbol "__lg_get-in-step"; target; key ]
        in
        expand default get rest
  in
  let bindings =
    [ FSymbol target_name; target ]
    @ List.concat_map fst key_bindings
    @
    match default_form with
    | Some default -> [ FSymbol default_name; default ]
    | None -> []
  in
  let default = Option.map (fun _ -> FSymbol default_name) default_form in
  let body =
    expand default (FSymbol target_name) (List.map snd key_bindings)
  in
  FList [ FSymbol "let"; FVector bindings; body ]

let assoc_in target keys value =
  let rec expand depth target keys =
    let target_name = "__lg_assoc_in_target_" ^ string_of_int depth in
    let key_name = "__lg_assoc_in_key_" ^ string_of_int depth in
    let key, rest =
      match keys with
      | [] -> (FSymbol "nil", [])
      | key :: rest -> (key, rest)
    in
    let nested_value =
      match rest with
      | [] -> value
      | _ ->
          expand (depth + 1)
            (core_call Core_get [ FSymbol target_name; FSymbol key_name ])
            rest
    in
    FList
      [
        FSymbol "let";
        FVector
          [ FSymbol target_name; target; FSymbol key_name; key ];
        core_call Core_assoc
          [ FSymbol target_name; FSymbol key_name; nested_value ];
      ]
  in
  expand 0 target keys

let update_in target keys function_form argument_forms =
  let rec update_arguments = function
    | [] -> function_form :: argument_forms
    | [ key ] -> key :: function_form :: argument_forms
    | key :: rest ->
        key :: FCoreSymbol Core_update :: update_arguments rest
  in
  core_call Core_update (target :: update_arguments keys)
