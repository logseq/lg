open Ast

let get_in target keys default =
  let rec expand target = function
    | [] -> target
    | key :: rest ->
        let get =
          match (rest, default) with
          | [], Some default ->
              FList [ FSymbol "get"; target; key; default ]
          | _ -> FList [ FSymbol "get"; target; key ]
        in
        expand get rest
  in
  expand target keys

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
            (FList [ FSymbol "get"; FSymbol target_name; FSymbol key_name ])
            rest
    in
    FList
      [
        FSymbol "let";
        FVector
          [ FSymbol target_name; target; FSymbol key_name; key ];
        FList
          [
            FSymbol "__lg_assoc";
            FSymbol target_name;
            FSymbol key_name;
            nested_value;
          ];
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
  FList
    (FCoreSymbol Core_update :: target :: update_arguments keys)
