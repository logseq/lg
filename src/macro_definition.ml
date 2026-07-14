open Ast

type arity = {
  params : form list;
  body : form list;
}

type t = {
  namespace : string;
  name : string;
  arities : arity list;
}

let drop_docstring = function FString _ :: rest -> rest | forms -> forms

let parse_arity = function
  | FList (FVector params :: body) -> Ok { params; body }
  | _ -> Error.error "macro arity expects a parameter vector and body"

let create ~namespace ~name forms =
  match drop_docstring forms with
  | FVector params :: body -> Ok { namespace; name; arities = [ { params; body } ] }
  | arity_forms ->
      let rec parse acc = function
        | [] when acc = [] -> Error.error "defmacro expects at least one arity"
        | [] -> Ok { namespace; name; arities = List.rev acc }
        | form :: rest -> (
            match parse_arity form with
            | Error _ as err -> err
            | Ok arity -> parse (arity :: acc) rest)
      in
      parse [] arity_forms
