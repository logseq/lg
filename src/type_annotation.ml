open Ast
open Types

let of_keyword = function
  | ":int" -> Ok TInt
  | ":string" -> Ok TString
  | ":keyword" -> Ok TKeyword
  | ":bool" -> Ok TBool
  | ":nil" -> Ok TNil
  | keyword -> Error.error ("unknown vector element type " ^ keyword)

let of_param_annotation annotation =
  if String.starts_with ~prefix:"^:" annotation then
    match of_keyword (String.sub annotation 1 (String.length annotation - 1)) with
    | Ok ty -> Ok ty
    | Error _ -> Error.error ("unknown parameter type " ^ annotation)
  else Error.error "function parameters must be symbols"

let parse_params = function
  | FVector params ->
      let rec loop acc = function
        | [] -> Ok (List.rev acc)
        | FSymbol annotation :: FSymbol name :: rest
          when String.starts_with ~prefix:"^:" annotation -> (
            match of_param_annotation annotation with
            | Error _ as err -> err
            | Ok ty -> loop ((name, ty) :: acc) rest)
        | FSymbol name :: rest -> loop ((name, TAny) :: acc) rest
        | _ -> Error.error "function parameters must be symbols"
      in
      loop [] params
  | _ -> Error.error "function parameters must be a vector"
