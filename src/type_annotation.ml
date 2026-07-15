open Ast
open Types

let split_top_level_type_args source =
  let rec loop depth start index acc =
    if index >= String.length source then
      let part = String.sub source start (index - start) |> String.trim in
      List.rev (part :: acc)
    else
      match source.[index] with
      | '<' -> loop (depth + 1) start (index + 1) acc
      | '>' when depth > 0 -> loop (depth - 1) start (index + 1) acc
      | (',' | ';') when depth = 0 ->
          let part = String.sub source start (index - start) |> String.trim in
          loop depth (index + 1) (index + 1) (part :: acc)
      | _ -> loop depth start (index + 1) acc
  in
  loop 0 0 0 []

let matching_type_application source =
  match String.index_opt source '<' with
  | None -> None
  | Some open_index ->
      if not (String.ends_with ~suffix:">" source) then None
      else
        let rec loop depth index =
          if index >= String.length source then depth = 0
          else
            match source.[index] with
            | '<' -> loop (depth + 1) (index + 1)
            | '>' ->
                let depth = depth - 1 in
                depth >= 0
                && (if depth = 0 then index = String.length source - 1
                    else loop depth (index + 1))
            | _ -> loop depth (index + 1)
        in
        if loop 0 open_index then Some open_index else None

let validate_ocaml_type_application name args =
  match (name, args) with
  | "tuple", _ :: _ :: _ -> Ok ()
  | "tuple", _ -> Error.error "tuple expects at least two type arguments"
  | "option", [ _ ] -> Ok ()
  | "option", _ -> Error.error "option expects one type argument"
  | "result", [ _; _ ] -> Ok ()
  | "result", _ -> Error.error "result expects two type arguments"
  | "fn", _ :: _ :: _ -> Ok ()
  | "fn", _ -> Error.error "fn expects at least one parameter and a return type"
  | _, [] -> Error.error "OCaml type application expects at least one argument"
  | _ -> Ok ()

let rec parse_ocaml_type source =
  let source = String.trim source in
  if source = "" then Error.error "empty OCaml type"
  else
    match matching_type_application source with
    | None ->
        if String.contains source '<' || String.contains source '>' then
          Error.error "malformed OCaml type application"
        else if source = "int" then Ok TInt
        else if source = "float" then Ok TFloat
        else if source = "char" then Ok TChar
        else if source = "string" then Ok TString
        else if source = "bool" then Ok TBool
        else if source = "unit" then Ok TUnit
        else if source = "dynamic" then Ok (Types.dynamic_constraint TUnknown)
        else
          (match String.rindex_opt source '/' with
          | Some separator when separator > 0 ->
              let module_path = String.sub source 0 separator in
              if Char.uppercase_ascii module_path.[0] = module_path.[0] then
                let type_name =
                  String.sub source (separator + 1)
                    (String.length source - separator - 1)
                  |> Names.sanitize_name
                in
                Ok (TOcaml (module_path ^ "." ^ type_name))
              else Ok (TOcaml source)
          | _ -> Ok (TOcaml source))
    | Some open_index ->
        let name = String.sub source 0 open_index |> String.trim in
        let inner =
          String.sub source (open_index + 1)
            (String.length source - open_index - 2)
        in
        if name = "" then Error.error "missing OCaml type constructor"
        else
          let arg_sources = split_top_level_type_args inner in
          if List.exists (( = ) "") arg_sources then
            Error.error "empty OCaml type argument"
          else
            let rec parse_args acc = function
              | [] -> Ok (List.rev acc)
              | arg :: rest -> (
                  match parse_ocaml_type arg with
                  | Error _ as err -> err
                  | Ok ty -> parse_args (ty :: acc) rest)
            in
            match parse_args [] arg_sources with
            | Error _ as err -> err
            | Ok args -> (
                match validate_ocaml_type_application name args with
                | Error _ as err -> err
                | Ok () ->
                    if name = "tuple" then Ok (TTuple args)
                    else if name = "array" then
                      match args with
                      | [ inner ] -> Ok (TArray inner)
                      | _ -> Error.error "array expects one type argument"
                    else if name = "ref" then
                      match args with
                      | [ inner ] -> Ok (TRef inner)
                      | _ -> Error.error "ref expects one type argument"
                    else if name = "fn" then
                      match List.rev args with
                      | return_ty :: reversed_params ->
                          Ok (TFn (List.rev reversed_params, return_ty))
                      | [] -> assert false
                    else Ok (TOcaml_app (name, args)))

let of_keyword = function
  | ":int" -> Ok TInt
  | ":float" -> Ok TFloat
  | ":char" -> Ok TChar
  | ":string" -> Ok TString
  | ":symbol" -> Ok TSymbol
  | ":keyword" -> Ok TKeyword
  | ":bool" -> Ok TBool
  | ":unit" -> Ok TUnit
  | ":dynamic" -> Ok (Types.dynamic_constraint TUnknown)
  | ":nil" -> Error.error "nil is not a valid type annotation"
  | keyword when String.starts_with ~prefix:":ocaml/" keyword ->
      Error.error "the :ocaml/ type prefix is not supported"
  | keyword when String.starts_with ~prefix:":param/" keyword ->
      Error.error "the :param/ type prefix is not supported"
  | keyword when String.starts_with ~prefix:":" keyword ->
      String.sub keyword 1 (String.length keyword - 1) |> parse_ocaml_type
  | keyword -> Error.error ("unknown vector element type " ^ keyword)

let rec resolve_type_parameters parameters = function
  | TOcaml name when List.mem name parameters ->
      Ok (TVar (Names.sanitize_name name))
  | TOcaml name
    when parameters <> [] && String.length name = 1
         && Char.lowercase_ascii name.[0] = name.[0] ->
      Error.error ("unknown type parameter " ^ name)
  | TOcaml_app (name, args) ->
      let rec resolve_args acc = function
        | [] -> Ok (TOcaml_app (name, List.rev acc))
        | arg :: rest -> (
            match resolve_type_parameters parameters arg with
            | Error _ as err -> err
            | Ok arg -> resolve_args (arg :: acc) rest)
      in
      resolve_args [] args
  | TArray inner ->
      resolve_type_parameters parameters inner |> Result.map (fun inner -> TArray inner)
  | TRef inner ->
      resolve_type_parameters parameters inner |> Result.map (fun inner -> TRef inner)
  | TTuple args ->
      let rec resolve_args acc = function
        | [] -> Ok (TTuple (List.rev acc))
        | arg :: rest -> (
            match resolve_type_parameters parameters arg with
            | Error _ as err -> err
            | Ok arg -> resolve_args (arg :: acc) rest)
      in
      resolve_args [] args
  | TFn (params, return_ty) ->
      let rec resolve_params acc = function
        | [] -> (
            match resolve_type_parameters parameters return_ty with
            | Error _ as err -> err
            | Ok return_ty -> Ok (TFn (List.rev acc, return_ty)))
        | param :: rest -> (
            match resolve_type_parameters parameters param with
            | Error _ as err -> err
            | Ok param -> resolve_params (param :: acc) rest)
      in
      resolve_params [] params
  | ty -> Ok ty

let of_keyword_with_parameters parameters keyword =
  match of_keyword keyword with
  | Error _ as err -> err
  | Ok ty -> resolve_type_parameters parameters ty

let of_param_annotation annotation =
  if String.starts_with ~prefix:"^:" annotation then
    match of_keyword (String.sub annotation 1 (String.length annotation - 1)) with
    | Ok ty -> Ok ty
    | (Error _ as err)
      when String.starts_with ~prefix:"^:ocaml/" annotation
           || String.starts_with ~prefix:"^:param/" annotation ->
        err
    | Error _
      when String.starts_with ~prefix:"^:option<" annotation
           || String.starts_with ~prefix:"^:result<" annotation
           || String.starts_with ~prefix:"^:tuple<" annotation
           || String.contains annotation '<' ->
        Error.error ("invalid type annotation " ^ annotation)
    | Error _ -> Error.error ("unknown parameter type " ^ annotation)
  else if String.starts_with ~prefix:"^" annotation && String.length annotation > 1
  then
    let type_name =
      String.sub annotation 1 (String.length annotation - 1)
    in
    (match type_name with
    | "int" | "long" | "number" -> Ok TInt
    | "boolean" | "Boolean" -> Ok TBool
    | "double" | "float" -> Ok TFloat
    | "String" -> Ok TString
    | _ -> (
        match Host_interop.type_annotation type_name with
        | Some host_type -> Ok (TOcaml host_type)
        | None -> Ok (TOcaml ("__lg_record:" ^ type_name))))
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
        | FSymbol name :: rest -> loop ((name, TUnknown) :: acc) rest
        | _ -> Error.error "function parameters must be symbols"
      in
      loop [] params
  | _ -> Error.error "function parameters must be a vector"
