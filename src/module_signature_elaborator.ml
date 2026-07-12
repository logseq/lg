open Ast
open Lowered

module Env = Compiler_environment

let compile scope env next_type signature_name item_forms =
  let rec parse items = function
    | [] -> Ok (List.rev items)
    | FList [ FSymbol "val"; FSymbol value_name; FKeyword keyword ] :: rest -> (
        match Type_annotation.of_keyword keyword with
        | Error _ -> Error.error ("unknown signature type " ^ keyword)
        | Ok value_type ->
            parse
              (Signature_value
                 {
                   source_name = value_name;
                   value_name = Names.sanitize_name value_name;
                   value_type;
                 }
              :: items)
              rest)
    | FList [ FSymbol "type"; FSymbol type_name; FKeyword keyword ] :: rest -> (
        match Type_annotation.of_keyword keyword with
        | Error _ -> Error.error ("unknown signature type " ^ keyword)
        | Ok manifest ->
            parse
              (Signature_type
                 {
                   type_name = Names.sanitize_name type_name;
                   type_parameters = [];
                   manifest = Some manifest;
                 }
              :: items)
              rest)
    | FList [ FSymbol "type"; FSymbol type_name ] :: rest ->
        parse
          (Signature_type
             {
               type_name = Names.sanitize_name type_name;
               type_parameters = [];
               manifest = None;
             }
          :: items)
          rest
    | FList [ FSymbol "module"; FSymbol module_name; FSymbol module_signature ]
      :: rest ->
        parse
          (Signature_module
             {
               source_name = module_name;
               module_name = Names.module_segment_to_ocaml module_name;
               module_signature = Names.module_path_to_ocaml module_signature;
             }
          :: items)
          rest
    | FList [ FSymbol "include"; FSymbol module_signature ] :: rest ->
        parse
          (Signature_include
             { module_signature = Names.module_path_to_ocaml module_signature }
          :: items)
          rest
    | FList (FSymbol "include" :: _) :: _ ->
        Error.error "module-signature include expects one module type"
    | FList
        [ FSymbol "type"; FSymbol type_name; (FVector _ as parameter_form);
          FKeyword keyword ]
      :: rest -> (
        match Type_parameters.parse parameter_form with
        | Error _ as err -> err
        | Ok type_parameters -> (
            match
              Type_annotation.of_keyword_with_parameters type_parameters keyword
            with
            | Error (err : Error.t)
              when String.starts_with ~prefix:"unknown type parameter " err.message ->
                Error err
            | Error _ -> Error.error ("unknown signature type " ^ keyword)
            | Ok manifest ->
                parse
                  (Signature_type
                     {
                       type_name = Names.sanitize_name type_name;
                       type_parameters;
                       manifest = Some manifest;
                     }
                  :: items)
                  rest))
    | FList [ FSymbol "type"; FSymbol type_name; (FVector _ as parameter_form) ]
      :: rest -> (
        match Type_parameters.parse parameter_form with
        | Error _ as err -> err
        | Ok type_parameters ->
            parse
              (Signature_type
                 {
                   type_name = Names.sanitize_name type_name;
                   type_parameters;
                   manifest = None;
                 }
              :: items)
              rest)
    | _ ->
        Error.error
          "module-signature items must be val, type, module, or include declarations"
  in
  match parse [] item_forms with
  | Error _ as err -> err
  | Ok [] -> Error.error "module-signature expects at least one signature item"
  | Ok items ->
      let signature_name = Names.module_segment_to_ocaml signature_name in
      let env =
        Env.add_bindings
          (Module_metadata.signature_bindings env signature_name items)
          env
      in
      Ok
        ( scope,
          env,
          next_type,
          Module_signature { signature_name; items } )

