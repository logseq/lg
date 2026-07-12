open Ast
open Types
open Lowered

module Env = Compiler_environment
let record_type_key = Resolver.record_type_key

let compile_type_alias scope env next_type name type_parameters manifest_form =
  match manifest_form with
  | FKeyword keyword -> (
      match Type_annotation.of_keyword_with_parameters type_parameters keyword with
      | Error _ as err when String.starts_with ~prefix:":param/" keyword -> err
      | Error _ -> Error.error ("unknown type alias target " ^ keyword)
      | Ok manifest ->
          let type_name = Names.sanitize_name name in
          Ok
            ( scope,
              env,
              next_type,
              Type_alias { type_name; type_parameters; manifest } ))
  | _ -> Error.error "type-alias expects a type keyword target"

let compile_type_record scope env next_type name type_parameters field_forms =
  let field_spec = function
    | FList [ FSymbol field_name; FKeyword keyword ] -> (
        match Type_annotation.of_keyword_with_parameters type_parameters keyword with
        | Error _ as err when String.starts_with ~prefix:":param/" keyword -> err
        | Error _ -> Error.error ("unknown record field type " ^ keyword)
        | Ok ty ->
            Ok
              {
                keyword = ":" ^ field_name;
                ocaml_name = Names.sanitize_name field_name;
                ty;
              })
    | _ -> Error.error "type-record fields must be (name :type)"
  in
  let rec parse (fields : field list) = function
    | [] -> Ok (List.rev fields)
    | field_form :: rest -> (
        match field_spec field_form with
        | Error _ as err -> err
        | Ok field ->
            if
              List.exists
                (fun (existing : field) -> existing.ocaml_name = field.ocaml_name)
                fields
            then Error.error "duplicate record field name"
            else parse (field :: fields) rest)
  in
  match parse [] field_forms with
  | Error _ as err -> err
  | Ok [] -> Error.error "type-record expects at least one field"
  | Ok fields ->
      let type_name = Names.sanitize_name name in
      let record_ty =
        Types.named_record ~nominal:true ~type_name ~type_parameters
          ~set_module_name:(type_name ^ "_set") fields
      in
      let env =
        Env.add (record_type_key scope name) (Types.binding type_name record_ty) env
      in
      Ok
        ( scope,
          env,
          next_type,
          Type_def { type_name; type_parameters; fields } )

let record_type_public_binding module_path name env =
  let key = record_type_key module_path name in
  match Env.find_opt key env with
  | Some binding -> Ok (key, binding)
  | None -> Error.error ("internal error: missing record metadata for " ^ name)

let compile_type_variant scope env next_type name type_parameters constructor_forms =
  let constructor_name = function
    | FSymbol constructor -> Ok constructor
    | _ -> Error.error "type-variant constructors must be symbols"
  in
  let payload_type = function
    | FKeyword keyword -> (
        match Type_annotation.of_keyword_with_parameters type_parameters keyword with
        | Ok ty -> Ok ty
        | Error _ as err when String.starts_with ~prefix:":param/" keyword -> err
        | Error _ -> Error.error ("unknown variant payload type " ^ keyword))
    | _ -> Error.error "type-variant payload types must be keywords"
  in
  let constructor_spec = function
    | FSymbol constructor ->
        Ok { constructor_name = constructor; payload_types = [] }
    | FList (constructor_form :: payload_forms) -> (
        match constructor_name constructor_form with
        | Error _ as err -> err
        | Ok constructor_name ->
            let rec parse_payloads acc = function
              | [] -> Ok (List.rev acc)
              | payload_form :: rest -> (
                  match payload_type payload_form with
                  | Error _ as err -> err
                  | Ok payload_ty -> parse_payloads (payload_ty :: acc) rest)
            in
            parse_payloads [] payload_forms
            |> Result.map (fun payload_types -> { constructor_name; payload_types }))
    | _ -> Error.error "type-variant constructors must be symbols"
  in
  let rec parse constructors = function
    | [] -> Ok (List.rev constructors)
    | constructor_form :: rest -> (
        match constructor_spec constructor_form with
        | Error _ as err -> err
        | Ok constructor ->
            if
              List.exists
                (fun existing ->
                  existing.constructor_name = constructor.constructor_name)
                constructors
            then Error.error ("duplicate variant constructor " ^ constructor.constructor_name)
            else parse (constructor :: constructors) rest)
  in
  match parse [] constructor_forms with
  | Error _ as err -> err
  | Ok [] -> Error.error "type-variant expects at least one constructor"
  | Ok constructors ->
      let type_name = Names.sanitize_name name in
      let constructor_bindings =
        let result_type =
          match type_parameters with
          | [] -> TOcaml type_name
          | parameters -> TOcaml_app (type_name, List.map (fun name -> TVar name) parameters)
        in
        constructors
        |> List.map (fun constructor ->
               ( Names.scoped_key scope constructor.constructor_name,
                 Types.binding constructor.constructor_name
                   (TFn (constructor.payload_types, result_type)) ))
      in
      Ok
        ( scope,
          Env.add_bindings constructor_bindings env,
          next_type,
          Type_variant { type_name; type_parameters; constructors } )

