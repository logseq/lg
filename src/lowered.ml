type value_pattern =
  | Named of string
  | Unit_pattern
  | Ignore_pattern

type signature_item =
  | Signature_value of {
      source_name : string;
      value_name : string;
      value_type : Types.ty;
    }
  | Signature_type of {
      type_name : string;
      type_parameters : string list;
      manifest : Types.ty option;
    }

type variant_constructor = {
  constructor_name : string;
  payload_types : Types.ty list;
}

type compiled_item =
  | Value_binding of {
      pattern : value_pattern;
      expression : Ocaml_ir.t;
    }
  | Comment of string
  | Type_def of {
      type_name : string;
      type_parameters : string list;
      fields : Types.field list;
    }
  | Type_alias of {
      type_name : string;
      type_parameters : string list;
      manifest : Types.ty;
    }
  | Type_variant of {
      type_name : string;
      type_parameters : string list;
      constructors : variant_constructor list;
    }
  | Group of compiled_item list
  | Module_def of {
      module_name : string;
      signature_name : string option;
      items : compiled_item list;
    }
  | Module_alias of {
      alias_name : string;
      target_name : string;
    }
  | Module_functor of {
      functor_name : string;
      parameter_name : string;
      parameter_signature : string;
      items : compiled_item list;
    }
  | Module_apply of {
      module_name : string;
      functor_name : string;
      argument_name : string;
    }
  | Module_signature of {
      signature_name : string;
      items : signature_item list;
    }
  | Open_module of string
  | Include_module of string
  | Record_def of {
      var_name : string;
      type_name : string;
      set_module_name : string;
      fields : Types.field list;
      values : (Types.field * Ocaml_ir.t) list;
    }
