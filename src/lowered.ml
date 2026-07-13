type value_pattern =
  | Named of string
  | Unit_pattern
  | Ignore_pattern
  | Located_value of Source_node_id.t * Location.t * value_pattern

type signature_item =
  | Signature_value of {
      source_name : string;
      value_name : string;
      value_type : Types.ty;
      location : Location.t option;
    }
  | Signature_type of {
      type_name : string;
      type_parameters : string list;
      manifest : Types.ty option;
      location : Location.t option;
    }
  | Signature_module of {
      source_name : string;
      module_name : string;
      module_signature : string;
      location : Location.t option;
      signature_location : Location.t option;
    }
  | Signature_include of {
      module_signature : string;
      signature_location : Location.t option;
    }

type variant_constructor = {
  constructor_name : string;
  payload_types : Types.ty list;
  location : Location.t option;
}

type functor_parameter = {
  parameter_name : string;
  parameter_location : Location.t option;
  signature_name : string;
  signature_location : Location.t option;
}

type module_reference = {
  module_name : string;
  location : Location.t option;
}

type compiled_item =
  | Value_binding of {
      pattern : value_pattern;
      expression : Semantic_ir.t;
    }
  | Recursive_value_binding of {
      name : string;
      identity : (Source_node_id.t * Location.t) option;
      expression : Semantic_ir.t;
    }
  | Comment of string
  | Type_def of {
      type_name : string;
      type_parameters : string list;
      fields : Types.field list;
      location : Location.t option;
    }
  | Type_alias of {
      type_name : string;
      type_parameters : string list;
      manifest : Types.ty;
      location : Location.t option;
    }
  | Type_variant of {
      type_name : string;
      type_parameters : string list;
      constructors : variant_constructor list;
      location : Location.t option;
    }
  | Group of compiled_item list
  | Module_def of {
      module_name : string;
      location : Location.t option;
      signature_name : string option;
      signature_location : Location.t option;
      items : compiled_item list;
    }
  | Module_alias of {
      alias_name : string;
      location : Location.t option;
      target_name : string;
      target_location : Location.t option;
    }
  | Module_functor of {
      functor_name : string;
      location : Location.t option;
      parameters : functor_parameter list;
      items : compiled_item list;
    }
  | Module_apply of {
      module_name : string;
      location : Location.t option;
      functor_name : string;
      functor_location : Location.t option;
      arguments : module_reference list;
    }
  | Module_signature of {
      signature_name : string;
      location : Location.t option;
      items : signature_item list;
    }
  | Open_module of module_reference
  | Include_module of module_reference
  | Record_def of {
      var_name : string;
      identity : (Source_node_id.t * Location.t) option;
      type_name : string;
      set_module_name : string;
      fields : Types.field list;
      values : (Types.field * Semantic_ir.t) list;
    }
