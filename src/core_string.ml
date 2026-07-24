open Types

let runtime name = "Lg_runtime.Runtime_string." ^ name

let fn args ret = TFn (args, ret)

let binding name ocaml_name ty = (name, Types.binding ocaml_name ty)

let bindings =
  [
    binding "blank?" (runtime "blank") (fn [ TString ] TBool);
    binding "capitalize" (runtime "capitalize") (fn [ TString ] TString);
    binding "ends-with?" (runtime "ends_with") (fn [ TString; TString ] TBool);
    binding "includes?" (runtime "includes") (fn [ TString; TString ] TBool);
    binding "index-of" (runtime "index_of_int") (fn [ TString; TString ] TInt);
    binding "join" (runtime "join")
      (fn [ TString; Types.seqable_constraint TString ] TString);
    binding "last-index-of" (runtime "last_index_of_int")
      (fn [ TString; TString ] TInt);
    binding "lower-case" "String.lowercase_ascii" (fn [ TString ] TString);
    binding "re-quote-replacement" (runtime "identity") (fn [ TString ] TString);
    binding "replace" (runtime "replace") (fn [ TString; TString; TString ] TString);
    binding "replace-first" (runtime "replace_first")
      (fn [ TString; TString; TString ] TString);
    binding "reverse" (runtime "reverse") (fn [ TString ] TString);
    binding "split" (runtime "split")
      (fn [ TString; TUnknown ] (TVector TString));
    binding "split-lines" (runtime "split_lines") (fn [ TString ] (TVector TString));
    binding "starts-with?" (runtime "starts_with") (fn [ TString; TString ] TBool);
    binding "trim" "String.trim" (fn [ TString ] TString);
    binding "trim-newline" (runtime "trim_newline") (fn [ TString ] TString);
    binding "triml" (runtime "triml") (fn [ TString ] TString);
    binding "trimr" (runtime "trimr") (fn [ TString ] TString);
    binding "upper-case" "String.uppercase_ascii" (fn [ TString ] TString);
  ]
