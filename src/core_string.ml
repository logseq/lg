open Types

let runtime name = "Cljml.Runtime_string." ^ name

let fn args ret = TFn (args, ret)

let binding name ocaml_name ty = (name, { ocaml_name; ty })

let bindings =
  [
    binding "blank?" "(fun source -> String.trim source = \"\")" (fn [ TString ] TBool);
    binding "capitalize" (runtime "capitalize") (fn [ TString ] TString);
    binding "ends-with?"
      "(fun source suffix -> String.ends_with ~suffix source)"
      (fn [ TString; TString ] TBool);
    binding "includes?"
      "(fun source needle -> Cljml.Runtime_string.index_of source needle >= 0)"
      (fn [ TString; TString ] TBool);
    binding "index-of" (runtime "index_of") (fn [ TString; TString ] TInt);
    binding "join"
      "(fun separator values -> String.concat separator (Rrbvec.to_list values))"
      (fn [ TString; TVector TString ] TString);
    binding "last-index-of" (runtime "last_index_of") (fn [ TString; TString ] TInt);
    binding "lower-case" "String.lowercase_ascii" (fn [ TString ] TString);
    binding "re-quote-replacement" "(fun source -> source)" (fn [ TString ] TString);
    binding "replace" (runtime "replace") (fn [ TString; TString; TString ] TString);
    binding "replace-first" (runtime "replace_first")
      (fn [ TString; TString; TString ] TString);
    binding "reverse" (runtime "reverse") (fn [ TString ] TString);
    binding "split" (runtime "split") (fn [ TString; TString ] (TVector TString));
    binding "split-lines" (runtime "split_lines") (fn [ TString ] (TVector TString));
    binding "starts-with?"
      "(fun source prefix -> String.starts_with ~prefix source)"
      (fn [ TString; TString ] TBool);
    binding "trim" "String.trim" (fn [ TString ] TString);
    binding "trim-newline" (runtime "trim_newline") (fn [ TString ] TString);
    binding "triml" (runtime "triml") (fn [ TString ] TString);
    binding "trimr" (runtime "trimr") (fn [ TString ] TString);
    binding "upper-case" "String.uppercase_ascii" (fn [ TString ] TString);
  ]
