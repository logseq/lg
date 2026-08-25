type complete = {
  source : string;
  remaining : string;
}

type result =
  | Empty
  | Complete of complete
  | Incomplete
  | Invalid of Lg.Compiler.compile_error

val read : string -> result
