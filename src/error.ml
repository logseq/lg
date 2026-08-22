type phase =
  [ `Lexing
  | `Parsing
  | `Semantic
  | `Lowering
  | `Ocaml
  | `Infrastructure ]

type t = {
  code : string;
  phase : phase;
  message : string;
  location : Location.t option;
}

let error ?location ?(code = "LG2000") ?(phase = `Semantic) message =
  Error { code; phase; message; location }

let with_location_if_missing location error =
  match (error.location, location) with
  | None, Some location -> { error with location = Some location }
  | _ -> error
