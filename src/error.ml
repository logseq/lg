type t = {
  message : string;
  location : Location.t option;
}

let error ?location message = Error { message; location }

let with_location_if_missing location error =
  match (error.location, location) with
  | None, Some location -> { error with location = Some location }
  | _ -> error
