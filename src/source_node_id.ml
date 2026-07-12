type t = string

let of_location (location : Location.t) =
  Printf.sprintf "%s:%d-%d" location.loc_start.pos_fname
    location.loc_start.pos_cnum location.loc_end.pos_cnum

let to_string id = id

