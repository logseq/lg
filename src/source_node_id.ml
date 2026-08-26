type origin = {
  location : Location.t;
  message : string;
}

type t = {
  source_unit : string;
  location : Location.t;
  generated_path : int list;
  origins : origin list;
}

let create ?(generated_path = []) ?(origins = []) ~source_unit location =
  { source_unit; location; generated_path; origins }

let origins id = id.origins

let generated ~source_unit ~location ~path ~origins =
  create ~source_unit ~generated_path:path ~origins location

let hex_encode value =
  let digits = "0123456789abcdef" in
  String.init (String.length value * 2) (fun index ->
      let byte = Char.code value.[index / 2] in
      digits.[if index mod 2 = 0 then byte lsr 4 else byte land 0x0f])

let hex_decode value =
  let digit = function
    | '0' .. '9' as value -> Some (Char.code value - Char.code '0')
    | 'a' .. 'f' as value -> Some (Char.code value - Char.code 'a' + 10)
    | _ -> None
  in
  if String.length value mod 2 <> 0 then None
  else
    let bytes = Bytes.create (String.length value / 2) in
    let rec decode index =
      if index = String.length value then Some (Bytes.to_string bytes)
      else
        match (digit value.[index], digit value.[index + 1]) with
        | Some high, Some low ->
            Bytes.set bytes (index / 2) (Char.chr ((high lsl 4) lor low));
            decode (index + 2)
        | _ -> None
    in
    decode 0

let origin_to_string (origin : origin) =
  let start = origin.location.Location.loc_start in
  let finish = origin.location.loc_end in
  String.concat ","
    [
      hex_encode start.pos_fname;
      string_of_int start.pos_lnum;
      string_of_int start.pos_bol;
      string_of_int start.pos_cnum;
      string_of_int finish.pos_lnum;
      string_of_int finish.pos_bol;
      string_of_int finish.pos_cnum;
      hex_encode origin.message;
    ]

let origin_of_string encoded =
  match String.split_on_char ',' encoded with
  | [ filename; start_line; start_bol; start_offset; end_line; end_bol;
      end_offset; message ] -> (
      match
        ( hex_decode filename,
          int_of_string_opt start_line,
          int_of_string_opt start_bol,
          int_of_string_opt start_offset,
          int_of_string_opt end_line,
          int_of_string_opt end_bol,
          int_of_string_opt end_offset,
          hex_decode message )
      with
      | ( Some filename,
          Some start_line,
          Some start_bol,
          Some start_offset,
          Some end_line,
          Some end_bol,
          Some end_offset,
          Some message ) ->
          let position pos_lnum pos_bol pos_cnum =
            { Lexing.pos_fname = filename; pos_lnum; pos_bol; pos_cnum }
          in
          Some
            {
              location =
                {
                  Location.loc_start = position start_line start_bol start_offset;
                  loc_end = position end_line end_bol end_offset;
                  loc_ghost = false;
                };
              message;
            }
      | _ -> None)
  | _ -> None

let to_string id =
  let path =
    match id.generated_path with
    | [] -> ""
    | path -> ":g" ^ String.concat "." (List.map string_of_int path)
  in
  Printf.sprintf "%s:%s%s:%d-%d" id.location.loc_start.pos_fname id.source_unit
    path id.location.loc_start.pos_cnum id.location.loc_end.pos_cnum
