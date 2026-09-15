type conversion = {
  code : char;
  flags : char list;
  width : int option;
  precision : int option;
  argument : int option;
}

let invalid message = invalid_arg ("format: " ^ message)

let iter ~text ~conversion source =
  let length = String.length source in
  let cursor = ref 0 and ordinary = ref 0 and previous = ref None in
  let digits () =
    let start = !cursor in
    while !cursor < length && source.[!cursor] >= '0' && source.[!cursor] <= '9' do incr cursor done;
    if start = !cursor then None
    else match int_of_string_opt (String.sub source start (!cursor - start)) with
      | Some value -> Some value | None -> invalid "numeric field is too large"
  in
  while !cursor < length do
    if source.[!cursor] <> '%' then (text source.[!cursor]; incr cursor)
    else (
      incr cursor;
      let start = !cursor in
      let index = match digits () with
        | Some index when !cursor < length && source.[!cursor] = '$' ->
            incr cursor;
            if index < 1 then invalid "argument index must be positive";
            Some (index - 1)
        | _ -> cursor := start; None
      in
      let flags = ref [] in
      while !cursor < length && String.contains "-#+ 0,(<" source.[!cursor] do
        let flag = source.[!cursor] in
        if List.mem flag !flags then invalid "duplicate flag";
        flags := flag :: !flags; incr cursor
      done;
      let has flag = List.mem flag !flags in
      let width = digits () in
      let precision =
        if !cursor < length && source.[!cursor] = '.' then (
          incr cursor; match digits () with Some n -> Some n | None -> invalid "missing precision")
        else None
      in
      if !cursor >= length then invalid "missing conversion";
      let code = source.[!cursor] in
      incr cursor;
      if not (String.contains "sSbBcCdoxXeEfgG%n" code) then invalid "unsupported conversion";
      let kind = Char.lowercase_ascii code in
      let allowed = match kind with
        | 's' | 'b' | 'c' -> "-<" | 'd' -> "-+ 0,(<"
        | 'o' | 'x' -> "-#0<" | 'e' -> "-#+ 0(<"
        | 'f' -> "-#+ 0,(<" | 'g' -> "-+ 0,(<"
        | '%' -> "-" | _ -> ""
      in
      if List.exists (fun flag -> not (String.contains allowed flag)) !flags then invalid "flag is not applicable";
      if has '+' && has ' ' || has '-' && has '0' then invalid "conflicting flags";
      if (has '-' || has '0') && width = None then invalid "flag requires a width";
      if kind = 'n' && width <> None then invalid "newline does not accept width";
      if precision <> None && not (String.contains "sbefg" kind) then invalid "precision is not applicable";
      let argument =
        if kind = '%' || kind = 'n' then None
        else (
          let selected = if has '<' then
              match !previous with Some index -> index | None -> invalid "no previous argument"
            else match index with Some index -> index
              | None -> let index = !ordinary in incr ordinary; index
          in
          previous := Some selected; Some selected)
      in
      conversion { code; flags = !flags; width; precision; argument })
  done

