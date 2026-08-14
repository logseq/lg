let leap_year year = year mod 4 = 0 && (year mod 100 <> 0 || year mod 400 = 0)

let days_in_month year = function
  | 1 | 3 | 5 | 7 | 8 | 10 | 12 -> 31
  | 4 | 6 | 9 | 11 -> 30
  | 2 -> if leap_year year then 29 else 28
  | _ -> 0

let days_from_civil year month day =
  let year = if month <= 2 then year - 1 else year in
  let era = if year >= 0 then year / 400 else (year - 399) / 400 in
  let year_of_era = year - (era * 400) in
  let shifted_month = month + if month > 2 then -3 else 9 in
  let day_of_year = ((153 * shifted_month) + 2) / 5 + day - 1 in
  let day_of_era =
    (year_of_era * 365) + (year_of_era / 4) - (year_of_era / 100)
    + day_of_year
  in
  (era * 146097) + day_of_era - 719468

let digits source offset length =
  if offset < 0 || length <= 0 || offset + length > String.length source then None
  else
    let rec loop index value =
      if index = offset + length then Some value
      else
        match source.[index] with
        | '0' .. '9' as digit ->
            loop (index + 1) ((value * 10) + Char.code digit - Char.code '0')
        | _ -> None
    in
    loop offset 0

let parse source =
  let invalid () = Error ("invalid #inst literal " ^ Printf.sprintf "%S" source) in
  let length = String.length source in
  if length < 20 then invalid ()
  else
    match
      ( digits source 0 4,
        digits source 5 2,
        digits source 8 2,
        digits source 11 2,
        digits source 14 2,
        digits source 17 2 )
    with
    | Some year, Some month, Some day, Some hour, Some minute, Some second
      when source.[4] = '-' && source.[7] = '-'
           && (source.[10] = 'T' || source.[10] = 't' || source.[10] = ' ')
           && source.[13] = ':' && source.[16] = ':'
           && month >= 1 && month <= 12
           && day >= 1 && day <= days_in_month year month
           && hour <= 23 && minute <= 59 && second <= 59 ->
        let fraction_start = 19 in
        let timezone_start, milliseconds =
          if fraction_start < length && source.[fraction_start] = '.' then
            let rec finish index =
              if index < length then
                match source.[index] with
                | '0' .. '9' -> finish (index + 1)
                | _ -> index
              else index
            in
            let finish = finish (fraction_start + 1) in
            let count = finish - fraction_start - 1 in
            let milliseconds =
              if count = 0 then None
              else
                let used = min 3 count in
                Option.map
                  (fun value -> value * (if used = 1 then 100 else if used = 2 then 10 else 1))
                  (digits source (fraction_start + 1) used)
            in
            (finish, milliseconds)
          else (fraction_start, Some 0)
        in
        let offset_minutes =
          if timezone_start + 1 = length
             && (source.[timezone_start] = 'Z' || source.[timezone_start] = 'z')
          then Some 0
          else if timezone_start + 6 = length
                  && (source.[timezone_start] = '+' || source.[timezone_start] = '-')
                  && source.[timezone_start + 3] = ':'
          then
            match
              ( digits source (timezone_start + 1) 2,
                digits source (timezone_start + 4) 2 )
            with
            | Some offset_hour, Some offset_minute
              when offset_hour <= 23 && offset_minute <= 59 ->
                let value = (offset_hour * 60) + offset_minute in
                Some (if source.[timezone_start] = '-' then -value else value)
            | _ -> None
          else None
        in
        (match (milliseconds, offset_minutes) with
        | Some milliseconds, Some offset_minutes ->
            let days = Int64.of_int (days_from_civil year month day) in
            let seconds =
              Int64.add
                (Int64.mul days 86400L)
                (Int64.of_int
                   ((hour * 3600) + (minute * 60) + second
                  - (offset_minutes * 60)))
            in
            Ok (Int64.add (Int64.mul seconds 1000L) (Int64.of_int milliseconds))
        | _ -> invalid ())
    | _ -> invalid ()
