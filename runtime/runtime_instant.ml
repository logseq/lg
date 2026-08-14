type t = int64

let of_epoch_millis value = value
let equal = Int64.equal

let civil_from_days days =
  let days = days + 719468 in
  let era = if days >= 0 then days / 146097 else (days - 146096) / 146097 in
  let day_of_era = days - (era * 146097) in
  let year_of_era =
    (day_of_era - (day_of_era / 1460) + (day_of_era / 36524)
   - (day_of_era / 146096))
    / 365
  in
  let year = year_of_era + (era * 400) in
  let day_of_year =
    day_of_era - ((365 * year_of_era) + (year_of_era / 4) - (year_of_era / 100))
  in
  let month_prime = ((5 * day_of_year) + 2) / 153 in
  let day = day_of_year - (((153 * month_prime) + 2) / 5) + 1 in
  let month = month_prime + if month_prime < 10 then 3 else -9 in
  let year = year + if month <= 2 then 1 else 0 in
  (year, month, day)

let to_string instant =
  let seconds = Int64.div instant 1000L in
  let milliseconds = Int64.to_int (Int64.rem instant 1000L) in
  let seconds, milliseconds =
    if milliseconds < 0 then (Int64.pred seconds, milliseconds + 1000)
    else (seconds, milliseconds)
  in
  let days = Int64.div seconds 86400L in
  let seconds_of_day = Int64.to_int (Int64.rem seconds 86400L) in
  let days, seconds_of_day =
    if seconds_of_day < 0 then (Int64.pred days, seconds_of_day + 86400)
    else (days, seconds_of_day)
  in
  let year, month, day = civil_from_days (Int64.to_int days) in
  let hour = seconds_of_day / 3600 in
  let minute = seconds_of_day mod 3600 / 60 in
  let second = seconds_of_day mod 60 in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02d.%03dZ" year month day hour minute
    second milliseconds

let to_edn_string instant = "#inst \"" ^ to_string instant ^ "\""
