external now : unit -> float = "lg_monotonic_time_ms"

let format_elapsed elapsed = Printf.sprintf "%.6f" elapsed
