external now : unit -> float = "now" [@@mel.scope "performance"]

let format_elapsed elapsed = Printf.sprintf "%.6f" elapsed
