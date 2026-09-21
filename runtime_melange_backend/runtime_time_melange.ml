external now : unit -> float = "now" [@@mel.scope "performance"]

let format_elapsed = Lg_runtime.Runtime_time.format_elapsed
