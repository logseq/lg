external parse_int : string -> int -> int = "parseInt" [@@mel.scope "globalThis"]
external is_nan : 'a -> bool = "isNaN" [@@mel.scope "globalThis"]
