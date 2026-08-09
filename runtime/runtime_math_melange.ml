external cbrt : float -> float = "cbrt" [@@mel.scope "Math"]
external pow : float -> float -> float = "pow" [@@mel.scope "Math"]

let fmod = mod_float
