external cbrt : float -> float = "cbrt" [@@mel.scope "Math"]
external pow : float -> float -> float = "pow" [@@mel.scope "Math"]

let fmod = mod_float
let abs = Float.abs
let copy_sign = Float.copy_sign
let trunc = Float.trunc
let get_exponent = Runtime_math_common.get_exponent
let next_after = Runtime_math_common.next_after
let ulp = Runtime_math_common.ulp
let scalb = Runtime_math_common.scalb
