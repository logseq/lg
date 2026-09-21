(* Some libm cbrt implementations are not correctly rounded (glibc returns
   3.0000000000000004 for 27.0). One Newton-Raphson step restores the
   correctly rounded result. *)
let cbrt x =
  if Float.equal x 0.0 then x
  else
    let y = Float.cbrt x in
    y -. (y *. y *. y -. x) /. (3.0 *. y *. y)
let pow = Float.pow
let fmod = Float.rem
let abs = Float.abs
let copy_sign = Float.copy_sign
let trunc = Float.trunc
let get_exponent = Runtime_math_common.get_exponent
let next_after = Runtime_math_common.next_after
let ulp = Runtime_math_common.ulp
let scalb = Runtime_math_common.scalb
