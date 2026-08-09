; Copyright (c) Rich Hickey. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; This LG port follows ClojureScript's cljs.math source API.

(ns cljs.math
  (:require [ocaml.Lg_runtime.Runtime_math :as runtime-math]
            [ocaml.Lg_runtime.Runtime_math_melange :as runtime-math-melange]
            [ocaml.Lg_runtime.Runtime_exception :as runtime-exception]
            [ocaml.Lg_runtime.Runtime_random :as runtime-random]
            [ocaml.Stdlib :as stdlib]))

(def E 2.718281828459045)
(def PI 3.141592653589793)

(def DEGREES-TO-RADIANS 0.017453292519943295)
(def RADIANS-TO-DEGREES 57.29577951308232)

(defn sin [a] (stdlib/sin a))
(defn cos [a] (stdlib/cos a))
(defn tan [a] (stdlib/tan a))
(defn asin [a] (stdlib/asin a))
(defn acos [a] (stdlib/acos a))
(defn atan [a] (stdlib/atan a))

(defn to-radians [degrees]
  (* degrees DEGREES-TO-RADIANS))

(defn to-degrees [radians]
  (* radians RADIANS-TO-DEGREES))

(defn exp [a] (stdlib/exp a))
(defn log [a] (stdlib/log a))
(defn log10 [a] (stdlib/log10 a))
(defn sqrt [a] (stdlib/sqrt a))
(defn ceil [a] (stdlib/ceil a))
(defn floor [a] (stdlib/floor a))
(defn atan2 [y x] (stdlib/atan2 y x))
(defn sinh [x] (stdlib/sinh x))
(defn cosh [x] (stdlib/cosh x))
(defn tanh [x] (stdlib/tanh x))
(defn hypot [x y] (stdlib/hypot x y))
(defn expm1 [x] (stdlib/expm1 x))
(defn log1p [x] (stdlib/log1p x))
(defn cbrt [a]
  #?(:melange (runtime-math-melange/cbrt a)
     :default (runtime-math/cbrt a)))

(defn pow [a b]
  #?(:melange (runtime-math-melange/pow a b)
     :default (runtime-math/pow a b)))

(defn- ieee-fmod [x y]
  #?(:melange (runtime-math-melange/fmod x y)
     :default (runtime-math/fmod x y)))

(defn random []
  (runtime-random/rand 1.0))

(defn- fabs [x]
  #?(:melange (runtime-math-melange/abs x)
     :default (runtime-math/abs x)))

(defn copy-sign [magnitude sign]
  #?(:melange (runtime-math-melange/copy-sign magnitude sign)
     :default (runtime-math/copy-sign magnitude sign)))

(defn rint [a]
  (let [two-to-the-52 4503599627370496.0
        sign (copy-sign 1.0 a)
        magnitude (fabs a)
        rounded (if (< magnitude two-to-the-52)
                  (- (+ two-to-the-52 magnitude) two-to-the-52)
                  magnitude)]
    (* sign rounded)))

(defn signum [d]
  (if (or (zero? d) (not (= d d)))
    d
    (copy-sign 1.0 d)))

(defn round [a]
  (cond
    (not (= a a)) 0.0
    (= a ##Inf) 9007199254740991.0
    (= a ##-Inf) -9007199254740991.0
    :else (floor (+ a 0.5))))

(defn get-exponent [d]
  #?(:melange (runtime-math-melange/get-exponent d)
     :default (runtime-math/get-exponent d)))

(defn next-after [start direction]
  #?(:melange (runtime-math-melange/next-after start direction)
     :default (runtime-math/next-after start direction)))

(defn next-up [d]
  (next-after d ##Inf))

(defn next-down [d]
  (next-after d ##-Inf))

(defn ulp [d]
  #?(:melange (runtime-math-melange/ulp d)
     :default (runtime-math/ulp d)))

(defn scalb [d scale-factor]
  #?(:melange (runtime-math-melange/scalb d scale-factor)
     :default (runtime-math/scalb d scale-factor)))

(defn IEEE-remainder [dividend divisor]
  (cond
    (zero? divisor) ##NaN
    (not (= divisor divisor)) ##NaN
    (not (= dividend dividend)) ##NaN
    (or (= dividend ##Inf) (= dividend ##-Inf)) ##NaN
    (or (= divisor ##Inf) (= divisor ##-Inf)) dividend
    :else
    (let [original-dividend dividend
          divisor-magnitude (fabs divisor)
          reduced-dividend (if (<= divisor-magnitude 8.988465674311579e307)
                             (ieee-fmod dividend (* divisor-magnitude 2.0))
                             dividend)
          dividend-magnitude (fabs reduced-dividend)]
      (if (= dividend-magnitude divisor-magnitude)
        (* 0.0 original-dividend)
        (let [remainder-magnitude
              (if (< divisor-magnitude 4.450147717014403e-308)
                (if (> (+ dividend-magnitude dividend-magnitude)
                       divisor-magnitude)
                  (let [reduced (- dividend-magnitude divisor-magnitude)]
                    (if (>= (+ reduced reduced) divisor-magnitude)
                      (- reduced divisor-magnitude)
                      reduced))
                  dividend-magnitude)
                (let [divisor-half (* 0.5 divisor-magnitude)]
                  (if (> dividend-magnitude divisor-half)
                    (let [reduced (- dividend-magnitude divisor-magnitude)]
                      (if (>= reduced divisor-half)
                        (- reduced divisor-magnitude)
                        reduced))
                    dividend-magnitude)))]
          (if (< original-dividend 0.0)
            (- remainder-magnitude)
            remainder-magnitude))))))

(defn- outside-safe-integer? [value]
  (or (> value 9007199254740991.0)
      (< value -9007199254740991.0)))

(defn- throw-integer-overflow [function-name]
  (throw (runtime-exception/integer-overflow function-name)))

(defn add-exact [x y]
  (let [result (+ x y)]
    (if (outside-safe-integer? result)
      (throw-integer-overflow "add-exact")
      result)))

(defn subtract-exact [x y]
  (let [result (- x y)]
    (if (outside-safe-integer? result)
      (throw-integer-overflow "subtract-exact")
      result)))

(defn multiply-exact [x y]
  (let [result (* x y)]
    (if (outside-safe-integer? result)
      (throw-integer-overflow "multiply-exact")
      result)))

(defn increment-exact [a]
  (if (or (>= a 9007199254740991.0)
          (< a -9007199254740991.0))
    (throw-integer-overflow "increment-exact")
    (+ a 1.0)))

(defn decrement-exact [a]
  (if (or (<= a -9007199254740991.0)
          (> a 9007199254740991.0))
    (throw-integer-overflow "decrement-exact")
    (- a 1.0)))

(defn negate-exact [a]
  (if (outside-safe-integer? a)
    (throw-integer-overflow "negate-exact")
    (- a)))

(defn- trunc [value]
  #?(:melange (runtime-math-melange/trunc value)
     :default (runtime-math/trunc value)))

(defn- safe-integer? [value]
  (if (= value (trunc value))
    (if (outside-safe-integer? value) false true)
    false))

(defn- xor [^:bool a ^:bool b]
  (if a
    (if b false true)
    b))

(defn floor-div [x y]
  (let [x-safe (safe-integer? x)
        y-safe (safe-integer? y)]
    (if-not (and x-safe y-safe)
      (throw (runtime-exception/unsafe-integer-arguments
               "floor-div" x-safe y-safe))
      (let [result (trunc (/ x y))]
        (if (xor (< x 0.0) (< y 0.0))
          (if (= (* result y) x)
            result
            (- result 1.0))
          result)))))

(defn floor-mod [x y]
  (let [x-safe (safe-integer? x)
        y-safe (safe-integer? y)]
    (if-not (and x-safe y-safe)
      (throw (runtime-exception/unsafe-integer-arguments
               "floor-mod" x-safe y-safe))
      (let [result (trunc (/ x y))]
        (if (xor (< x 0.0) (< y 0.0))
          (if (= (* result y) x)
            (- x (* y result))
            (- x (* y result) (- y)))
          (- x (* y result)))))))
