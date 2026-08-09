; Copyright (c) Rich Hickey. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; This LG port follows ClojureScript's cljs.math source API.

(ns cljs.math
  (:require [ocaml.Stdlib :as stdlib]))

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
