; Copyright (c) Rich Hickey. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0.

(ns cljs.pprint
  (:refer-clojure :exclude [float?])
  (:require [ocaml.Lg_runtime.Runtime_string :as runtime-string]))

(defprotocol IPPrintFloatPredicate
  (-pprint-float? [value] :bool))

(defprotocol ICharCode
  (-char-code [value] :int))

(extend-type :int
  IPPrintFloatPredicate
  (-pprint-float? [_value] false)
  ICharCode
  (-char-code [value] value))

(extend-type :float
  IPPrintFloatPredicate
  (-pprint-float? [value]
    (and (not (js/isNaN value))
         (not (= value ##Inf))
         (not (= value ##-Inf))
         (not (= value (double (int value)))))))

(extend-type :default
  IPPrintFloatPredicate
  (-pprint-float? [_value] false))

(extend-type :char
  ICharCode
  (-char-code [value]
    (runtime-string/char-code-of-char value)))

(extend-type :string
  ICharCode
  (-char-code [value]
    (runtime-string/char-code-of-string value)))

(defn float?
  {:inline (fn [value]
             (list 'cljs.pprint/-pprint-float? value))}
  [value]
  (IPPrintFloatPredicate/-pprint-float? value))

(defn char-code
  {:inline (fn [value]
             (list 'cljs.pprint/-char-code value))}
  [value]
  (ICharCode/-char-code value))
