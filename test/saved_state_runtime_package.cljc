(ns saved-state-runtime-package
  (:require [ocaml.Lg_runtime.Runtime_int :as runtime-int]))

(defn combine-hashes [^:int left ^:int right]
  (runtime-int/hash-combine left right))

(defn make-holder [value]
  (record holder
          (value value)
          (ref-type (Lg_runtime.Runtime_ref_type.Weak))))

(defn repeat-holder [value]
  (Array.make 2 (make-holder value)))
