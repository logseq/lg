; Copyright (c) Rich Hickey. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; This LG port follows ClojureScript's clojure.string public API.

(ns clojure.string
  (:require [ocaml.Lg_runtime.Runtime_string :as runtime]))

(defn blank? [source]
  (runtime/blank source))

(defn capitalize [source]
  (runtime/capitalize source))

(defn ends-with? [source suffix]
  (runtime/ends-with source suffix))

(defn escape [source replacements]
  (runtime/escape source replacements))

(defn includes? [source substring]
  (runtime/includes source substring))

(defn index-of
  ([source substring]
   (runtime/index-of-int source substring))
  ([source substring from-index]
   (runtime/index-of-from source substring from-index)))

(defn join
  ([coll]
   (runtime/join-seq "" (seq coll)))
  ([separator coll]
   (runtime/join-seq separator (seq coll))))

(defn last-index-of
  ([source substring]
   (runtime/last-index-of-int source substring))
  ([source substring from-index]
   (runtime/last-index-of-from source substring from-index)))

(defn lower-case [source]
  (runtime/lower-case source))

(defn re-quote-replacement [replacement]
  (runtime/identity replacement))

(defn replace [source match replacement]
  (runtime/replace source match replacement))

(defn replace-first [source match replacement]
  (runtime/replace-first source match replacement))

(defn reverse [source]
  (runtime/reverse source))

(defn split-lines [source]
  (runtime/split-lines source))

(defn starts-with? [source prefix]
  (runtime/starts-with source prefix))

(defn trim [source]
  (runtime/trim source))

(defn trim-newline [source]
  (runtime/trim-newline source))

(defn triml [source]
  (runtime/triml source))

(defn trimr [source]
  (runtime/trimr source))

(defn upper-case [source]
  (runtime/upper-case source))
