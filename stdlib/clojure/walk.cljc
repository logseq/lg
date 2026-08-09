; Copyright (c) Rich Hickey. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; This LG port follows ClojureScript's clojure.walk traversal order.

(ns clojure.walk
  (:require [ocaml.Lg_runtime.Runtime_walk :as runtime]))

(defn walk [inner outer form]
  (outer (runtime/walk inner form)))

(defn postwalk [f form]
  (walk (fn [value] (postwalk f value)) f form))

(defn prewalk [f form]
  (walk (fn [value] (prewalk f value)) identity (f form)))

(defn keywordize-keys [m]
  (postwalk (fn [value] (runtime/keywordize-map-keys value)) m))

(defn stringify-keys [m]
  (postwalk (fn [value] (runtime/stringify-map-keys value)) m))

(defn prewalk-replace [smap form]
  (prewalk (fn [value] (runtime/replace smap value)) form))

(defn postwalk-replace [smap form]
  (postwalk (fn [value] (runtime/replace smap value)) form))
