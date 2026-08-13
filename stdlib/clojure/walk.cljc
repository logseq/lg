; Copyright (c) Rich Hickey. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; This LG port follows ClojureScript's clojure.walk traversal order.

(ns clojure.walk
  (:require [ocaml.Lg_runtime.Runtime_walk :as runtime]))

(defn walk
  [^:fn<Lg_edn_backend.t;Lg_edn_backend.t> inner
   ^:fn<Lg_edn_backend.t;Lg_edn_backend.t> outer
   ^:Lg_edn_backend.t form]
  (outer (runtime/walk inner form)))

(defn postwalk
  [^:fn<Lg_edn_backend.t;Lg_edn_backend.t> f ^:Lg_edn_backend.t form]
  (walk (fn [^:Lg_edn_backend.t value] (postwalk f value)) f form))

(defn prewalk
  [^:fn<Lg_edn_backend.t;Lg_edn_backend.t> f ^:Lg_edn_backend.t form]
  (walk
   (fn [^:Lg_edn_backend.t value] (prewalk f value))
   (fn [^:Lg_edn_backend.t value] value)
   (f form)))

(defn keywordize-keys [m]
  (postwalk (fn [value] (runtime/keywordize-map-keys value)) m))

(defn stringify-keys [m]
  (postwalk (fn [value] (runtime/stringify-map-keys value)) m))

(defn prewalk-replace [smap form]
  (prewalk (fn [value] (runtime/replace smap value)) form))

(defn postwalk-replace [smap form]
  (postwalk (fn [value] (runtime/replace smap value)) form))
