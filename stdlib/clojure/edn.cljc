; Copyright (c) Rich Hickey. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).

(ns clojure.edn
  (:refer-clojure :exclude [read-string])
  (:require [ocaml.Lg_runtime.Runtime_edn :as runtime]))

(defn read-string [source]
  (runtime/read-string source))

(defn read
  ([source]
   (read-string source))
  ([^:map<keyword;Lg_edn_backend.t> opts ^:string source]
   (if (= source "")
     (get opts :eof (read-string source))
     (read-string source))))
