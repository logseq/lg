; Copyright (c) Rich Hickey. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).

(ns cljs.reader
  (:require [ocaml.Lg_runtime.Runtime_edn :as runtime]))

(defn read-string [source]
  (runtime/read-string source))

(defn register-tag-parser! [tag f]
  (runtime/register-tag-parser (str tag) f))

(defn deregister-tag-parser! [tag]
  (runtime/deregister-tag-parser (str tag)))

(defn register-default-tag-parser! [f]
  (runtime/register-default-tag-parser
   (fn [^:string tag value]
     (f (symbol tag) value))))

(defn deregister-default-tag-parser! []
  (runtime/deregister-default-tag-parser))
