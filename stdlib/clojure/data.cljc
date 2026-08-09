; Copyright (c) Rich Hickey. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; This LG port preserves ClojureScript's clojure.data diff partitions.

(ns clojure.data
  (:require [ocaml.Lg_runtime.Runtime_data :as runtime]))

(defn diff [left right]
  (runtime/diff left right))
