; Copyright (c) Rich Hickey. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; This LG port preserves ClojureScript's clojure.data diff partitions.

(ns clojure.data
  (:require [ocaml.Lg_runtime.Runtime_data :as runtime]))

(defprotocol EqualityPartition
  (equality-partition [value] :keyword))

(defprotocol Diff
  (diff-similar [left right] :Lg_edn_backend.t))

(extend-type :Lg_edn_backend.t
  EqualityPartition
  (equality-partition [value]
    (let [partition (runtime/equality-partition-tag value)]
      (cond
        (= partition 0) :atom
        (= partition 1) :map
        (= partition 2) :set
        :else :sequential)))
  Diff
  (diff-similar [left right]
    (runtime/diff-similar left right)))

(defn diff [left right]
  (runtime/diff left right))
