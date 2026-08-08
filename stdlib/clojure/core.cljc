; Copyright (c) Rich Hickey. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; This LG port follows ClojureScript's cljs.core source algorithms.

(ns clojure.core)

(defn identity [x]
  x)

(defn complement [f]
  (fn [x]
    (not (f x))))

(defn even? [n]
  (zero? (bit-and n 1)))

(defn odd? [n]
  (not (even? n)))

(defn not-every? [pred coll]
  (not (every? pred coll)))

(defn not-any? [pred coll]
  (not (some pred coll)))

(defn bit-clear [x n]
  (bit-and x (bit-not (bit-shift-left 1 n))))

(defn bit-flip [x n]
  (bit-xor x (bit-shift-left 1 n)))

(defn bit-set [x n]
  (bit-or x (bit-shift-left 1 n)))

(defn bit-test [x n]
  (not (zero? (bit-and x (bit-shift-left 1 n)))))
