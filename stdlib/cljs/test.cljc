; Copyright (c) Rich Hickey. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; This LG port follows ClojureScript's cljs.test source API.

(ns cljs.test)

(defn- default-fixture [fixture]
  (fixture))

(defn compose-fixtures
  "Composes fixture functions `f1` and `f2` into one fixture function.

  Function fixtures are incompatible with map fixtures."
  [f1 f2]
  (fn [fixture]
    (f1 (fn [] (f2 fixture)))))

(defn join-fixtures
  "Composes `fixtures` in order.

  Returns a valid identity fixture when `fixtures` is empty. Function fixtures
  are incompatible with map fixtures."
  [fixtures]
  (reduce compose-fixtures default-fixture fixtures))

(defn successful?
  "Returns `true` when `summary` reports no failures or errors."
  [summary]
  (and (zero? (:fail summary 0))
       (zero? (:error summary 0))))
