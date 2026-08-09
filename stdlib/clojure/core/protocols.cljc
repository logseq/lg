; Copyright (c) Rich Hickey. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; This LG port follows ClojureScript's clojure.core.protocols definitions.

(ns clojure.core.protocols)

(defprotocol Datafiable
  (datafy [value]))

(extend-protocol Datafiable
  :default
  (datafy [value] value))

(defprotocol Navigable
  (nav [collection key value]))

(extend-protocol Navigable
  :default
  (nav [_collection _key value] value))
