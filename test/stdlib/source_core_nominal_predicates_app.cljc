(ns source-core-nominal-predicates-app
  (:require [cljs.core :as core :refer [delay? uuid?]]))

(def source-uuid-value
  (uuid "550e8400-e29b-41d4-a716-446655440000"))
(println (uuid? source-uuid-value))
(println (not (uuid? "550e8400-e29b-41d4-a716-446655440000")))
(println (core/uuid? source-uuid-value))
(println (not (clojure.core/uuid? "not-a-uuid")))
(def source-uuid-predicate uuid?)
(println (source-uuid-predicate source-uuid-value))
(def source-uuid-predicate-evaluations (atom 0))
(println
 (and (not (uuid? (do (swap! source-uuid-predicate-evaluations inc)
                       "ordinary-string")))
      (= 1 @source-uuid-predicate-evaluations)))

(def source-delayed-value (delay (+ 20 22)))
(println (delay? source-delayed-value))
(println (not (delay? 42)))
(println (core/delay? source-delayed-value))
(println (not (clojure.core/delay? "not-a-delay")))
(def source-delay-predicate delay?)
(println (source-delay-predicate source-delayed-value))
(def source-delay-predicate-evaluations (atom 0))
(println
 (and (not (delay? (do (swap! source-delay-predicate-evaluations inc) 7)))
      (= 1 @source-delay-predicate-evaluations)))
