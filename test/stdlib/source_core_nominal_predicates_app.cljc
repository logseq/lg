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

(def source-force-evaluations (atom 0))
(def source-force-delayed-value
  (delay
    (do
      (swap! source-force-evaluations inc)
      42)))
(println (= 42 (force source-force-delayed-value)))
(println (= 42 (core/force source-force-delayed-value)))
(println (= 42 (clojure.core/force source-force-delayed-value)))
(println (= 1 @source-force-evaluations))
(def source-force-function force)
(println (= 42 (source-force-function source-force-delayed-value)))
(println (= 1 @source-force-evaluations))
(println (= 7 (force 7)))
(def source-force-argument-evaluations (atom 0))
(println
 (= "ordinary"
    (force
     (do
       (swap! source-force-argument-evaluations inc)
       "ordinary"))))
(println (= 1 @source-force-argument-evaluations))

(def source-reduced-value (ensure-reduced 8))
(println (reduced? source-reduced-value))
(println (= 8 (deref source-reduced-value)))
(println
 (identical? source-reduced-value
             (ensure-reduced source-reduced-value)))
(println
 (identical? source-reduced-value
             (core/ensure-reduced source-reduced-value)))
(println
 (identical? source-reduced-value
             (clojure.core/ensure-reduced source-reduced-value)))
(def source-ensure-reduced-function ensure-reduced)
(println (= 9 (deref (source-ensure-reduced-function 9))))
(def source-ensure-argument-evaluations (atom 0))
(println
 (= 10
    (deref
     (ensure-reduced
      (do
        (swap! source-ensure-argument-evaluations inc)
        10)))))
(println (= 1 @source-ensure-argument-evaluations))
