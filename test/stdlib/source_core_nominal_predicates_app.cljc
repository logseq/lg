(ns source-core-nominal-predicates-app
  (:require [cljs.core :as core :refer [delay? rand to-array-2d uuid?]]))

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

(def source-random-function rand)
(def source-random-argument-evaluations (atom 0))
(def source-random-default (rand))
(def source-random-float (rand 2.5))
(def source-random-negative (rand -3))
(println (and (<= 0.0 source-random-default)
              (< source-random-default 1.0)))
(println (= 0.0 (core/rand 0)))
(println (= 0.0 (clojure.core/rand 0.0)))
(println (let [value (source-random-function)]
           (and (<= 0.0 value) (< value 1.0))))
(println (let [value (source-random-function 2.0)]
           (and (<= 0.0 value) (< value 2.0))))
(println (and (<= 0.0 source-random-float)
              (< source-random-float 2.5)))
(println (and (<= -3.0 source-random-negative)
              (<= source-random-negative 0.0)))
(println
 (= 0.0
    (rand (do (swap! source-random-argument-evaluations inc) 0))))
(println (= 1 @source-random-argument-evaluations))

(def source-nested-arrays (to-array-2d [[1 2] [3]]))
(println
 (and (= 2 (alength source-nested-arrays))
      (= 2 (alength (aget source-nested-arrays 0)))
      (= 1 (aget (aget source-nested-arrays 0) 0))
      (= 3 (aget (aget source-nested-arrays 1) 0))))
(def source-ragged-arrays (core/to-array-2d [[] [4 5]]))
(println
 (and (= 0 (alength (aget source-ragged-arrays 0)))
      (= 5 (aget (aget source-ragged-arrays 1) 1))))
(def source-listed-arrays
  (clojure.core/to-array-2d (list (list 6 7) (list 8))))
(println
 (and (= 7 (aget (aget source-listed-arrays 0) 1))
      (= 8 (aget (aget source-listed-arrays 1) 0))))
(def source-character-arrays (to-array-2d ["ab" "c"]))
(println (= \b (aget (aget source-character-arrays 0) 1)))
(def source-two-dimensional-array-function to-array-2d)
(def source-function-arrays
  (source-two-dimensional-array-function [[9] [10 11]]))
(println (= 11 (aget (aget source-function-arrays 1) 1)))
(def source-array-argument-evaluations (atom 0))
(def source-evaluated-arrays
  (to-array-2d
   (do
     (swap! source-array-argument-evaluations inc)
     [[12]])))
(println (= 12 (aget (aget source-evaluated-arrays 0) 0)))
(println (= 1 @source-array-argument-evaluations))
