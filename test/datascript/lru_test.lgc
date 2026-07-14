(ns datascript.test.lru
  (:require [datascript.lru :as lru]))

(def environment #?(:native "native" :melange "melange"))

(def l0 (lru/lru 2))
(def l1 (assoc l0 :a 1))
(def l2 (assoc l1 :b 2))
(def l3 (assoc l2 :c 3))
(def l4 (assoc l3 :b 4))
(def l5 (assoc l4 :d 5))

(println
  (str environment ":"
       (nil? (get l0 :a)) ":"
       (= 1 (get l1 :a)) ":"
       (= 1 (get l2 :a)) ":"
       (= 2 (get l2 :b)) ":"
       (nil? (get l3 :a)) ":"
       (= 2 (get l3 :b)) ":"
       (= 3 (get l3 :c)) ":"
       (= 2 (get l4 :b)) ":"
       (= 3 (get l4 :c)) ":"
       (= 2 (get l5 :b)) ":"
       (nil? (get l5 :c)) ":"
       (= 5 (get l5 :d))))

(def cache-impl (lru/cache 2))
(def a-time (volatile! 0))
(def b-time (volatile! 0))
(def c-time (volatile! 0))
(def a-fn #(do (vswap! a-time inc) 1))
(def b-fn #(do (vswap! b-time inc) 2))
(def c-fn #(do (vswap! c-time inc) 3))

(println
  (str environment ":"
       (= 1 (lru/-get cache-impl :a a-fn)) ":"
       (= 2 (lru/-get cache-impl :b b-fn)) ":"
       (= 1 (lru/-get cache-impl :a a-fn)) ":"
       (= 3 (lru/-get cache-impl :c c-fn)) ":"
       (= 2 (lru/-get cache-impl :b b-fn)) ":"
       (= 1 (deref a-time)) ":"
       (= 2 (deref b-time)) ":"
       (= 1 (deref c-time))))
