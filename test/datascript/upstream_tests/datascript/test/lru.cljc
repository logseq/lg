(ns datascript.test.lru
  (:require
   [clojure.test :as t :refer [are deftest is testing]]
   [datascript.lru :as lru]))

(deftest test-lru
  (let [l0 (lru/lru 2)
        l1 (assoc l0 :a 1)
        l2 (assoc l1 :b 2)
        l3 (assoc l2 :c 3)
        l4 (assoc l3 :b 4)
        l5 (assoc l4 :d 5)]
    (are [l k v] (= (get l k) v)
      l0 :a nil
      l1 :a 1
      l2 :a 1
      l2 :b 2
      l3 :a nil
      l3 :b 2
      l3 :c 3
      l4 :b 2
      l4 :c 3
      l5 :b 2
      l5 :c nil
      l5 :d 5)))

(deftest test-cache
  (let [cache (lru/cache 2)
        a-time (volatile! 0)
        b-time (volatile! 0)
        c-time (volatile! 0)
        a-fn (fn [] (vswap! a-time inc) 1)
        b-fn (fn [] (vswap! b-time inc) 2)
        c-fn (fn [] (vswap! c-time inc) 3)]
    (is (= 1 (lru/-get cache :a a-fn)))
    (is (= 2 (lru/-get cache :b b-fn)))
    (is (= 1 (lru/-get cache :a a-fn)))
    (is (= 3 (lru/-get cache :c c-fn)))
    (is (= 2 (lru/-get cache :b b-fn)))
    (is (= 1 @a-time))
    (is (= 2 @b-time))
    (is (= 1 @c-time))))
