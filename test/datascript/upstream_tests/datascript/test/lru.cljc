(ns datascript.test.lru
  (:require
    [clojure.test :as t :refer [is are deftest testing]]
    [datascript.lru :as lru]))

(signature datascript.test.lru/new-lru
  :fn<int;datascript.lru/lru-state<keyword;int>>)

(defn new-lru [limit]
  (lru/lru limit))

(deftest test-lru
  (let [l0 (new-lru 2)
        l1 (lru/assoc-lru l0 :a 1)
        l2 (lru/assoc-lru l1 :b 2)
        l3 (lru/assoc-lru l2 :c 3)
        l4 (lru/assoc-lru l3 :b 4)
        l5 (lru/assoc-lru l4 :d 5)]
    (are [l k v] (= (lru/get-lru l k) v)
      l0 :a None
      l1 :a (Some 1)
      l2 :a (Some 1)
      l2 :b (Some 2)
      l3 :a None ;; :a get evicted on third insert
      l3 :b (Some 2)
      l3 :c (Some 3)
      l4 :b (Some 2) ;; assoc updates access time, but does not change a value
      l4 :c (Some 3)
      l5 :b (Some 2) ;; :b remains
      l5 :c None ;; :c gets evicted as the oldest one
      l5 :d (Some 5))
    (is (= 42 (lru/get-lru-default l0 :a 42)))
    (is (= 1 (lru/get-lru-default l1 :a 42)))))

(deftest test-cache
  (let [cache  (lru/cache 2)
        a-time (volatile! 0)
        b-time (volatile! 0)
        c-time (volatile! 0)
        a-fn   #(do (vswap! a-time inc) 1)
        b-fn   #(do (vswap! b-time inc) 2)
        c-fn   #(do (vswap! c-time inc) 3)]
    (is (satisfies? lru/ICache cache))
    (is (= 1 (lru/-get cache :a a-fn)))
    (is (= 2 (lru/-get cache :b b-fn)))
    (is (= 1 (lru/-get cache :a a-fn))) ;; :a is now newer
    (is (= 3 (lru/-get cache :c c-fn))) ;; :b is evicted instead
    (is (= 2 (lru/-get cache :b b-fn)))

    (is (= 1 @a-time))
    (is (= 2 @b-time))  ;; b-fn runs twice
    (is (= 1 @c-time))))
