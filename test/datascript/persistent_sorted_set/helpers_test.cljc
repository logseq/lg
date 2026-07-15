(ns me.tonsky.persistent-sorted-set.test.helpers
  (:require
    [me.tonsky.persistent-sorted-set.arrays :as arrays]
    [me.tonsky.persistent-sorted-set.array-ops :as ops]
    [me.tonsky.persistent-sorted-set.search :as search]))

(defn int-compare [left right]
  (compare left right))

(def search-values (arrays/array 1 3 3 5))
(println
  (str "search:"
       (= 1 (search/binary-search-l int-compare search-values 3 3)) ":"
       (= 3 (search/binary-search-r int-compare search-values 3 3)) ":"
       (= 1 (search/lookup-exact int-compare search-values 3)) ":"
       (= -1 (search/lookup-exact int-compare search-values 4)) ":"
       (= 3 (search/lookup-range int-compare search-values 4)) ":"
       (= -1 (search/lookup-range int-compare search-values 6))))

(def values (arrays/array 1 2 3 4))
(def spliced (ops/splice values 1 3 (arrays/array 8 9)))
(def inserted (ops/insert values 2 (arrays/array 7)))
(def split (ops/merge-n-split (arrays/array 1 2 3 4) (arrays/array 5 6 7)))
(println
  (str "arrays:"
       (ops/eq-arr int-compare spliced 0 4 (arrays/array 1 8 9 4) 0 4) ":"
       (ops/eq-arr int-compare inserted 0 5 (arrays/array 1 2 7 3 4) 0 5) ":"
       (ops/eq-arr int-compare (arrays/aget split 0) 0 3 (arrays/array 1 2 3) 0 3) ":"
       (ops/eq-arr int-compare (arrays/aget split 1) 0 4 (arrays/array 4 5 6 7) 0 4) ":"
       (ops/eq-arr int-compare values 0 4 (ops/check-n-splice int-compare values 0 4 (arrays/array 1 2 3 4)) 0 4)))
