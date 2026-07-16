(ns me.tonsky.persistent-sorted-set.test.path
  (:require [me.tonsky.persistent-sorted-set :as pss]))

(def encoded
  (pss/path-set
   (pss/path-set
    (pss/path-set pss/empty-path 0 7)
    1
    30)
   2
   11))
(def encoded-parts (pss/path-str encoded))

(println
 (str "path:"
      (= 7 (pss/path-get encoded 0)) ":"
      (= 30 (pss/path-get encoded 1)) ":"
      (= 11 (pss/path-get encoded 2)) ":"
      (= 3 (count encoded-parts)) ":"
      (= 11 (+ (nth encoded-parts 0) 0)) ":"
      (pss/path-same-leaf encoded (pss/path-inc encoded)) ":"
      (not
       (pss/path-same-leaf
        encoded
        #?(:native (+ encoded pss/max-len)
           :melange (Float.add encoded 32.0))))))

(def large
  (pss/path-set
   (pss/path-set pss/empty-path 7 17)
   8
   9))

(println
 (str "large:"
      (= 17 (pss/path-get large 7)) ":"
      (= 9 (pss/path-get large 8)) ":"
      (= large (pss/path-dec (pss/path-inc large))) ":"
      (pss/path-lt large (pss/path-inc large)) ":"
      (pss/path-lte large large) ":"
      (pss/path-eq large large) ":"
      (zero? (pss/path-cmp large large))))
