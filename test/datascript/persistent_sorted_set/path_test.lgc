(ns me.tonsky.persistent-sorted-set.test.path
  (:require [me.tonsky.persistent-sorted-set.path :as path]))

(def encoded
  (path/path-set
    (path/path-set
      (path/path-set path/empty-path 0 7)
      1
      30)
    2
    11))
(def encoded-parts (path/path-str encoded))

(println
  (str "path:"
       (= 7 (path/path-get encoded 0)) ":"
       (= 30 (path/path-get encoded 1)) ":"
       (= 11 (path/path-get encoded 2)) ":"
       (= 3 (count encoded-parts)) ":"
       (= 11 (+ (nth encoded-parts 0) 0)) ":"
       (path/path-same-leaf encoded (path/path-inc encoded)) ":"
       (not
         (path/path-same-leaf
           encoded
           #?(:native (+ encoded path/max-len)
              :melange (Float.add encoded 32.0))))))

(def large
  (path/path-set
    (path/path-set path/empty-path 7 17)
    8
    9))

(println
  (str "large:"
       (= 17 (path/path-get large 7)) ":"
       (= 9 (path/path-get large 8)) ":"
       (= large (path/path-dec (path/path-inc large))) ":"
       (path/path-lt large (path/path-inc large)) ":"
       (path/path-lte large large) ":"
       (path/path-eq large large) ":"
       (zero? (path/path-cmp large large))))
