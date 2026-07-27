(ns datascript.inline
  (:refer-clojure :exclude [assoc update]))

(defmacro assoc [m k v & kvs]
  (loop [result `(clojure.core/assoc ~m ~k ~v)
         remaining kvs]
    (if (empty? remaining)
      result
      (do
        (assert
         (next remaining)
         "assoc expects an even number of arguments after map/vector")
        (recur
         `(clojure.core/assoc
           ~result
           ~(first remaining)
           ~(second remaining))
         (nnext remaining))))))

(defmacro update [m k f & more]
  `(let [m# ~m
         k# ~k]
     (assoc m# k# (~f (get m# k#) ~@more))))
