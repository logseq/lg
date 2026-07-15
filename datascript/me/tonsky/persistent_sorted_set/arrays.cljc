(ns me.tonsky.persistent-sorted-set.arrays
  (:refer-clojure :exclude [make-array into-array array aget aset alength array? aclone]))

(defmacro empty-array []
  `(Array.of_list (list)))

(defmacro make-array [size initial]
  `(Array.make ~size ~initial))

(defmacro into-array [values]
  `(array-from ~values))

(defmacro aget [values index]
  `(unsafe-aget ~values ~index))

(defmacro aset [values index value]
  `(unsafe-aset ~values ~index ~value))

(defmacro alength [values]
  `(Array.length ~values))

(defmacro array [& values]
  `(array-values ~@values))

(defmacro acopy [source source-start source-end target target-start]
  #?(:melange
     `(let [length# (- ~source-end ~source-start)]
        (loop [idx# 0]
          (if (< idx# length#)
            (do
              (unsafe-aset
                ~target
                (+ idx# ~target-start)
                (unsafe-aget ~source (+ idx# ~source-start)))
              (recur (inc idx#)))
            nil)))
     :native
     `(Array.blit
        ~source
        ~source-start
        ~target
        ~target-start
        (- ~source-end ~source-start))))

(defmacro aclone [values]
  `(Array.copy ~values))

(defmacro aslice [values from to]
  `(Array.sub ~values ~from (- ~to ~from)))

(defmacro aconcat [left right]
  `(Array.append ~left ~right))

(defmacro amap [f values]
  `(Array.map ~f ~values))

(defmacro asort [values cmp]
  `(let [values# ~values]
     (do
       (asort! ~cmp values#)
       values#)))

(defmacro array? [value]
  `(array-value? ~value))

(defmacro alast [values]
  `(let [values# ~values]
     (unsafe-aget values# (dec (Array.length values#)))))

(defmacro half [value]
  `(quot ~value 2))
