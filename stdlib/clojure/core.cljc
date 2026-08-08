; Copyright (c) Rich Hickey. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; This LG port follows ClojureScript's cljs.core source algorithms.

(ns clojure.core
  (:require [ocaml.Stdlib :as stdlib]
            [ocaml.Rrbvec :as rrb-vector]
            [ocaml.Lg_runtime.Runtime_array :as runtime-array]
            [ocaml.Lg_runtime.Runtime_array_melange :as runtime-array-melange]
            [ocaml.Lg_runtime.Runtime_int :as runtime-int]
            [ocaml.Lg_runtime.Runtime_random :as runtime-random]
            [ocaml.Lg_runtime.Runtime_reduced :as runtime-reduced]
            [ocaml.Lg_runtime.Runtime_seq :as runtime-seq]
            [ocaml.Lg_runtime.Runtime_static_value :as runtime-static-value]
            [ocaml.Lg_runtime.Runtime_string :as runtime-string]))

(defn identity [x]
  x)

(defn complement [f]
  (fn [x]
    (not (f x))))

(defn constantly [x]
  (fn
    ([] x)
    ([_arg] x)
    ([_left _right] x)
    ([_left _right & _args] x)))

(defn boolean [x]
  (if x true false))

(defn not [x]
  (if x false true))

(defn reduced [x]
  (runtime-reduced/reduced x))

(defn subs
  ([source start]
   (runtime-string/substring-from source start))
  ([source start end]
   (runtime-string/substring-range source start end)))

(defn int-to-string-radix [value radix]
  (runtime-string/int-to-string-radix value radix))

(defn parse-boolean [source]
  (case source
    "true" true
    "false" false
    nil))

(defn any? [x]
  (runtime-static-value/consume x)
  true)

(defn range
  ([] (runtime-seq/range 0 1))
  ([end] (runtime-seq/range-until 0 end 1))
  ([start end] (runtime-seq/range-until start end 1))
  ([start end step] (runtime-seq/range-until start end step)))

(defn shuffle [coll]
  (runtime-random/shuffle-seq (seq coll)))

(defn alength [values]
  (runtime-array/length values))

(defn acopy [source source-start source-end target target-start]
  (runtime-array/copy-range source source-start source-end target target-start))

(defn aslice [values from to]
  (runtime-array/slice values from to))

(defn aconcat [left right]
  (runtime-array/append left right))

(defn array-to-seq [values]
  (runtime-seq/of-array values))

(defn array-to-rseq [values]
  (runtime-seq/of-array-rev values))

(defmacro amap [values index result expression]
  `(let [values# ~values
         length# (alength values#)
         ~result (aclone values#)]
     (loop [~index 0]
       (if (< ~index length#)
         (do
           (aset ~result ~index ~expression)
           (recur (inc ~index)))
         ~result))))

(defn asort! [compare values]
  #?(:melange
     (runtime-array-melange/sort values compare)
     :default
     (runtime-array/sort values compare)))

(defn quot [n d]
  (runtime-int/int-quot n d))

(defn rem [n d]
  (runtime-int/int-rem n d))

(defn mod [n d]
  (runtime-int/clojure-mod n d))

(defn unchecked-add [x y]
  (+ x y))

(defn unchecked-add-int [x y]
  (+ x y))

(defn unchecked-subtract [x y]
  (- x y))

(defn unchecked-subtract-int [x y]
  (- x y))

(defn unchecked-multiply [x y]
  (* x y))

(defn unchecked-multiply-int [x y]
  (* x y))

(defn unchecked-divide-int [x y]
  (runtime-int/int-quot x y))

(defn unchecked-remainder-int [x y]
  (runtime-int/int-rem x y))

(defn unchecked-inc [x]
  (+ x 1))

(defn unchecked-inc-int [x]
  (+ x 1))

(defn unchecked-dec [x]
  (- x 1))

(defn unchecked-dec-int [x]
  (- x 1))

(defn unchecked-negate [x]
  (- 0 x))

(defn unchecked-negate-int [x]
  (- 0 x))

(defn rand-int [n]
  (runtime-random/rand-int n))

(defn rand-nth [coll]
  (nth coll (rand-int (count coll))))

(defn bit-shift-right-zero-fill [x n]
  (runtime-int/logical-shift-right x n))

(defn- bit-and-not-two [x y]
  (bit-and x (bit-not y)))

(defn bit-and-not
  "Returns the bitwise intersection of `x` with the complements of the remaining arguments."
  ([x y]
   (bit-and-not-two x y))
  ([x y & more]
   (reduce bit-and-not-two (bit-and-not-two x y) more)))

(defn unsigned-bit-shift-right
  "Returns `x` shifted right by `n` bits without sign extension."
  [x n]
  (bit-shift-right-zero-fill x n))

(defn bit-count
  "Returns the number of set bits in `value`."
  [value]
  (let [value (- value
                 (bit-and (bit-shift-right value 1) 0x55555555))
        value (+ (bit-and value 0x33333333)
                 (bit-and (bit-shift-right value 2) 0x33333333))]
    (bit-shift-right
     (* (bit-and (+ value (bit-shift-right value 4)) 0xF0F0F0F)
        0x1010101)
     24)))

(defn second [coll]
  (first (next coll)))

(defn last [coll]
  (loop [remaining (seq coll)]
    (let [tail (next remaining)]
      (if tail
        (recur tail)
        (first remaining)))))

(defn even? [n]
  (zero? (bit-and n 1)))

(defn odd? [n]
  (not (even? n)))

(defn every? [pred coll]
  (loop [remaining (seq coll)]
    (if remaining
      (if (pred (nth remaining 0))
        (recur (next remaining))
        false)
      true)))

(defn ffirst [coll]
  (first (first coll)))

(defn fnext [coll]
  (first (next coll)))

(defn nfirst [coll]
  (next (first coll)))

(defn nnext [coll]
  (next (next coll)))

(defn not-every? [pred coll]
  (not (every? pred coll)))

(defn not-any? [pred coll]
  (loop [remaining (seq coll)]
    (if remaining
      (if (pred (nth remaining 0))
        false
        (recur (next remaining)))
      true)))

(defn split-at [n coll]
  [(take n coll) (drop n coll)])

(defn split-with [pred coll]
  [(take-while pred coll) (drop-while pred coll)])

(defn nthnext [coll n]
  (drop n coll))

(defn nthrest [coll n]
  (drop n coll))

(defn bounded-count [n coll]
  (count (take n coll)))

(defn butlast [coll]
  (take (dec (count coll)) coll))

(defn take-last [n coll]
  (drop (- (count coll) n) coll))

(defn drop-last
  ([coll]
   (drop-last 1 coll))
  ([n coll]
   (take (- (count coll) n) coll)))

(defn reverse [coll]
  (reduce (fn [result item] (conj result item)) (list) coll))

(defn interpose [separator coll]
  (drop 1 (interleave (repeat separator) coll)))

(defn replicate [n x]
  (take n (repeat x)))

(defn dedupe [coll]
  (map (fn [values] (nth values 0))
       (partition-by (fn [value] value) coll)))

(defn distinct [coll]
  (let [remaining (seq coll)]
    (if remaining
      (let [item (nth remaining 0)]
        (loop [seen (hash-set item)
               result (list item)
               remaining (next remaining)]
          (if remaining
            (let [item (nth remaining 0)]
              (if (contains? seen item)
                (recur seen result (next remaining))
                (recur (conj seen item)
                       (conj result item)
                       (next remaining))))
            (reverse result))))
      (list))))

(defn zipmap [keys values]
  (loop [result {}
         remaining-keys (seq keys)
         remaining-values (seq values)]
    (if remaining-keys
      (if remaining-values
        (recur
          (assoc result (nth remaining-keys 0) (nth remaining-values 0))
          (next remaining-keys)
          (next remaining-values))
        result)
      result)))

(defn key [map-entry]
  (stdlib/fst map-entry))

(defn val [map-entry]
  (stdlib/snd map-entry))

(defn vec [coll]
  (rrb-vector/of-list (runtime-seq/to-list (seq coll))))

(defn comparator
  "Returns a comparator that orders `x` and `y` using `pred`."
  [pred]
  (fn [x y]
    (if (pred x y)
      -1
      (if (pred y x) 1 0))))

(defn max-key
  ([k x]
   (let [_ k] x))
  ([k x y] (if (> (k x) (k y)) x y))
  ([k x y & more]
   (reduce (fn [best item] (max-key k best item))
           (max-key k x y)
           more)))

(defn min-key
  ([k x]
   (let [_ k] x))
  ([k x y] (if (< (k x) (k y)) x y))
  ([k x y & more]
   (reduce (fn [best item] (min-key k best item))
           (min-key k x y)
           more)))

(defn frequencies
  "Returns a map from each distinct item in `coll` to its occurrence count."
  [coll]
  (reduce
   (fn [counts value]
     (assoc counts value (inc (get counts value 0))))
   {}
   coll))

(defn update-vals
  "Returns `m` with `f` applied to every value."
  [m f]
  (with-meta
    (reduce-kv
     (fn [result key value]
       (assoc result key (f value)))
     {}
     m)
    (meta m)))

(defn update-keys
  "Returns `m` with `f` applied to every key."
  [m f]
  (with-meta
    (reduce-kv
     (fn [result key value]
       (assoc result (f key) value))
     {}
     m)
    (meta m)))

(defn bit-clear [x n]
  (bit-and x (bit-not (bit-shift-left 1 n))))

(defn bit-flip [x n]
  (bit-xor x (bit-shift-left 1 n)))

(defn bit-set [x n]
  (bit-or x (bit-shift-left 1 n)))

(defn bit-test [x n]
  (not (zero? (bit-and x (bit-shift-left 1 n)))))

(defn hash-combine [seed hash-value]
  (runtime-int/hash-combine seed hash-value))
