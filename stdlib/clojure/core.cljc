; Copyright (c) Rich Hickey. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; This LG port follows ClojureScript's cljs.core source algorithms.

(ns clojure.core
  (:require [ocaml.Stdlib :as stdlib]
            [ocaml.Rrbvec :as rrb-vector]
            [ocaml.Lg_runtime.Runtime_array :as runtime-array]
            [ocaml.Lg_runtime.Runtime_array_melange :as runtime-array-melange]
            [ocaml.Lg_runtime.Runtime_future :as runtime-future]
            [ocaml.Lg_runtime.Runtime_int :as runtime-int]
            [ocaml.Lg_runtime.Runtime_int_melange :as runtime-int-melange]
            [ocaml.Lg_runtime.Runtime_number_melange :as runtime-number-melange]
            [ocaml.Lg_runtime.Runtime_random :as runtime-random]
            [ocaml.Lg_runtime.Runtime_reduced :as runtime-reduced]
            [ocaml.Lg_runtime.Runtime_seq :as runtime-seq]
            [ocaml.Lg_runtime.Runtime_static_value :as runtime-static-value]
            [ocaml.Lg_runtime.Runtime_string :as runtime-string]
            [ocaml.Lg_runtime.Runtime_time :as runtime-time]
            [ocaml.Lg_runtime.Runtime_time_melange :as runtime-time-melange]
            [ocaml.Lg_runtime.Runtime_uuid :as runtime-uuid]))

(defmacro comment [& _body])

(defmacro if-not
  ([test then]
   `(if ~test nil ~then))
  ([test then else]
   `(if ~test ~else ~then)))

(defmacro when [test & body]
  (if body
    `(if ~test
       (do ~@body)
       nil)
    `(if ~test nil nil)))

(defmacro when-not [test & body]
  (if body
    `(if ~test
       nil
       (do ~@body))
    `(if ~test nil nil)))

(defmacro cond [& clauses]
  (assert (even? (count clauses))
          "cond requires an even number of forms")
  (if clauses
    (let [test (first clauses)
          expression (second clauses)]
      (if (= true test)
        expression
        (if (= :else test)
          expression
          `(if ~test
             ~expression
             (cond ~@(nnext clauses))))))
    nil))

(defmacro and [& forms]
  `(__lg_logical-and ~@forms))

(defmacro or [& forms]
  `(__lg_logical-or ~@forms))

(defmacro if-let
  ([bindings then]
   `(if-let ~bindings ~then nil))
  ([bindings then else & oldform]
   (assert (vector? bindings) "if-let requires a binding vector")
   (assert (= 2 (count bindings))
           "if-let requires exactly two binding forms")
   (assert (empty? oldform) "if-let accepts one or two result forms")
   (let [[form test] bindings]
     `(__lg_if-let [~form ~test] ~then ~else))))

(defmacro when-let [bindings & body]
  (assert (vector? bindings) "when-let requires a binding vector")
  (assert (= 2 (count bindings))
          "when-let requires exactly two binding forms")
  (let [[form test] bindings]
    (if body
      `(__lg_when-let [~form ~test] ~@body)
      `(__lg_when-let [~form ~test] nil))))

(defmacro if-some
  ([bindings then]
   `(if-some ~bindings ~then nil))
  ([bindings then else & oldform]
   (assert (vector? bindings) "if-some requires a binding vector")
   (assert (= 2 (count bindings))
           "if-some requires exactly two binding forms")
   (assert (empty? oldform) "if-some accepts one or two result forms")
   (let [[form test] bindings]
     `(__lg_if-some [~form ~test] ~then ~else))))

(defmacro when-some [bindings & body]
  (assert (vector? bindings) "when-some requires a binding vector")
  (assert (= 2 (count bindings))
          "when-some requires exactly two binding forms")
  (let [[form test] bindings]
    (if body
      `(__lg_when-some [~form ~test] ~@body)
      `(__lg_when-some [~form ~test] nil))))

(defmacro doto [x & forms]
  (let [gx (gensym)]
    `(let [~gx ~x]
       ~@(map (fn [form]
                (if (seq? form)
                  `(~(first form) ~gx ~@(next form))
                  `(~form ~gx)))
              forms)
       ~gx)))

(defmacro when-first [bindings & body]
  (assert (vector? bindings)
          "when-first requires a vector for its binding")
  (assert (= 2 (count bindings))
          "when-first requires exactly 2 forms in its binding vector")
  (let [[name source] bindings]
    `(when-let [source# (seq ~source)]
       (let [~name (first source#)]
         ~@body))))

(defmacro while [test & body]
  `(loop []
     (if ~test
       (do
         ~@body
         (recur))
       nil)))

(defmacro -> [x & forms]
  (loop [x x
         forms forms]
    (if forms
      (let [form (first forms)
            threaded (if (seq? form)
                       (with-meta `(~(first form) ~x ~@(next form)) (meta form))
                       (list form x))]
        (recur threaded (next forms)))
      x)))

(defmacro ->> [x & forms]
  (loop [x x
         forms forms]
    (if forms
      (let [form (first forms)
            threaded (if (seq? form)
                       (with-meta `(~(first form) ~@(next form) ~x) (meta form))
                       (list form x))]
        (recur threaded (next forms)))
      x)))

(defmacro as-> [expr name & forms]
  `(let [~name ~expr
         ~@(mapcat (fn [form] [name form]) (butlast forms))]
     ~(if (empty? forms)
        name
        (last forms))))

(defmacro cond-> [expr & clauses]
  (assert (even? (count clauses))
          "cond-> requires an even number of clauses")
  (let [result (gensym)
        steps (map (fn [[test step]]
                     `(if ~test (-> ~result ~step) ~result))
                   (partition 2 clauses))]
    `(let [~result ~expr
           ~@(mapcat (fn [step] [result step]) (butlast steps))]
       ~(if (empty? steps)
          result
          (last steps)))))

(defmacro cond->> [expr & clauses]
  (assert (even? (count clauses))
          "cond->> requires an even number of clauses")
  (let [result (gensym)
        steps (map (fn [[test step]]
                     `(if ~test (->> ~result ~step) ~result))
                   (partition 2 clauses))]
    `(let [~result ~expr
           ~@(mapcat (fn [step] [result step]) (butlast steps))]
       ~(if (empty? steps)
          result
          (last steps)))))

(defmacro some-> [expr & forms]
  (let [result (gensym)
        steps (map (fn [form]
                     `(if (nil? ~result) nil (-> ~result ~form)))
                   forms)]
    `(let [~result ~expr
           ~@(mapcat (fn [step] [result step]) (butlast steps))]
       ~(if (empty? steps)
          result
          (last steps)))))

(defmacro some->> [expr & forms]
  (let [result (gensym)
        steps (map (fn [form]
                     `(if (nil? ~result) nil (->> ~result ~form)))
                   forms)]
    `(let [~result ~expr
           ~@(mapcat (fn [step] [result step]) (butlast steps))]
       ~(if (empty? steps)
          result
          (last steps)))))

(defn identity [x]
  x)

(defn completing
  ([f]
   (fn
     ([] (f))
     ([x] x)
     ([x y] (f x y))))
  ([f cf]
   (fn
     ([] (f))
     ([x] (cf x))
     ([x y] (f x y)))))

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

(defn truth_
  {:inline (fn [x] (list 'boolean x))}
  [x]
  (boolean x))

(defn not [x]
  (if x false true))

(defn nil?
  {:inline (fn [x] (list '__lg_nil-predicate x))}
  [x]
  (__lg_nil-predicate x))

(defn true?
  {:inline (fn [x] (list '__lg_true-predicate x))}
  [x]
  (__lg_true-predicate x))

(defn false?
  {:inline (fn [x] (list '__lg_false-predicate x))}
  [x]
  (__lg_false-predicate x))

(defn int?
  {:inline (fn [x] (list '__lg_int-predicate x))}
  [x]
  (__lg_int-predicate x))

(defn number?
  {:inline (fn [x] (list '__lg_number-predicate x))}
  [x]
  (__lg_number-predicate x))

(defn string?
  {:inline (fn [x] (list '__lg_string-predicate x))}
  [x]
  (__lg_string-predicate x))

(defn keyword?
  {:inline (fn [x] (list '__lg_keyword-predicate x))}
  [x]
  (__lg_keyword-predicate x))

(defn symbol?
  {:inline (fn [x] (list '__lg_symbol-predicate x))}
  [x]
  (__lg_symbol-predicate x))

(defn vector?
  {:inline (fn [x] (list 'satisfies? 'IVector x))}
  [x]
  (satisfies? IVector x))

(defn list?
  {:inline (fn [x] (list '__lg_list-predicate x))}
  [x]
  (__lg_list-predicate x))

(defn seq?
  {:inline (fn [x] (list '__lg_seq-predicate x))}
  [x]
  (__lg_seq-predicate x))

(defn set?
  {:inline (fn [x] (list '__lg_set-predicate x))}
  [x]
  (__lg_set-predicate x))

(defn map?
  {:inline (fn [x] (list 'satisfies? 'IMap x))}
  [x]
  (if (nil? x)
    false
    (satisfies? IMap x)))

(defn fn?
  {:inline (fn [x] (list '__lg_fn-predicate x))}
  [x]
  (__lg_fn-predicate x))

(defn coll?
  {:inline (fn [x] (list '__lg_coll-predicate x))}
  [x]
  (__lg_coll-predicate x))

(defn associative?
  {:inline (fn [x] (list 'satisfies? 'IAssociative x))}
  [x]
  (satisfies? IAssociative x))

(defn rational?
  {:inline (fn [x] (list '__lg_rational-predicate x))}
  [x]
  (__lg_rational-predicate x))

(defn float?
  {:inline (fn [x] (list '__lg_float-predicate x))}
  [x]
  (__lg_float-predicate x))

(defn double?
  {:inline (fn [x] (list '__lg_double-predicate x))}
  [x]
  (__lg_double-predicate x))

(defn sequential?
  {:inline (fn [x] (list '__lg_sequential-predicate x))}
  [x]
  (__lg_sequential-predicate x))

(defn reversible?
  {:inline (fn [x] (list '__lg_reversible-predicate x))}
  [x]
  (__lg_reversible-predicate x))

(defn sorted?
  {:inline (fn [x] (list '__lg_sorted-predicate x))}
  [x]
  (__lg_sorted-predicate x))

(defn zero?
  {:inline (fn [x] (list '__lg_zero-predicate x))}
  [x]
  (__lg_zero-predicate x))

(defn pos?
  {:inline (fn [x] (list '__lg_pos-predicate x))}
  [x]
  (__lg_pos-predicate x))

(defn neg?
  {:inline (fn [x] (list '__lg_neg-predicate x))}
  [x]
  (__lg_neg-predicate x))

(defn abs
  {:inline (fn [x] (list '__lg_abs x))}
  [x]
  (__lg_abs x))

(defmacro unchecked-max
  ([x] x)
  ([x y]
   `(let [x# ~x
          y# ~y]
      (if (> x# y#) x# y#)))
  ([x y & more]
   `(max (max ~x ~y) ~@more)))

(defmacro unchecked-min
  ([x] x)
  ([x y]
   `(let [x# ~x
          y# ~y]
      (if (< x# y#) x# y#)))
  ([x y & more]
   `(min (min ~x ~y) ~@more)))

(defn byte
  {:inline (fn [x] x)}
  [x]
  x)

(defn float
  {:inline (fn [x] x)}
  [x]
  x)

(defn short
  {:inline (fn [x] x)}
  [x]
  x)

(defn unchecked-byte
  {:inline (fn [x] x)}
  [x]
  x)

(defn unchecked-char
  {:inline (fn [x] x)}
  [x]
  x)

(defn unchecked-short
  {:inline (fn [x] x)}
  [x]
  x)

(defn unchecked-float
  {:inline (fn [x] x)}
  [x]
  x)

(defn unchecked-double
  {:inline (fn [x] x)}
  [x]
  x)

(defn double
  {:inline (fn [x] (list '__lg_double x))}
  [^:int x]
  (__lg_double x))

(defn int
  {:inline (fn [x] (list '__lg_int x))}
  [^:int x]
  (__lg_int x))

(defn long
  {:inline (fn [x] (list '__lg_long x))}
  [^:int x]
  (__lg_long x))

(defn unchecked-int
  {:inline (fn [x] (list '__lg_long x))}
  [^:int x]
  (__lg_long x))

(defn unchecked-long
  {:inline (fn [x] (list '__lg_long x))}
  [^:int x]
  (__lg_long x))

(defn char?
  {:inline (fn [x] (list '__lg_char-predicate x))}
  [x]
  (__lg_char-predicate x))

(defn identical?
  {:inline (fn [x y] (list '__lg_identical-predicate x y))}
  [x y]
  (__lg_identical-predicate x y))

(defn array?
  {:inline (fn [x] (list '__lg_array-predicate x))}
  [x]
  (__lg_array-predicate x))

(defn array-value?
  {:inline (fn [x] (list '__lg_array-value-predicate x))}
  [x]
  (__lg_array-value-predicate x))

(defn reduced?
  {:inline (fn [x] (list '__lg_reduced-predicate x))}
  [x]
  (__lg_reduced-predicate x))

(defn some?
  {:inline (fn [x] (list 'not (list '__lg_nil-predicate x)))}
  [x]
  (not (__lg_nil-predicate x)))

(defn boolean?
  {:inline
   (fn [x]
     (let [value (gensym)]
       (list 'let [value x]
             (list 'or
                   (list '__lg_true-predicate value)
                   (list '__lg_false-predicate value)))))}
  [x]
  (or (__lg_true-predicate x) (__lg_false-predicate x)))

(defn integer?
  {:inline (fn [x] (list '__lg_int-predicate x))}
  [x]
  (__lg_int-predicate x))

(defn pos-int?
  {:inline
   (fn [x]
     (let [value (gensym)]
       (list 'let [value x]
             (list 'if (list '__lg_int-predicate value)
                   (list 'pos? value) false))))}
  [x]
  (if (__lg_int-predicate x) (pos? x) false))

(defn neg-int?
  {:inline
   (fn [x]
     (let [value (gensym)]
       (list 'let [value x]
             (list 'if (list '__lg_int-predicate value)
                   (list 'neg? value) false))))}
  [x]
  (if (__lg_int-predicate x) (neg? x) false))

(defn nat-int?
  {:inline
   (fn [x]
     (let [value (gensym)]
       (list 'let [value x]
             (list 'if (list '__lg_int-predicate value)
                   (list 'not (list 'neg? value)) false))))}
  [x]
  (if (__lg_int-predicate x) (not (neg? x)) false))

(defn ident?
  {:inline
   (fn [x]
     (let [value (gensym)]
       (list 'let [value x]
             (list 'or (list 'keyword? value) (list 'symbol? value)))))}
  [x]
  (__lg_keyword-predicate x))

(defn simple-ident?
  {:inline
   (fn [x]
     (let [value (gensym)]
       (list 'let [value x]
             (list 'or
                   (list 'simple-keyword? value)
                   (list 'simple-symbol? value)))))}
  [x]
  (simple-keyword? x))

(defn qualified-ident?
  {:inline
   (fn [x]
     (let [value (gensym)]
       (list 'let [value x]
             (list 'or
                   (list 'qualified-keyword? value)
                   (list 'qualified-symbol? value)))))}
  [x]
  (qualified-keyword? x))

(defn simple-symbol?
  {:inline
   (fn [x]
     (let [value (gensym)]
       (list 'let [value x]
             (list 'if (list '__lg_symbol-predicate value)
                   (list '__lg_nil-predicate (list 'namespace value))
                   false))))}
  [x]
  (__lg_nil-predicate (namespace x)))

(defn qualified-symbol?
  {:inline
   (fn [x]
     (let [value (gensym)]
       (list 'let [value x]
             (list 'if (list '__lg_symbol-predicate value)
                   (list 'not
                         (list '__lg_nil-predicate (list 'namespace value)))
                   false))))}
  [x]
  (not (__lg_nil-predicate (namespace x))))

(defn simple-keyword?
  {:inline
   (fn [x]
     (let [value (gensym)]
       (list 'let [value x]
             (list 'if (list '__lg_keyword-predicate value)
                   (list '__lg_nil-predicate (list 'namespace value))
                   false))))}
  [x]
  (__lg_nil-predicate (namespace x)))

(defn qualified-keyword?
  {:inline
   (fn [x]
     (let [value (gensym)]
       (list 'let [value x]
             (list 'if (list '__lg_keyword-predicate value)
                   (list 'not
                         (list '__lg_nil-predicate (list 'namespace value)))
                   false))))}
  [x]
  (not (__lg_nil-predicate (namespace x))))

(defn counted?
  {:inline
   (fn [x]
     (let [value (gensym)]
       (list 'let [value x]
             (list 'or
                   (list 'map? value)
                   (list 'satisfies? 'clojure.core/ICounted value)))))}
  [x]
  (satisfies? ICounted x))

(defn seqable?
  {:inline
   (fn [x]
     (let [value (gensym)]
       (list 'let [value x]
             (list 'or
                   (list 'nil? value)
                   (list 'map? value)
                   (list 'satisfies? 'clojure.core/ISeqable value)))))}
  [x]
  (satisfies? ISeqable x))

(defn empty?
  {:inline
   (fn [coll]
     (let [value (gensym)]
       (list 'let [value coll] (list 'not (list 'seq value)))))}
  [coll]
  (not (seq coll)))

(defn not-empty
  {:inline
   (fn [coll]
     (let [value (gensym)]
       (list 'let [value coll]
             (list 'if (list 'seq value) value nil))))}
  [coll]
  (if (seq coll) coll nil))

(defn ex-message [ex]
  (__lg_ex-message ex))

(defn ex-cause [ex]
  (__lg_ex-cause ex))

(defn re-pattern
  {:inline (fn [expression] (list '__lg_re-pattern expression))}
  [expression]
  (__lg_re-pattern expression))

(defn inc [x]
  (+ x 1))

(defn dec [x]
  (- x 1))

(defn- bit-and-two [x y]
  (runtime-int/bit-and x y))

(defn bit-and
  ([x y]
   (bit-and-two x y))
  ([x y & more]
   (reduce bit-and-two (bit-and-two x y) more)))

(defn- bit-or-two [x y]
  (runtime-int/bit-or x y))

(defn bit-or
  ([x y]
   (bit-or-two x y))
  ([x y & more]
   (reduce bit-or-two (bit-or-two x y) more)))

(defn- bit-xor-two [x y]
  (runtime-int/bit-xor x y))

(defn bit-xor
  ([x y]
   (bit-xor-two x y))
  ([x y & more]
   (reduce bit-xor-two (bit-xor-two x y) more)))

(defn bit-shift-left [x n]
  (runtime-int/shift-left x n))

(defn bit-shift-right [x n]
  (runtime-int/shift-right x n))

(defn bit-not [x]
  (bit-xor-two x -1))

(defn int-rotate-left [x n]
  (runtime-int/int32
   (bit-or
    (runtime-int/shift-left-32 x n)
    (runtime-int/logical-shift-right-32 x (- n)))))

(defn imul [a b]
  (let [ah (bit-and (runtime-int/logical-shift-right-32 a 16) 0xffff)
        al (bit-and a 0xffff)
        bh (bit-and (runtime-int/logical-shift-right-32 b 16) 0xffff)
        bl (bit-and b 0xffff)]
    (runtime-int/int32
     (+ (* al bl)
        (runtime-int/logical-shift-right-32
         (runtime-int/shift-left-32 (+ (* ah bl) (* al bh)) 16)
         0)))))

(def m3-seed 0)
(def m3-C1 (runtime-int/int32 0xcc9e2d51))
(def m3-C2 (runtime-int/int32 0x1b873593))

(defn m3-mix-K1 [k1]
  (-> (runtime-int/int32 k1)
      (imul m3-C1)
      (int-rotate-left 15)
      (imul m3-C2)))

(defn m3-mix-H1 [h1 k1]
  (runtime-int/int32
   (-> (runtime-int/int32 h1)
       (bit-xor (runtime-int/int32 k1))
       (int-rotate-left 13)
       (imul 5)
       (+ (runtime-int/int32 0xe6546b64)))))

(defn m3-fmix [h1 len]
  (as-> (runtime-int/int32 h1) h1
    (bit-xor h1 len)
    (bit-xor h1 (runtime-int/logical-shift-right-32 h1 16))
    (imul h1 (runtime-int/int32 0x85ebca6b))
    (bit-xor h1 (runtime-int/logical-shift-right-32 h1 13))
    (imul h1 (runtime-int/int32 0xc2b2ae35))
    (runtime-int/int32
     (bit-xor h1 (runtime-int/logical-shift-right-32 h1 16)))))

(defn m3-hash-int [input]
  (if (zero? input)
    input
    (let [k1 (m3-mix-K1 input)
          h1 (m3-mix-H1 m3-seed k1)]
      (m3-fmix h1 4))))

(defn- utf16-code-units [source]
  (runtime-string/utf16-code-units source))

(defn- hash-string-code-unit [hash-code code-unit]
  #?(:melange
     (runtime-int-melange/of-float-unchecked
      (+ (runtime-int-melange/to-float-unchecked (imul 31 hash-code))
         (runtime-int-melange/to-float-unchecked code-unit)))
     :default
     (+ (imul 31 hash-code) code-unit)))

(defn m3-hash-unencoded-chars [input]
  (let [code-units (utf16-code-units input)
        length (alength code-units)
        h1 (loop [i 1 h1 m3-seed]
             (if (< i length)
               (recur
                (+ i 2)
                (m3-mix-H1
                 h1
                 (m3-mix-K1
                  (bit-or
                   (aget code-units (dec i))
                   (runtime-int/shift-left-32
                    (aget code-units i)
                    16)))))
               h1))
        h1 (if (= (bit-and length 1) 1)
             (bit-xor
              h1
              (m3-mix-K1 (aget code-units (dec length))))
             h1)]
    (m3-fmix h1 (imul 2 length))))

(defn hash-string* [source]
  (if-not (nil? source)
    (let [code-units (utf16-code-units source)
          length (alength code-units)]
      (if (pos? length)
        (loop [index 0 hash-code 0]
          (if (< index length)
            (recur
             (inc index)
             (hash-string-code-unit hash-code (aget code-units index)))
            hash-code))
        0))
    0))

(defn mix-collection-hash [hash-basis count]
  (let [h1 m3-seed
        k1 (m3-mix-K1 hash-basis)
        h1 (m3-mix-H1 h1 k1)]
    (m3-fmix h1 count)))

(defn reduced [x]
  (runtime-reduced/reduced x))

(defn reset-vals! [reference new-value]
  (let [old-value (deref reference)]
    [old-value (reset! reference new-value)]))

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

(defn parse-long [source]
  (if (and (runtime-string/decimal-integer-string source)
           (runtime-string/safe-decimal-integer-string source))
    (let [value #?(:melange (runtime-number-melange/parse-int source 10)
                   :default (runtime-string/parse-int-radix source 10))]
      (when (and (<= value 9007199254740991)
                 (>= value -9007199254740991))
        value))
    nil))

(defn parse-double [source]
  (cond
    (runtime-string/double-nan-string source) ##NaN
    (runtime-string/double-number-string source)
    (runtime-string/parse-decimal-float source)
    :else nil))

(defn NaN? [value]
  (js/isNaN value))

(defn infinite? [value]
  (or (= value ##Inf)
      (= value ##-Inf)))

(defn any? [x]
  (runtime-static-value/consume x)
  true)

(defn ratio? [x]
  (runtime-static-value/consume x)
  false)

(defn decimal? [x]
  (runtime-static-value/consume x)
  false)

(defn realized? [future]
  (runtime-future/realized future))

(defn range
  ([] (runtime-seq/range 0 1))
  ([end] (runtime-seq/range-until 0 end 1))
  ([start end] (runtime-seq/range-until start end 1))
  ([start end step] (runtime-seq/range-until start end step)))

(defn shuffle [coll]
  (runtime-random/shuffle-seq (seq coll)))

(defn alength [values]
  (runtime-array/length values))

(defn aclone [arr]
  (runtime-array/copy arr))

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

(defn array-seq
  ([values]
   (array-to-seq values))
  ([values index]
   (drop index (array-to-seq values))))

(defn to-array [coll]
  (runtime-array/of-seq (seq coll)))

(defn into-array [coll]
  (to-array coll))

(defmacro array-values [value & values]
  `(array ~value ~@values))

(defn array-from [coll]
  (to-array coll))

(defn array-binary-search-left [compare values right key]
  (loop [left 0
         right right]
    (if (> left right)
      (double left)
      (let [middle (+ left (/ (- right left) 2))]
        (if (< (compare (aget values middle) key) 0)
          (recur (inc middle) right)
          (recur left (dec middle)))))))

(defn array-binary-search-right [compare values right key]
  (loop [left 0
         right right]
    (if (> left right)
      (double left)
      (let [middle (+ left (/ (- right left) 2))]
        (if (> (compare (aget values middle) key) 0)
          (recur left (dec middle))
          (recur (inc middle) right))))))

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

(defn booleans [x] x)
(defn bytes [x] x)
(defn chars [x] x)
(defn shorts [x] x)
(defn ints [x] x)
(defn floats [x] x)
(defn doubles [x] x)
(defn longs [x] x)

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

(defn- random-uuid-quad-hex []
  (let [unpadded-hex (int-to-string-radix (rand-int 65536) 16)]
    (case (count unpadded-hex)
      1 (str "000" unpadded-hex)
      2 (str "00" unpadded-hex)
      3 (str "0" unpadded-hex)
      unpadded-hex)))

(defn random-uuid []
  (let [version-hex
        (int-to-string-radix
         (bit-or 0x4000 (bit-and 0x0fff (rand-int 65536))) 16)
        variant-hex
        (int-to-string-radix
         (bit-or 0x8000 (bit-and 0x3fff (rand-int 65536))) 16)]
    (uuid
     (str (random-uuid-quad-hex) (random-uuid-quad-hex) "-"
          (random-uuid-quad-hex) "-" version-hex "-" variant-hex "-"
          (random-uuid-quad-hex) (random-uuid-quad-hex)
          (random-uuid-quad-hex)))))

(defn parse-uuid [source]
  (when (runtime-uuid/valid-string source)
    (uuid source)))

(defn system-time []
  #?(:melange (runtime-time-melange/now)
     :default (runtime-time/now)))

(def gensym_counter (atom 0))

(defn gensym
  ([]
   (gensym "G__"))
  ([prefix-string]
   (symbol (str prefix-string (swap! gensym_counter inc)))))

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

(defmacro mask [hash shift]
  `(bit-and (unsigned-bit-shift-right ~hash ~shift) 0x01f))

(defmacro bitpos [hash shift]
  `(bit-shift-left 1 (mask ~hash ~shift)))

(defmacro caching-hash [coll hash-fn hash-key]
  (assert (symbol? hash-key) "hash-key is substituted twice")
  `(let [h# ~hash-key]
     (if-not (nil? h#)
       h#
       (let [h# (~hash-fn ~coll)]
         (set! ~hash-key h#)
         h#))))

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

(defn hash-long [high low]
  (bit-xor high low))

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

(defn splitv-at [n coll]
  [(into [] (take n) coll) (drop n coll)])

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

(defn distinct?
  ([_x]
   true)
  ([x y]
   (not (= x y)))
  ([x y & more]
   (if (not (= x y))
     (loop [seen (hash-set x y)
            remaining more]
       (if remaining
         (let [item (nth remaining 0)]
           (if (contains? seen item)
             false
             (recur (conj seen item) (next remaining))))
         true))
     false)))

(defn not=
  ([_x]
   false)
  ([x y]
   (not (= x y)))
  ([x y & more]
   (if (not (= x y))
     true
     (loop [previous y
            remaining more]
       (if remaining
         (let [current (nth remaining 0)]
           (if (= previous current)
             (recur current (next remaining))
             true))
         false)))))

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

(defn keyword-identical? [left right]
  (= left right))

(defn symbol-identical? [left right]
  (= left right))

(defn special-symbol? [value]
  (contains?
   #{'if 'def 'fn* 'do 'let* 'loop* 'letfn* 'throw 'try 'catch 'finally
     'recur 'new 'set! 'ns 'deftype* 'defrecord* '. 'js* '& 'quote 'case*
     'var 'ns*}
   value))

(defn- merge-entry-with [f m entry]
  (let [k (key entry)
        v (val entry)]
    (if (contains? m k)
      (assoc m k (f (get m k v) v))
      (assoc m k v))))

(defn- merge-two-with [f m1 m2]
  (reduce (fn [m entry] (merge-entry-with f m entry))
          m1
          (seq m2)))

(defn merge-with
  "Returns a map consisting of all input maps. When a key occurs in more than
  one map, combines its values from left to right by calling `f`. With at least
  one map argument, treats `nil` maps as empty maps and returns a map."
  ([_f]
   nil)
  ([f first-map & maps]
   (reduce (fn [m1 m2] (merge-two-with f m1 m2))
           (if-some [m first-map] m {})
           maps)))

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
