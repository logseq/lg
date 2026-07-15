(ns me.tonsky.persistent-sorted-set.path)

(def max-safe-path #?(:native 2147483648 :cljs 2147483648.0))
(def bits-per-level 5)
(def max-len 32)
(def min-len 16)
(def max-safe-level 6)
(def bit-mask 31)
(def factors
  #?(:native
     (array
       1 32 1024 32768 1048576 33554432 1073741824 34359738368
       1099511627776 35184372088832 1125899906842624)
     :cljs
     (array
       1.0 32.0 1024.0 32768.0 1048576.0 33554432.0 1073741824.0
       34359738368.0 1099511627776.0 35184372088832.0 1125899906842624.0)))
(def empty-path #?(:native 0 :cljs 0.0))

(defn path-get
  #?(:native [^:int path ^:int level]
     :cljs [^:float path ^:int level])
  #?(:native
     (if (< level max-safe-level)
       (bit-and (bit-shift-right path (* level bits-per-level)) bit-mask)
       (bit-and (quot path (aget factors level)) bit-mask))
     :cljs
     (if (< level max-safe-level)
       (bit-and
         (bit-shift-right-zero-fill (int path) (* level bits-per-level))
         bit-mask)
       (bit-and
         (int (Float.floor (/ path (aget factors level))))
         bit-mask))))

(defn path-set
  #?(:native [^:int path ^:int level ^:int idx]
     :cljs [^:float path ^:int level ^:int idx])
  #?(:native
     (let [small? (and (< path max-safe-path) (< level max-safe-level))
           old (path-get path level)
           factor (aget factors level)
           minus (if small?
                   (bit-shift-left old (* level bits-per-level))
                   (* old factor))
           plus (if small?
                  (bit-shift-left idx (* level bits-per-level))
                  (* idx factor))]
       (+ (- path minus) plus))
     :cljs
     (let [old (path-get path level)
           factor (aget factors level)]
       (Float.add
         (Float.sub path (Float.mul (double old) factor))
         (Float.mul (double idx) factor)))))

(defn path-inc #?(:native [^:int path] :cljs [^:float path])
  #?(:native (inc path) :cljs (Float.add path 1.0)))

(defn path-dec #?(:native [^:int path] :cljs [^:float path])
  #?(:native (dec path) :cljs (Float.sub path 1.0)))

(defn path-cmp
  #?(:native [^:int path1 ^:int path2]
     :cljs [^:float path1 ^:float path2])
  #?(:native (- path1 path2) :cljs (Float.sub path1 path2)))

(defn path-lt
  #?(:native [^:int path1 ^:int path2]
     :cljs [^:float path1 ^:float path2])
  (< path1 path2))

(defn path-lte
  #?(:native [^:int path1 ^:int path2]
     :cljs [^:float path1 ^:float path2])
  (<= path1 path2))

(defn path-eq
  #?(:native [^:int path1 ^:int path2]
     :cljs [^:float path1 ^:float path2])
  (= path1 path2))

(defn path-same-leaf
  #?(:native [^:int path1 ^:int path2]
     :cljs [^:float path1 ^:float path2])
  #?(:native
     (if (and (< path1 max-safe-path) (< path2 max-safe-path))
       (= (bit-shift-right path1 bits-per-level)
          (bit-shift-right path2 bits-per-level))
       (= (quot path1 max-len)
          (quot path2 max-len)))
     :cljs
     (= (Float.floor (/ path1 32.0))
        (Float.floor (/ path2 32.0)))))

(defn path-str #?(:native [^:int path] :cljs [^:float path])
  #?(:native
     (loop [result []
            path path]
       (if (= path 0)
         (vec (reverse result))
         (recur (conj result (mod path max-len))
                (quot path max-len))))
     :cljs
     (loop [result []
            path path]
       (if (= path 0.0)
         (vec (reverse result))
         (recur (conj result (int (Float.rem path 32.0)))
                (Float.floor (/ path 32.0)))))))
