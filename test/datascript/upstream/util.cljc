(ns datascript.util
  (:refer-clojure :exclude [find]))

#?(:clj
   (defmacro raise [& fragments]
     (let [msgs (butlast fragments)
           _data (last fragments)]
       `(Stdlib.invalid_arg
         (str ~@(map (fn [m#] (if (string? m#) m# (list 'pr-str m#))) msgs))))))

#?(:clj
   (defn- if+-rewrite-cond-impl [cond syms]
     (clojure.core/cond
       (empty? cond)
       true
    
       (and
         (= :let (first cond))
         (empty? (second cond)))
       (if+-rewrite-cond-impl (nnext cond) syms)
    
       (= :let (first cond))
       (let [[var val & rest] (second cond)
             sym                (gensym)]
         (vswap! syms conj [var sym])
         (list 'let [var (list 'clojure.core/vreset! sym val)]
           (if+-rewrite-cond-impl
             (cons 
               :let
               (cons rest
                 (nnext cond)))
             syms)))
    
       :else
       (list 'and
         (first cond)
         (if+-rewrite-cond-impl (next cond) syms)))))

#?(:clj
   (defn- if+-rewrite-cond [cond]
     (let [syms (volatile! [])]
       [(if+-rewrite-cond-impl cond syms) @syms])))

#?(:clj
   (defn- flatten-1 [xs]
     (vec
       (mapcat identity xs))))

#?(:clj
   (defmacro if+
     "Allows sharing local variables between condition and then clause.
      
      Use `:let [...]` form (not nested!) inside `and` condition and its bindings
      will be visible in later `and` clauses and inside `then` branch:
      
        (if+ (and
               (= 1 2)
               ;; same :let syntax as in doseq/for
               :let [x 3
                     y (+ x 4)]
               ;; x and y visible downstream
               (> y x))
          
          ;; then: x and y visible here!
          (+ x y 5)
          
          ;; else: no x or y
          6)"
     [cond then else]
     (if (and
           (seq? cond)
           (or
             (= 'and (first cond))
             (= 'clojure.core/and (first cond))))
       (let [[cond' syms] (if+-rewrite-cond (next cond))]
         `(let ~(flatten-1
                  (for [[_ sym] syms]
                    [sym '(volatile! nil)]))
            (if ~cond'
              (let ~(flatten-1
                      (for [[binding sym] syms]
                        [binding (list 'deref sym)]))
                ~then)
              ~else)))
       (list 'if cond then else))))

#?(:clj
   (defmacro cond+ [& clauses]
     (when-some [[test expr & rest] clauses]
       (case test
         :do   `(do ~expr (util/cond+ ~@rest))
         :let  `(let ~expr (util/cond+ ~@rest))
         :some `(or ~expr (util/cond+ ~@rest))
         `(util/if+ ~test ~expr (util/cond+ ~@rest))))))

#?(:clj
   (defmacro some-of
     ([] nil)
     ([x] x)
     ([x & more]
      `(let [x# ~x] (if (nil? x#) (some-of ~@more) x#)))))

(defn- rand-bits [pow]
  (rand-int (bit-shift-left 1 pow)))

(defn- to-hex-string [n l]
  (let [s (int-to-string-radix n 16)
        c (count s)]
    (cond
      (> c l) (subs s 0 l)
      (< c l) (str (apply str (repeat (- l c) "0")) s)
      :else   s)))

(defn squuid
  ([]
   (squuid #?(:clj  (current-time-millis)
              :cljs (.getTime (js/Date.)))))
  ([msec]
   (uuid
     (str
       (-> (int (/ msec 1000))
         (to-hex-string 8))
       "-" (-> (rand-bits 16) (to-hex-string 4))
       "-" (-> (rand-bits 16) (bit-and 0x0FFF) (bit-or 0x4000) (to-hex-string 4))
       "-" (-> (rand-bits 16) (bit-and 0x3FFF) (bit-or 0x8000) (to-hex-string 4))
       "-" (-> (rand-bits 16) (to-hex-string 4))
       (-> (rand-bits 16) (to-hex-string 4))
       (-> (rand-bits 16) (to-hex-string 4))))))

(defn squuid-time-millis
  "Returns time that was used in [[squuid]] call, in milliseconds, rounded to the closest second."
  [^:string uuid]
  (-> (subs (str uuid) 0 8)
    (js/parseInt 16)
    (* 1000)))
