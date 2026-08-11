; Copyright (c) Rich Hickey. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; This LG port is derived from ClojureScript's clojure.set implementation.

(ns clojure.set)

(defn- bubble-max-key [key-fn coll]
  (let [maximal
        (__lg_reduce
         (fn [best candidate]
           (if (> (key-fn candidate) (key-fn best))
             candidate
             best))
         (nth coll 0)
         (drop 1 coll))]
    (cons maximal
          (filter (fn [item] (not (identical? maximal item))) coll))))

(defn- union-two [s1 s2]
  (if (< (count s1) (count s2))
    (__lg_reduce conj s2 s1)
    (__lg_reduce conj s1 s2)))

(defn union
  "Return a set that is the union of the input sets."
  ([] #{})
  ([s1] s1)
  ([s1 s2] (union-two s1 s2))
  ([s1 s2 & sets]
   (__lg_reduce union-two (union-two s1 s2) sets)))

(defn- intersection-two [s1 s2]
  (if (< (count s2) (count s1))
    (intersection-two s2 s1)
    (__lg_reduce (fn [result item]
              (if (contains? s2 item)
                result
                (disj result item)))
            s1
            s1)))

(defn intersection
  "Return a set that is the intersection of the input sets."
  ([s1] s1)
  ([s1 s2] (intersection-two s1 s2))
  ([s1 s2 & sets]
   (__lg_reduce intersection-two (intersection-two s1 s2) sets)))

(defn- difference-two [s1 s2]
  (if (< (count s1) (count s2))
    (__lg_reduce (fn [result item]
              (if (contains? s2 item)
                (disj result item)
                result))
            s1
            s1)
    (__lg_reduce disj s1 s2)))

(defn difference
  "Return the first set without elements of the remaining sets."
  ([s1] s1)
  ([s1 s2] (difference-two s1 s2))
  ([s1 s2 & sets]
   (__lg_reduce difference-two (difference-two s1 s2) sets)))

(defn subset?
  "Return whether set1 is a subset of set2."
  [set1 set2]
  (and (<= (count set1) (count set2))
       (every? (fn [value] (contains? set2 value)) set1)))

(defn superset?
  "Returns whether `set1` is a superset of `set2`."
  [set1 set2]
  (and (>= (count set1) (count set2))
       (every? (fn [value] (contains? set1 value)) set2)))

(defn select
  "Returns the elements of `xset` for which `pred` is truthy."
  [pred xset]
  (__lg_reduce
   (fn [result value]
     (if (pred value) result (disj result value)))
   xset
   xset))

(defn project
  "Returns the maps in `xrel` with only the keys in `keys`."
  [xrel keys]
  (__lg_reduce
   (fn [result item]
     (__lg_conj result (select-keys item keys)))
   (empty xrel)
   xrel))

(defn map-invert
  "Returns a map whose values are the keys of `m` and whose keys are its values."
  [m]
  (__lg_reduce-kv
   (fn [result key value]
     (assoc result value key))
   {}
   m))

(defn- remove-renamed-keys [m key-map]
  (__lg_reduce-kv
   (fn [result old _new]
     (dissoc result old))
   m
   key-map))

(defn rename-keys
  "Returns `m` with keys renamed according to `key-map`."
  [m key-map]
  (__lg_reduce-kv
   (fn [result old new]
     (if-some [value (get m old)]
       (assoc result new value)
       result))
   (remove-renamed-keys m key-map)
   key-map))

(defn rename
  "Returns the maps in `xrel` with keys renamed according to `key-map`."
  [xrel key-map]
  (__lg_reduce
   (fn [result item]
     (__lg_conj result (rename-keys item key-map)))
   (empty xrel)
   xrel))

(defn index
  "Returns a map from each distinct projection of `keys` to the matching maps."
  [xrel keys]
  (__lg_reduce
   (fn [result item]
     (let [index-key (select-keys item keys)]
       (assoc result
              index-key
              (__lg_conj (get result index-key (empty xrel)) item))))
   {}
   xrel))

(defn- join-key-mapped [indexed-relation scanned-relation scan-key-map]
  (let [indexed (index indexed-relation (vals scan-key-map))]
    (__lg_reduce
     (fn [result item]
       (let [index-key (rename-keys
                        (select-keys item (keys scan-key-map))
                        scan-key-map)]
         (if-some [found (get indexed index-key)]
           (__lg_reduce
            (fn [joined indexed-item]
              (__lg_conj joined (merge indexed-item item)))
            result
            found)
           result)))
     (empty indexed-relation)
     scanned-relation)))

(defn- join-indexed [indexed-relation scanned-relation join-keys]
  (let [indexed (index indexed-relation join-keys)]
    (__lg_reduce
     (fn [result item]
       (if-some [found (get indexed (select-keys item join-keys))]
         (__lg_reduce
          (fn [joined indexed-item]
            (__lg_conj joined (merge indexed-item item)))
          result
          found)
         result))
     (empty indexed-relation)
     scanned-relation)))

(defn join
  "Returns the natural or key-mapped relational join of two relations."
  ([xrel yrel]
   (if (= 0 (count xrel))
     (empty xrel)
     (if (= 0 (count yrel))
       (empty xrel)
       (if-some [xfirst (first xrel)]
         (if-some [yfirst (first yrel)]
           (let [join-keys (intersection (set (keys xfirst))
                                         (set (keys yfirst)))]
             (if (<= (count xrel) (count yrel))
               (join-indexed xrel yrel join-keys)
               (join-indexed yrel xrel join-keys)))
           (empty xrel))
         (empty xrel)))))
  ([xrel yrel key-map]
   (if (<= (count xrel) (count yrel))
     (join-key-mapped xrel yrel (map-invert key-map))
     (join-key-mapped yrel xrel key-map))))
