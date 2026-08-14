; Copyright (c) Rich Hickey. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; This LG port follows ClojureScript's cljs.core source algorithms.

(ns clojure.core
  (:require [ocaml.Buffer :as buffer]
            [ocaml.Stdlib :as stdlib]
            [ocaml.Rrbvec :as rrb-vector]
            [ocaml.Lg_runtime.Runtime_array :as runtime-array]
            [ocaml.Lg_runtime.Runtime_array_melange :as runtime-array-melange]
            [ocaml.Lg_runtime.Runtime_chunk_buffer :as runtime-chunk-buffer]
            [ocaml.Lg_runtime.Runtime_collection :as runtime-collection]
            [ocaml.Lg_runtime.Runtime_future :as runtime-future]
            [ocaml.Lg_runtime.Runtime_hierarchy :as runtime-hierarchy]
            [ocaml.Lg_runtime.Runtime_int :as runtime-int]
            [ocaml.Lg_runtime.Runtime_int_melange :as runtime-int-melange]
            [ocaml.Lg_runtime.Runtime_map :as runtime-map]
            [ocaml.Lg_runtime.Runtime_number_melange :as runtime-number-melange]
            [ocaml.Lg_runtime.Runtime_random :as runtime-random]
            [ocaml.Lg_runtime.Runtime_reduced :as runtime-reduced]
            [ocaml.Lg_runtime.Runtime_seq :as runtime-seq]
            [ocaml.Lg_runtime.Runtime_static_value :as runtime-static-value]
            [ocaml.Lg_runtime.Runtime_string :as runtime-string]
            [ocaml.Lg_runtime.Runtime_time :as runtime-time]
            [ocaml.Lg_runtime.Runtime_time_melange :as runtime-time-melange]
            [ocaml.Lg_runtime.Runtime_uuid :as runtime-uuid]
            [ocaml.Lg_runtime.Runtime_weak :as runtime-weak]))

(def not-native nil)

(defprotocol INamed
  (-name [value])
  (-namespace [value]))

;; ClojureScript char accepts multiple runtime input domains. A private
;; protocol keeps that dispatch first-class and statically witnessed in LG.
(defprotocol ^:private ICharCoercion
  (-char [value] :char))

(defprotocol ^:private INameCoercion
  (-coerce-name [value] :string))

(defprotocol ^:private IKeywordCoercion
  (-coerce-keyword [value] :keyword))

(defprotocol ^:private ISymbolCoercion
  (-coerce-symbol [value] :symbol))

(defprotocol ^:private IIdentifierNamespaceCoercion
  (-coerce-identifier-namespace [value] :option<string>))

(defprotocol ^:private INativeIdentifierNamespacePart
  (-coerce-native-identifier-namespace [value] :option<string>))

(defprotocol ^:private INativeIdentifierNamePart
  (-coerce-native-identifier-name [value] :string))

(defprotocol ^:private IMungeCoercion
  (-munge [value] :self))

(defprotocol ^:private IDemungeCoercion
  (-demunge [value] :self))

(extend-type :int
  ICharCoercion
  (-char [value] (runtime-string/char-of-int value)))

(extend-type :string
  ICharCoercion
  (-char [value] (runtime-string/char-of-string value)))

(extend-type :char
  ICharCoercion
  (-char [value] value))

(defn char [value]
  (-char value))

(extend-type :string
  INameCoercion
  (-coerce-name [value] value)
  IKeywordCoercion
  (-coerce-keyword [value] (__lg_builtin-keyword value))
  ISymbolCoercion
  (-coerce-symbol [value] (__lg_builtin-symbol value))
  IIdentifierNamespaceCoercion
  (-coerce-identifier-namespace [value] (Some value))
  IMungeCoercion
  (-munge [value] (runtime-string/munge value))
  IDemungeCoercion
  (-demunge [value] (runtime-string/demunge value)))

(defn- invalid-native-identifier-part [function-name]
  (raise
   (Invalid_argument
    (str function-name " expects an optional string namespace and string name"))))

#?(:default
   (extend-type :string
     INativeIdentifierNamespacePart
     (-coerce-native-identifier-namespace [value] (Some value))
     INativeIdentifierNamePart
     (-coerce-native-identifier-name [value] value)))

(extend-type nil
  IIdentifierNamespaceCoercion
  (-coerce-identifier-namespace [value] value))

#?(:default
   (extend-type nil
     INativeIdentifierNamespacePart
     (-coerce-native-identifier-namespace [value] None)
     INativeIdentifierNamePart
     (-coerce-native-identifier-name [value]
       (invalid-native-identifier-part "identifier"))))

#?(:default
   (extend-type :symbol
     INativeIdentifierNamespacePart
     (-coerce-native-identifier-namespace [value]
       (invalid-native-identifier-part "identifier"))
     INativeIdentifierNamePart
     (-coerce-native-identifier-name [value]
       (invalid-native-identifier-part "identifier"))))

#?(:default
   (extend-type :keyword
     INativeIdentifierNamespacePart
     (-coerce-native-identifier-namespace [value]
       (invalid-native-identifier-part "identifier"))
     INativeIdentifierNamePart
     (-coerce-native-identifier-name [value]
       (invalid-native-identifier-part "identifier"))))

(defn name [value]
  (INameCoercion/-coerce-name value))

(defn- keyword-one [value]
  (IKeywordCoercion/-coerce-keyword value))

#?(:cljs
   (defn- keyword-two [namespace value]
     (__lg_builtin-keyword
      (IIdentifierNamespaceCoercion/-coerce-identifier-namespace namespace)
      (INameCoercion/-coerce-name value)))
   :default
   (defn- keyword-two [namespace value]
     (__lg_builtin-keyword
      (-coerce-native-identifier-namespace namespace)
      (-coerce-native-identifier-name value))))

(defn keyword
  {:inline (fn
             ([value]
              (if (= value nil)
                nil
                (list 'keyword-one value)))
             ([namespace value]
              (list 'keyword-two namespace value)))}
  ([value]
   (keyword-one value))
  ([namespace value]
   (keyword-two namespace value)))

(defn- symbol-one [value]
  (ISymbolCoercion/-coerce-symbol value))

#?(:cljs
   (defn- symbol-two [namespace value]
     (__lg_builtin-symbol
      (IIdentifierNamespaceCoercion/-coerce-identifier-namespace namespace)
      (str value)))
   :default
   (defn- symbol-two [namespace value]
     (__lg_builtin-symbol
      (-coerce-native-identifier-namespace namespace)
      (-coerce-native-identifier-name value))))

(defn symbol
  {:inline
   (fn
     ([value]
      (if (and (seq? value)
               (or (= 'var (first value))
                   (= '__lg-var-quote (first value))))
        (list '__lg-var-symbol value)
        (list 'symbol-one value)))
     ([namespace value]
      (list 'symbol-two namespace value)))}
  ([value]
   (symbol-one value))
  ([namespace value]
   (symbol-two namespace value)))

(defn var?
  {:inline
   (fn [value]
     (and (seq? value)
          (or (= 'var (first value))
              (= '__lg-var-quote (first value)))))}
  [_value]
  false)

;; These declarations mirror the statically supported portion of the
;; ClojureScript core protocol surface. The compiler registry supplies typed
;; implementations for built-in receivers; source records and types can extend
;; the same protocols without routing public method names through call dispatch.
(defprotocol ICounted
  (-count [coll]))

(defprotocol IChunk
  (-drop-first [coll]))

(defprotocol IChunkedSeq
  (-chunked-first [coll])
  (-chunked-rest [coll]))

(defprotocol IChunkedNext
  (-chunked-next [coll]))

(defprotocol IEmptyableCollection
  (-empty [coll]))

(defprotocol ICollection
  (-conj [coll value]))

(defprotocol IIndexed
  (-nth [coll index] [coll index not-found]))

(defprotocol ILookup
  (-lookup [object key] [object key not-found]))

(defprotocol IAssociative
  (-contains-key? [coll key])
  (-assoc [coll key value]))

(defprotocol IFind
  (-find [coll key]))

(defprotocol IMap
  (-dissoc [coll key]))

(defprotocol IMapEntry
  (-key [entry])
  (-val [entry]))

(defprotocol ISet
  (-disjoin [coll value]))

(defprotocol IStack
  (-peek [coll])
  (-pop [coll]))

(defprotocol ICloneable
  (-clone [value] :self))

(defprotocol IRecord)

(defprotocol ITaggedLiteral)

(defprotocol IReplaceCollection
  (-replace-collection [coll replacements]))

(defprotocol IVector
  (-assoc-n [coll index value]))

(defprotocol IDeref
  (-deref [value]))

(defprotocol IPending
  (-realized? [value] :bool))

(defprotocol IWatchable
  (-notify-watches [reference old-value new-value])
  (-add-watch [reference key callback])
  (-remove-watch [reference key]))

(defprotocol IMeta
  (-meta [value]))

(defprotocol IWithMeta
  (-with-meta [value metadata] :self))

;; LG's typed reduction primitive always supplies the initial accumulator.
(defprotocol IReduce
  (-reduce [coll reducer initial]))

(defprotocol IKVReduce
  (-kv-reduce [coll reducer initial]))

(defprotocol IEquiv
  (-equiv [value other] :bool))

(defprotocol IHash
  (-hash [value]))

(defprotocol ISeqable
  (-seq [value]))

(defprotocol ISeq
  (-first [value])
  (-rest [value]))

(defprotocol INext
  (-next [value]))

(defprotocol IDrop
  (-drop [value count]))

(defprotocol IReversible
  (-rseq [coll]))

(defprotocol ISorted
  (-sorted-seq [coll ascending?])
  (-sorted-seq-from [coll key ascending?])
  (-entry-key [coll entry])
  (-comparator [coll]))

(defprotocol IEditableCollection
  (-as-transient [coll]))

(defprotocol ITransientCollection
  (-conj! [coll value])
  (-persistent! [coll]))

(defprotocol ITransientAssociative
  (-assoc! [coll key value]))

(defprotocol ITransientMap
  (-dissoc! [coll key]))

(defprotocol ITransientVector
  (-assoc-n! [coll index value])
  (-pop! [coll]))

(defprotocol ITransientSet
  (-disjoin! [coll value]))

(defprotocol IComparable
  (-compare [left right]))

(defprotocol IAtom
  (-compare-and-set! [reference old-value new-value]))

(defprotocol IReset
  (-reset! [reference value]))

;; Extra swap! arguments are captured by the source wrapper's typed updater.
(defprotocol ISwap
  (-swap! [reference updater]))

(defprotocol IVolatile
  (-vreset! [reference value]))

(type-variant persistent-tree-map-node [key value]
  TreeMapEmpty
  (TreeMapRed
   :key
   :value
   :persistent-tree-map-node<key;value>
   :persistent-tree-map-node<key;value>)
  (TreeMapBlack
   :key
   :value
   :persistent-tree-map-node<key;value>
   :persistent-tree-map-node<key;value>))

(type-record persistent-tree-map [key value]
  (comparator :fn<key;key;int>)
  (tree :persistent-tree-map-node<key;value>)
  (size :int)
  (metadata :Lg_edn_backend.t))

(type-record persistent-tree-set [value]
  (mapping :persistent-tree-map<value;bool>)
  (metadata :Lg_edn_backend.t))

(type-record ArrayChunk [value]
  (chunk-values :array<value>)
  (chunk-offset :int)
  (chunk-end :int))

(type-record chunked-cons-value [value]
  (chunked-cons-chunk :option<ArrayChunk<value>>)
  (chunked-cons-more :seq<value>)
  (chunked-cons-metadata :Lg_edn_backend.t))

;; The payload stays parameterized instead of crossing an open dynamic boundary.
(type-record TaggedLiteral [form]
  (tag :symbol)
  (form :form)
  (form-hash :int))

(extend-type TaggedLiteral
  ITaggedLiteral
  IHash
  (-hash [value]
    (+ (* 31 (hash (:tag value)))
       (:form-hash value))))

(defn- tagged-literal-tag-value [value]
  (:tag value))

(defn- tagged-literal-form-value [value]
  (:form value))

(extend-type :keyword
  INamed
  (-name [value] (__lg_builtin-name value))
  (-namespace [value] (__lg_builtin-namespace value))
  INameCoercion
  (-coerce-name [value] (__lg_builtin-name value))
  IKeywordCoercion
  (-coerce-keyword [value] value)
  ISymbolCoercion
  (-coerce-symbol [value] (__lg_builtin-symbol value))
  IIdentifierNamespaceCoercion
  (-coerce-identifier-namespace [value] (Some (__lg_builtin-name value))))

(extend-type :symbol
  INamed
  (-name [value] (__lg_builtin-name value))
  (-namespace [value] (__lg_builtin-namespace value))
  INameCoercion
  (-coerce-name [value] (__lg_builtin-name value))
  IKeywordCoercion
  (-coerce-keyword [value] (__lg_builtin-keyword value))
  ISymbolCoercion
  (-coerce-symbol [value] value)
  IIdentifierNamespaceCoercion
  (-coerce-identifier-namespace [value] (Some (__lg_builtin-name value)))
  IMungeCoercion
  (-munge [value] (symbol (runtime-string/munge (str value))))
  IDemungeCoercion
  (-demunge [value] (symbol (runtime-string/demunge (str value)))))

(defn munge [value]
  (IMungeCoercion/-munge value))

(defn demunge [value]
  (IDemungeCoercion/-demunge value))

(defprotocol IWriter
  (-write [writer source])
  (-flush [writer]))

(extend-type :Buffer.t
  IWriter
  (-write [writer source] (buffer/add-string writer source))
  (-flush [_writer] nil))

(defn hash [value]
  (__lg_hash value))

(defn compare [left right]
  (__lg_compare left right))

(defn seq
  {:inline (fn [coll] (list '__lg_seq coll))}
  [coll]
  (__lg_seq coll))

(defn first
  {:inline (fn [coll] (list '__lg_first coll))}
  [coll]
  (__lg_first coll))

(defn rest
  {:inline (fn [coll] (list '__lg_rest coll))}
  [coll]
  (__lg_rest coll))

(defn next
  {:inline (fn [coll] (list '__lg_next coll))}
  [coll]
  (__lg_next coll))

(defn cons
  {:inline (fn [value coll] (list '__lg_cons value coll))}
  [value coll]
  (__lg_cons value coll))

(defn conj
  {:inline
   (fn
     ([] [])
     ([coll] coll)
     ([coll value & values]
      (cons '__lg_conj (cons coll (cons value values)))))}
  ([] [])
  ([coll] coll)
  ([coll value] (ICollection/-conj coll value))
  ([coll value & values]
   (loop [result (ICollection/-conj coll value)
          remaining values]
     (if (seq remaining)
       (recur (ICollection/-conj result (first remaining))
              (next remaining))
       result))))

(defn make-array
  {:inline (fn [size initial]
             (list '__lg_make-array size initial))}
  [size initial]
  (__lg_make-array size initial))

;; ClojureScript dispatches numeric array constructors between a size and a
;; seqable value at runtime. LG preserves that public shape with static protocol
;; witnesses. Numeric size-only arrays use the Clojure zero value because an
;; OCaml array cannot contain JavaScript's uninitialized holes.
(defprotocol ^:private IIntArraySource
  (-int-array-source [source] :array<int>))

(defprotocol ^:private IIntArrayInitial
  (-int-array-initial [initial size] :array<int>))

(defprotocol ^:private IDoubleArraySource
  (-double-array-source [source] :array<float>))

(defprotocol ^:private IDoubleArrayInitial
  (-double-array-initial [initial size] :array<float>))

(defprotocol ^:private IObjectArraySource
  (-object-array-source [source]))

(defprotocol ^:private IObjectArrayInitial
  (-object-array-initial [initial size]))

(defn- ^:array<int> int-array-from-sized-seq
  [^:int size ^:seq<int> values]
  (runtime-array/of-seq-padded size 0 values))

(defn- ^:array<float> double-array-from-sized-seq
  [^:int size ^:seq<float> values]
  (runtime-array/of-seq-padded size 0.0 values))

(defn- object-array-from-sized-list
  [^:int size values]
  (runtime-array/of-list-option-padded size values))

(defn- object-array-from-sized-vector
  [^:int size values]
  (runtime-array/of-vector-option-padded size values))

(defn- object-array-from-sized-seq
  [^:int size values]
  (runtime-array/of-seq-option-padded size values))

(defn- object-array-from-sized-array
  [^:int size values]
  (runtime-array/of-array-option-padded size values))

(extend-type :int
  IIntArraySource
  (-int-array-source [^:int size] (make-array size 0))
  IIntArrayInitial
  (-int-array-initial [^:int initial ^:int size]
    (make-array size initial)))

(extend-type :float
  IDoubleArrayInitial
  (-double-array-initial [^:float initial ^:int size]
    (make-array size initial)))

(extend-type :nil
  IIntArrayInitial
  (-int-array-initial [_ ^:int size] (make-array size 0))
  IDoubleArrayInitial
  (-double-array-initial [_ ^:int size] (make-array size 0.0))
  IObjectArrayInitial
  (-object-array-initial [_ ^:int size] (make-array size nil)))

(extend-protocol IIntArraySource
  :list
  (-int-array-source [values] (runtime-array/of-list values))
  :vector
  (-int-array-source [values] (runtime-array/of-vector values))
  :seq
  (-int-array-source [values] (runtime-array/of-seq values))
  :array
  (-int-array-source [values] (runtime-array/copy values)))

(extend-protocol IIntArrayInitial
  :list
  (-int-array-initial [values ^:int size]
    (runtime-array/of-list-padded size 0 values))
  :vector
  (-int-array-initial [values ^:int size]
    (runtime-array/of-vector-padded size 0 values))
  :seq
  (-int-array-initial [values ^:int size]
    (int-array-from-sized-seq size values))
  :array
  (-int-array-initial [values ^:int size]
    (runtime-array/of-array-padded size 0 values)))

(extend-protocol IDoubleArraySource
  :list
  (-double-array-source [values] (runtime-array/of-list values))
  :vector
  (-double-array-source [values] (runtime-array/of-vector values))
  :seq
  (-double-array-source [values] (runtime-array/of-seq values))
  :array
  (-double-array-source [values] (runtime-array/copy values)))

(extend-protocol IDoubleArrayInitial
  :list
  (-double-array-initial [values ^:int size]
    (runtime-array/of-list-padded size 0.0 values))
  :vector
  (-double-array-initial [values ^:int size]
    (runtime-array/of-vector-padded size 0.0 values))
  :seq
  (-double-array-initial [values ^:int size]
    (double-array-from-sized-seq size values))
  :array
  (-double-array-initial [values ^:int size]
    (runtime-array/of-array-padded size 0.0 values)))

(extend-protocol IObjectArraySource
  :int
  (-object-array-source [^:int size] (make-array size nil))
  :list
  (-object-array-source [values] (runtime-array/of-list values))
  :vector
  (-object-array-source [values] (runtime-array/of-vector values))
  :seq
  (-object-array-source [values] (runtime-array/of-seq values))
  :array
  (-object-array-source [values] (runtime-array/copy values)))

(extend-protocol IObjectArrayInitial
  :list
  (-object-array-initial [values ^:int size]
    (object-array-from-sized-list size values))
  :vector
  (-object-array-initial [values ^:int size]
    (object-array-from-sized-vector size values))
  :seq
  (-object-array-initial [values ^:int size]
    (object-array-from-sized-seq size values))
  :array
  (-object-array-initial [values ^:int size]
    (object-array-from-sized-array size values))
  :string
  (-object-array-initial [value ^:int size] (make-array size value))
  :keyword
  (-object-array-initial [value ^:int size] (make-array size value))
  :symbol
  (-object-array-initial [value ^:int size] (make-array size value))
  :int
  (-object-array-initial [value ^:int size] (make-array size value))
  :float
  (-object-array-initial [value ^:int size] (make-array size value))
  :bool
  (-object-array-initial [value ^:int size] (make-array size value))
  :char
  (-object-array-initial [value ^:int size] (make-array size value)))

(defn int-array
  ([size-or-seq]
   (-int-array-source size-or-seq))
  ([size initial-or-seq]
   (-int-array-initial initial-or-seq size)))

(defn long-array
  ([size-or-seq]
   (-int-array-source size-or-seq))
  ([size initial-or-seq]
   (-int-array-initial initial-or-seq size)))

(defn double-array
  ([size-or-seq]
   (-double-array-source size-or-seq))
  ([size initial-or-seq]
   (-double-array-initial initial-or-seq size)))

(defn float-array
  ([size-or-seq]
   (-double-array-source size-or-seq))
  ([size initial-or-seq]
   (-double-array-initial initial-or-seq size)))

(defn object-array
  ([size-or-seq]
   (-object-array-source size-or-seq))
  ([size initial-or-seq]
   (-object-array-initial initial-or-seq size)))

(extend-protocol ICloneable
  :list
  (-clone [values] (runtime-collection/clone-list values))
  :vector
  (-clone [values] (runtime-collection/clone-vector values))
  :seq
  (-clone [values] (map identity values))
  :map
  (-clone [mapping] (runtime-map/copy mapping)))

(defn clone [value]
  (-clone value))

(defn cloneable?
  {:inline (fn [value] (list 'satisfies? 'ICloneable value))}
  [value]
  (satisfies? ICloneable value))

(defn aget
  {:inline (fn [array index]
             (list '__lg_aget array index))}
  [array index]
  (__lg_aget array index))

(defn aset
  {:inline (fn [array index value]
             (list '__lg_aset array index value))}
  [array index value]
  (__lg_aset array index value))

(defn atom
  {:inline (fn [& values] (cons '__lg_atom values))}
  [value]
  (__lg_atom value))

(defn volatile!
  {:inline (fn [value]
             (list '__lg_volatile! value))}
  [value]
  (__lg_volatile! value))

(defn contains?
  {:inline (fn [collection key]
             (list '__lg_contains collection key))}
  [collection key]
  (__lg_contains collection key))

(defn assoc
  {:inline
   (fn [collection key value & keyvals]
     (loop [expression (list '__lg_assoc collection key value)
            remaining keyvals]
       (if (nil? remaining)
         expression
         (let [tail (next remaining)]
           (if (nil? tail)
             (cons '__lg_assoc
                   (cons collection (cons key (cons value keyvals))))
             (recur (list '__lg_assoc expression
                          (first remaining)
                          (first tail))
                    (next tail)))))))}
  ([collection key value]
   (IAssociative/-assoc collection key value))
  ([collection key value & keyvals]
   (loop [result (IAssociative/-assoc collection key value)
          remaining keyvals]
     (if (seq remaining)
       (let [tail (next remaining)]
         (if (seq tail)
           (recur (IAssociative/-assoc result (first remaining) (first tail))
                  (next tail))
           (throw (ex-info "assoc expects an even number of key/value forms" {}))))
       result))))

(defn dissoc
  {:inline
   (fn [collection & keys]
     (loop [expression collection
            remaining keys]
       (if (nil? remaining)
         expression
         (recur (list '__lg_dissoc expression (first remaining))
                (next remaining)))))}
  ([collection] collection)
  ([collection key]
   (IMap/-dissoc collection key))
  ([collection key & keys]
   (loop [result (IMap/-dissoc collection key)
          remaining keys]
     (if (seq remaining)
       (recur (IMap/-dissoc result (first remaining))
              (next remaining))
       result))))

(defn keys
  {:inline (fn [collection]
             (list '__lg_keys collection))}
  [collection]
  (__lg_keys collection))

(defn get
  {:inline (fn [& args] (cons '__lg_get args))}
  ([object key]
   (ILookup/-lookup object key))
  ([object key not-found]
   (ILookup/-lookup object key not-found)))

(defn subvec
  {:inline (fn [vector start & end]
             (if (nil? end)
               (list '__lg_subvec vector start)
               (list '__lg_subvec vector start (first end))))}
  ([vector start]
   (__lg_subvec vector start))
  ([vector start end]
   (__lg_subvec vector start end)))

(defn make-hierarchy []
  (runtime-hierarchy/make))

(defn isa?
  ([child parent]
   (runtime-hierarchy/global-isa child parent))
  ([hierarchy child parent]
   (runtime-hierarchy/isa hierarchy child parent)))

(defn parents
  ([tag]
   (runtime-hierarchy/global-parents tag))
  ([hierarchy tag]
   (runtime-hierarchy/parents hierarchy tag)))

(defn ancestors
  ([tag]
   (runtime-hierarchy/global-ancestors tag))
  ([hierarchy tag]
   (runtime-hierarchy/ancestors hierarchy tag)))

(defn descendants
  ([tag]
   (runtime-hierarchy/global-descendants tag))
  ([hierarchy tag]
   (runtime-hierarchy/descendants hierarchy tag)))

(defn derive
  ([tag parent]
   (runtime-hierarchy/global-derive tag parent))
  ([hierarchy tag parent]
   (runtime-hierarchy/derive hierarchy tag parent)))

(defn underive
  ([tag parent]
   (runtime-hierarchy/global-underive tag parent))
  ([hierarchy tag parent]
   (runtime-hierarchy/underive hierarchy tag parent)))

(defn list
  {:inline (fn [& values] (cons '__lg_list values))}
  [& values]
  (runtime-seq/to-list (seq values)))

(defn vector
  {:inline (fn [& values] (cons '__lg_vector values))}
  [& values]
  (rrb-vector/of-list (runtime-seq/to-list (seq values))))

(defn vector-lite
  {:inline (fn [& values] (cons '__lg_vector values))}
  [& values]
  (rrb-vector/of-list (runtime-seq/to-list (seq values))))

(defn array
  {:inline (fn [& values] (cons '__lg_array values))}
  [& values]
  (runtime-array/of-seq (seq values)))

(defn- tree-map-balance-black [key value left right]
  (match (tuple left right)
    (tuple
     (TreeMapRed left-key left-value
                 (TreeMapRed far-key far-value far-left far-right)
                 near-right)
     other-right)
    (TreeMapRed
     left-key left-value
     (TreeMapBlack far-key far-value far-left far-right)
     (TreeMapBlack key value near-right other-right))

    (tuple
     (TreeMapRed left-key left-value
                 near-left
                 (TreeMapRed near-key near-value near-left-child near-right-child))
     other-right)
    (TreeMapRed
     near-key near-value
     (TreeMapBlack left-key left-value near-left near-left-child)
     (TreeMapBlack key value near-right-child other-right))

    (tuple
     other-left
     (TreeMapRed right-key right-value
                 (TreeMapRed near-key near-value near-left-child near-right-child)
                 far-right))
    (TreeMapRed
     near-key near-value
     (TreeMapBlack key value other-left near-left-child)
     (TreeMapBlack right-key right-value near-right-child far-right))

    (tuple
     other-left
     (TreeMapRed right-key right-value
                 near-left
                 (TreeMapRed far-key far-value far-left far-right)))
    (TreeMapRed
     right-key right-value
     (TreeMapBlack key value other-left near-left)
     (TreeMapBlack far-key far-value far-left far-right))

    (tuple other-left other-right)
    (TreeMapBlack key value other-left other-right)))

(defn- tree-map-redden [tree]
  (match tree
    (TreeMapBlack key value left right)
    (TreeMapRed key value left right)
    _
    (raise (Invalid_argument "red-black tree invariant violation"))))

(defn- tree-map-balance-left-insert [key value inserted right]
  (match inserted
    (TreeMapRed inserted-key inserted-value
                (TreeMapRed left-key left-value left-left left-right)
                inserted-right)
    (TreeMapRed
     inserted-key inserted-value
     (TreeMapBlack left-key left-value left-left left-right)
     (TreeMapBlack key value inserted-right right))

    (TreeMapRed inserted-key inserted-value inserted-left
                (TreeMapRed right-key right-value right-left right-right))
    (TreeMapRed
     right-key right-value
     (TreeMapBlack inserted-key inserted-value inserted-left right-left)
     (TreeMapBlack key value right-right right))

    _
    (TreeMapBlack key value inserted right)))

(defn- tree-map-balance-right-insert [key value left inserted]
  (match inserted
    (TreeMapRed inserted-key inserted-value inserted-left
                (TreeMapRed right-key right-value right-left right-right))
    (TreeMapRed
     inserted-key inserted-value
     (TreeMapBlack key value left inserted-left)
     (TreeMapBlack right-key right-value right-left right-right))

    (TreeMapRed inserted-key inserted-value
                (TreeMapRed left-key left-value left-left left-right)
                inserted-right)
    (TreeMapRed
     left-key left-value
     (TreeMapBlack key value left left-left)
     (TreeMapBlack inserted-key inserted-value left-right inserted-right))

    _
    (TreeMapBlack key value left inserted)))

(defn- tree-map-balance-left-delete [key value deleted right]
  (match deleted
    (TreeMapRed deleted-key deleted-value deleted-left deleted-right)
    (TreeMapRed
     key value
     (TreeMapBlack deleted-key deleted-value deleted-left deleted-right)
     right)

    _
    (match right
      (TreeMapBlack right-key right-value right-left right-right)
      (tree-map-balance-right-insert
       key value deleted
       (TreeMapRed right-key right-value right-left right-right))

      (TreeMapRed right-key right-value
                  (TreeMapBlack pivot-key pivot-value pivot-left pivot-right)
                  right-right)
      (TreeMapRed
       pivot-key pivot-value
       (TreeMapBlack key value deleted pivot-left)
       (tree-map-balance-right-insert
        right-key right-value pivot-right (tree-map-redden right-right)))

      _
      (raise (Invalid_argument "red-black tree invariant violation")))))

(defn- tree-map-balance-right-delete [key value left deleted]
  (match deleted
    (TreeMapRed deleted-key deleted-value deleted-left deleted-right)
    (TreeMapRed
     key value left
     (TreeMapBlack deleted-key deleted-value deleted-left deleted-right))

    _
    (match left
      (TreeMapBlack left-key left-value left-left left-right)
      (tree-map-balance-left-insert
       key value (TreeMapRed left-key left-value left-left left-right) deleted)

      (TreeMapRed left-key left-value left-left
                  (TreeMapBlack pivot-key pivot-value pivot-left pivot-right))
      (TreeMapRed
       pivot-key pivot-value
       (tree-map-balance-left-insert
        left-key left-value (tree-map-redden left-left) pivot-left)
       (TreeMapBlack key value pivot-right deleted))

      _
      (raise (Invalid_argument "red-black tree invariant violation")))))

(defn- tree-map-append [left right]
  (match (tuple left right)
    (tuple TreeMapEmpty other)
    other

    (tuple other TreeMapEmpty)
    other

    (tuple
     (TreeMapRed left-key left-value left-left left-right)
     (TreeMapRed right-key right-value right-left right-right))
    (let [appended (tree-map-append left-right right-left)]
      (match appended
        (TreeMapRed pivot-key pivot-value pivot-left pivot-right)
        (TreeMapRed
         pivot-key pivot-value
         (TreeMapRed left-key left-value left-left pivot-left)
         (TreeMapRed right-key right-value pivot-right right-right))
        _
        (TreeMapRed
         left-key left-value left-left
         (TreeMapRed right-key right-value appended right-right))))

    (tuple
     (TreeMapRed left-key left-value left-left left-right)
     other-right)
    (TreeMapRed
     left-key left-value left-left
     (tree-map-append left-right other-right))

    (tuple
     other-left
     (TreeMapRed right-key right-value right-left right-right))
    (TreeMapRed
     right-key right-value
     (tree-map-append other-left right-left) right-right)

    (tuple
     (TreeMapBlack left-key left-value left-left left-right)
     (TreeMapBlack right-key right-value right-left right-right))
    (let [appended (tree-map-append left-right right-left)]
      (match appended
        (TreeMapRed pivot-key pivot-value pivot-left pivot-right)
        (TreeMapRed
         pivot-key pivot-value
         (TreeMapBlack left-key left-value left-left pivot-left)
         (TreeMapBlack right-key right-value pivot-right right-right))
        _
        (tree-map-balance-left-delete
         left-key left-value left-left
         (TreeMapBlack right-key right-value appended right-right))))))

(defn- tree-map-remove-node [comparator tree removed-key]
  (match tree
    TreeMapEmpty
    (tuple TreeMapEmpty false)

    (TreeMapRed key value left right)
    (let [compared (comparator removed-key key)]
      (cond
        (= compared 0)
        (tuple (tree-map-append left right) true)

        (< compared 0)
        (let [removed (tree-map-remove-node comparator left removed-key)]
          (if (stdlib/snd removed)
            (tuple
             (match left
               (TreeMapBlack _ _ _ _)
               (tree-map-balance-left-delete
                key value (stdlib/fst removed) right)
               _
               (TreeMapRed key value (stdlib/fst removed) right))
             true)
            (tuple tree false)))

        :else
        (let [removed (tree-map-remove-node comparator right removed-key)]
          (if (stdlib/snd removed)
            (tuple
             (match right
               (TreeMapBlack _ _ _ _)
               (tree-map-balance-right-delete
                key value left (stdlib/fst removed))
               _
               (TreeMapRed key value left (stdlib/fst removed)))
             true)
            (tuple tree false)))))

    (TreeMapBlack key value left right)
    (let [compared (comparator removed-key key)]
      (cond
        (= compared 0)
        (tuple (tree-map-append left right) true)

        (< compared 0)
        (let [removed (tree-map-remove-node comparator left removed-key)]
          (if (stdlib/snd removed)
            (tuple
             (match left
               (TreeMapBlack _ _ _ _)
               (tree-map-balance-left-delete
                key value (stdlib/fst removed) right)
               _
               (TreeMapRed key value (stdlib/fst removed) right))
             true)
            (tuple tree false)))

        :else
        (let [removed (tree-map-remove-node comparator right removed-key)]
          (if (stdlib/snd removed)
            (tuple
             (match right
               (TreeMapBlack _ _ _ _)
               (tree-map-balance-right-delete
                key value left (stdlib/fst removed))
               _
               (TreeMapRed key value left (stdlib/fst removed)))
             true)
            (tuple tree false)))))))

(defn- tree-map-insert-node [comparator tree key value]
  (match tree
    TreeMapEmpty
    (tuple (TreeMapRed key value TreeMapEmpty TreeMapEmpty) true)

    (TreeMapRed node-key node-value left right)
    (let [compared (comparator key node-key)]
      (cond
        (< compared 0)
        (let [inserted (tree-map-insert-node comparator left key value)]
          (tuple
           (TreeMapRed node-key node-value (stdlib/fst inserted) right)
           (stdlib/snd inserted)))

        (> compared 0)
        (let [inserted (tree-map-insert-node comparator right key value)]
          (tuple
           (TreeMapRed node-key node-value left (stdlib/fst inserted))
           (stdlib/snd inserted)))

        :else
        (tuple (TreeMapRed key value left right) false)))

    (TreeMapBlack node-key node-value left right)
    (let [compared (comparator key node-key)]
      (cond
        (< compared 0)
        (let [inserted (tree-map-insert-node comparator left key value)]
          (tuple
           (tree-map-balance-black
            node-key node-value (stdlib/fst inserted) right)
           (stdlib/snd inserted)))

        (> compared 0)
        (let [inserted (tree-map-insert-node comparator right key value)]
          (tuple
           (tree-map-balance-black
            node-key node-value left (stdlib/fst inserted))
           (stdlib/snd inserted)))

        :else
        (tuple (TreeMapBlack key value left right) false)))))

(defn- tree-map-blacken [tree]
  (match tree
    TreeMapEmpty TreeMapEmpty
    (TreeMapRed key value left right)
    (TreeMapBlack key value left right)
    (TreeMapBlack key value left right)
    (TreeMapBlack key value left right)))

(defn- tree-map-empty [comparator metadata]
  (record persistent-tree-map
          (comparator comparator)
          (tree TreeMapEmpty)
          (size 0)
          (metadata metadata)))

(defn- tree-map-assoc [mapping key value]
  (let [inserted
        (tree-map-insert-node
         (:comparator mapping) (:tree mapping) key value)]
    (record persistent-tree-map
            (comparator (:comparator mapping))
            (tree (tree-map-blacken (stdlib/fst inserted)))
            (size (if (stdlib/snd inserted)
                    (inc (:size mapping))
                    (:size mapping)))
            (metadata (:metadata mapping)))))

(defn- tree-map-singleton [comparator metadata key value]
  (tree-map-assoc (tree-map-empty comparator metadata) key value))

(defn- tree-map-get-node [comparator tree key]
  (match tree
    TreeMapEmpty nil
    (TreeMapRed node-key node-value left right)
    (let [compared (comparator key node-key)]
      (cond
        (< compared 0) (tree-map-get-node comparator left key)
        (> compared 0) (tree-map-get-node comparator right key)
        :else (Some node-value)))
    (TreeMapBlack node-key node-value left right)
    (let [compared (comparator key node-key)]
      (cond
        (< compared 0) (tree-map-get-node comparator left key)
        (> compared 0) (tree-map-get-node comparator right key)
        :else (Some node-value)))))

(defn- tree-map-get [mapping key]
  (tree-map-get-node (:comparator mapping) (:tree mapping) key))

(defn- tree-map-pairs [tree tail]
  (match tree
    TreeMapEmpty tail
    (TreeMapRed key value left right)
    (tree-map-pairs left (cons (tuple key value) (tree-map-pairs right tail)))
    (TreeMapBlack key value left right)
    (tree-map-pairs left (cons (tuple key value) (tree-map-pairs right tail)))))

(defn- tree-map-reverse-pairs [tree tail]
  (match tree
    TreeMapEmpty tail
    (TreeMapRed key value left right)
    (tree-map-reverse-pairs
     right (cons (tuple key value) (tree-map-reverse-pairs left tail)))
    (TreeMapBlack key value left right)
    (tree-map-reverse-pairs
     right (cons (tuple key value) (tree-map-reverse-pairs left tail)))))

(defn- tree-map-seq [mapping ascending?]
  (if ascending?
    (tree-map-pairs (:tree mapping) (seq []))
    (tree-map-reverse-pairs (:tree mapping) (seq []))))

(defn- tree-map-seq-from-node
  [comparator tree start-key ascending? tail]
  (match tree
    TreeMapEmpty tail
    (TreeMapRed key value left right)
    (let [compared (comparator key start-key)]
      (if ascending?
        (if (< compared 0)
          (tree-map-seq-from-node comparator right start-key true tail)
          (tree-map-seq-from-node
           comparator left start-key true
           (cons (tuple key value)
                 (tree-map-seq-from-node
                  comparator right start-key true tail))))
        (if (> compared 0)
          (tree-map-seq-from-node comparator left start-key false tail)
          (tree-map-seq-from-node
           comparator right start-key false
           (cons (tuple key value)
                 (tree-map-seq-from-node
                  comparator left start-key false tail))))))
    (TreeMapBlack key value left right)
    (let [compared (comparator key start-key)]
      (if ascending?
        (if (< compared 0)
          (tree-map-seq-from-node comparator right start-key true tail)
          (tree-map-seq-from-node
           comparator left start-key true
           (cons (tuple key value)
                 (tree-map-seq-from-node
                  comparator right start-key true tail))))
        (if (> compared 0)
          (tree-map-seq-from-node comparator left start-key false tail)
          (tree-map-seq-from-node
           comparator right start-key false
           (cons (tuple key value)
                 (tree-map-seq-from-node
                  comparator left start-key false tail))))))))

(defn- tree-map-seq-from [mapping start-key ascending?]
  (tree-map-seq-from-node
   (:comparator mapping) (:tree mapping) start-key ascending? (seq [])))

(defn- tree-map-entry-key [_mapping entry]
  (key entry))

(defn- tree-map-comparator [mapping]
  (:comparator mapping))

(defn- tree-map-size [mapping]
  (:size mapping))

(defn- tree-map-dissoc [mapping removed-key]
  (let [removed
        (tree-map-remove-node
         (:comparator mapping) (:tree mapping) removed-key)]
    (if (stdlib/snd removed)
      (record persistent-tree-map
              (comparator (:comparator mapping))
              (tree (tree-map-blacken (stdlib/fst removed)))
              (size (dec (:size mapping)))
              (metadata (:metadata mapping)))
      mapping)))

(extend-type persistent-tree-map
  IEquiv
  (-equiv [mapping other]
    (if (= (:size mapping) (:size other))
      (every?
       (fn [entry]
         (match (tree-map-get other (key entry))
           (Some other-value) (= other-value (val entry))
           None false))
       (tree-map-seq mapping true))
      false))
  ISeqable
  (-seq [mapping]
    (tree-map-seq mapping true))
  IReversible
  (-rseq [mapping]
    (tree-map-seq mapping false))
  ICollection
  (-conj [mapping entry]
    (tree-map-assoc mapping (key entry) (val entry)))
  ILookup
  (-lookup [mapping key]
    (tree-map-get mapping key))
  (-lookup [mapping key not-found]
    (match (tree-map-get mapping key)
      (Some value) value
      None not-found))
  IAssociative
  (-contains-key? [mapping key]
    (match (tree-map-get mapping key)
      None false
      (Some _) true))
  (-assoc [mapping key value]
    (tree-map-assoc mapping key value))
  IMap
  (-dissoc [mapping key]
    (tree-map-dissoc mapping key))
  ICounted
  (-count [mapping]
    (:size mapping))
  IEmptyableCollection
  (-empty [mapping]
    (tree-map-empty (:comparator mapping) (:metadata mapping)))
  IMeta
  (-meta [mapping]
    (:metadata mapping))
  IWithMeta
  (-with-meta [mapping metadata]
    (record persistent-tree-map
            (comparator (:comparator mapping))
            (tree (:tree mapping))
            (size (:size mapping))
            (metadata metadata)))
  ISorted
  (-sorted-seq [mapping ascending?]
    (tree-map-seq mapping ascending?))
  (-sorted-seq-from [mapping key ascending?]
    (tree-map-seq-from mapping key ascending?))
  (-entry-key [mapping entry]
    (tree-map-entry-key mapping entry))
  (-comparator [mapping]
    (tree-map-comparator mapping)))

(defn sorted-map
  {:inline
   (fn [& keyvals]
     (let [left (gensym)
           right (gensym)
           comparator (list 'fn [left right]
                            (list 'stdlib/compare left right))
           metadata (list 'meta {})]
       (if (nil? keyvals)
         (list 'tree-map-empty comparator metadata)
         (let [tail (next keyvals)]
           (if (nil? tail)
             (raise (Invalid_argument "No value supplied for key"))
             (loop [remaining (next tail)
                    expression
                    (list 'tree-map-singleton comparator metadata
                          (first keyvals) (first tail))]
               (if (nil? remaining)
                 expression
                 (let [remaining-tail (next remaining)]
                   (if (nil? remaining-tail)
                     (raise (Invalid_argument "No value supplied for key"))
                     (recur (next remaining-tail)
                            (list 'tree-map-assoc expression
                                  (first remaining)
                                  (first remaining-tail))))))))))))}
  ([] (tree-map-empty (fn [left right] (stdlib/compare left right)) (meta {})))
  ([k1 v1]
   (tree-map-singleton
    (fn [left right] (stdlib/compare left right)) (meta {}) k1 v1))
  ([k1 v1 k2 v2]
   (tree-map-assoc (sorted-map k1 v1) k2 v2))
  ([k1 v1 k2 v2 k3 v3]
   (tree-map-assoc (sorted-map k1 v1 k2 v2) k3 v3))
  ([k1 v1 k2 v2 k3 v3 k4 v4]
   (tree-map-assoc (sorted-map k1 v1 k2 v2 k3 v3) k4 v4)))

(defn sorted-map-by
  {:inline
   (fn [comparator & keyvals]
     (let [comparator-name (gensym)
           normalized-comparator
           (list '__lg_fn-to-comparator comparator)
           metadata (list 'meta {})
           expression
           (if (nil? keyvals)
             (list 'tree-map-empty comparator-name metadata)
             (let [tail (next keyvals)]
               (if (nil? tail)
                 (raise (Invalid_argument "No value supplied for key"))
                 (loop [remaining (next tail)
                        expression
                        (list 'tree-map-singleton comparator-name metadata
                              (first keyvals) (first tail))]
                   (if (nil? remaining)
                     expression
                     (let [remaining-tail (next remaining)]
                       (if (nil? remaining-tail)
                         (raise (Invalid_argument "No value supplied for key"))
                         (recur (next remaining-tail)
                                (list 'tree-map-assoc expression
                                      (first remaining)
                                      (first remaining-tail))))))))))]
       (list 'let [comparator-name normalized-comparator] expression))) }
  ([comparator]
   (tree-map-empty (__lg_fn-to-comparator comparator) (meta {})))
  ([comparator k1 v1]
   (tree-map-singleton
    (__lg_fn-to-comparator comparator) (meta {}) k1 v1))
  ([comparator k1 v1 k2 v2]
   (tree-map-assoc (sorted-map-by comparator k1 v1) k2 v2))
  ([comparator k1 v1 k2 v2 k3 v3]
   (tree-map-assoc (sorted-map-by comparator k1 v1 k2 v2) k3 v3))
  ([comparator k1 v1 k2 v2 k3 v3 k4 v4]
   (tree-map-assoc
    (sorted-map-by comparator k1 v1 k2 v2 k3 v3) k4 v4)))

(defn- tree-set-empty [comparator metadata]
  (record persistent-tree-set
          (mapping (tree-map-empty comparator metadata))
          (metadata metadata)))

(defn- tree-set-singleton [comparator metadata value]
  (record persistent-tree-set
          (mapping (tree-map-singleton comparator metadata value true))
          (metadata metadata)))

(defn- tree-set-conj [set value]
  (match (tree-map-get (:mapping set) value)
    (Some _) set
    None
    (record persistent-tree-set
            (mapping (tree-map-assoc (:mapping set) value true))
            (metadata (:metadata set)))))

(defn- tree-set-disjoin [set value]
  (let [mapping (tree-map-dissoc (:mapping set) value)]
    (if (identical? mapping (:mapping set))
      set
      (record persistent-tree-set
              (mapping mapping)
              (metadata (:metadata set))))))

(defn- tree-set-values [set ascending?]
  (map key (tree-map-seq (:mapping set) ascending?)))

(defn- tree-set-values-from [set value ascending?]
  (map key (tree-map-seq-from (:mapping set) value ascending?)))

(defn- tree-set-get [set value]
  (match (tree-map-get (:mapping set) value)
    (Some _) (Some value)
    None None))

(extend-type persistent-tree-set
  IEquiv
  (-equiv [set other]
    (if (= (tree-map-size (:mapping set))
           (tree-map-size (:mapping other)))
      (every?
       (fn [value]
         (match (tree-set-get other value)
           (Some _) true
           None false))
       (tree-set-values set true))
      false))
  ISeqable
  (-seq [set]
    (tree-set-values set true))
  ICollection
  (-conj [set value]
    (tree-set-conj set value))
  ILookup
  (-lookup [set value]
    (tree-set-get set value))
  (-lookup [set value not-found]
    (match (tree-set-get set value)
      (Some found) found
      None not-found))
  ISet
  (-disjoin [set value]
    (tree-set-disjoin set value))
  ICounted
  (-count [set]
    (tree-map-size (:mapping set)))
  IEmptyableCollection
  (-empty [set]
    (tree-set-empty (tree-map-comparator (:mapping set)) (:metadata set)))
  IMeta
  (-meta [set]
    (:metadata set))
  IWithMeta
  (-with-meta [set metadata]
    (record persistent-tree-set
            (mapping (:mapping set))
            (metadata metadata)))
  ISorted
  (-sorted-seq [set ascending?]
    (tree-set-values set ascending?))
  (-sorted-seq-from [set value ascending?]
    (tree-set-values-from set value ascending?))
  (-entry-key [_set entry]
    entry)
  (-comparator [set]
    (tree-map-comparator (:mapping set)))
  IReversible
  (-rseq [set]
    (tree-set-values set false)))

(defn sorted-set
  {:inline
   (fn [& values]
     (let [left (gensym)
           right (gensym)
           comparator (list 'fn [left right]
                            (list 'stdlib/compare left right))
           metadata (list 'meta {})]
       (if (nil? values)
         (list 'tree-set-empty comparator metadata)
         (loop [expression
                (list 'tree-set-singleton comparator metadata (first values))
                remaining (next values)]
           (if (nil? remaining)
             expression
             (recur (list 'tree-set-conj expression (first remaining))
                    (next remaining)))))))}
  ([] (tree-set-empty (fn [left right] (stdlib/compare left right)) (meta {})))
  ([v1]
   (tree-set-singleton
    (fn [left right] (stdlib/compare left right)) (meta {}) v1))
  ([v1 v2]
   (tree-set-conj (sorted-set v1) v2))
  ([v1 v2 v3]
   (tree-set-conj (sorted-set v1 v2) v3))
  ([v1 v2 v3 v4]
   (tree-set-conj (sorted-set v1 v2 v3) v4)))

(defn sorted-set-by
  {:inline
   (fn [comparator & values]
     (let [comparator-name (gensym)
           normalized-comparator
           (list '__lg_fn-to-comparator comparator)
           metadata (list 'meta {})]
       (list
        'let [comparator-name normalized-comparator]
        (if (nil? values)
          (list 'tree-set-empty comparator-name metadata)
          (loop [expression
                 (list 'tree-set-singleton comparator-name metadata
                       (first values))
                 remaining (next values)]
            (if (nil? remaining)
              expression
              (recur (list 'tree-set-conj expression (first remaining))
                     (next remaining))))))))}
  ([comparator]
   (tree-set-empty (__lg_fn-to-comparator comparator) (meta {})))
  ([comparator v1]
   (tree-set-singleton
    (__lg_fn-to-comparator comparator) (meta {}) v1))
  ([comparator v1 v2]
   (tree-set-conj (sorted-set-by comparator v1) v2))
  ([comparator v1 v2 v3]
   (tree-set-conj (sorted-set-by comparator v1 v2) v3))
  ([comparator v1 v2 v3 v4]
   (tree-set-conj (sorted-set-by comparator v1 v2 v3) v4)))

(defn mk-bound-fn
  [sorted-collection ^:fn<int;int;bool> test key]
  (fn [entry]
    (let [comparator (ISorted/-comparator sorted-collection)]
      (test
       (comparator (ISorted/-entry-key sorted-collection entry) key)
       0))))

(defn subseq
  ([sorted-collection ^:fn<int;int;bool> test key]
   (let [include (mk-bound-fn sorted-collection test key)]
     (if (test 1 0)
       (let [entries
             (ISorted/-sorted-seq-from sorted-collection key true)]
         (if (not (seq entries))
           entries
           (if (include (nth entries 0)) entries (next entries))))
       (take-while include
                   (ISorted/-sorted-seq sorted-collection true)))))
  ([sorted-collection
    ^:fn<int;int;bool> start-test start-key
    ^:fn<int;int;bool> end-test end-key]
   (let [entries
         (ISorted/-sorted-seq-from sorted-collection start-key true)]
     (if (not (seq entries))
       entries
       (let [include-start
             (mk-bound-fn sorted-collection start-test start-key)
             bounded
             (if (include-start (nth entries 0)) entries (next entries))]
         (take-while
          (mk-bound-fn sorted-collection end-test end-key)
          bounded))))))

(defn rsubseq
  ([sorted-collection ^:fn<int;int;bool> test key]
   (let [include (mk-bound-fn sorted-collection test key)]
     (if (test -1 0)
       (let [entries
             (ISorted/-sorted-seq-from sorted-collection key false)]
         (if (not (seq entries))
           entries
           (if (include (nth entries 0)) entries (next entries))))
       (take-while include
                   (ISorted/-sorted-seq sorted-collection false)))))
  ([sorted-collection
    ^:fn<int;int;bool> start-test start-key
    ^:fn<int;int;bool> end-test end-key]
   (let [entries
         (ISorted/-sorted-seq-from sorted-collection end-key false)]
     (if (not (seq entries))
       entries
       (let [include-end
             (mk-bound-fn sorted-collection end-test end-key)
             bounded
             (if (include-end (nth entries 0)) entries (next entries))]
         (take-while
          (mk-bound-fn sorted-collection start-test start-key)
          bounded))))))

(defn- map-from-keyvals [keyvals]
  (loop [remaining (seq keyvals)
         result {}]
    (if remaining
      (let [tail (next remaining)]
        (if tail
          (recur (next tail)
                 (assoc result (first remaining) (first tail)))
          (raise (Invalid_argument "No value supplied for key"))))
      result)))

(defn hash-map
  {:inline (fn [& keyvals] (cons '__lg_hash-map keyvals))}
  [& keyvals]
  (map-from-keyvals keyvals))

(defn hash-map-lite
  {:inline (fn [& keyvals] (cons '__lg_hash-map keyvals))}
  [& keyvals]
  (map-from-keyvals keyvals))

(defn array-map
  {:inline (fn [& keyvals] (cons '__lg_array-map keyvals))}
  [& keyvals]
  (map-from-keyvals keyvals))

(defn- set-from-coll [coll]
  (__lg_reduce (fn [result value] (__lg_conj result value)) #{} coll))

(defn set
  {:inline (fn [coll] (list '__lg_set coll))}
  [coll]
  (set-from-coll coll))

(defn set-lite
  {:inline (fn [coll] (list '__lg_set coll))}
  [coll]
  (set-from-coll coll))

(defn hash-set
  {:inline (fn [& keys] (cons '__lg_hash-set keys))}
  [& keys]
  (set-from-coll keys))

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
    (if body
      `(when-let [source# (seq ~source)]
         (let [~name (first source#)]
           ~@body))
      `(when-let [source# (seq ~source)]
         (let [~name (first source#)] nil)))))

(defmacro while [test & body]
  `(loop []
     (if ~test
       (do
         ~@body
         (recur))
       nil)))

(defmacro doseq [seq-exprs & body]
  (assert (vector? seq-exprs)
          "doseq requires a vector for its binding")
  (assert (even? (count seq-exprs))
          "doseq requires an even number of forms in binding vector")
  `(__lg_doseq ~seq-exprs ~@body))

(defmacro with-redefs [bindings & body]
  `(__lg_with_redefs ~bindings ~@body))

(defmacro with-precision [precision & expressions]
  (if (= :rounding (first expressions))
    `(__lg_with-precision ~precision ~(name (second expressions))
       (fn [] ~@(nnext expressions)))
    `(__lg_with-precision ~precision "HALF_UP" (fn [] ~@expressions))))

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

(defmacro lazy-seq [& body]
  `(__lg_defer_seq (fn [] (do ~@body))))

(defmacro lazy-cat [& colls]
  `(concat ~@(map (fn [coll] `(lazy-seq ~coll)) colls)))

(defn apply
  {:inline
   (fn
     ([function args]
      (list '__lg_apply
            (if (or (= function 'pr)
                    (= function 'clojure.core/pr)
                    (= function 'cljs.core/pr))
              '__lg_pr
              function)
            args))
     ([function x args]
      (list '__lg_apply
            (if (or (= function 'pr)
                    (= function 'clojure.core/pr)
                    (= function 'cljs.core/pr))
              '__lg_pr
              function)
            x args))
     ([function x y args]
      (list '__lg_apply
            (if (or (= function 'pr)
                    (= function 'clojure.core/pr)
                    (= function 'cljs.core/pr))
              '__lg_pr
              function)
            x y args))
     ([function x y z args]
      (list '__lg_apply
            (if (or (= function 'pr)
                    (= function 'clojure.core/pr)
                    (= function 'cljs.core/pr))
              '__lg_pr
              function)
            x y z args))
     ([function x y z w args]
      (list '__lg_apply
            (if (or (= function 'pr)
                    (= function 'clojure.core/pr)
                    (= function 'cljs.core/pr))
              '__lg_pr
              function)
            x y z w args)))}
  ([f args]
   (__lg_apply f args))
  ([f x args]
   (__lg_apply f x args))
  ([f x y args]
   (__lg_apply f x y args))
  ([f x y z args]
   (__lg_apply f x y z args))
  ([f x y z w args]
   (__lg_apply f x y z w args)))

(defn trampoline [f]
  (match (f)
    (TrampolineCall next) (trampoline next)
    (TrampolineDone value) value))

(defmacro memoize [f]
  (list '__lg_memoize f))

(defn- map-seq [f coll]
  (lazy-seq
   (if coll
     (cons (f (nth coll 0))
           (map-seq f (rest coll)))
     nil)))

(defn- map2-seq [f left right]
  (lazy-seq
   (if (and left right)
     (cons (f (nth left 0) (nth right 0))
           (map2-seq f (rest left) (rest right)))
     nil)))

(defn- map3-seq [f first-coll second-coll third-coll]
  (lazy-seq
   (if (and first-coll second-coll third-coll)
     (cons (f (nth first-coll 0)
              (nth second-coll 0)
              (nth third-coll 0))
           (map3-seq f
                     (rest first-coll)
                     (rest second-coll)
                     (rest third-coll)))
     nil)))

(declare map)

(defn- map-many-seq [f first-coll second-coll third-coll colls]
  (lazy-seq
   (if (and first-coll
            second-coll
            third-coll
            (every? (fn [coll] (seq coll)) colls))
     (cons
      (__lg_apply f
             (nth first-coll 0)
             (nth second-coll 0)
             (nth third-coll 0)
             (map-seq (fn [coll] (nth coll 0)) colls))
      (map-many-seq f
                    (rest first-coll)
                    (rest second-coll)
                    (rest third-coll)
                    (map-seq (fn [coll] (rest coll)) colls)))
     nil)))

(defn- map-transducer [f]
  (fn [rf]
    (fn
      ([] (rf))
      ([result] (rf result))
      ([result input] (rf result (f input))))))

(defn- map-one [f coll]
  (map-seq f (seq coll)))

(defn map
  {:inline (fn
             ([f] (list 'map-transducer f))
             ([f coll] (list '__lg_map f coll))
             ([f first-coll second-coll]
              (list '__lg_map f first-coll second-coll))
             ([f first-coll second-coll third-coll]
              (list '__lg_map f first-coll second-coll third-coll))
             ([f first-coll second-coll third-coll & colls]
              (cons '__lg_map
                    (cons f
                          (cons first-coll
                                (cons second-coll
                                      (cons third-coll colls)))))))}
  ([f]
   (map-transducer f))
  ([f coll]
   (map-one f coll))
  ([f left right]
   (map2-seq f (seq left) (seq right)))
  ([f first-coll second-coll third-coll]
   (map3-seq f
             (seq first-coll)
             (seq second-coll)
             (seq third-coll)))
  ([f first-coll second-coll third-coll & colls]
   (map-many-seq f
                 (seq first-coll)
                 (seq second-coll)
                 (seq third-coll)
                 (map-seq (fn [coll] (seq coll)) (seq colls)))))

(defn- filter-seq [pred coll]
  (lazy-seq
   (if coll
     (let [item (nth coll 0)
           tail (rest coll)]
       (if (pred item)
         (cons item (filter-seq pred tail))
         (filter-seq pred tail)))
     nil)))

(defn filter
  ([pred]
   (fn [rf]
     (fn
       ([] (rf))
       ([result] (rf result))
       ([result input]
        (if (pred input)
          (rf result input)
          (runtime-reduced/continue result))))))
  ([pred coll]
   (filter-seq pred (seq coll))))

(defn- remove-nil-transducer [rf]
  (fn
    ([] (rf))
    ([result] (rf result))
    ([result input]
     (match input
       None
       (runtime-reduced/continue result)
       (Some value)
       (rf result value)))))

(defn remove
  {:inline
   (fn
     ([pred]
      (if (or (= pred 'nil?)
              (= pred 'clojure.core/nil?)
              (= pred 'cljs.core/nil?))
        (list 'fn ['rf] (list 'remove-nil-transducer 'rf))
        (list 'filter (list 'complement pred))))
     ([pred coll]
      (list 'filter (list 'complement pred) coll)))}
  ([pred]
   (filter (complement pred)))
  ([pred coll]
   (filter (complement pred) coll)))

(defn- take-seq [n coll]
  (lazy-seq
   (if (and (pos? n) coll)
     (cons (nth coll 0)
           (take-seq (dec n) (rest coll)))
     nil)))

(defn take
  ([n]
   (fn [rf]
     (let [remaining-count (volatile! n)]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result input]
          (let [remaining @remaining-count
                next-count (vswap! remaining-count dec)
                stepped (if (pos? remaining)
                          (rf result input)
                          (runtime-reduced/continue result))]
            (if (pos? next-count)
              stepped
              (runtime-reduced/stop stepped))))))))
  ([n coll]
   (take-seq n (seq coll))))

(defn- drop-seq [n coll]
  (loop [remaining-count n
         remaining (seq coll)]
    (if (pos? remaining-count)
      (if remaining
        (recur (dec remaining-count) (rest remaining))
        remaining)
      remaining)))

(defn drop
  ([n]
   (fn [rf]
     (let [remaining-count (volatile! n)]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result input]
          (let [remaining @remaining-count]
            (vswap! remaining-count dec)
            (if (pos? remaining)
              (runtime-reduced/continue result)
              (rf result input))))))))
  ([n coll]
   (lazy-seq (drop-seq n coll))))

(defn- take-while-seq [pred coll]
  (lazy-seq
   (if coll
     (let [item (nth coll 0)]
       (if (pred item)
         (cons item (take-while-seq pred (rest coll)))
         nil))
     nil)))

(defn take-while
  ([pred]
   (fn [rf]
     (fn
       ([] (rf))
       ([result] (rf result))
       ([result input]
        (if (pred input)
          (rf result input)
          (reduced result))))))
  ([pred coll]
   (take-while-seq pred (seq coll))))

(defn- drop-while-seq [pred coll]
  (loop [remaining (seq coll)]
    (if remaining
      (if (pred (nth remaining 0))
        (recur (rest remaining))
        remaining)
      remaining)))

(defn drop-while
  ([pred]
   (fn [rf]
     (let [dropping (volatile! true)]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result input]
          (if (and @dropping (pred input))
            (runtime-reduced/continue result)
            (do
              (vreset! dropping false)
              (rf result input))))))))
  ([pred coll]
   (lazy-seq (drop-while-seq pred coll))))

(defn- map-indexed-from [f index coll]
  (lazy-seq
   (let [remaining coll]
     (if remaining
       (cons (f index (nth remaining 0))
             (map-indexed-from f (inc index) (rest remaining)))
       nil))))

(defn map-indexed
  ([f]
   (fn [rf]
     (let [index (volatile! -1)]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result input]
          (rf result (f (vswap! index inc) input)))))))
  ([f coll]
   (map-indexed-from f 0 (seq coll))))

(defn- keep-seq [f coll]
  (lazy-seq
   (if coll
     (if-some [value (f (nth coll 0))]
       (cons value (keep-seq f (rest coll)))
       (keep-seq f (rest coll)))
     nil)))

(defn keep
  ([f]
   (fn [rf]
     (fn
       ([] (rf))
       ([result] (rf result))
       ([result input]
        (if-some [value (f input)]
          (rf result value)
          (runtime-reduced/continue result))))))
  ([f coll]
   (keep-seq f (seq coll))))

(defn- keep-indexed-seq [f index coll]
  (lazy-seq
   (if coll
     (let [value (f index (nth coll 0))]
       (if-some [kept value]
         (cons kept (keep-indexed-seq f (inc index) (rest coll)))
         (keep-indexed-seq f (inc index) (rest coll))))
     nil)))

(defn keep-indexed
  ([f]
   (fn [rf]
     (let [index (volatile! -1)]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result input]
          (let [value (f (vswap! index inc) input)]
            (if-some [kept value]
              (rf result kept)
              (runtime-reduced/continue result))))))))
  ([f coll]
   (keep-indexed-seq f 0 (seq coll))))

(defn- mapcat-seq [f current colls]
  (lazy-seq
   (if current
     (cons (nth current 0)
           (mapcat-seq f (rest current) colls))
     (if colls
       (mapcat-seq f
                   (seq (f (nth colls 0)))
                   (rest colls))
       nil))))

(declare cat)

(defn mapcat
  ([f]
   (fn [rf]
     ((map f) (cat rf))))
  ([f coll]
   (mapcat-seq f (seq []) (seq coll)))
  ([f left right]
   (apply concat (map f left right)))
  ([f first-coll second-coll third-coll]
   (apply concat (map f first-coll second-coll third-coll)))
  ([f first-coll second-coll third-coll & colls]
   (apply concat
          (map-many-seq
           f
           (seq first-coll)
           (seq second-coll)
           (seq third-coll)
           (map-seq (fn [coll] (seq coll)) (seq colls))))))

(defn cat [rf]
  (fn
    ([] (rf))
    ([result] (rf result))
    ([result input]
     (runtime-reduced/continue (__lg_reduce rf result input)))))

(defn halt-when
  ([pred]
   (fn [rf]
     (fn
       ([] (rf))
       ([result] (rf result))
       ([result input]
        (if (pred input)
          (runtime-reduced/halted input)
          (rf result input))))))
  ([pred retf]
   (fn [rf]
     (fn
       ([] (rf))
       ([result] (rf result))
       ([result input]
        (if (pred input)
          (runtime-reduced/halted
           (retf (rf result) input))
          (rf result input)))))))

(defn- conj-reducer-step [result input]
  (__lg_conj result input))

(defn- conj-reducer []
  (fn
    ([] [])
    ([result] result)
    ([result input] (conj-reducer-step result input))))

(defn- transduce*
  ([xform f coll]
   (transduce* xform f (f) coll))
  ([xform f init coll]
   (let [reducing-function
         (fn
           ([] (f))
           ([result] (f result))
           ([result input]
            (runtime-reduced/continue (f result input))))
         transformed (xform reducing-function)
         result (__lg_reduce_transformed transformed init coll)]
     (__lg_complete_transformed transformed result))))

(defn transduce
  {:inline
   (fn
     ([xform f coll]
      (if (or (= f 'conj)
              (= f 'clojure.core/conj)
              (= f 'cljs.core/conj))
        (list 'transduce* xform (list 'conj-reducer) [] coll)
        (list 'transduce* xform f coll)))
     ([xform f init coll]
      (if (or (= f 'conj)
              (= f 'clojure.core/conj)
              (= f 'cljs.core/conj))
        (list 'transduce* xform (list 'conj-reducer) init coll)
        (list 'transduce* xform f init coll))))}
  ([xform f coll]
   (transduce* xform f coll))
  ([xform f init coll]
   (transduce* xform f init coll)))

(defn into
  {:inline (fn [& args]
             (cond
               (= 0 (count args)) []
               (= 1 (count args)) (first args)
               :else (cons '__lg_into args)))}
  ([] [])
  ([to] to)
  ([to from]
   (__lg_reduce (fn [result item] (__lg_conj result item)) to from))
  ([to xform from]
   (into to (sequence xform from))))

(defn- mapv-many-seq
  [f first-coll second-coll third-coll colls result]
  (if (and first-coll
           second-coll
           third-coll
           (every? (fn [coll] (seq coll)) colls))
    (mapv-many-seq
     f
     (rest first-coll)
     (rest second-coll)
     (rest third-coll)
     (map (fn [coll] (rest coll)) colls)
     (__lg_conj result
           (__lg_apply f
                  (nth first-coll 0)
                  (nth second-coll 0)
                  (nth third-coll 0)
                  (map (fn [coll] (nth coll 0)) colls))))
    result))

(defn mapv
  {:inline (fn [& args] (cons '__lg_mapv args))}
  ([f coll]
   (persistent!
    (__lg_reduce (fn [result item] (conj! result (f item)))
                 (transient [])
                 coll)))
  ([f first-coll second-coll]
   (into [] (map f first-coll second-coll)))
  ([f first-coll second-coll third-coll]
   (into [] (map f first-coll second-coll third-coll)))
  ([f first-coll second-coll third-coll & colls]
   (mapv-many-seq f
                  (seq first-coll)
                  (seq second-coll)
                  (seq third-coll)
                  (map (fn [coll] (seq coll)) colls)
                  [])))

(defn- concat-two-seq [current next-seq]
  (lazy-seq
   (if current
     (cons (nth current 0)
           (concat-two-seq (rest current) next-seq))
     (next-seq))))

(defn- concat-many-seq [current colls]
  (lazy-seq
   (if current
     (cons (nth current 0)
           (concat-many-seq (rest current) colls))
     (if colls
       (concat-many-seq (seq (nth colls 0)) (rest colls))
       (seq [])))))

(defn concat
  {:inline (fn [& colls] (cons '__lg_concat colls))}
  ([] (lazy-seq (seq [])))
  ([x] (lazy-seq (seq x)))
  ([x y]
   (lazy-seq
    (concat-two-seq (seq x) (fn [] (seq y)))))
  ([x y & colls]
   (concat-many-seq (concat x y) colls)))

(defmacro list*
  "Creates a list by prepending values to the final argument, which is treated as a sequence."
  [& values]
  (cons '__lg_list-star values))

(defn- interleave-one [coll]
  (lazy-seq (seq coll)))

(defn- interleave-two-seq [left right]
  (lazy-seq
   (if (and left right)
     (cons (nth left 0)
           (cons (nth right 0)
                 (interleave-two-seq (rest left) (rest right))))
     nil)))

(defn- interleave-many-seq [colls]
  (lazy-seq
   (if (and colls (every? (fn [coll] (seq coll)) colls))
     (concat (map-seq (fn [coll] (nth coll 0)) colls)
             (interleave-many-seq
              (map-seq (fn [coll] (rest coll)) colls)))
     nil)))

(defn interleave
  {:inline (fn
             ([] (list '__lg_list))
             ([coll] (list 'interleave-one coll))
             ([left right] (list '__lg_interleave left right))
             ([first-coll second-coll & colls]
              (cons '__lg_interleave
                    (cons first-coll (cons second-coll colls)))))}
  ([] (list))
  ([coll] (interleave-one coll))
  ([left right]
   (interleave-two-seq (seq left) (seq right)))
  ([first-coll second-coll & colls]
   (interleave-many-seq
    (cons (seq first-coll)
          (cons (seq second-coll)
                (map-seq (fn [coll] (seq coll)) (seq colls)))))))

(defn sequence
  ([coll]
   (let [values (seq coll)]
     (if values values (seq []))))
  ([xform coll]
   (__lg_transformer_sequence xform coll)))

(defmacro eduction
  ([xform coll]
   (list '->Eduction xform coll))
  ([xform next-xform & more]
   (let [forms (cons xform (cons next-xform more))]
     (list '->Eduction (cons 'comp (butlast forms)) (last forms)))))

(defn repeatedly
  ([f]
   (lazy-seq
    (cons (f) (repeatedly f))))
  ([n f]
   (take n (repeatedly f))))

(defn repeat
  ([value]
   (lazy-seq
    (cons value (repeat value))))
  ([n value]
   (take n (repeat value))))

(defn- cycle-seq [values remaining]
  (lazy-seq
   (if remaining
     (cons (nth remaining 0)
           (cycle-seq values (rest remaining)))
     (cycle-seq values values))))

(defn cycle [coll]
  (let [values (seq coll)]
    (if values
      (cycle-seq values values)
      (seq []))))

(defn- dorun-seq [coll]
  (loop [remaining coll]
    (if remaining
      (recur (rest remaining))
      nil)))

(defn- dorun-n-seq [n coll]
  (loop [remaining-count n
         remaining coll]
    (if remaining
      (if (pos? remaining-count)
        (recur (dec remaining-count) (rest remaining))
        nil)
      nil)))

(defn dorun
  ([coll]
   (dorun-seq (seq coll)))
  ([n coll]
   (dorun-n-seq n (seq coll))))

(defn doall
  ([coll]
   (dorun coll)
   coll)
  ([n coll]
   (dorun n coll)
   coll))

(defn run! [proc coll]
  (dorun (map proc coll)))

(defn group-by [f coll]
  (__lg_reduce
   (fn [result input]
     (let [key (f input)]
       (assoc result key (__lg_conj (get result key []) input))))
   {}
   coll))

(defn reduce
  {:inline (fn
             ([f coll]
              (list '__lg_reduce f coll))
             ([f init coll]
              (list '__lg_reduce f init coll)))}
  ([f coll]
   (__lg_reduce f coll))
  ([f init coll]
   (__lg_reduce f init coll)))

(defn reductions
  ([f coll]
   (__lg_reductions f coll))
  ([f init coll]
   (__lg_reductions f init coll)))

(defn reduce-kv
  {:inline (fn [f init coll]
             (list '__lg_reduce-kv f init coll))}
  [f init coll]
  (IKVReduce/-kv-reduce coll f init))

(defn- take-nth-seq [n coll]
  (lazy-seq
   (if coll
     (cons (nth coll 0)
           (take-nth-seq n (drop n coll)))
     nil)))

(defn take-nth
  ([n]
   (fn [rf]
     (let [index (volatile! -1)]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result input]
          (if (zero? (rem (vswap! index inc) n))
            (rf result input)
            (runtime-reduced/continue result)))))))
  ([n coll]
   (take-nth-seq n (seq coll))))

#?(:melange
   (defn random-sample
     ([^:option<float> probability]
      (let [probability (match probability
                          (Some value) value
                          None 0.0)]
        (filter (fn [_] (< (rand) probability)))))
     ([^:option<float> probability coll]
      (let [probability (match probability
                          (Some value) value
                          None 0.0)]
        (filter (fn [_] (< (rand) probability)) coll))))
   :default
   (defn random-sample
     ([^:float probability]
      (filter (fn [_] (< (rand) probability))))
     ([^:float probability coll]
      (filter (fn [_] (< (rand) probability)) coll))))

(defn filterv [pred coll]
  (__lg_reduce
   (fn [result input]
     (if (pred input)
       (__lg_conj result input)
       result))
   []
   coll))

(defn- partition-seq [n step coll]
  (lazy-seq
   (if coll
     (let [part (take n coll)]
       (if (= n (count part))
         (cons part (partition-seq n step (drop step coll)))
         nil))
     nil)))

(defn- padded-partition-seq [n step pad coll]
  (lazy-seq
   (if coll
     (let [part (take n coll)]
       (if (= n (count part))
         (cons part (padded-partition-seq n step pad (drop step coll)))
         (seq (list (take n (concat part pad))))))
     nil)))

(defn partition
  ([n coll]
   (partition-seq n n (seq coll)))
  ([n step coll]
   (partition-seq n step (seq coll)))
  ([n step pad coll]
   (padded-partition-seq n step pad (seq coll))))

(defn- partition-all-seq [n step coll]
  (lazy-seq
   (if coll
     (cons (take n coll)
           (partition-all-seq n step (drop step coll)))
     nil)))

(defn partition-all
  ([n]
   (fn [rf]
     (let [buffer (volatile! [])]
       (fn
         ([] (rf))
         ([result]
          (let [pending @buffer]
            (if (empty? pending)
              (rf result)
              (do
                (vreset! buffer [])
                (rf (runtime-reduced/unreduced (rf result pending)))))))
         ([result input]
          (let [pending (__lg_conj @buffer input)]
            (if (= n (count pending))
              (do
                (vreset! buffer [])
                (rf result pending))
              (do
                (vreset! buffer pending)
                (runtime-reduced/continue result)))))))))
  ([n coll]
   (partition-all-seq n n (seq coll)))
  ([n step coll]
   (partition-all-seq n step (seq coll))))

(defn- partitionv-all-seq [n step coll]
  (lazy-seq
   (if coll
     (cons (vec (take n coll))
           (partitionv-all-seq n step (drop step coll)))
     nil)))

(defn partitionv-all
  ([n]
   (partition-all n))
  ([n coll]
   (partitionv-all n n coll))
  ([n step coll]
   (partitionv-all-seq n step (seq coll))))

(defn- partition-by-run [f expected coll]
  (lazy-seq
   (if coll
     (let [value (nth coll 0)]
       (if (= expected (f value))
         (cons value (partition-by-run f expected (next coll)))
         nil))
     nil)))

(defn- partition-by-seq [f coll]
  (lazy-seq
   (when-let [values (seq coll)]
     (let [fst (nth values 0)
           fv (f fst)
           run (cons fst
                     (partition-by-run f fv (next values)))]
       (cons run
             (partition-by-seq
              f
              (lazy-seq (drop (count run) values))))))))

(defn partition-by
  ([f]
   (fn [rf]
     (let [buffer (volatile! [])
           keys (volatile! [])]
       (fn
         ([] (rf))
         ([result]
          (let [pending @buffer]
            (if (empty? pending)
              (rf result)
              (do
                (vreset! buffer [])
                (vreset! keys [])
                (rf (runtime-reduced/unreduced (rf result pending)))))))
         ([result input]
          (let [key (f input)
                pending @buffer]
            (if (or (empty? pending)
                    (= key (nth @keys 0)))
              (do
                (vreset! buffer (__lg_conj pending input))
                (vreset! keys [key])
                (runtime-reduced/continue result))
              (do
                (vreset! buffer [])
                (vreset! keys [])
                (let [ret (rf result pending)]
                  (if (reduced? ret)
                    ret
                    (do
                      (vreset! buffer [input])
                      (vreset! keys [key])
                      ret)))))))))))
  ([f coll]
   (partition-by-seq f coll)))

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

(defn complement
  {:inline (fn [f] (list '__lg_complement f))}
  [f]
  (fn [x]
    (not (f x))))

(defn constantly
  {:inline (fn [x] (list '__lg_constantly x))}
  [x]
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
  {:inline (fn [x] (list 'satisfies? 'ISet x))}
  [x]
  (if (nil? x) false (satisfies? ISet x)))

(defn map?
  {:inline (fn [x] (list 'satisfies? 'IMap x))}
  [x]
  (if (nil? x)
    false
    (satisfies? IMap x)))

(defmacro implements? [protocol value]
  (list 'satisfies? protocol value))

(defn record?
  {:inline (fn [x] (list 'satisfies? 'IRecord x))}
  [x]
  (satisfies? IRecord x))

(defn tagged-literal?
  {:inline (fn [value] (list 'satisfies? 'ITaggedLiteral value))}
  [value]
  (satisfies? ITaggedLiteral value))

(defn tagged-literal [tag form]
  (record TaggedLiteral
          (tag tag)
          (form form)
          (form-hash (hash form))))

(defn chunked-seq?
  {:inline (fn [x] (list 'implements? 'IChunkedSeq x))}
  [x]
  (implements? IChunkedSeq x))

(defn fn?
  {:inline (fn [x] (list '__lg_fn-predicate x))}
  [x]
  (__lg_fn-predicate x))

(defn ifn?
  {:inline (fn [value] (list '__lg_ifn-predicate value))}
  [value]
  (__lg_ifn-predicate value))

(defn coll?
  {:inline (fn [x] (list 'satisfies? 'ICollection x))}
  [x]
  (if (nil? x) false (satisfies? ICollection x)))

(defn associative?
  {:inline (fn [x] (list 'satisfies? 'IAssociative x))}
  [x]
  (satisfies? IAssociative x))

(defn indexed?
  [x]
  (satisfies? IIndexed x))

(defn ifind?
  {:inline (fn [x] (list 'satisfies? 'IFind x))}
  [x]
  (satisfies? IFind x))

(defn map-entry?
  {:inline (fn [x] (list 'satisfies? 'IMapEntry x))}
  [x]
  (satisfies? IMapEntry x))

(defn regexp?
  {:inline (fn [x] (list '__lg_regex-predicate x))}
  [x]
  (__lg_regex-predicate x))

(defn volatile?
  {:inline (fn [x] (list 'satisfies? 'IVolatile x))}
  [x]
  (satisfies? IVolatile x))

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
  {:inline (fn [x] (list 'satisfies? 'ISequential x))}
  [x]
  (satisfies? ISequential x))

(defn reversible?
  {:inline (fn [x] (list 'satisfies? 'IReversible x))}
  [x]
  (satisfies? IReversible x))

(defn sorted?
  {:inline (fn [x] (list 'satisfies? 'ISorted x))}
  [x]
  (satisfies? ISorted x))

(defn reduceable?
  {:inline (fn [x] (list 'satisfies? 'IReduce x))}
  [x]
  (satisfies? IReduce x))

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

(defmacro divide
  ([x]
   `(let [x# ~x]
      (/ 1 x#)))
  ([x y]
   `(let [x# ~x
          y# ~y]
      (/ x# y#)))
  ([x y & more]
   `(let [x# ~x
          y# ~y]
      (divide (/ x# y#) ~@more))))

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

(defn bigdec
  {:inline (fn [x] (list '__lg_bigdec x))}
  [^:float x]
  (__lg_bigdec x))

(defn bigint
  {:inline (fn [x] (list '__lg_bigint x))}
  [^:int x]
  (__lg_bigint x))

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

(defn ensure-reduced
  {:inline (fn [x] (list '__lg_ensure-reduced x))}
  [x]
  (__lg_ensure-reduced x))

(defn uuid?
  {:inline (fn [x] (list '__lg_uuid-predicate x))}
  [x]
  (__lg_uuid-predicate x))

(defn delay?
  {:inline (fn [x] (list '__lg_delay-predicate x))}
  [x]
  (__lg_delay-predicate x))

(defn force
  {:inline (fn [x] (list '__lg_force x))}
  [x]
  (__lg_force x))

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
  (__lg_nil-predicate (__lg_namespace x)))

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
  (not (__lg_nil-predicate (__lg_namespace x))))

(defn simple-keyword?
  {:inline
   (fn [x]
     (let [value (gensym)]
       (list 'let [value x]
             (list 'if (list '__lg_keyword-predicate value)
                   (list '__lg_nil-predicate (list 'namespace value))
                   false))))}
  [x]
  (__lg_nil-predicate (__lg_namespace x)))

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
  (not (__lg_nil-predicate (__lg_namespace x))))

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
   (fn [coll] (list '__lg_empty-predicate coll))}
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

(defn empty
  {:inline (fn [coll]
             (list 'IEmptyableCollection/-empty coll))}
  [coll]
  (IEmptyableCollection/-empty coll))

(defn peek
  {:inline (fn [coll]
             (list 'IStack/-peek coll))}
  [coll]
  (IStack/-peek coll))

(defn pop
  {:inline (fn [coll]
             (list 'IStack/-pop coll))}
  [coll]
  (IStack/-pop coll))

(defn disj
  {:inline
   (fn [coll & keys]
     (let [result (gensym)]
       (list
        'let [result coll]
        (loop [expression result
               remaining keys]
          (if (nil? remaining)
            expression
            (recur (list 'ISet/-disjoin expression (first remaining))
                   (next remaining)))))))}
  ([coll] coll)
  ([coll key]
   (ISet/-disjoin coll key))
  ([coll key & keys]
   (loop [result (ISet/-disjoin coll key)
          remaining keys]
     (if (seq remaining)
       (recur (ISet/-disjoin result (first remaining))
              (rest remaining))
       result))))

(defn count
  {:inline (fn [coll]
             (list '__lg_count coll))}
  [coll]
  (ICounted/-count coll))

(defn nth
  {:inline
   (fn [coll index & not-found]
     (if (nil? not-found)
       (list '__lg_nth coll index)
       (list '__lg_nth coll index (first not-found))))}
  ([coll index]
   (IIndexed/-nth coll index))
  ([coll index not-found]
   (IIndexed/-nth coll index not-found)))

(defn ex-message [ex]
  (__lg_ex-message ex))

(defn ex-cause [ex]
  (__lg_ex-cause ex))

(defn ex-data [ex]
  (__lg_ex-data ex))

(defn *exec-tap-fn*
  {:inline (fn [f] (list '__lg_exec-tap-fn f))}
  [f]
  (__lg_exec-tap-fn f))

(defn add-tap
  {:inline (fn [f] (list 'do (list '__lg_add-tap f) nil))}
  [f]
  (__lg_add-tap f)
  nil)

(defn remove-tap
  {:inline (fn [f] (list 'do (list '__lg_remove-tap f) nil))}
  [f]
  (__lg_remove-tap f)
  nil)

(defn tap>
  {:inline (fn [x] (list '__lg_tap x))}
  [x]
  (__lg_tap x))

(defmacro defmulti
  [& _forms])

(defmacro defmethod
  [& _forms])

(defmacro methods
  [multifn]
  (list '__lg_multimethod-methods multifn))

(defmacro get-method
  [multifn dispatch-value]
  (list '__lg_multimethod-get-method multifn dispatch-value))

(defmacro dispatch-fn
  [multifn]
  (list '__lg_multimethod-dispatch-fn multifn))

(defmacro remove-method
  [multifn dispatch-value]
  (list '__lg_multimethod-remove-method multifn dispatch-value))

(defmacro remove-all-methods
  [multifn]
  (list '__lg_multimethod-remove-all-methods multifn))

(defmacro default-dispatch-val
  [multifn]
  (list '__lg_multimethod-default-dispatch-val multifn))

(defmacro prefer-method
  [multifn preferred other]
  (list '__lg_multimethod-prefer-method multifn preferred other))

(defmacro prefers
  [multifn]
  (list '__lg_multimethod-prefers multifn))

(defn re-pattern
  {:inline (fn [expression] (list '__lg_re-pattern expression))}
  [expression]
  (__lg_re-pattern expression))

(defn re-matcher
  {:inline (fn [expression source] (list '__lg_re-matcher expression source))}
  [^:regex expression ^:string source]
  (__lg_re-matcher expression source))

(defn re-find
  {:inline (fn
             ([matcher] (list '__lg_re-find matcher))
             ([expression source] (list '__lg_re-find expression source)))}
  ([^:Lg_runtime.Runtime_string.regex_matcher matcher]
   (__lg_re-find matcher))
  ([^:regex expression ^:string source]
   (__lg_re-find expression source)))

(defn re-matches
  {:inline (fn [expression source] (list '__lg_re-matches expression source))}
  [expression source]
  (__lg_re-matches expression source))

(defn re-seq
  {:inline (fn [expression source] (list '__lg_re-seq expression source))}
  [expression source]
  (__lg_re-seq expression source))

(defn +
  {:inline (fn [& values] (cons '__lg_add values))}
  ([] (__lg_add))
  ([x] x)
  ([x y] (__lg_add x y))
  ([x y & more] (__lg_reduce + (__lg_add x y) more)))

(defn -
  {:inline (fn [& values] (cons '__lg_subtract values))}
  ([x] (__lg_subtract x))
  ([x y] (__lg_subtract x y))
  ([x y & more] (__lg_reduce - (__lg_subtract x y) more)))

(defn *
  {:inline (fn [& values] (cons '__lg_multiply values))}
  ([] (__lg_multiply))
  ([x] x)
  ([x y] (__lg_multiply x y))
  ([x y & more] (__lg_reduce * (__lg_multiply x y) more)))

(defn /
  {:inline (fn [& values] (cons '__lg_divide values))}
  ([x] (__lg_divide-int 1 x))
  ([x y] (__lg_divide-int x y))
  ([x y & more] (__lg_reduce / (__lg_divide-int x y) more)))

(defn- less-chain [x y more]
  (if (__lg_less x y)
    (if (next more)
      (recur y (first more) (next more))
      (__lg_less y (first more)))
    false))

(defn <
  {:inline (fn [& values] (cons '__lg_less values))}
  ([x] true)
  ([x y] (__lg_less x y))
  ([x y & more] (less-chain x y more)))

(defn- less-equal-chain [x y more]
  (if (__lg_less-equal x y)
    (if (next more)
      (recur y (first more) (next more))
      (__lg_less-equal y (first more)))
    false))

(defn <=
  {:inline (fn [& values] (cons '__lg_less-equal values))}
  ([x] true)
  ([x y] (__lg_less-equal x y))
  ([x y & more] (less-equal-chain x y more)))

(defn- greater-chain [x y more]
  (if (__lg_greater x y)
    (if (next more)
      (recur y (first more) (next more))
      (__lg_greater y (first more)))
    false))

(defn >
  {:inline (fn [& values] (cons '__lg_greater values))}
  ([x] true)
  ([x y] (__lg_greater x y))
  ([x y & more] (greater-chain x y more)))

(defn- greater-equal-chain [x y more]
  (if (__lg_greater-equal x y)
    (if (next more)
      (recur y (first more) (next more))
      (__lg_greater-equal y (first more)))
    false))

(defn >=
  {:inline (fn [& values] (cons '__lg_greater-equal values))}
  ([x] true)
  ([x y] (__lg_greater-equal x y))
  ([x y & more] (greater-equal-chain x y more)))

(defn- numeric-equal-chain [x y more]
  (if (__lg_numeric-equal x y)
    (if (next more)
      (recur y (first more) (next more))
      (__lg_numeric-equal y (first more)))
    false))

(defn- equal-chain [x y more]
  (if (__lg_equal x y)
    (if (next more)
      (recur y (first more) (next more))
      (__lg_equal y (first more)))
    false))

(defn =
  {:inline (fn [& values] (cons '__lg_equal values))}
  ([x] true)
  ([x y] (__lg_equal x y))
  ([x y & more] (equal-chain x y more)))

(defn ==
  {:inline (fn [& values] (cons '__lg_numeric-equal values))}
  ([x] true)
  ([x y] (__lg_numeric-equal x y))
  ([x y & more] (numeric-equal-chain x y more)))

(defn inc
  {:inline (fn [x] (list '__lg_add x 1))}
  [x]
  (+ x 1))

(defn dec
  {:inline (fn [x] (list '__lg_dec x))}
  [x]
  (- x 1))

(defn inc' [x]
  (inc x))

(defn dec' [x]
  (dec x))

(defn- bit-and-two [x y]
  (runtime-int/bit-and x y))

(defn unsafe-bit-and [x y]
  (bit-and-two x y))

(defn bit-and
  ([x y]
   (bit-and-two x y))
  ([x y & more]
   (__lg_reduce bit-and-two (bit-and-two x y) more)))

(defn- bit-or-two [x y]
  (runtime-int/bit-or x y))

(defn bit-or
  ([x y]
   (bit-or-two x y))
  ([x y & more]
   (__lg_reduce bit-or-two (bit-or-two x y) more)))

(defn- bit-xor-two [x y]
  (runtime-int/bit-xor x y))

(defn bit-xor
  ([x y]
   (bit-xor-two x y))
  ([x y & more]
   (__lg_reduce bit-xor-two (bit-xor-two x y) more)))

(defn bit-shift-left [x n]
  (runtime-int/shift-left x n))

(defn bit-shift-right [x n]
  (runtime-int/shift-right x n))

(defn bit-not [x]
  (bit-xor-two x -1))

(defn- int32-value [value]
  #?(:melange (runtime-int-melange/int32 value)
     :default (runtime-int/int32 value)))

(defn int-rotate-left [x n]
  (int32-value
   (bit-or
    (runtime-int/shift-left-32 x n)
    (runtime-int/logical-shift-right-32 x (- n)))))

(defn imul [a b]
  (let [ah (bit-and (runtime-int/logical-shift-right-32 a 16) 0xffff)
        al (bit-and a 0xffff)
        bh (bit-and (runtime-int/logical-shift-right-32 b 16) 0xffff)
        bl (bit-and b 0xffff)]
    (int32-value
     (+ (* al bl)
        (runtime-int/logical-shift-right-32
         (runtime-int/shift-left-32 (+ (* ah bl) (* al bh)) 16)
         0)))))

(def m3-seed 0)
(def m3-C1 (int32-value 0xcc9e2d51))
(def m3-C2 (int32-value 0x1b873593))

(defn m3-mix-K1 [k1]
  (-> (int32-value k1)
      (imul m3-C1)
      (int-rotate-left 15)
      (imul m3-C2)))

(defn m3-mix-H1 [h1 k1]
  (int32-value
   (-> (int32-value h1)
       (bit-xor (int32-value k1))
       (int-rotate-left 13)
       (imul 5)
       (+ (int32-value 0xe6546b64)))))

(defn m3-fmix [h1 len]
  (as-> (int32-value h1) h1
    (bit-xor h1 len)
    (bit-xor h1 (runtime-int/logical-shift-right-32 h1 16))
    (imul h1 (int32-value 0x85ebca6b))
    (bit-xor h1 (runtime-int/logical-shift-right-32 h1 13))
    (imul h1 (int32-value 0xc2b2ae35))
    (int32-value
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

(defn hash-double [value]
  (hash value))

(defn hash-keyword [value]
  (hash value))

(defn hash-string [value]
  (hash-string* value))

(defn add-to-string-hash-cache [value]
  (hash-string* value))

(defn mix-collection-hash [hash-basis count]
  (let [h1 m3-seed
        k1 (m3-mix-K1 hash-basis)
        h1 (m3-mix-H1 h1 k1)]
    (m3-fmix h1 count)))

(defn hash-ordered-coll [coll]
  (loop [n 0 hash-code 1 coll (seq coll)]
    (if-not (nil? coll)
      (recur (inc n)
             (bit-or (+ (imul 31 hash-code) (hash (first coll))) 0)
             (next coll))
      (mix-collection-hash hash-code n))))

(defn hash-unordered-coll [coll]
  (loop [n 0 hash-code 0 coll (seq coll)]
    (if-not (nil? coll)
      (recur (inc n)
             (bit-or (+ hash-code (hash (first coll))) 0)
             (next coll))
      (mix-collection-hash hash-code n))))

(defn reduced [x]
  (runtime-reduced/reduced x))

(defn unreduced
  {:inline (fn [value] (list '__lg_unreduced value))}
  [value]
  (__lg_unreduced value))

(defn deref
  {:inline
   (fn [reference]
     (if (and (seq? reference)
              (or (= 'var (first reference))
                  (= '__lg-var-quote (first reference))))
       (second reference)
       (list 'IDeref/-deref reference)))}
  [reference]
  (IDeref/-deref reference))

(defn reset!
  {:inline (fn [reference value]
             (list 'IReset/-reset! reference value))}
  [reference value]
  (IReset/-reset! reference value))

(defn vreset!
  {:inline (fn [reference value]
             (list 'IVolatile/-vreset! reference value))}
  [reference value]
  (IVolatile/-vreset! reference value))

(defn -notify-watches
  [reference old-value new-value]
  (IWatchable/-notify-watches reference old-value new-value))

(defn -add-watch
  [reference key callback]
  (IWatchable/-add-watch reference key callback))

(defn -remove-watch
  [reference key]
  (IWatchable/-remove-watch reference key))

(defn add-watch
  [reference key callback]
  (-add-watch reference key callback)
  reference)

(defn remove-watch
  [reference key]
  (-remove-watch reference key)
  reference)

(defn get-validator
  {:inline (fn [reference] (list '__lg_get-validator reference))}
  [reference]
  (__lg_get-validator reference))

(defn set-validator!
  {:inline (fn [reference validator]
             (list 'do (list '__lg_set-validator! reference validator) nil))}
  [reference validator]
  (__lg_set-validator! reference validator)
  nil)

(defn reset-meta!
  {:inline (fn [reference metadata]
             (list '__lg_reset-meta! reference metadata))}
  [reference metadata]
  (__lg_reset-meta! reference metadata))

(defn- alter-meta-apply [update-fn metadata a b c d args]
  (__lg_apply update-fn metadata a b c d args))

(defn alter-meta!
  ([reference update-fn]
   (reset-meta! reference (update-fn (IMeta/-meta reference))))
  ([reference update-fn a]
   (reset-meta! reference (update-fn (IMeta/-meta reference) a)))
  ([reference update-fn a b]
   (reset-meta! reference (update-fn (IMeta/-meta reference) a b)))
  ([reference update-fn a b c]
   (reset-meta! reference (update-fn (IMeta/-meta reference) a b c)))
  ([reference update-fn a b c d]
   (reset-meta! reference (update-fn (IMeta/-meta reference) a b c d)))
  ([reference update-fn a b c d & args]
   (reset-meta!
    reference
    (alter-meta-apply update-fn (IMeta/-meta reference) a b c d args))))

(defmacro vswap! [reference update-fn & args]
  `(IVolatile/-vreset!
    ~reference
    (~update-fn (IDeref/-deref ~reference) ~@args)))

(defn- swap-apply [update-fn value x y more]
  (__lg_apply update-fn value x y more))

(defn swap!
  {:inline
   (fn [reference update-fn & args]
     (cons '__lg_swap!
           (cons reference (cons update-fn args))))}
  ([reference update-fn]
   (ISwap/-swap! reference update-fn))
  ([reference update-fn x]
   (ISwap/-swap! reference (fn [value] (update-fn value x))))
  ([reference update-fn x y]
   (ISwap/-swap! reference (fn [value] (update-fn value x y))))
  ([reference update-fn x y & more]
   (ISwap/-swap!
    reference
    (fn [value] (swap-apply update-fn value x y more)))))

(defn- swap-vals-with [reference update-fn]
  (let [old-value (IDeref/-deref reference)]
    [old-value (ISwap/-swap! reference update-fn)]))

(defn swap-vals!
  {:inline
   (fn [reference update-fn & args]
     (let [target (gensym)
           old-value (gensym)
           new-value (gensym)]
       (list
        'let [target reference
              old-value (list 'IDeref/-deref target)
              new-value
              (cons '__lg_swap!
                    (cons target (cons update-fn args)))]
        [old-value new-value])))}
  ([reference update-fn]
   (swap-vals-with reference update-fn))
  ([reference update-fn x]
   (swap-vals-with reference (fn [value] (update-fn value x))))
  ([reference update-fn x y]
   (swap-vals-with reference (fn [value] (update-fn value x y))))
  ([reference update-fn x y & more]
   (swap-vals-with
    reference
    (fn [value] (swap-apply update-fn value x y more)))))

(defn compare-and-set!
  {:inline
   (fn [reference old-value new-value]
     (let [target (gensym)
           expected (gensym)
           replacement (gensym)]
       (list
        'let [target reference
              expected old-value
              replacement new-value]
        (list 'if
              (list '= (list 'IDeref/-deref target) expected)
              (list 'do
                    (list 'IReset/-reset! target replacement)
                    true)
              false))))}
  [reference old-value new-value]
  (if (= (deref reference) old-value)
    (do (reset! reference new-value) true)
    false))

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

(defn NaN?
  {:inline (fn [value] (list '__lg_nan-predicate value))}
  [value]
  (js/isNaN value))

;; ClojureScript numbers share one JavaScript representation. LG keeps int and
;; float distinct, so first-class extrema use a private static protocol while
;; direct calls inline to a typed primitive that can also widen mixed inputs.
(defprotocol ^:private INumericExtrema
  (-max-two [x y] :self)
  (-min-two [x y] :self))

(extend-type :int
  INumericExtrema
  (-max-two [x y] (if (> x y) x y))
  (-min-two [x y] (if (< x y) x y)))

(extend-type :float
  INumericExtrema
  (-max-two [x y]
    (cond
      (js/isNaN x) x
      (js/isNaN y) y
      (> x y) x
      :else y))
  (-min-two [x y]
    (cond
      (js/isNaN x) x
      (js/isNaN y) y
      (< x y) x
      :else y)))

(defn max
  {:inline (fn [& args] (cons '__lg_max args))}
  ([x] x)
  ([x y] (INumericExtrema/-max-two x y))
  ([x y & more]
   (__lg_reduce (fn [best item] (INumericExtrema/-max-two best item))
                (INumericExtrema/-max-two x y)
                more)))

(defn min
  {:inline (fn [& args] (cons '__lg_min args))}
  ([x] x)
  ([x y] (INumericExtrema/-min-two x y))
  ([x y & more]
   (__lg_reduce (fn [best item] (INumericExtrema/-min-two best item))
                (INumericExtrema/-min-two x y)
                more)))

(defn infinite? [value]
  (or (= value ##Inf)
      (= value ##-Inf)))

(defn flush []
  nil)

(defn any? [x]
  (runtime-static-value/consume x)
  true)

(defn ratio?
  {:inline (fn [x] (list '__lg_ratio-predicate x))}
  [x]
  (__lg_ratio-predicate x))

(defn decimal?
  {:inline (fn [x] (list '__lg_decimal-predicate x))}
  [x]
  (__lg_decimal-predicate x))

(defn realized?
  {:inline (fn [value] (list 'IPending/-realized? value))}
  [value]
  (IPending/-realized? value))

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

(defn prim-seq
  ([values]
   (prim-seq values 0))
  ([values index]
   (array-seq values index)))

(defn to-array [coll]
  (runtime-array/of-seq (seq coll)))

(defn to-array-2d [coll]
  (to-array (map (fn [values] (to-array values)) coll)))

(defn into-array [coll]
  (to-array coll))

(defmacro array-values [value & values]
  `(array ~value ~@values))

(defn array-from [coll]
  (to-array coll))

(defn- array-chunk-value [values offset end]
  (record ArrayChunk
          (chunk-values values)
          (chunk-offset offset)
          (chunk-end end)))

(defn- array-chunk-count [chunk]
  (- (:chunk-end chunk) (:chunk-offset chunk)))

(extend-type ArrayChunk
  ICounted
  (-count [chunk]
    (array-chunk-count chunk))
  IIndexed
  (-nth [chunk index]
    (aget (:chunk-values chunk) (+ (:chunk-offset chunk) index)))
  (-nth [chunk index not-found]
    (if (and (>= index 0)
             (< index (array-chunk-count chunk)))
      (aget (:chunk-values chunk) (+ (:chunk-offset chunk) index))
      not-found))
  IChunk
  (-drop-first [chunk]
    (if (= (:chunk-offset chunk) (:chunk-end chunk))
      (raise (Invalid_argument "-drop-first of empty chunk"))
      (array-chunk-value
       (:chunk-values chunk)
       (+ (:chunk-offset chunk) 1)
       (:chunk-end chunk))))
  IReduce
  (-reduce [chunk reducer initial]
    (loop [result initial
           index (:chunk-offset chunk)]
      (if (< index (:chunk-end chunk))
        (recur (reducer result (aget (:chunk-values chunk) index))
               (+ index 1))
        result))))

(defn array-chunk
  "Creates an array-backed chunk over `values` between `offset` and `end`."
  ([values]
   (array-chunk-value values 0 (alength values)))
  ([values offset]
   (array-chunk-value values offset (alength values)))
  ([values offset end]
   (array-chunk-value values offset end)))

(defn chunk-buffer
  "Creates a typed mutable chunk buffer with `capacity` slots."
  [capacity]
  (runtime-chunk-buffer/create capacity))

(defn chunk-append
  "Appends `value` to `buffer`."
  [buffer value]
  (runtime-chunk-buffer/append buffer value))

(defn chunk
  "Returns the populated values of `buffer` as an array-backed chunk."
  [buffer]
  (array-chunk (runtime-chunk-buffer/to-array buffer)))

(defn- array-chunk-to-seq [chunk index]
  (lazy-seq
   (if (< index (array-chunk-count chunk))
     (cons (aget (:chunk-values chunk) (+ (:chunk-offset chunk) index))
           (array-chunk-to-seq chunk (inc index)))
     nil)))

(defn- chunked-cons-seq [value]
  (match (:chunked-cons-chunk value)
    None (:chunked-cons-more value)
    (Some chunk)
    (concat
     (array-chunk-to-seq chunk 0)
     (:chunked-cons-more value))))

(defn- chunked-cons-with-meta [value metadata]
  (record chunked-cons-value
          (chunked-cons-chunk (:chunked-cons-chunk value))
          (chunked-cons-more (:chunked-cons-more value))
          (chunked-cons-metadata metadata)))

(defn- chunked-cons-first-value [value]
  (match (:chunked-cons-chunk value)
    None
    (raise (Invalid_argument "chunk-first of a non-chunked rest"))
    (Some chunk) chunk))

(defn- chunked-cons-rest-value [value]
  (match (:chunked-cons-chunk value)
    None
    (raise (Invalid_argument "chunk-rest of a non-chunked rest"))
    (Some _) (:chunked-cons-more value)))

(defn- chunked-cons-next-value [value]
  (match (:chunked-cons-chunk value)
    None
    (raise (Invalid_argument "chunk-next of a non-chunked rest"))
    (Some _) (:chunked-cons-more value)))

(extend-type chunked-cons-value
  ISeqable
  (-seq [value]
    (chunked-cons-seq value))
  IMeta
  (-meta [value]
    (:chunked-cons-metadata value))
  IWithMeta
  (-with-meta [value metadata]
    (chunked-cons-with-meta value metadata))
  IChunkedSeq
  (-chunked-first [value]
    (chunked-cons-first-value value))
  (-chunked-rest [value]
    (chunked-cons-rest-value value))
  IChunkedNext
  (-chunked-next [value]
    (chunked-cons-next-value value)))

(defn chunk-cons
  "Prepends `chunk` to `rest` as a statically typed chunked sequence."
  [chunk rest]
  (let [remaining (seq rest)
        metadata (meta {})]
    (record chunked-cons-value
            (chunked-cons-chunk
             (if (zero? (array-chunk-count chunk)) None (Some chunk)))
            (chunked-cons-more remaining)
            (chunked-cons-metadata metadata))))

(defn chunk-first
  "Returns the first array-backed chunk of `sequence`."
  [sequence]
  (chunked-cons-first-value sequence))

(defn chunk-rest
  "Returns the sequence following the first chunk of `sequence`."
  [sequence]
  (chunked-cons-rest-value sequence))

(defn chunk-next
  "Returns the next sequence following the first chunk of `sequence`."
  [sequence]
  (chunked-cons-next-value sequence))

(defn set-from-indexed-seq [indexed-seq]
  (set indexed-seq))

(defn munge-str [source]
  (runtime-string/munge-str source))

(defn array-index-of [values key]
  (let [length (alength values)]
    (loop [index 0]
      (cond
        (<= length index) -1
        (= key (aget values index)) index
        :else (recur (+ index 2))))))

(defn array-binary-search-left [compare values right key]
  (loop [left 0
         right right]
    (if (> left right)
      (double left)
      (let [middle (+ left (quot (- right left) 2))]
        (if (< (compare (aget values middle) key) 0)
          (recur (inc middle) right)
          (recur left (dec middle)))))))

(defn array-binary-search-right [compare values right key]
  (loop [left 0
         right right]
    (if (> left right)
      (double left)
      (let [middle (+ left (quot (- right left) 2))]
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

(defmacro areduce [values index result init expression]
  `(let [values# ~values
         length# (alength values#)]
     (loop [~index 0
            ~result ~init]
       (if (< ~index length#)
         (recur (inc ~index) ~expression)
         ~result))))

(defmacro locking [_lock & forms]
  `(do nil ~@forms))

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

(defn rand
  {:inline (fn [& args] (cons '__lg_rand args))}
  ([] (__lg_rand))
  ([n] (__lg_rand n)))

(defn rand-nth
  {:inline (fn [coll]
             (if (nil? coll)
               nil
               (list 'nth coll (list 'rand-int (list 'count coll)))))}
  [coll]
  (let [values (seq coll)]
    (if values
      (nth values (rand-int (count values)))
      nil)))

(defn uuid
  "Returns a UUID consistent with string `source`."
  [source]
  (runtime-uuid/of-string source))

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

(defn weak-deref
  {:inline (fn [reference]
             (list '__lg_weak-deref reference))}
  [reference]
  (runtime-weak/get reference))

(defn weak-clear!
  {:inline (fn [reference]
             (list '__lg_weak-clear! reference))}
  [reference]
  (runtime-weak/clear reference))

(defn future-call [f]
  (runtime-future/call f))

(defn enable-console-print! []
  nil)

(def gensym_counter (atom 0))

(defn gensym
  ([]
   (gensym "G__"))
  ([prefix-string]
   (symbol (str prefix-string (swap! gensym_counter inc)))))

(defn bit-shift-right-zero-fill [x n]
  #?(:melange (runtime-int-melange/logical-shift-right x n)
     :default (runtime-int/logical-shift-right x n)))

(defn- bit-and-not-two [x y]
  (bit-and x (bit-not y)))

(defn bit-and-not
  "Returns the bitwise intersection of `x` with the complements of the remaining arguments."
  ([x y]
   (bit-and-not-two x y))
  ([x y & more]
   (__lg_reduce bit-and-not-two (bit-and-not-two x y) more)))

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

(defn some
  {:inline (fn [pred coll] (list '__lg_some pred coll))}
  [pred coll]
  (loop [remaining (seq coll)]
    (if remaining
      (let [result (pred (first remaining))]
        (if result
          result
          (recur (next remaining))))
      nil)))

(defn every-pred
  ([p]
   (fn
     ([] true)
     ([x] (every? p (list x)))
     ([x y] (every? p (list x y)))
     ([x y z] (every? p (list x y z)))
     ([x y z & args]
      (and (every? p (list x y z)) (every? p args)))))
  ([p1 p2]
   (fn
     ([] true)
     ([x] (and (every? p1 (list x)) (every? p2 (list x))))
     ([x y]
      (and (every? p1 (list x y)) (every? p2 (list x y))))
     ([x y z]
      (and (every? p1 (list x y z))
           (every? p2 (list x y z))))
     ([x y z & args]
      (and (every? p1 (list x y z))
           (every? p2 (list x y z))
           (every? (fn [value] (and (p1 value) (p2 value))) args)))))
  ([p1 p2 p3]
   (fn
     ([] true)
     ([x]
      (and (every? p1 (list x))
           (every? p2 (list x))
           (every? p3 (list x))))
     ([x y]
      (and (every? p1 (list x y))
           (every? p2 (list x y))
           (every? p3 (list x y))))
     ([x y z]
      (and (every? p1 (list x y z))
           (every? p2 (list x y z))
           (every? p3 (list x y z))))
     ([x y z & args]
      (and (every? p1 (list x y z))
           (every? p2 (list x y z))
           (every? p3 (list x y z))
           (every? (fn [value]
                     (and (p1 value) (p2 value) (p3 value)))
                   args)))))
  ([p1 p2 p3 & ps]
   (let [predicates (list* p1 p2 p3 ps)]
     (fn
       ([] true)
       ([x]
        (loop [remaining (seq predicates)]
          (if remaining
            (if ((nth remaining 0) x)
              (recur (next remaining))
              false)
            true)))
       ([x y]
        (loop [remaining (seq predicates)]
          (if remaining
            (let [predicate (nth remaining 0)]
              (if (every? predicate (list x y))
                (recur (next remaining))
                false))
            true)))
       ([x y z]
        (loop [remaining (seq predicates)]
          (if remaining
            (let [predicate (nth remaining 0)]
              (if (every? predicate (list x y z))
                (recur (next remaining))
                false))
            true)))
       ([x y z & args]
        (and
          (loop [remaining (seq predicates)]
            (if remaining
              (let [predicate (nth remaining 0)]
                (if (every? predicate (list x y z))
                  (recur (next remaining))
                  false))
              true))
          (loop [remaining (seq predicates)]
            (if remaining
              (if (every? (nth remaining 0) args)
                (recur (next remaining))
                false)
              true))))))))

(defn some-fn
  ([p]
   (fn
     ([] nil)
     ([x]
      (let [result (p x)] result))
     ([x y] (or (p x) (p y)))
     ([x y z] (or (p x) (p y) (p z)))
     ([x y z & args]
      (or (p x) (p y) (p z) (some p args)))))
  ([p1 p2]
   (fn
     ([] nil)
     ([x] (or (p1 x) (p2 x)))
     ([x y] (or (p1 x) (p1 y) (p2 x) (p2 y)))
     ([x y z]
      (or (p1 x) (p1 y) (p1 z)
          (p2 x) (p2 y) (p2 z)))
     ([x y z & args]
      (or (p1 x) (p1 y) (p1 z)
          (p2 x) (p2 y) (p2 z)
          (some (fn [value] (or (p1 value) (p2 value))) args)))))
  ([p1 p2 p3]
   (fn
     ([] nil)
     ([x] (or (p1 x) (p2 x) (p3 x)))
     ([x y]
      (or (p1 x) (p1 y)
          (p2 x) (p2 y)
          (p3 x) (p3 y)))
     ([x y z]
      (or (p1 x) (p1 y) (p1 z)
          (p2 x) (p2 y) (p2 z)
          (p3 x) (p3 y) (p3 z)))
     ([x y z & args]
      (or (p1 x) (p1 y) (p1 z)
          (p2 x) (p2 y) (p2 z)
          (p3 x) (p3 y) (p3 z)
          (some (fn [value]
                  (or (p1 value) (p2 value) (p3 value)))
                args)))))
  ([p1 p2 p3 & ps]
   (let [predicates (list* p1 p2 p3 ps)]
     (fn
       ([] nil)
       ([x]
        (loop [remaining (seq predicates)]
          (if remaining
            (let [result ((nth remaining 0) x)]
              (if result
                result
                (recur (next remaining))))
            nil)))
       ([x y]
        (loop [remaining (seq predicates)]
          (if remaining
            (let [predicate (nth remaining 0)]
              (let [result (predicate x)]
                (if result
                  result
                  (let [result (predicate y)]
                    (if result
                      result
                      (recur (next remaining)))))))
            nil)))
       ([x y z]
        (loop [remaining (seq predicates)]
          (if remaining
            (let [predicate (nth remaining 0)]
              (let [result (predicate x)]
                (if result
                  result
                  (let [result (predicate y)]
                    (if result
                      result
                      (let [result (predicate z)]
                        (if result
                          result
                          (recur (next remaining)))))))))
            nil)))
       ([x y z & args]
        (or
         (loop [remaining (seq predicates)]
           (if remaining
             (let [predicate (nth remaining 0)]
               (let [result (predicate x)]
                 (if result
                   result
                   (let [result (predicate y)]
                     (if result
                       result
                       (let [result (predicate z)]
                         (if result
                           result
                           (recur (next remaining)))))))))
             nil))
         (loop [remaining (seq predicates)]
           (if remaining
             (let [result (some (nth remaining 0) args)]
               (if result
                 result
                 (recur (next remaining))))
             nil))))))))

(defn- juxt-apply [function x y z args]
  (__lg_apply function x y z args))

(defn- comp-apply [function x y z args]
  (__lg_apply function x y z args))

(defn- comp-many [functions x]
  (loop [result ((nth functions 0) x)
         remaining (next functions)]
    (if remaining
      (recur ((nth remaining 0) result) (next remaining))
      result)))

(defn- partial-apply-one [function arg1 x y z args]
  (__lg_apply function arg1 x y z args))

(defn- partial-apply-two [function arg1 arg2 x y z args]
  (__lg_apply function arg1 arg2 x y z args))

(defn- partial-apply-three [function arg1 arg2 arg3 x y z args]
  (__lg_apply function arg1 arg2 arg3 x y z args))

(defn partial
  {:inline (fn [& arguments] (cons '__lg_partial arguments))}
  ([f] f)
  ([f arg1]
   (fn
     ([] (f arg1))
     ([x] (f arg1 x))
     ([x y] (f arg1 x y))
     ([x y z] (f arg1 x y z))
     ([x y z & args] (__lg_apply f arg1 x y z args))))
  ([f arg1 arg2]
   (fn
     ([] (f arg1 arg2))
     ([x] (f arg1 arg2 x))
     ([x y] (f arg1 arg2 x y))
     ([x y z] (f arg1 arg2 x y z))
     ([x y z & args] (__lg_apply f arg1 arg2 x y z args))))
  ([f arg1 arg2 arg3]
   (fn
     ([] (f arg1 arg2 arg3))
     ([x] (f arg1 arg2 arg3 x))
     ([x y] (f arg1 arg2 arg3 x y))
     ([x y z] (f arg1 arg2 arg3 x y z))
     ([x y z & args]
      (__lg_apply f arg1 arg2 arg3 x y z args))))
  ([f arg1 arg2 arg3 & more]
   (fn [& args]
     (__lg_apply f arg1 arg2 arg3 (concat more args)))))

(defn- fnil-value [fallback value]
  (if (nil? value) fallback value))

(defn fnil
  {:inline
   (fn
     ([] (list '__lg_fnil))
     ([function & defaults]
      (cons '__lg_fnil
            (cons (if (or (= function 'conj)
                          (= function 'clojure.core/conj)
                          (= function 'cljs.core/conj))
                    '__lg_conj
                    function)
                  defaults))))}
  ([f x]
   (fn
     ([a] (f (fnil-value x a)))
     ([a b] (f (fnil-value x a) b))
     ([a b c] (f (fnil-value x a) b c))
     ([a b c & args] (__lg_apply f (fnil-value x a) b c args))))
  ([f x y]
   (fn
     ([a b] (f (fnil-value x a) (fnil-value y b)))
     ([a b c] (f (fnil-value x a) (fnil-value y b) c))
     ([a b c & args]
      (__lg_apply f (fnil-value x a) (fnil-value y b) c args))))
  ([f x y z]
   (fn
     ([a b] (f (fnil-value x a) (fnil-value y b)))
     ([a b c]
      (f (fnil-value x a)
         (fnil-value y b)
         (fnil-value z c)))
     ([a b c & args]
      (__lg_apply f
             (fnil-value x a)
             (fnil-value y b)
             (fnil-value z c)
             args)))))

(defn comp
  {:inline (fn [& functions] (cons '__lg_comp functions))}
  ([] identity)
  ([f] f)
  ([f g]
   (fn
     ([] (f (g)))
     ([x] (f (g x)))
     ([x y] (f (g x y)))
     ([x y z] (f (g x y z)))
     ([x y z & args] (f (comp-apply g x y z args)))))
  ([f g h]
   (fn
     ([] (f (g (h))))
     ([x] (f (g (h x))))
     ([x y] (f (g (h x y))))
     ([x y z] (f (g (h x y z))))
     ([x y z & args] (f (g (comp-apply h x y z args))))))
  ([f1 f2 f3 & fs]
   (let [functions (reverse (list* f1 f2 f3 fs))]
     (fn [x] (comp-many functions x)))))

(defn juxt
  {:inline (fn [& functions] (cons '__lg_juxt functions))}
  ([f]
   (fn
     ([] (vector (f)))
     ([x] (vector (f x)))
     ([x y] (vector (f x y)))
     ([x y z] (vector (f x y z)))
     ([x y z & args] (vector (__lg_apply f x y z args)))))
  ([f g]
   (fn
     ([]
      (let [f-result (f)
            g-result (g)]
        (vector f-result g-result)))
     ([x]
      (let [f-result (f x)
            g-result (g x)]
        (vector f-result g-result)))
     ([x y]
      (let [f-result (f x y)
            g-result (g x y)]
        (vector f-result g-result)))
     ([x y z]
      (let [f-result (f x y z)
            g-result (g x y z)]
        (vector f-result g-result)))
     ([x y z & args]
      (let [f-result (__lg_apply f x y z args)
            g-result (__lg_apply g x y z args)]
        (vector f-result g-result)))))
  ([f g h]
   (fn
     ([]
      (let [f-result (f)
            g-result (g)
            h-result (h)]
        (vector f-result g-result h-result)))
     ([x]
      (let [f-result (f x)
            g-result (g x)
            h-result (h x)]
        (vector f-result g-result h-result)))
     ([x y]
      (let [f-result (f x y)
            g-result (g x y)
            h-result (h x y)]
        (vector f-result g-result h-result)))
     ([x y z]
      (let [f-result (f x y z)
            g-result (g x y z)
            h-result (h x y z)]
        (vector f-result g-result h-result)))
     ([x y z & args]
      (let [f-result (__lg_apply f x y z args)
            g-result (__lg_apply g x y z args)
            h-result (__lg_apply h x y z args)]
        (vector f-result g-result h-result)))))
  ([f g h & fs]
   (let [functions (list* f g h fs)]
     (fn
       ([]
        (reduce (fn [results function]
                  (__lg_conj results (function)))
                []
                functions))
       ([x]
        (reduce (fn [results function]
                  (__lg_conj results (function x)))
                []
                functions))
       ([x y]
        (reduce (fn [results function]
                  (__lg_conj results (function x y)))
                []
                functions))
       ([x y z]
        (reduce (fn [results function]
                  (__lg_conj results (function x y z)))
                []
                functions))
       ([x y z & args]
        (reduce (fn [results function]
                  (__lg_conj results (juxt-apply function x y z args)))
                []
                functions))))))

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
  (seq (drop (- (count coll) n) coll)))

(defn drop-last
  ([coll]
   (drop-last 1 coll))
  ([n coll]
   (take (- (count coll) n) coll)))

(defn reverse [coll]
  (__lg_reduce (fn [result item] (__lg_conj result item)) (list) coll))

(defn interpose [separator coll]
  (drop 1 (interleave (repeat separator) coll)))

(defn iterate [f x]
  (runtime-seq/unfold-memoized
   (fn [value]
     (Some (tuple value (f value))))
   x))

(defn- iteration-next [step somef vf kf ret]
  (lazy-seq
   (if (somef ret)
     (cons (vf ret)
           (if-some [k (kf ret)]
             (iteration-next step somef vf kf (step k))
             nil))
     nil)))

(defn- iteration-static [step somef vf kf initk]
  (iteration-next step somef vf kf (step initk)))

(defn iteration
  "Creates a static seqable iteration from `step`, `somef`, `vf`, `kf`, and `initk`.

  Direct calls also accept the upstream keyword form:

  ```clojure
  (iteration step :somef somef :vf vf :kf kf :initk initk)
  ```

  First-class calls use the five-argument static ABI because LG does not erase
  heterogeneous keyword varargs into a dynamic rest sequence."
  {:inline
   (fn [step & args]
     (if (= 4 (count args))
       (let [[somef vf kf initk] args]
         (list 'clojure.core/iteration-static step somef vf kf initk))
       (do
         (assert (= 8 (count args))
                 "iteration expects static args or :somef/:vf/:kf/:initk keyword args")
         (let [[somef-key somef vf-key vf kf-key kf initk-key initk] args]
           (assert (= somef-key :somef)
                   "iteration expects :somef as the first option")
           (assert (= vf-key :vf)
                   "iteration expects :vf as the second option")
           (assert (= kf-key :kf)
                   "iteration expects :kf as the third option")
           (assert (= initk-key :initk)
                   "iteration expects :initk as the fourth option")
           (list 'clojure.core/iteration-static step somef vf kf initk)))))}
  [step somef vf kf initk]
  (iteration-static step somef vf kf initk))

(defn- tree-seq-step [branch? children pending]
  (if pending
    (let [node (nth pending 0)
          siblings (rest pending)
          child-seq (if (branch? node) (seq (children node)) (take 0 pending))
          pending (if child-seq (concat child-seq siblings) siblings)]
      (Some (tuple node pending)))
    None))

(defn tree-seq [branch? children root]
  (runtime-seq/unfold-memoized
   (fn [pending]
     (tree-seq-step branch? children pending))
   (seq [root])))

(defn flatten
  {:inline (fn [x] (list '__lg_flatten x))}
  [x]
  (__lg_flatten x))

(defn- partitionv-step [n step remaining]
  (if remaining
    (let [partition (vec (take n remaining))]
      (if (= n (count partition))
        (Some (tuple partition (nthrest remaining step)))
        None))
    None))

(defn- padded-partitionv-step [n step pad remaining]
  (if remaining
    (let [partition (vec (take n remaining))]
      (if (= n (count partition))
        (Some (tuple partition (nthrest remaining step)))
        (Some (tuple (vec (take n (concat partition pad))) (seq [])))))
    None))

(defn partitionv
  ([n coll]
   (partitionv n n coll))
  ([n step coll]
   (runtime-seq/unfold-memoized
    (fn [remaining]
      (partitionv-step n step remaining))
    (seq coll)))
  ([n step pad coll]
   (runtime-seq/unfold-memoized
    (fn [remaining]
      (padded-partitionv-step n step pad remaining))
    (seq coll))))

(defn replicate [n x]
  (take n (repeat x)))

(defn dedupe [coll]
  (map (fn [values] (nth values 0))
       (partition-by (fn [value] value) coll)))

(defn- distinct-seq [seen remaining]
  (lazy-seq
   (if remaining
     (let [item (nth remaining 0)]
       (if (contains? seen item)
         (distinct-seq seen (next remaining))
         (cons item
               (distinct-seq (__lg_conj seen item)
                             (next remaining)))))
     nil)))

(defn- distinct-transducer-step [seen rf result input]
  (match @seen
    None
    (do
      (vreset! seen (Some (hash-set input)))
      (rf result input))
    (Some seen-set)
    (if (contains? seen-set input)
      (runtime-reduced/continue result)
      (do
        (vreset! seen (Some (__lg_conj seen-set input)))
        (rf result input)))))

(defn- distinct-transducer [rf]
  (let [seen (volatile! nil)]
    (fn
      ([] (rf))
      ([result] (rf result))
      ([result input]
       (distinct-transducer-step seen rf result input)))))

(defn distinct
  ([]
   (fn [rf] (distinct-transducer rf)))
  ([coll]
   (let [remaining (seq coll)]
     (if remaining
       (let [item (nth remaining 0)]
         (cons item
               (distinct-seq (hash-set item)
                             (next remaining))))
       (lazy-seq nil)))))

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
             (recur (__lg_conj seen item) (next remaining))))
         true))
     false)))

(defn not=
  {:inline (fn [& values] (list 'not (cons '__lg_equal values)))}
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

(defn key
  {:inline (fn [map-entry] (list 'IMapEntry/-key map-entry))}
  [map-entry]
  (IMapEntry/-key map-entry))

(defn val
  {:inline (fn [map-entry] (list 'IMapEntry/-val map-entry))}
  [map-entry]
  (IMapEntry/-val map-entry))

(defn rseq
  {:inline (fn [rev] (list 'IReversible/-rseq rev))}
  [rev]
  (IReversible/-rseq rev))

(defn find
  {:inline (fn [coll key] (list '__lg_find coll key))}
  [coll key]
  (__lg_find coll key))

(defn- replace-value [replacements value]
  (match (find replacements value)
    (Some entry) (val entry)
    None value))

(extend-protocol IReplaceCollection
  :vector
  (-replace-collection [coll replacements]
    (mapv (fn [value] (replace-value replacements value)) coll))
  :list
  (-replace-collection [coll replacements]
    (map (fn [value] (replace-value replacements value)) coll))
  :seq
  (-replace-collection [coll replacements]
    (map (fn [value] (replace-value replacements value)) coll))
  :array
  (-replace-collection [coll replacements]
    (map (fn [value] (replace-value replacements value)) coll))
  :set
  (-replace-collection [coll replacements]
    (map (fn [value] (replace-value replacements value)) coll))
  :map
  (-replace-collection [coll replacements]
    (map (fn [value] (replace-value replacements value)) coll)))

(defn- replace-transducer [replacements]
  (map (fn [value] (replace-value replacements value))))

(defn replace
  {:inline (fn
             ([replacements]
              (list 'replace-transducer replacements))
             ([replacements coll]
              (list '-replace-collection coll replacements)))}
  ([replacements]
   (replace-transducer replacements))
  ([replacements coll]
   (-replace-collection coll replacements)))

(defn equiv-map
  "Test map equivalence. Returns true if x equals y, otherwise returns false."
  [x y]
  (if (= (count x) (count y))
    (every?
     (fn [entry]
       (match (find y (key entry))
         (Some other) (= (val other) (val entry))
         None false))
     x)
    false))

(defn with-meta
  {:inline (fn [value metadata]
             (list '__lg_with-meta value metadata))}
  [value metadata]
  (__lg_with-meta value metadata))

(defn meta
  {:inline (fn [value] (list 'IMeta/-meta value))}
  [value]
  (IMeta/-meta value))

(defn- vary-meta-apply [update-fn metadata a b c d args]
  (__lg_apply update-fn metadata a b c d args))

(defn vary-meta
  {:inline
   (fn [value update-fn & args]
     (let [target (gensym)]
       (list
        'let [target value]
        (list
         'IWithMeta/-with-meta target
         (cons update-fn
               (cons (list 'IMeta/-meta target) args))))))}
  ([value update-fn]
   (with-meta value (update-fn (meta value))))
  ([value update-fn a]
   (with-meta value (update-fn (meta value) a)))
  ([value update-fn a b]
   (with-meta value (update-fn (meta value) a b)))
  ([value update-fn a b c]
   (with-meta value (update-fn (meta value) a b c)))
  ([value update-fn a b c d]
   (with-meta value (update-fn (meta value) a b c d)))
  ([value update-fn a b c d & args]
   (with-meta
     value
     (vary-meta-apply update-fn (meta value) a b c d args))))

(defn keyword-identical? [left right]
  (= left right))

(defn key-test [key other]
  (cond
    (identical? key other) true
    (keyword-identical? key other) true
    :else (= key other)))

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
  (__lg_reduce (fn [m entry] (merge-entry-with f m entry))
               m1
               (seq m2)))

(defn merge-with
  "Returns a map consisting of all input maps. When a key occurs in more than
  one map, combines its values from left to right by calling `f`. With at least
  one map argument, treats `nil` maps as empty maps and returns a map."
  ([_f]
   nil)
  ([f first-map & maps]
   (__lg_reduce (fn [m1 m2] (merge-two-with f m1 m2))
                (if-some [m first-map] m {})
                maps)))

(defn vec [coll]
  (rrb-vector/of-list (runtime-seq/to-list (seq coll))))

(defn vec-lite [coll]
  (vec coll))

(defn comparator
  "Returns a comparator that orders `x` and `y` using `pred`."
  [pred]
  (fn [x y]
    (if (pred x y)
      -1
      (if (pred y x) 1 0))))

(defn sort
  {:inline (fn [& args] (cons '__lg_sort args))}
  ([coll]
   (__lg_sort coll))
  ([comp coll]
   (__lg_sort comp coll)))

(defn sort-by
  ([keyfn coll]
   (__lg_sort-by keyfn coll))
  ([keyfn comp coll]
   (__lg_sort-by keyfn comp coll)))

(defn max-key
  ([k x]
   (let [_ k] x))
  ([k x y] (if (> (k x) (k y)) x y))
  ([k x y & more]
   (__lg_reduce (fn [best item] (max-key k best item))
                (max-key k x y)
                more)))

(defn min-key
  ([k x]
   (let [_ k] x))
  ([k x y] (if (< (k x) (k y)) x y))
  ([k x y & more]
   (__lg_reduce (fn [best item] (min-key k best item))
                (min-key k x y)
                more)))

(defn frequencies
  "Returns a map from each distinct item in `coll` to its occurrence count."
  [coll]
  (__lg_reduce
   (fn [counts value]
     (assoc counts value (inc (get counts value 0))))
   {}
   coll))

(defn update-vals
  "Returns `m` with `f` applied to every value."
  [m f]
  (with-meta
    (__lg_reduce-kv
     (fn [result key value]
       (assoc result key (f value)))
     {}
     m)
    (meta m)))

(defn update-keys
  "Returns `m` with `f` applied to every key."
  [m f]
  (with-meta
    (__lg_reduce-kv
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

;; Transients follow the public ClojureScript algorithms. Mutation is exposed
;; only through the closed, statically typed transient protocols.
(defn transient
  {:inline (fn [coll] (list '__lg_transient coll))}
  [coll]
  (IEditableCollection/-as-transient coll))

(defn persistent!
  {:inline (fn [tcoll] (list '__lg_persistent! tcoll))}
  [tcoll]
  (ITransientCollection/-persistent! tcoll))

(defn conj!
  {:inline (fn [& args] (cons '__lg_conj! args))}
  ([] (transient []))
  ([tcoll] tcoll)
  ([tcoll val] (ITransientCollection/-conj! tcoll val))
  ([tcoll val & vals]
   (loop [result (ITransientCollection/-conj! tcoll val)
          remaining vals]
     (if (seq remaining)
       (recur (ITransientCollection/-conj! result (first remaining))
              (next remaining))
       result))))

(defn assoc!
  {:inline (fn [& args] (cons '__lg_assoc! args))}
  ([tcoll key val] (ITransientAssociative/-assoc! tcoll key val))
  ([tcoll key val & kvs]
   (loop [result (ITransientAssociative/-assoc! tcoll key val)
          remaining kvs]
     (if (seq remaining)
       (let [tail (next remaining)]
         (if (seq tail)
           (recur (ITransientAssociative/-assoc! result
                                                 (first remaining)
                                                 (first tail))
                  (next tail))
           (stdlib/invalid-arg "assoc! expects an even number of key/value forms")))
       result))))

(defn dissoc!
  {:inline
   (fn [tcoll key & keys]
     (loop [expression (list '__lg_dissoc! tcoll key)
            remaining keys]
       (if (nil? remaining)
         expression
         (recur (list '__lg_dissoc! expression (first remaining))
                (next remaining)))))}
  ([tcoll key] (ITransientMap/-dissoc! tcoll key))
  ([tcoll key & keys]
   (loop [result (ITransientMap/-dissoc! tcoll key)
          remaining keys]
     (if (seq remaining)
       (recur (ITransientMap/-dissoc! result (first remaining))
              (next remaining))
       result))))

(defn pop!
  {:inline (fn [tcoll] (list 'ITransientVector/-pop! tcoll))}
  [tcoll]
  (ITransientVector/-pop! tcoll))

(defn disj!
  {:inline
   (fn [tcoll val & vals]
     (loop [expression (list 'ITransientSet/-disjoin! tcoll val)
            remaining vals]
       (if (nil? remaining)
         expression
         (recur (list 'ITransientSet/-disjoin! expression (first remaining))
                (next remaining)))))}
  ([tcoll val] (ITransientSet/-disjoin! tcoll val))
  ([tcoll val & vals]
   (loop [result (ITransientSet/-disjoin! tcoll val)
          remaining vals]
     (if (seq remaining)
       (recur (ITransientSet/-disjoin! result (first remaining))
              (next remaining))
       result))))

(defn get-in
  {:inline (fn [& args] (cons '__lg_get-in args))}
  ([map keys]
   (loop [value map
          remaining (seq keys)]
     (if (nil? remaining)
       value
       (recur (get value (first remaining)) (next remaining)))))
  ([map keys not-found]
   (loop [value map
          remaining (seq keys)]
     (if (nil? remaining)
       value
       (let [next-value (get value (first remaining) not-found)]
         (if (__lg_equal next-value not-found)
           not-found
           (recur next-value (next remaining))))))))

(defn assoc-in
  {:inline (fn [map keys value] (list '__lg_assoc-in map keys value))}
  [map [key & keys] value]
  (if (seq keys)
    (assoc map key (assoc-in (get map key) (vec keys) value))
    (assoc map key value)))

(defn update-in
  {:inline (fn [& args] (cons '__lg_update-in args))}
  ([map [key & keys] function]
   (if (seq keys)
     (assoc map key (update-in (get map key) (vec keys) function))
     (assoc map key (function (get map key)))))
  ([map [key & keys] function first-arg]
   (if (seq keys)
     (assoc map key (update-in (get map key) (vec keys) function first-arg))
     (assoc map key (function (get map key) first-arg))))
  ([map [key & keys] function first-arg second-arg]
   (if (seq keys)
     (assoc map key
            (update-in (get map key) (vec keys) function first-arg second-arg))
     (assoc map key (function (get map key) first-arg second-arg))))
  ([map [key & keys] function first-arg second-arg third-arg]
   (if (seq keys)
     (assoc map key
            (update-in (get map key) (vec keys) function
                       first-arg second-arg third-arg))
     (assoc map key
            (function (get map key) first-arg second-arg third-arg)))))

(defn update
  {:inline
   (fn [map key function & args]
     (cons '__lg_update
           (cons map
                 (cons key
                       (cons (if (or (= function 'update)
                                     (= function 'clojure.core/update))
                               '__lg_update
                               (if (or (= function 'conj)
                                       (= function 'clojure.core/conj)
                                       (= function 'cljs.core/conj))
                                 '__lg_conj
                                 function))
                             args)))))}
  ([map key function]
   (assoc map key (function (get map key))))
  ([map key function first-arg]
   (assoc map key (function (get map key) first-arg)))
  ([map key function first-arg second-arg]
   (assoc map key (function (get map key) first-arg second-arg)))
  ([map key function first-arg second-arg third-arg]
   (assoc map key
          (function (get map key) first-arg second-arg third-arg))))

(defn select-keys
  {:inline (fn [map keys] (list '__lg_select-keys map keys))}
  [map keyseq]
  (loop [result (empty map)
         keys (seq keyseq)]
    (if (seq keys)
      (let [key (first keys)
            entry (get map key ::not-found)]
        (recur (if (not= entry ::not-found)
                 (assoc result key entry)
                 result)
               (next keys)))
      (with-meta result (meta map)))))

(defn merge
  {:inline (fn [& maps] (cons '__lg_merge maps))}
  ([] nil)
  ([map] map)
  ([first-map second-map & maps]
   (__lg_reduce
    (fn [result map]
      (if-some [current map]
        (merge-two-with (fn [_old new] new) result current)
        result))
    (if-some [current second-map]
      (merge-two-with
       (fn [_old new] new)
       (if-some [initial first-map] initial {})
       current)
      (if-some [initial first-map] initial {}))
    maps)))

(defn vals
  {:inline (fn [m] (list '__lg_vals m))}
  [m]
  (map val m))

(defn namespace
  {:inline (fn [value] (list '__lg_namespace value))}
  [value]
  (INamed/-namespace value))

(def ^:dynamic *flush-on-newline* true)
(def ^:dynamic *print-newline* true)
(def ^:dynamic *print-readably* true)
(def ^:dynamic *print-meta* false)
(def ^:dynamic *print-dup* false)
(def ^:dynamic *print-namespace-maps* false)
(def ^:dynamic *print-length* None)
(def ^:dynamic *print-level* None)

(defn str
  {:inline (fn [& values] (cons '__lg_str values))}
  [& values]
  (__lg_render_display_values "" values))

#?(:native
   (defn format [^:string fmt] fmt))

(defn pr-str
  {:inline (fn [& values]
             (list 'if '*print-readably*
                   (cons '__lg_pr_str values)
                   (cons '__lg_print_str values)))}
  [& values]
  (if *print-readably*
    (__lg_render_readable_values " " values)
    (__lg_render_display_values " " values)))

(defn pr-str*
  {:inline (fn [value]
             (list 'if '*print-readably*
                   (list '__lg_pr_str value)
                   (list '__lg_print_str value)))}
  [value]
  (if *print-readably*
    (__lg_pr_str value)
    (__lg_print_str value)))

(defn print-str
  {:inline (fn [& values] (cons '__lg_print_str values))}
  [& values]
  (__lg_render_display_values " " values))

(defn println-str
  {:inline (fn [& values]
             (list '__lg_str (cons '__lg_print_str values) "\n"))}
  [& values]
  (__lg_str (__lg_render_display_values " " values) "\n"))

(defn prn-str
  {:inline (fn [& values]
             (list '__lg_str
                   (list 'if '*print-readably*
                         (cons '__lg_pr_str values)
                         (cons '__lg_print_str values))
                   "\n"))}
  [& values]
  (__lg_str
   (if *print-readably*
     (__lg_render_readable_values " " values)
     (__lg_render_display_values " " values))
   "\n"))

(defn pr
  {:inline (fn [& values]
             (list 'if '*print-readably*
                   (cons '__lg_pr values)
                   (cons '__lg_print_values values)))}
  [& values]
  (__lg_print_output
   (if *print-readably*
     (__lg_render_readable_values " " values)
     (__lg_render_display_values " " values))))

(defn print
  {:inline (fn [& values]
             (list '__lg_print_output (cons '__lg_print_str values)))}
  [& values]
  (__lg_print_output (__lg_render_display_values " " values)))

(defn println
  {:inline (fn [& values]
             (list '__lg_print_output_line
                   (cons '__lg_print_str values)
                   '*print-newline*
                   '*flush-on-newline*))}
  [& values]
  (__lg_print_output_line
   (__lg_render_display_values " " values)
   *print-newline*
   *flush-on-newline*))

(defn prn
  {:inline (fn [& values]
             (list '__lg_print_output_line
                   (list 'if '*print-readably*
                         (cons '__lg_pr_str values)
                         (cons '__lg_print_str values))
                   '*print-newline*
                   '*flush-on-newline*))}
  [& values]
  (__lg_print_output_line
   (if *print-readably*
     (__lg_render_readable_values " " values)
     (__lg_render_display_values " " values))
   *print-newline*
   *flush-on-newline*))

(defprotocol IPrintWithWriter
  (-pr-writer [value writer options]))

(defn pr-writer
  {:inline (fn [value writer options]
             (list '__lg_pr-writer value writer options))}
  [value writer options]
  (__lg_pr-writer value writer options))

(extend-protocol IPrintWithWriter
  :nil
  (-pr-writer [value writer options] (pr-writer value writer options))
  :bool
  (-pr-writer [value writer options] (pr-writer value writer options))
  :int
  (-pr-writer [value writer options] (pr-writer value writer options))
  :float
  (-pr-writer [value writer options] (pr-writer value writer options))
  :char
  (-pr-writer [value writer options] (pr-writer value writer options))
  :string
  (-pr-writer [value writer options] (pr-writer value writer options))
  :keyword
  (-pr-writer [value writer options] (pr-writer value writer options))
  :symbol
  (-pr-writer [value writer options] (pr-writer value writer options))
  :list
  (-pr-writer [value writer options] (pr-writer value writer options))
  :vector
  (-pr-writer [value writer options] (pr-writer value writer options))
  :seq
  (-pr-writer [value writer options] (pr-writer value writer options))
  :map
  (-pr-writer [value writer options] (pr-writer value writer options))
  :set
  (-pr-writer [value writer options] (pr-writer value writer options))
  :array
  (-pr-writer [value writer options] (pr-writer value writer options)))

(defn pr-sequential-writer
  [writer print-one begin separator end options collection]
  (IWriter/-write writer begin)
  (reduce
   (fn [first-value value]
     (if first-value
       nil
       (IWriter/-write writer separator))
     (print-one value writer options)
     false)
   true
  collection)
  (IWriter/-write writer end))

(defn pr-seq-writer
  [objects writer options]
  (pr-sequential-writer writer pr-writer "" " " "" options objects))

(defn print-prefix-map
  {:inline (fn [prefix m print-one writer options]
             (list '__lg_print-prefix-map prefix m print-one writer options))}
  [prefix m print-one writer options]
  (__lg_print-prefix-map prefix m print-one writer options))

(defn print-map
  {:inline (fn [m print-one writer options]
             (list '__lg_print-map m print-one writer options))}
  [m print-one writer options]
  (__lg_print-map m print-one writer options))

(defn print-meta?
  {:inline (fn [options value]
             (list '__lg_print-meta? options value))}
  [options value]
  (__lg_print-meta? options value))

(defn pr-str-with-opts
  [objects options]
  (__lg_render_readable_values_with_opts " " objects options))

(defn prn-str-with-opts
  [objects options]
  (str (pr-str-with-opts objects options) "\n"))

(defn write-all [writer & strings]
  (doseq [source strings]
    (IWriter/-write writer source)))

(defn string-print
  {:inline (fn [source] (list '__lg_print_output source))}
  [source]
  (__lg_print_output source))

(defn newline
  ([] (string-print "\n"))
  ([_options] (string-print "\n")))
