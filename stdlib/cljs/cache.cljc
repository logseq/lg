; Copyright (c) Rich Hickey. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0.
; This LG port follows cljs-cache 0.1.4's cljs.cache algorithms.

(ns cljs.cache
  (:require [ocaml.Lg_runtime.Runtime_cache :as runtime-cache]))

(defprotocol CacheProtocol
  (lookup [cache item] [cache item not-found])
  (has? [cache item])
  (hit [cache item])
  (miss [cache item result])
  (evict [cache item])
  (seed [cache base]))

(deftype BasicCache [state]
  CacheProtocol
  (lookup [_ item]
    (runtime-cache/basic-lookup state item))
  (lookup [_ item not-found]
    (runtime-cache/basic-lookup-default state item not-found))
  (has? [_ item]
    (runtime-cache/basic-contains state item))
  (hit [this _item]
    this)
  (miss [_ item result]
    (BasicCache. (runtime-cache/basic-miss state item result)))
  (evict [_ item]
    (BasicCache. (runtime-cache/basic-evict state item)))
  (seed [_ base]
    (BasicCache. (runtime-cache/basic-of-map base)))
  ILookup
  (-lookup [this item] (lookup this item))
  (-lookup [this item not-found] (lookup this item not-found))
  IAssociative
  (-assoc [this item result] (miss this item result))
  (-contains-key? [this item] (has? this item))
  IMap
  (-dissoc [this item] (evict this item))
  ICounted
  (-count [_] (-count (runtime-cache/basic-to-map state)))
  ICollection
  (-conj [this entry]
    (seed this (-conj (runtime-cache/basic-to-map state) entry)))
  IEquiv
  (-equiv [_ other] (= other (runtime-cache/basic-to-map state)))
  IEmptyableCollection
  (-empty [this] (seed this (-empty (runtime-cache/basic-to-map state))))
  ISeqable
  (-seq [_] (-seq (runtime-cache/basic-to-map state))))

(defn- get-time []
  (system-time))

(deftype TTLCache [state]
  CacheProtocol
  (lookup [_ item]
    (runtime-cache/ttl-lookup state item (get-time)))
  (lookup [_ item not-found]
    (runtime-cache/ttl-lookup-default state item not-found (get-time)))
  (has? [_ item]
    (runtime-cache/ttl-contains state item (get-time)))
  (hit [this _item]
    this)
  (miss [_ item result]
    (TTLCache. (runtime-cache/ttl-miss state item result (get-time))))
  (evict [_ item]
    (TTLCache. (runtime-cache/ttl-evict state item)))
  (seed [_ base]
    (TTLCache. (runtime-cache/ttl-seed state (get-time) base)))
  ILookup
  (-lookup [this item] (lookup this item))
  (-lookup [this item not-found] (lookup this item not-found))
  IAssociative
  (-assoc [this item result] (miss this item result))
  (-contains-key? [this item] (has? this item))
  IMap
  (-dissoc [this item] (evict this item))
  ICounted
  (-count [_] (-count (runtime-cache/ttl-to-map state)))
  ICollection
  (-conj [this entry]
    (seed this (-conj (runtime-cache/ttl-to-map state) entry)))
  IEquiv
  (-equiv [_ other] (= other (runtime-cache/ttl-to-map state)))
  IEmptyableCollection
  (-empty [this] (seed this (-empty (runtime-cache/ttl-to-map state))))
  ISeqable
  (-seq [_] (-seq (runtime-cache/ttl-to-map state))))

(deftype LRUCache [state]
  CacheProtocol
  (lookup [_ item]
    (runtime-cache/lru-lookup state item))
  (lookup [_ item not-found]
    (runtime-cache/lru-lookup-default state item not-found))
  (has? [_ item]
    (runtime-cache/lru-contains state item))
  (hit [_ item]
    (LRUCache. (runtime-cache/lru-hit state item)))
  (miss [_ item result]
    (LRUCache. (runtime-cache/lru-miss state item result)))
  (evict [_ item]
    (LRUCache. (runtime-cache/lru-evict state item)))
  (seed [_ base]
    (LRUCache. (runtime-cache/lru-seed state base)))
  ILookup
  (-lookup [this item] (lookup this item))
  (-lookup [this item not-found] (lookup this item not-found))
  IAssociative
  (-assoc [this item result] (miss this item result))
  (-contains-key? [this item] (has? this item))
  IMap
  (-dissoc [this item] (evict this item))
  ICounted
  (-count [_] (-count (runtime-cache/lru-to-map state)))
  ICollection
  (-conj [this entry]
    (seed this (-conj (runtime-cache/lru-to-map state) entry)))
  IEquiv
  (-equiv [_ other] (= other (runtime-cache/lru-to-map state)))
  IEmptyableCollection
  (-empty [this] (seed this (-empty (runtime-cache/lru-to-map state))))
  ISeqable
  (-seq [_] (-seq (runtime-cache/lru-to-map state))))

(defn- default-wrapper-fn [value-fn item]
  (value-fn item))

(defn through
  ([cache item]
   (if (has? cache item)
     (hit cache item)
     (miss cache item (default-wrapper-fn identity item))))
  ([value-fn cache item]
   (if (has? cache item)
     (hit cache item)
     (miss cache item (default-wrapper-fn value-fn item))))
  ([wrap-fn value-fn cache item]
   (if (has? cache item)
     (hit cache item)
     (miss cache item
       (wrap-fn (fn [value] (value-fn value)) item)))))

(defn basic-cache-factory [base]
  (BasicCache. (runtime-cache/basic-of-map base)))

(defn ttl-cache-factory
  ([base]
   (TTLCache. (runtime-cache/ttl-of-map 2000 (get-time) base)))
  ([base option ttl]
   (if (= option :ttl)
     (TTLCache. (runtime-cache/ttl-of-map ttl (get-time) base))
     (throw (ex-info "ttl-cache-factory expects :ttl" {})))))

(defn lru-cache-factory
  ([base]
   (LRUCache. (runtime-cache/lru-of-map 32 base)))
  ([base option threshold]
   (if (= option :threshold)
     (LRUCache. (runtime-cache/lru-of-map threshold base))
     (throw (ex-info "lru-cache-factory expects :threshold" {})))))
