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

(defrecord BasicCache [state]
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
    (BasicCache. (runtime-cache/basic-of-map base))))

(defrecord LRUCache [state]
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
    (LRUCache. (runtime-cache/lru-seed state base))))

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

(defn lru-cache-factory
  ([base]
   (LRUCache. (runtime-cache/lru-of-map 32 base)))
  ([base option threshold]
   (if (= option :threshold)
     (LRUCache. (runtime-cache/lru-of-map threshold base))
     (throw (ex-info "lru-cache-factory expects :threshold" {})))))
