(ns ^:no-doc datascript.impl.entity
  (:require [datascript.db :as db]))

(declare entity ->Entity equiv-entity lookup-entity touch touch-entity
  touch-components datoms->cache hash-entity)

(defn- entid [db eid]
  (when (or (number? eid)
          (sequential? eid)
          (keyword? eid))
    (db/entid db eid)))

(defn entity [db eid]
  {:pre [(db/db? db)]}
  (when-let [e (entid db eid)]
    (when (db/numeric-eid-exists? db e)
      (->Entity db e (volatile! false) (volatile! {})))))

(defn- entity-attr [db a datoms]
  (if (db/multival? db a)
    (if (db/ref? db a)
      (reduce #(conj %1 (entity db (:v %2))) #{} datoms)
      (reduce #(conj %1 (:v %2)) #{} datoms))
    (if (db/ref? db a)
      (entity db (:v (first datoms)))
      (:v (first datoms)))))

(defn- -lookup-backwards [db eid attr not-found]
  (if-let [datoms (not-empty (db/-search db [nil attr eid]))]
    (if (db/component? db attr)
      (entity db (:e (first datoms)))
      (reduce #(conj %1 (entity db (:e %2))) #{} datoms))
    not-found))

(deftype Entity [db ^int eid touched cache]
  IEquiv
  (-equiv [this other]
    (equiv-entity this other))

  IHash
  (-hash [this]
    (hash-entity this))

  Seqable
  (-seq [this]
    (touch this)
    (seq @cache))

  Counted
  (-count [this]
    (touch this)
    (count @cache))

  ILookup
  (-lookup [this attr]
    (lookup-entity this attr nil))
  (-lookup [this attr not-found]
    (lookup-entity this attr not-found))

  IAssociative
  (-contains-key? [this key]
    (not= ::nf (lookup-entity this key ::nf)))

  IFn
  (-invoke [this key]
    (lookup-entity this key))
  (-invoke [this key not-found]
    (lookup-entity this key not-found))

  IPrintWithWriter
  (-pr-writer [_ writer opts]
    (-pr-writer (assoc @cache :db/id eid) writer opts)))

(defn entity? [value]
  (instance? Entity value))

(defn- equiv-entity [^Entity this that]
  (and
    (instance? Entity that)
    (identical? (.-db this) (.-db ^Entity that))
    (= (.-eid this) (.-eid ^Entity that))))

(defn- hash-entity [^Entity entity]
  (db/combine-hashes
    (hash (.-eid entity))
    (hash (.-db entity))))

(defn- lookup-entity
  ([this attr]
   (lookup-entity this attr nil))
  ([^Entity this attr not-found]
   (if (= attr :db/id)
     (.-eid this)
     (if (db/reverse-ref? attr)
       (-lookup-backwards
         (.-db this)
         (.-eid this)
         (db/reverse-ref attr)
         not-found)
       (if-some [value (@(.-cache this) attr)]
         value
         (if @(.-touched this)
           not-found
           (if-some [datoms
                     (not-empty
                       (db/-search (.-db this) [(.-eid this) attr]))]
             (let [value (entity-attr (.-db this) attr datoms)]
               (vreset!
                 (.-cache this)
                 (assoc @(.-cache this) attr value))
               value)
             not-found)))))))

(defn- ^Entity touch-entity [^Entity entity]
  (when-not @(.-touched entity)
    (when-let [datoms
               (not-empty (db/-search (.-db entity) [(.-eid entity)]))]
      (vreset!
        (.-cache entity)
        (->> datoms
          (datoms->cache (.-db entity))
          (touch-components (.-db entity))))
      (vreset! (.-touched entity) true)))
  entity)

(defn touch-components [db attributes]
  (reduce-kv
    (fn [result attr value]
      (if (db/component? db attr)
        (if (db/multival? db attr)
          (assoc result attr (set (map touch-entity value)))
          (assoc result attr (touch-entity value)))
        (assoc result attr value)))
    {} attributes))

(defn- datoms->cache [db datoms]
  (reduce
    (fn [result part]
      (let [attr (:a (first part))]
        (assoc result attr (entity-attr db attr part))))
    {}
    (partition-by
      (fn [^datascript.db/Datom datom] (.-a datom))
      datoms)))

(defn touch [entity]
  {:pre [(or (nil? entity) (entity? entity))]}
  (when (some? entity)
    (touch-entity ^Entity entity)))
