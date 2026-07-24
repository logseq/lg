(ns ^:no-doc datascript.pull-parser
  (:require
   [datascript.db :as db]))

(type-record PullAttrData
  (alias :keyword)
  (recursion-key :int)
  (default :option<Datascript_runtime.Data_value.t>)
  (limit :option<int>)
  (name :keyword)
  (recursion-limit :option<int>)
  (recursive :bool)
  (reverse :bool)
  (xform
   :option<fn<option<Datascript_runtime.Data_value.t>;option<Datascript_runtime.Data_value.t>>>)
  (multival :bool)
  (ref :bool)
  (component :bool))

(def attr-key-counter (atom 0))

(defn ^int next-attr-key []
  (swap! attr-key-counter inc))

(type-variant pull-attr
  (PullAttribute :datascript.pull-parser/PullAttrData)
  (PullNested
   :datascript.pull-parser/PullAttrData
   :vector<pull-attr>
   :option<pull-attr>
   :option<pull-attr>
   :vector<pull-attr>
   :bool))

(type-record PullPattern
  (attrs :vector<pull-attr>)
  (first-attr :option<pull-attr>)
  (last-attr :option<pull-attr>)
  (reverse-attrs :vector<pull-attr>)
  (wildcard :bool))

(def ^pull-attr default-db-id-attr
  (PullAttribute
   (record PullAttrData
    (alias :db/id)
    (recursion-key 0)
    (default None)
    (limit None)
    (name :db/id)
    (recursion-limit None)
    (recursive false)
    (reverse false)
    (xform None)
    (multival false)
    (ref false)
    (component false))))

(def ^PullPattern default-pattern-ref
  (record PullPattern
    (attrs [default-db-id-attr])
    (first-attr None)
    (last-attr None)
    (reverse-attrs [])
    (wildcard false)))

(def ^PullPattern default-pattern-component
  (record PullPattern
    (attrs [default-db-id-attr])
    (first-attr None)
    (last-attr None)
    (reverse-attrs [])
    (wildcard true)))

(defn ^pull-attr attribute
  [^datascript.db/DB database ^:keyword source-attr]
  (let [reverse (db/reverse-ref? source-attr)
        name (if reverse (db/reverse-ref source-attr) source-attr)
        ref (db/ref? database name)
        component (db/component? database name)
        multival (db/multival? database name)]
    (when (and reverse (not ref))
      (Stdlib.invalid_arg
       "Reverse pull attribute requires :db.type/ref"))
    (let [data
          (record PullAttrData
            (alias source-attr)
            (recursion-key (next-attr-key))
            (default None)
            (limit (if multival (Some 1000) None))
            (name name)
            (recursion-limit None)
            (recursive false)
            (reverse reverse)
            (xform None)
            (multival multival)
            (ref ref)
            (component component))]
      (if ref
        (let [default-pattern
              (if (and component (not reverse))
                default-pattern-component
                default-pattern-ref)]
          (PullNested
           data
           (:attrs default-pattern)
           (:first-attr default-pattern)
           (:last-attr default-pattern)
           (:reverse-attrs default-pattern)
           (:wildcard default-pattern)))
        (PullAttribute data)))))

(defn ^PullAttrData attr-data [^pull-attr attr]
  (match attr
    (PullAttribute data) data
    (PullNested data _ _ _ _ _) data))

(defn ^pull-attr replace-attr-data
  [^pull-attr attr ^PullAttrData data]
  (match attr
    (PullAttribute _) (PullAttribute data)
    (PullNested _ attrs first-attr last-attr reverse-attrs wildcard)
    (PullNested
     data attrs first-attr last-attr reverse-attrs wildcard)))

(defn ^:option<PullPattern> attr-pattern [^pull-attr attr]
  (match attr
    (PullAttribute _) None
    (PullNested _ attrs first-attr last-attr reverse-attrs wildcard)
    (Some
     (record PullPattern
       (attrs attrs)
       (first-attr first-attr)
       (last-attr last-attr)
       (reverse-attrs reverse-attrs)
       (wildcard wildcard)))))

(defn ^boolean attr-pattern-wildcard [^pull-attr attr]
  (match attr
    (PullAttribute _) false
    (PullNested _ _ _ _ _ wildcard) wildcard))

(defn ^pull-attr with-pattern
  [^pull-attr attr ^PullPattern pattern]
  (let [data (attr-data attr)]
    (when-not (.-ref data)
      (Stdlib.invalid_arg
       "Nested pull pattern requires :db.type/ref"))
    (PullNested
     data
     (:attrs pattern)
     (:first-attr pattern)
     (:last-attr pattern)
     (:reverse-attrs pattern)
     (:wildcard pattern))))

(defn ^pull-attr with-default
  [^pull-attr attr
   ^:Datascript_runtime.Data_value.t default]
  (replace-attr-data
   attr
   (assoc (attr-data attr) :default (Some default))))

(defn ^pull-attr with-limit
  [^pull-attr attr ^:option<int> limit]
  (let [data (attr-data attr)]
    (when-not (.-multival data)
      (Stdlib.invalid_arg
       "Pull limit requires :db.cardinality/many"))
    (match limit
      (Some limit)
      (when-not (pos? limit)
        (Stdlib.invalid_arg
         "Pull limit must be positive"))
      None (Stdlib.ignore 0))
    (replace-attr-data
     attr
     (assoc data :limit limit))))

(defn ^pull-attr with-xform
  [^pull-attr attr
   ^:fn<option<Datascript_runtime.Data_value.t>;option<Datascript_runtime.Data_value.t>> xform]
  (replace-attr-data
   attr
   (assoc (attr-data attr) :xform (Some xform))))

(defn ^pull-attr recursive-attribute
  [^datascript.db/DB database ^:keyword source-attr]
  (let [data (attr-data (attribute database source-attr))]
    (when-not (.-ref data)
      (Stdlib.invalid_arg
       "Recursive pull attribute requires :db.type/ref"))
    (PullAttribute
     (record PullAttrData
      (alias (.-alias data))
      (recursion-key (.-recursion-key data))
      (default (.-default data))
      (limit (.-limit data))
      (name (.-name data))
      (recursion-limit None)
      (recursive true)
      (reverse (.-reverse data))
      (xform (.-xform data))
      (multival (.-multival data))
      (ref true)
      (component (.-component data))))))

(defn ^pull-attr recursive-attribute-with-limit
  [^datascript.db/DB database
   ^:keyword source-attr
   ^int limit]
  (when-not (pos? limit)
    (Stdlib.invalid_arg
     "Recursive pull limit must be positive"))
  (let [attr (recursive-attribute database source-attr)]
    (replace-attr-data
     attr
     (assoc
      (attr-data attr)
      :recursion-limit
      (Some limit)))))

(defn ^:vector<pull-attr> upsert-attr
  [^:vector<pull-attr> attrs ^pull-attr attr]
  (let [alias (.-alias (attr-data attr))]
    (loop [index 0]
      (if (= index (count attrs))
        (conj attrs attr)
        (if (= alias (.-alias (attr-data (nth attrs index))))
          (assoc attrs index attr)
          (recur (inc index)))))))

(defn ^PullPattern pattern
  [^:vector<pull-attr> attrs ^boolean wildcard]
  (let [attrs (reduce upsert-attr [] attrs)
        attrs
        (if (and
             wildcard
             (not
              (some
               (fn [^pull-attr attr]
                 (= :db/id (.-name (attr-data attr))))
               attrs)))
          (conj attrs default-db-id-attr)
          attrs)
        key-fn
        (fn [^pull-attr attr]
          (.-name (attr-data attr)))
        forward-attrs
        (vec
         (sort-by
          key-fn
          (filter
           (fn [^pull-attr attr]
             (not (.-reverse (attr-data attr))))
           attrs)))
        reverse-attrs
        (vec
         (sort-by
          key-fn
          (filter
           (fn [^pull-attr attr]
             (.-reverse (attr-data attr)))
           attrs)))
        datom-attrs
        (vec
         (filter
          (fn [^pull-attr attr]
            (not (= :db/id (.-name (attr-data attr)))))
          forward-attrs))]
    (record PullPattern
      (attrs forward-attrs)
      (first-attr (first datom-attrs))
      (last-attr (last datom-attrs))
      (reverse-attrs reverse-attrs)
      (wildcard wildcard))))

(defn ^PullPattern nested-pattern
  [^:vector<pull-attr> attrs ^boolean wildcard]
  (pattern attrs wildcard))

(defn ^PullPattern recursive-pattern
  [^datascript.db/DB database
   ^:vector<keyword> attrs
   ^:keyword recursive-attr
   ^boolean wildcard]
  (pattern
   (conj
    (mapv
     (fn [^:keyword attr]
       (attribute database attr))
     attrs)
    (recursive-attribute database recursive-attr))
   wildcard))

(defn ^PullPattern parse-pattern [^PullPattern pattern]
  pattern)
