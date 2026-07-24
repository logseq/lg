(ns datascript.test.core
  (:require
   [datascript.core :as d]))

(def available true)

(defn no-namespace-maps [run]
  (run))

(defn ^:Datascript_runtime.Data_value.t string-value [^:string value]
  (Datascript_runtime.Data_value.String value))

(defn ^:Datascript_runtime.Data_value.t int-value [^:int value]
  (Datascript_runtime.Data_value.Int value))

(defn ^:Datascript_runtime.Data_value.t keyword-value [^:keyword value]
  (Datascript_runtime.Data_value.Keyword (str value)))

(defn ^:Datascript_runtime.Data_value.t entity-id-value [^:int value]
  (Datascript_runtime.Data_value.Ref_to
   (Datascript_runtime.Data_value.Entity_id value)))

(type-variant datom-component
  (Entity :int)
  (Attribute :keyword)
  (Value :Datascript_runtime.Data_value.t))

(defn all-datoms [^datascript.db/DB db]
  (into
   #{}
   (map
    (fn [^datascript.db/Datom datom]
      [(Entity (.-e datom))
       (Attribute (.-a datom))
       (Value (.-v datom))]))
   (d/datoms db :eavt)))
