(ns datascript.test.core
  (:require
   [datascript.core :as d]))

(def available true)

(defn no-namespace-maps [run]
  (run))

(defn all-datoms [db]
  (into #{} (map (juxt :e :a :v)) (d/datoms db :eavt)))
