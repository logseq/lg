(ns stdlib.clojure-set-relational-bad-type
  (:require [clojure.set :as set]))

(set/rename [{:a 1}] {:a :b})
