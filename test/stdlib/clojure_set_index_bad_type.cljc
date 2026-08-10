(ns stdlib.clojure-set-index-bad-type
  (:require [clojure.set :as set]))

(set/index [{:a 1}] [:a])
