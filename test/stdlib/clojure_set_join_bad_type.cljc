(ns stdlib.clojure-set-join-bad-type
  (:require [clojure.set :as set]))

(set/join [{:a 1}] #{{:a 1}})
