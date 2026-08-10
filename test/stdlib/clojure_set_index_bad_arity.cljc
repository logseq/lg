(ns stdlib.clojure-set-index-bad-arity
  (:require [clojure.set :as set]))

(set/index #{{:a 1}})
