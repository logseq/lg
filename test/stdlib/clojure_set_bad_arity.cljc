(ns stdlib.clojure-set-bad-arity
  (:require [clojure.set :as set]))

(set/subset? #{1})
