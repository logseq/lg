(ns stdlib.clojure-set-rename-bad-arity
  (:require [clojure.set :as set]))

(set/rename #{{:a 1}})
