(ns stdlib.clojure-set-join-bad-arity
  (:require [clojure.set :as set]))

(set/join #{{:a 1}})
