(ns stdlib.clojure-set-join-keymap-bad-type
  (:require [clojure.set :as set]))

(set/join #{{:a 1}} #{{:b 1}} [:a :b])
