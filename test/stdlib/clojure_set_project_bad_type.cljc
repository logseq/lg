(ns stdlib.clojure-set-project-bad-type
  (:require [clojure.set :as set]))

(set/project [{:a 1}] [:a])
