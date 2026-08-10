(ns stdlib.clojure-data-diff-similar-bad-arity
  (:require [cljs.reader :as reader]
            [clojure.data :as data]))

(data/diff-similar (reader/read-string "1"))
