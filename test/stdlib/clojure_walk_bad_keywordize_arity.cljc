(ns clojure-walk-bad-keywordize-arity
  (:require [clojure.walk :as walk]
            [cljs.reader :as reader]))

(walk/keywordize-keys (reader/read-string "{}") (reader/read-string "{}"))
