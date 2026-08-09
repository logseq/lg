(ns reader-deregister-default-tag-parser-bad-arity
  (:require [cljs.reader :as reader]))

(reader/deregister-default-tag-parser! identity)
