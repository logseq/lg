(ns clojure-suite-smoke
  (:require
   [clojure.core-test.and :as and-test]
   [clojure.core-test.any-qmark :as any-qmark-test]
   [clojure.core-test.comment :as comment-test]
   [clojure.core-test.fn-qmark :as fn-qmark-test]
   [clojure.core-test.or :as or-test]
   [clojure.core-test.when :as when-test]
   [clojure.core-test.when-not :as when-not-test]
   [clojure.test :refer [run-tests]]))

(run-tests)
