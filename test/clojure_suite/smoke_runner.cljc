(ns clojure-suite-smoke
  (:require
   [clojure.core-test.and :as and-test]
   [clojure.core-test.any-qmark :as any-qmark-test]
   [clojure.core-test.associative-qmark :as associative-qmark-test]
   [clojure.core-test.comment :as comment-test]
   [clojure.core-test.fn-qmark :as fn-qmark-test]
   [clojure.core-test.name :as name-test]
   [clojure.core-test.or :as or-test]
   [clojure.core-test.rand-int :as rand-int-test]
   [clojure.core-test.sequential-qmark :as sequential-qmark-test]
   [clojure.core-test.when :as when-test]
   [clojure.core-test.when-not :as when-not-test]
   [clojure.test :refer [run-tests]]))

(run-tests)
