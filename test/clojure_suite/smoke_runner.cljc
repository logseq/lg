(ns clojure-suite-smoke
  (:require
   [clojure.core-test.any-qmark :as any-qmark-test]
   [clojure.core-test.fn-qmark :as fn-qmark-test]
   [clojure.test :refer [run-tests]]))

(run-tests)
