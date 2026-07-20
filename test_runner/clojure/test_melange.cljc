(ns clojure.test
  (:require
   [ocaml.package/lg-test.runtime]
   [ocaml.Lg_test_runtime :as runtime]))

(defn run-tests []
  (runtime/run "clojure.test"))
