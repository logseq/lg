(ns clojure.test
  (:require
   [ocaml.package/lg-test.alcotest]
   [ocaml.Lg_test_alcotest :as runner]))

(defn run-tests []
  (runner/run "clojure.test"))
