(ns clojure.core-test.fnil
  (:require [clojure.test :refer [deftest is testing]]
            [cljs.core :as core]))

(defn format-fields
  ([^:int count]
   (str count))
  ([^:int count ^:string label]
   (str count ":" label))
  ([^:int count ^:string label ^:bool enabled]
   (str count ":" label ":" enabled))
  ([^:int count ^:string label ^:bool enabled & ^:seq<string> _extra]
   (str count ":" label ":" enabled)))

(deftest test-fnil-independent-default-types
  (let [with-defaults ((identity fnil) format-fields 7 "fallback" true)]
    (is (= "7:fallback" (with-defaults nil nil)))
    (is (= "2:fallback" (with-defaults 2 nil)))
    (is (= "7:actual" (with-defaults nil "actual")))
    (is (= "7:fallback:true" (with-defaults nil nil nil)))
    (is (= "2:actual:false" (with-defaults 2 "actual" false)))))

(deftest test-fnil-qualified-access
  (testing "alias and qualified core calls retain independent argument types"
    (let [aliased (core/fnil format-fields 8 "alias" false)
          qualified (clojure.core/fnil format-fields 9 "qualified" true)]
      (is (= "8:alias:false" (aliased nil nil nil)))
      (is (= "9:qualified:true" (qualified nil nil nil))))))

(deftest test-fnil-defaults-evaluate-once
  (let [evaluations (atom 0)
        wrapped (fnil (fn [^:int value] value) (swap! evaluations inc))]
    (is (= 1 (wrapped nil)))
    (is (= 1 (wrapped nil)))
    (is (= 1 @evaluations))))
