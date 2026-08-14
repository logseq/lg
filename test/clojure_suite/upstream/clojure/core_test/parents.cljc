(ns clojure.core-test.parents
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.core-test.portability #?(:cljs :refer-macros :default :refer) [when-var-exists]]))

(when-var-exists parents
  (def parents-hierarchy
    (-> (make-hierarchy)
        (derive ::a ::root)
        (derive ::b ::a)
        (derive ::c ::a)
        (derive ::d ::b)
        (derive ::d ::c)))

  (deftest test-parents
    (testing "parents returns only direct parents"
      (is (= #{::root} (parents parents-hierarchy ::a)))
      (is (= #{::b ::c} (parents parents-hierarchy ::d)))
      (is (nil? (parents parents-hierarchy ::root)))
      (is (nil? (parents parents-hierarchy ::missing))))

    (testing "local hierarchies do not mutate the global hierarchy"
      (is (nil? (parents ::d))))

    (testing "global hierarchy"
      (is (nil? (derive ::global-child ::global-parent)))
      (is (= #{::global-parent} (parents ::global-child)))
      (is (nil? (underive ::global-child ::global-parent)))
      (is (nil? (parents ::global-child))))))
