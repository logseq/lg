(ns clojure.core-test.descendants
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.core-test.portability #?(:cljs :refer-macros :default :refer) [when-var-exists]]))

(when-var-exists descendants
  (def descendants-hierarchy
    (-> (make-hierarchy)
        (derive ::a ::root)
        (derive ::b ::a)
        (derive ::c ::a)
        (derive ::d ::b)
        (derive ::d ::c)
        (derive ::leaf ::d)))

  (deftest test-descendants
    (testing "descendants returns the transitive closure"
      (is (= #{::a ::b ::c ::d ::leaf}
             (descendants descendants-hierarchy ::root)))
      (is (= #{::b ::c ::d ::leaf}
             (descendants descendants-hierarchy ::a)))
      (is (= #{::d ::leaf} (descendants descendants-hierarchy ::b)))
      (is (nil? (descendants descendants-hierarchy ::leaf)))
      (is (nil? (descendants descendants-hierarchy ::missing))))

    (testing "global transitive closure"
      (derive ::global-middle ::global-root)
      (derive ::global-leaf ::global-middle)
      (is (= #{::global-middle ::global-leaf} (descendants ::global-root)))
      (underive ::global-leaf ::global-middle)
      (underive ::global-middle ::global-root))))
