(ns clojure.core-test.ancestors
  (:require [clojure.test :refer [deftest is testing]]))

(def ancestors-hierarchy
  (-> (make-hierarchy)
      (derive ::a ::root)
      (derive ::b ::a)
      (derive ::c ::a)
      (derive ::d ::b)
      (derive ::d ::c)
      (derive ::leaf ::d)))

(deftest test-ancestors
  (testing "ancestors returns the transitive closure"
    (is (= #{::root} (ancestors ancestors-hierarchy ::a)))
    (is (= #{::root ::a ::b ::c} (ancestors ancestors-hierarchy ::d)))
    (is (= #{::root ::a ::b ::c ::d}
           (ancestors ancestors-hierarchy ::leaf)))
    (is (nil? (ancestors ancestors-hierarchy ::root)))
    (is (nil? (ancestors ancestors-hierarchy ::missing))))

  (testing "global transitive closure"
    (derive ::global-middle ::global-root)
    (derive ::global-leaf ::global-middle)
    (is (= #{::global-middle ::global-root} (ancestors ::global-leaf)))
    (underive ::global-leaf ::global-middle)
    (underive ::global-middle ::global-root)))
