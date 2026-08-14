(ns clojure.core-test.random-uuid
  (:require [clojure.test :as t :refer [deftest is]]
            [clojure.core-test.portability #?(:cljs :refer-macros :default :refer) [when-var-exists]]
            [clojure.string :as str]))

(when-var-exists random-uuid
  (defn has-format-4? [uuid]
    (= \4 (-> uuid
              str
              (str/split #"-")
              (get-in [2 0]))))

  (deftest test-random-uuid
    (let [uuids (repeatedly 10 random-uuid)]
      (is (every? uuid? uuids))
      (is (= (count uuids) (count (set uuids))))
      (is (every? has-format-4? uuids)))))
