(ns datascript.test.listen
  (:require
   [clojure.test :as t :refer [are deftest is testing]]
   [datascript.core :as d]
   [datascript.db :as db]))

(deftest test-listen!
  (let [connection (d/create-conn)
        reports (atom [])]
    (d/transact!
     connection
     [[:db/add -1 :name "Alex"]
      [:db/add -2 :name "Boris"]])
    (d/listen! connection :test
               (fn [report]
                 (swap! reports conj report)))
    (d/transact!
     connection
     [[:db/add -1 :name "Dima"]
      [:db/add -1 :age 19]
      [:db/add -2 :name "Evgeny"]]
     {:some-metadata 1})
    (d/transact!
     connection
     [[:db/add -1 :name "Fedor"]
      [:db/add 1 :name "Alex2"]
      [:db/retract 2 :name "Not Boris"]
      [:db/retract 4 :name "Evgeny"]])
    (d/unlisten! connection :test)
    (d/transact! connection [[:db/add -1 :name "Geogry"]])

    (is (= (:tx-data (first @reports))
           [(db/datom 3 :name "Dima" (+ d/tx0 2) true)
            (db/datom 3 :age 19 (+ d/tx0 2) true)
            (db/datom 4 :name "Evgeny" (+ d/tx0 2) true)]))
    (is (= (:tx-meta (first @reports))
           {:some-metadata 1}))
    (is (= (:tx-data (second @reports))
           [(db/datom 5 :name "Fedor" (+ d/tx0 3) true)
            (db/datom 1 :name "Alex" (+ d/tx0 3) false)
            (db/datom 1 :name "Alex2" (+ d/tx0 3) true)
            (db/datom 4 :name "Evgeny" (+ d/tx0 3) false)]))
    (is (= (:tx-meta (second @reports))
           nil))))
