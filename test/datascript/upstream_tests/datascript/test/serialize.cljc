(ns datascript.test.serialize
  (:require
    [clojure.edn :as edn]
    [clojure.test :as t :refer [is are deftest testing]]
    [datascript.core :as d]
    [datascript.db :as db]
    [datascript.test.core :as tdc]
    [ocaml.Lg_runtime.Runtime_edn :as runtime-edn]
    [ocaml.melange-edn-native/Melange_edn_native :as native-edn]
    #?(:clj [ocaml.yojson/Yojson.Safe :as yojson]))
  #?(:clj
     (:import
       [clojure.lang ExceptionInfo])))

(t/use-fixtures :once tdc/no-namespace-maps)

#?(:clj
(defn yojson-write [value]
     (yojson/to-string
       (native-edn/to-json
         (native-edn/of-edn-string (runtime-edn/write-string value))))))

#?(:clj
(defn yojson-read [source]
     (runtime-edn/read-string
       (native-edn/to-edn-string
         (native-edn/of-json (yojson/from-string source))))))

#?(:cljs
(defn melange-json-write [value]
  (native-edn/to-json-string
    (native-edn/of-edn-string (runtime-edn/write-string value)))))

#?(:cljs
(defn melange-json-read [source]
  (runtime-edn/read-string
    (native-edn/to-edn-string (native-edn/of-json-string source)))))

(def readers
  {#?@(:cljs ["cljs.reader/read-string"  cljs.reader/read-string]
       :clj  ["clojure.edn/read-string"  #(clojure.edn/read-string {:readers d/data-readers} %)
              "clojure.core/read-string" #(binding [*data-readers* (merge *data-readers* d/data-readers)]
                                            (read-string %))])})

(deftest test-pr-read
  (doseq [[r read-fn] readers]
    (testing r
      (let [d (db/datom 1 :name "Oleg" 17 true)]
        (is (= (pr-str d) "#datascript/Datom [1 :name \"Oleg\" 17 true]"))
        (is (= d (read-fn (pr-str d)))))
      
      (let [d (db/datom 1 :name 3)]
        (is (= (pr-str d) "#datascript/Datom [1 :name 3 536870912 true]"))
        (is (= d (read-fn (pr-str d)))))
      
      (let [db (-> (d/empty-db {:name {:db/unique :db.unique/identity}})
                 (d/db-with [[:db/add 1 :name "Petr"]
                             [:db/add 1 :age 44]])
                 (d/db-with [[:db/add 2 :name "Ivan"]]))]
        (is (= (pr-str db)
              (str "#datascript/DB {"
                ":schema {:name {:db/unique :db.unique/identity}}, "
                ":datoms ["
                "[1 :age 44 536870913] "
                "[1 :name \"Petr\" 536870913] "
                "[2 :name \"Ivan\" 536870914]"
                "]}")))
        (is (= db (read-fn (pr-str db))))))))

(def data
  [[1 :name    "Petr"]
   [1 :aka     "Devil"]
   [1 :aka     "Tupen"]
   [1 :age     15]
   [1 :follows 2]
   [1 :email   "petr@gmail.com"]
   [1 :avatar  10]
   [10 :url    "http://"]
   [1 :attach  {:some-key :some-value}]
   [2 :name    "Oleg"]
   [2 :age     30]
   [2 :email   "oleg@gmail.com"]
   [2 :attach  [:just :values]]
   [3 :name    "Ivan"]
   [3 :age     15]
   [3 :follows 2]
   [3 :attach  {:another :map}]
   [3 :avatar  30]
   [4 :name    "Nick" d/tx0]
   [5 :inf     ##Inf]
   [5 :-inf    ##-Inf]
   ;; check that facts about transactions doesn’t set off max-eid
   [d/tx0      :txInstant 0xdeadbeef]
   [30 :url    "https://"]])

(def schema 
  {:name    {} ;; nothing special about name
   :aka     {:db/cardinality :db.cardinality/many}
   :age     {:db/index true}
   :follows {:db/valueType :db.type/ref}
   :email   {:db/unique :db.unique/identity}
   :avatar  {:db/valueType :db.type/ref, :db/isComponent true}
   :url     {}   ;; just a component prop
   :attach  {}}) ;; should skip index

(deftest test-init-db
  (let [db-init     (d/init-db
                      (map (fn [[e a v]] (d/datom e a v)) data)
                      schema)
        db-transact (d/db-with
                      (d/empty-db schema)
                      (map (fn [[e a v]] [:db/add e a v]) data))]

    (testing "db-init produces the same result as regular transactions"
      (is (= db-init db-transact)))

    (testing "db-init produces the same max-eid as regular transactions"
      (let [assertions [[:db/add -1 :name "Lex"]]]
        (is (= (d/db-with db-init assertions)
              (d/db-with db-transact assertions)))))
    
    (testing "Roundtrip"
      (doseq [[r read-fn] readers]
        (testing r
          (is (= db-init (read-fn (pr-str db-init)))))))

    (testing "Reporting"
      (is (thrown-with-msg? ExceptionInfo #"init-db expects list of Datoms, got "
            (d/init-db [[:add -1 :name "Ivan"] {:add -1 :age 35}] schema))))))

(deftest ^{:doc "issue-463"} test-max-eid-from-refs
  (let [db (-> (d/empty-db {:ref {:db/valueType :db.type/ref}})
             (d/db-with [[:db/add 1 :name "Ivan"]])
             (d/db-with [{:db/id 1 :ref {:name "Oleg"}}]))]
    (is (= 2 (:max-eid db)))
    (doseq [[r read-fn] readers]
      (testing r
        (let [db' (read-fn (pr-str db))]
          (is (= 2 (:max-eid db'))))))))

(deftest serialize
  (let [db (d/db-with
             (d/empty-db schema)
             (map (fn [[e a v]] [:db/add e a v]) data))]
    (is (= db (-> db d/serializable d/from-serializable)))
    (is (= db (-> db d/serializable pr-str edn/read-string d/from-serializable)))
    (is (= db (-> db (d/serializable {:freeze-fn tdc/transit-write-str}) pr-str edn/read-string (d/from-serializable {:thaw-fn tdc/transit-read-str}))))
    (doseq [type [:json :json-verbose]]
      (testing type
        (is (= db (-> db d/serializable (tdc/transit-write type) (tdc/transit-read type) d/from-serializable)))))
    #?(:clj
       (is (= db (-> db d/serializable yojson-write yojson-read d/from-serializable))))
    #?(:cljs
       (is (= db (-> db d/serializable melange-json-write melange-json-read d/from-serializable))))))

(deftest test-nan
  (let [db (d/db-with
             (d/empty-db schema)
             [[:db/add 1 :nan ##NaN]])
        valid? #(#?(:clj Double/isNaN :cljs js/isNaN) (:nan (d/entity % 1)))]
    (is (valid? (-> db d/serializable d/from-serializable)))
    (is (valid? (-> db d/serializable pr-str edn/read-string d/from-serializable)))
    (is (valid? (-> db (d/serializable {:freeze-fn tdc/transit-write-str}) pr-str edn/read-string (d/from-serializable {:thaw-fn tdc/transit-read-str}))))
    (doseq [type [:json :json-verbose]]
      (testing type
        (is (valid? (-> db d/serializable (tdc/transit-write type) (tdc/transit-read type) d/from-serializable)))))
    #?(:clj
       (is (valid? (-> db d/serializable yojson-write yojson-read d/from-serializable))))
    #?(:cljs
       (is (valid? (-> db d/serializable melange-json-write melange-json-read d/from-serializable))))))
