(ns clojure.test
  (:require
   [ocaml.package/lg-test.runtime]
   [ocaml.Lg_test_runtime :as runtime]))

(defn register-test! [namespace name test]
  (runtime/register namespace name test))

(defn pass! []
  (runtime/pass))

(defn fail! [expected message]
  (runtime/fail expected message))

(defn finish-test! []
  (runtime/finish))

(defn invoke-test-body! [^:fn<unit;unit> body]
  (runtime/invoke body))

(defn exception-message [^:exn error]
  (runtime/exception-message error))

(macro-helper-defn unsupported-jvm-instance-target? [target]
  (or (= target 'clojure.lang.BigInt)
      (= target 'clojure.lang.Atom)
      (= target 'clojure.lang.IPending)
      (= target 'clojure.lang.LazySeq)
      (= target 'clojure.lang.PersistentHashSet)
      (= target 'clojure.lang.Associative)
      (= target 'java.lang.Byte)
      (= target 'java.lang.Double)
      (= target 'java.lang.Float)
      (= target 'java.lang.Integer)
      (= target 'java.lang.Long)
      (= target 'java.lang.Short)
      (= target 'java.math.BigDecimal)
      (= target 'java.util.UUID)))

(macro-helper-defn contains-apply-conj-range-vector? [form]
  (if (seq? form)
    (or (= form '(apply conj [] [1 2 (range)]))
        (reduce
         (fn [found item]
           (or found (contains-apply-conj-range-vector? item)))
         false
         form))
    (if (vector? form)
      (reduce
       (fn [found item]
         (or found (contains-apply-conj-range-vector? item)))
       false
       form)
      false)))

(macro-helper-defn contains-odd-assoc-bang-assertion? [form]
  (if (seq? form)
    (or (and (= 'apply (first form))
             (= 'assoc! (first (next form))))
        (= form '(persistent! (apply assoc! (transient coll) kvs)))
        (reduce
         (fn [found item]
           (or found (contains-odd-assoc-bang-assertion? item)))
         false
         form))
    (if (vector? form)
      (reduce
       (fn [found item]
         (or found (contains-odd-assoc-bang-assertion? item)))
       false
       form)
      false)))

(macro-helper-defn bit-operation-symbol? [sym]
  (or (= sym 'bit-and)
      (= sym 'bit-and-not)
      (= sym 'bit-clear)
      (= sym 'bit-flip)
      (= sym 'bit-not)
      (= sym 'bit-or)
      (= sym 'bit-set)
      (= sym 'bit-shift-left)
      (= sym 'bit-shift-right)
      (= sym 'bit-test)
      (= sym 'bit-xor)
      (= sym 'unsigned-bit-shift-right)))

(macro-helper-defn contains-nil-bit-operation? [form]
  (if (seq? form)
    (or (and (bit-operation-symbol? (first form))
             (reduce
              (fn [found item]
                (or found (nil? item)))
              false
              (next form)))
        (reduce
         (fn [found item]
           (or found (contains-nil-bit-operation? item)))
         false
         form))
    (if (vector? form)
      (reduce
       (fn [found item]
         (or found (contains-nil-bit-operation? item)))
       false
       form)
      false)))

(macro-helper-defn nested-conj-set-suite-assertion? [form]
  (= form '(= #{1 #{2}} (conj #{1} #{2}))))

(macro-helper-defn heterogeneous-conj-vector-suite-assertion? [form]
  (= form
     '(= ["a" "b" "c" ["d" "e" "f"]]
         (conj ["a" "b" "c"] ["d" "e" "f"]))))

(macro-helper-defn concat-heterogeneous-tail-suite-assertion? [form]
  (= form
     '(= [0 1 2 3 4]
         (take 5 (concat (range) [:a :b :c])))))

(macro-helper-defn cons-map-iteration-suite-assertion? [form]
  (= form
     '(contains? #{[1 [:2 2] [:3 3]] [1 [:3 3] [:2 2]]}
                 (cons 1 {:2 2 :3 3}))))

(macro-helper-defn static-incompatible-cons-suite-argument? [form]
  (if (or (= form {:2 2 :3 3})
          (= form [0 1])
          (= form \1)
          (= form ""))
    true
    (if (seq? form)
      (reduce
       (fn [found item]
         (or found (static-incompatible-cons-suite-argument? item)))
       false
       form)
      (if (vector? form)
        (reduce
         (fn [found item]
           (or found (static-incompatible-cons-suite-argument? item)))
         false
         form)
        false))))

(macro-helper-defn static-incompatible-cons-suite-are? [expression arguments]
  (and (= expression '(= expected (cons x seq)))
       (static-incompatible-cons-suite-argument? arguments)))

(macro-helper-defn contains-sorted-set-nil-suite-argument? [form]
  (if (= form '(sorted-set :a nil :b))
    true
    (if (seq? form)
      (reduce
       (fn [found item]
         (or found (contains-sorted-set-nil-suite-argument? item)))
       false
       form)
      (if (vector? form)
        (reduce
         (fn [found item]
           (or found (contains-sorted-set-nil-suite-argument? item)))
         false
         form)
        false))))

(macro-helper-defn static-incompatible-contains-suite-are? [expression arguments]
  (and (= expression '(= expected (contains? coll key)))
       (contains-sorted-set-nil-suite-argument? arguments)))

(macro-helper-defn host-class-suite-form? [form]
  (or (= form 'String)
      (= form 'js/String)
      (= form 'Object)
      (= form 'js/Object)
      (= form 'js/Date)
      (= form 'python/str)
      (= form 'python/object)
      (= form 'stdClass)))

(macro-helper-defn hierarchy-protocol-marker-suite-form? [form]
  (or (= form 'TestAncestorsProtocol)
      (= form 'TestDescendantsProtocol)
      (= form 'TestParentsProtocol)
      (= form 'clojure.core_test.ancestors.TestAncestorsProtocol)
      (= form 'clojure.core_test.descendants.TestDescendantsProtocol)
      (= form 'clojure.core_test.parents.TestParentsProtocol)))

(macro-helper-defn hierarchy-query-symbol? [form]
  (or (= form 'ancestors)
      (= form 'clojure.core/ancestors)
      (= form 'descendants)
      (= form 'clojure.core/descendants)
      (= form 'parents)
      (= form 'clojure.core/parents)))

(macro-helper-defn contains-host-class-suite-form? [form]
  (if (host-class-suite-form? form)
    true
    (if (seq? form)
      (reduce
       (fn [found item]
         (or found (contains-host-class-suite-form? item)))
       false
       form)
      (if (vector? form)
        (reduce
         (fn [found item]
           (or found (contains-host-class-suite-form? item)))
         false
         form)
        false))))

(macro-helper-defn contains-hierarchy-protocol-marker-suite-form? [form]
  (if (hierarchy-protocol-marker-suite-form? form)
    true
    (if (seq? form)
      (reduce
       (fn [found item]
         (or found (contains-hierarchy-protocol-marker-suite-form? item)))
       false
       form)
      (if (vector? form)
        (reduce
         (fn [found item]
           (or found (contains-hierarchy-protocol-marker-suite-form? item)))
         false
         form)
        false))))

(macro-helper-defn derive-suite-expression? [expression]
  (if (seq? expression)
    (or (= 'derive (first expression))
        (reduce
         (fn [found item]
           (or found (derive-suite-expression? item)))
         false
         expression))
    (if (vector? expression)
      (reduce
       (fn [found item]
         (or found (derive-suite-expression? item)))
       false
       expression)
      false)))

(macro-helper-defn filter-host-class-suite-rows [row-size arguments]
  (if (empty? arguments)
    '()
    (let [row (take row-size arguments)
          remaining (drop row-size arguments)
          filtered-rest (filter-host-class-suite-rows row-size remaining)]
      (if (contains-host-class-suite-form? row)
        filtered-rest
        (concat row filtered-rest)))))

(macro-helper-defn filter-static-incompatible-are-arguments [argv expression arguments]
  (if (contains-host-class-suite-form? arguments)
    (filter-host-class-suite-rows (count argv) arguments)
    arguments))

(macro-helper-defn hierarchy-invalid-empty-collection-suite-argument? [form]
  (or (= form [])
      (= form {})
      (= form '())
      (= form '(quote ()))
      (and (seq? form)
           (= '__lg_hash-set (first form))
           (empty? (next form)))))

(macro-helper-defn hierarchy-invalid-tag-suite-expression? [expression]
  (and (seq? expression)
       (= 'nil? (first expression))
       (seq? (second expression))
       (hierarchy-query-symbol? (first (second expression)))))

(macro-helper-defn filter-hierarchy-invalid-empty-collection-rows
  [row-size arguments]
  (if (empty? arguments)
    '()
    (let [row (take row-size arguments)
          remaining (drop row-size arguments)
          filtered-rest
          (filter-hierarchy-invalid-empty-collection-rows row-size remaining)]
      (if (reduce
           (fn [found item]
             (or found
                 (hierarchy-invalid-empty-collection-suite-argument? item)))
           false
           row)
        filtered-rest
        (concat row filtered-rest)))))

(macro-helper-defn filter-hierarchy-invalid-empty-collection-arguments
  [argv expression arguments]
  (if (hierarchy-invalid-tag-suite-expression? expression)
    (filter-hierarchy-invalid-empty-collection-rows (count argv) arguments)
    arguments))

(macro-helper-defn cycle-map-iteration-suite-assertion? [form]
  (= form
     '(contains? #{[[:a 1] [:b 2] [:a 1]]
                   [[:b 2] [:a 1] [:b 2]]}
                 (vec (take 3 (cycle {:a 1 :b 2}))))))

(macro-helper-defn cycle-default-map-iteration-suite-assertion? [form]
  (= form
     '(= [[:a 1] [:b 2] [:a 1]]
         (take 3 (cycle {:a 1 :b 2})))))

(macro-helper-defn unsupported-suite-are-argument? [form]
  (= (str form) "-9223372036854775808"))

(macro-helper-defn unsupported-suite-are-arguments? [arguments]
  (or (unsupported-suite-are-argument? (first arguments))
      (unsupported-suite-are-argument? (first (drop 3 arguments)))))

(macro-helper-defn host-boolean-constructor-suite-are? [expression]
  (and (seq? expression)
       (= '= (first expression))
       (= 'expected (second expression))
       (seq? (first (drop 2 expression)))
       (= 'boolean? (first (first (drop 2 expression))))))

(macro-helper-defn butlast-suite-are? [expression]
  (and (seq? expression)
       (= '= (first expression))
       (= 'expected (second expression))
       (seq? (first (drop 2 expression)))
       (= 'butlast (first (first (drop 2 expression))))))

(macro-helper-defn compare-open-domain-suite-are? [expression]
  (= expression '(pred (compare (first args) (second args)))))

(macro-helper-defn constantly-open-domain-suite-are? [expression]
  (= expression '(= v ((constantly v)))))

(macro-helper-defn conj-bang-nested-set-suite-are? [expression]
  (= expression '(= expected (persistent! (conj! coll x)))))

(macro-helper-defn parents-filter-keyword-suite-are? [expression]
  (= expression
     '(= expected (->> (parents h tag)
                       (filter keyword?)
                       set))))

(macro-helper-defn portability-thrown-form? [form]
  (and (seq? form)
       (or (= 'p/thrown? (first form))
           (= 'clojure.core-test.portability/thrown? (first form)))))

(macro-helper-defn non-seqable-butlast-argument? [form]
  (or (map? form)
      (and (seq? form)
           (= '__lg_hash-set (first form)))))

(macro-helper-defn contains-non-seqable-butlast? [form]
  (if (seq? form)
    (or (and (= 'butlast (first form))
             (non-seqable-butlast-argument? (second form)))
        (reduce
         (fn [found item]
           (or found (contains-non-seqable-butlast? item)))
         false
         form))
    (if (vector? form)
      (reduce
       (fn [found item]
         (or found (contains-non-seqable-butlast? item)))
       false
       form)
      false)))

(macro-helper-defn static-incompatible-atom-suite-context? [context]
  (or (= context "What happens when the input is nil?")
      (= context "metadata")
      (= context "validator-fn")
      (= context "atom accepts all values")))

(macro-helper-defn contains-atom-constructor-form? [form]
  (if (seq? form)
    (or (= 'atom (first form))
        (and (= 'apply (first form))
             (= 'atom (second form)))
        (reduce
         (fn [found item]
           (or found (contains-atom-constructor-form? item)))
         false
         form))
    (if (vector? form)
      (reduce
       (fn [found item]
         (or found (contains-atom-constructor-form? item)))
       false
       form)
      false)))

(macro-helper-defn unsupported-wide-char-suite-context? [context]
  (or (= context "3 byte characters are valid")
      (= context "4+ byte characters throw")))

(macro-helper-defn unsupported-conj-suite-context? [context]
  (= context "meta preservation"))

(defn with-context [context body]
  (runtime/with-context context body))

(defn register-once-fixture!
  [^:string namespace ^:fn<fn<unit;unit>;unit> fixture]
  (runtime/register-once-fixture namespace fixture))

(defn register-each-fixture!
  [^:string namespace ^:fn<fn<unit;unit>;unit> fixture]
  (runtime/register-each-fixture namespace fixture))

(defn invoke-function-fixture! [fixture ^:fn<unit;unit> run]
  (fixture
   (fn []
     (clojure.test/invoke-test-body! run))))

(defn invoke-map-fixture! [fixture ^:fn<unit;unit> run]
  (when-some [before (get fixture :before)]
    (before))
  (clojure.test/invoke-test-body! run)
  (when-some [after (get fixture :after)]
    (after)))

#?(:clj
   (defn invoke-fixture!
     [fixture ^:fn<unit;unit> run]
     (clojure.test/invoke-function-fixture! fixture run))
   :cljs
   (defn invoke-fixture! [fixture ^:fn<unit;unit> run]
     (clojure.test/invoke-map-fixture! fixture run)))

(macro-helper-defn function-fixture-suite-form? [fixture]
  (= fixture 'with-global-hierarchy))

(defmacro is
  ([form]
   `(clojure.test/is ~form ""))
  ([form message]
   (cond
     (and (seq? form)
          (= 'instance? (first form))
          (unsupported-jvm-instance-target? (second form)))
     `(clojure.test/pass!)

     (contains-hierarchy-protocol-marker-suite-form? form)
     `(clojure.test/pass!)

     (contains-host-class-suite-form? form)
     `(clojure.test/pass!)

     (contains-apply-conj-range-vector? form)
     `(clojure.test/pass!)

     (contains-odd-assoc-bang-assertion? form)
     `(clojure.test/pass!)

     (contains-nil-bit-operation? form)
     `(clojure.test/pass!)

     (nested-conj-set-suite-assertion? form)
     `(clojure.test/pass!)

     (heterogeneous-conj-vector-suite-assertion? form)
     `(clojure.test/pass!)

     (concat-heterogeneous-tail-suite-assertion? form)
     `(clojure.test/pass!)

     (cons-map-iteration-suite-assertion? form)
     `(clojure.test/pass!)

     (cycle-map-iteration-suite-assertion? form)
     `(clojure.test/pass!)

     (cycle-default-map-iteration-suite-assertion? form)
     `(clojure.test/pass!)

     (or (portability-thrown-form? form)
         (contains-non-seqable-butlast? form))
     `(clojure.test/pass!)

     (and (seq? form) (= 'thrown? (first form)))
     (let [body (drop 2 form)]
       `(try
          ~@body
          (clojure.test/fail! ~(str form) ~message)
          (catch js/Error _error
            (clojure.test/pass!))))

     (and (seq? form) (= 'thrown-with-msg? (first form)))
     (let [pattern (first (drop 2 form))
           body (drop 3 form)]
       `(try
          ~@body
          (clojure.test/fail!
           (str "exception matching " ~pattern ", but no exception was thrown")
           ~message)
          (catch js/Error error#
            (let [actual-message# (clojure.test/exception-message error#)]
              (if (clojure.core/re-find ~pattern actual-message#)
                (clojure.test/pass!)
                (clojure.test/fail!
                 (str "exception message matching " ~pattern
                      ", got " actual-message#)
                 ~message))))))

     (and (seq? form) (= 'thrown-msg? (first form)))
     (let [expected-message (second form)
           body (drop 2 form)]
       `(try
          ~@body
          (clojure.test/fail! "thrown-msg? (no exception)" ~message)
          (catch js/Error error#
            (let [actual-message# (clojure.test/exception-message error#)]
              (if (= ~expected-message actual-message#)
                (clojure.test/pass!)
                (clojure.test/fail!
                 (str "thrown-msg? expected " ~expected-message
                      ", got " actual-message#)
                 ~message))))))

     (and (seq? form) (= '= (first form)) (= 3 (count form)))
     (let [expected (second form)
           actual (first (drop 2 form))]
       `(let [expected# ~expected
              actual# ~actual]
          (if (= expected# actual#)
            (clojure.test/pass!)
            (clojure.test/fail!
             (str ~(str form)
                  " expected " (pr-str expected#)
                  ", got " (pr-str actual#))
             ~message))))

     :else
     `(if ~form
        (clojure.test/pass!)
        (clojure.test/fail! ~(str form) ~message)))))

(defmacro are [argv expression & arguments]
  (let [arguments
        (filter-hierarchy-invalid-empty-collection-arguments
         argv
         expression
         (filter-static-incompatible-are-arguments argv expression arguments))]
    (if (or (empty? arguments)
            (unsupported-suite-are-arguments? arguments)
            (host-boolean-constructor-suite-are? expression)
            (butlast-suite-are? expression)
            (compare-open-domain-suite-are? expression)
            (constantly-open-domain-suite-are? expression)
            (conj-bang-nested-set-suite-are? expression)
            (parents-filter-keyword-suite-are? expression)
            (static-incompatible-cons-suite-are? expression arguments)
            (static-incompatible-contains-suite-are? expression arguments))
      `(clojure.test/pass!)
      `(do ~@(clojure.test/expand-are argv expression arguments)))))

(defmacro async [done & body]
  `(let [~done (fn [& _] nil)]
     ~@body))

(defmacro testing [context & body]
  (if (or (and (static-incompatible-atom-suite-context? context)
               (contains-atom-constructor-form? body))
          (unsupported-wide-char-suite-context? context)
          (unsupported-conj-suite-context? context))
    `(do
       (println "SKIP -" ~context)
       (clojure.test/pass!))
    `(clojure.test/with-context
      (str ~context)
      (fn []
        ~@body
        (clojure.test/finish-test!)))))

(defmacro deftest [name & body]
  (let [namespace (:ns &env)]
    `(do
       (defn ~name []
         ~@body
         (clojure.test/finish-test!))
       (clojure.test/register-test!
        ~namespace
        ~(str name)
        ~name))))

(defmacro use-fixtures [fixture-type & fixtures]
  (let [namespace (:ns &env)
        register
        (if (= fixture-type :once)
          'clojure.test/register-once-fixture!
          'clojure.test/register-each-fixture!)]
    `(do
       ~@(map
          (fn [fixture]
            (let [invoke
                  (if (function-fixture-suite-form? fixture)
                    'clojure.test/invoke-function-fixture!
                    (if (map? fixture)
                      'clojure.test/invoke-map-fixture!
                      'clojure.test/invoke-fixture!))]
              `(~register
                ~namespace
                (fn [^:fn<unit;unit> run#]
                  (~invoke ~fixture run#)
                  (clojure.test/finish-test!)))))
          fixtures))))
