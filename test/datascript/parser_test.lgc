(ns datascript.test.parser
  (:require [datascript.parser :as parser]))

(def environment #?(:native "native" :melange "melange"))

(defn scalar [symbol]
  (parser/->BindScalar (parser/->Variable symbol)))

(println
  (str environment ":bindings:"
       (= (scalar '?x)
          (parser/parse-binding '?x)) ":"
       (= (parser/->BindIgnore)
          (parser/parse-binding '_)) ":"
       (= (parser/->BindColl (scalar '?x))
          (parser/parse-binding '[?x ...])) ":"
       (= (parser/->BindTuple [(scalar '?x) (scalar '?y)])
          (parser/parse-binding '[?x ?y])) ":"
       (= (parser/->BindColl
            (parser/->BindTuple
              [(parser/->BindIgnore)
               (parser/->BindColl (scalar '?x))]))
          (parser/parse-binding '[[_ [?x ...]] ...])) ":"
       (= (parser/->BindColl
            (parser/->BindTuple
              [(scalar '?a) (scalar '?b) (scalar '?c)]))
          (parser/parse-binding '[[?a ?b ?c]]))))
