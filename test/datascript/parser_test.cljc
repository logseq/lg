(ns datascript.test.parser
  (:require [datascript.parser :as parser]))

(def environment #?(:native "native" :melange "melange"))

(defn scalar [symbol]
  (parser/->BindScalar (parser/->Variable symbol)))

(defn throws? [f]
  (try
    (f)
    false
    (catch _
      true)))

(println
  (str environment ":bindings:"
       (= (scalar '?x)
          (parser/parse-binding '?x)) ":"
       (= (parser/->BindIgnore)
          (parser/parse-binding '_)) ":"
       (= (parser/->BindColl (scalar '?x))
          (parser/parse-binding '[?x ...])) ":"
       (= (parser/->BindTuple [(scalar '?x)])
          (parser/parse-binding '[?x])) ":"
       (= (parser/->BindTuple [(scalar '?x) (scalar '?y)])
          (parser/parse-binding '[?x ?y])) ":"
       (= (parser/->BindTuple [(parser/->BindIgnore) (scalar '?y)])
          (parser/parse-binding '[_ ?y])) ":"
       (= (parser/->BindColl
            (parser/->BindTuple
              [(parser/->BindIgnore)
               (parser/->BindColl (scalar '?x))]))
          (parser/parse-binding '[[_ [?x ...]] ...])) ":"
       (= (parser/->BindColl
            (parser/->BindTuple
              [(scalar '?a) (scalar '?b) (scalar '?c)]))
          (parser/parse-binding '[[?a ?b ?c]])) ":"
       (throws? (fn [] (parser/parse-binding :key)))))

(println
  (str environment ":in:"
       (= [(scalar '?x)]
          (parser/parse-in '[?x])) ":"
       (= [(parser/->BindScalar (parser/->SrcVar '$))
           (parser/->BindScalar (parser/->SrcVar '$1))
           (parser/->BindScalar (parser/->RulesVar))
           (parser/->BindIgnore)
           (scalar '?x)]
          (parser/parse-in '[$ $1 % _ ?x])) ":"
       (= [(parser/->BindScalar (parser/->SrcVar '$))
           (parser/->BindColl
             (parser/->BindTuple
               [(parser/->BindIgnore)
                (parser/->BindColl (scalar '?x))]))]
          (parser/parse-in '[$ [[_ [?x ...]] ...]])) ":"
       (throws? (fn [] (parser/parse-in ['?x :key])))))

(println
  (str environment ":with:"
       (= [(parser/->Variable '?x) (parser/->Variable '?y)]
          (parser/parse-with '[?x ?y])) ":"
       (throws? (fn [] (parser/parse-with '[?x _])))))
