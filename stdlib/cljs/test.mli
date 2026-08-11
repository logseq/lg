(ns cljs.test)

(signature cljs.test/compose-fixtures [result]
  :fn<fn<fn<result>;result>;fn<fn<result>;result>;fn<fn<result>;result>>)
(signature cljs.test/join-fixtures [result storage]
  :fn<seqable<fn<fn<result>;result>;storage>;fn<fn<result>;result>>)
(signature cljs.test/successful?
  :fn<map<keyword;int>;bool>)
