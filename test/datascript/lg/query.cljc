(ns datascript.lg.query
  (:require
   [datascript.lg.query-types :as query-types]
   [datascript.parser]))

(signature datascript.lg.query/q
  :fn<datascript.parser/Query;vector<datascript.lg.query-types/input>;datascript.lg.query-types/output>)

(defn ^datascript.lg.query-types/output q
  [^datascript.parser/Query query
   ^:vector<datascript.lg.query-types/input> inputs]
  (query-types/execute-query query inputs))
