(ns datascript.bench.runner
  (:require
   [datascript.bench.datascript :as benchmark]
   [goog.object :as object]))

(benchmark/-main
  (or (object/get (object/get js/process "env") "LG_BENCHMARK") "q1"))
