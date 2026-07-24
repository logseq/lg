(ns datascript.bench.runner
  (:require
   [datascript.bench.datascript :as benchmark]
   [ocaml.Lg_runtime.Runtime_random :as runtime-random]))

(match (Sys.getenv_opt "LG_BENCH_SEED")
  (Some value) (runtime-random/seed (Stdlib.int_of_string value))
  None nil)

(def benchmark-names
  ["add-1"
   "add-5"
   "add-all"
   "init"
   "find-datoms"
   "find-datom"
   "retract-5"
   "q1"
   "q2"
   "q3"
   "q4"
   "q5-shortcircuit"
   "qpred1"
   "qpred2"
   "pull-one"
   "pull-many"
   "pull-wildcard"
   "freeze"
   "thaw"])

(def selected-benchmark-names
  (match (Sys.getenv_opt "LG_BENCHMARK")
    (Some name) [name]
    None benchmark-names))

(doseq [name selected-benchmark-names]
  (println (str name ":" (Float.to_string (benchmark/run-benchmark name)))))
