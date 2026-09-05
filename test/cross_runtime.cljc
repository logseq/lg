(require [clojure.string :as string])

(assert (= "   中" (format "%4.1s" "中文")))
(assert (= "  😀" (format "%4.2s" "😀x")))
(assert (= "item:0007:2.50" (format "%s:%04d:%.2f" "item" 7 2.5)))
(assert (= "1.01 2.68 1.3" (format "%.2f %.2f %.1f" 1.005 2.675 1.25)))
(assert (= "1.01e+00 1.01" (format "%.2e %.3g" 1.005 1.005)))
(assert (= "1. 1.e+00" (format "%#.0f %#.0e" 1.0 1.0)))
(assert (= "1.0E7 1.0E-4 1.2345678901234 -0.0"
           (format "%s %s %s %s" 1.0e7 0.0001 1.2345678901234 -0.0)))
(assert (= "donenil\n" (with-out-str (prn (printf "%s" "done")))))
(defn format-optional [^:option<int> value] (format "%d:%s:%b" value value value))
(assert (= "7:7:true" (format-optional 7)))
(assert (= "null:null:false" (format-optional nil)))

(def timed-calls (volatile! 0))
(def timed-output
  (with-out-str
    (let [result (time (do (vswap! timed-calls inc) [1 2]))]
      (assert (= [1 2] result)))))
(assert (= 1 @timed-calls))
(assert (boolean (re-find #"Elapsed time: " timed-output)))
#?(:cljs
   (assert (boolean (re-find #"[0-9]+\.[0-9]{6} msecs" timed-output))))

(def printed-output
  (with-out-str
    (assert (nil? (print)))
    (assert (nil? (pr)))
    (assert (nil? (println)))
    (assert (nil? (prn)))
    (assert (nil? (print "a" 1)))
    (assert (nil? (pr "b" :c)))
    (assert (nil? (println "d" 2)))
    (assert (nil? (prn "e" :f)))
    (assert (nil? (newline (do (vswap! timed-calls inc) nil))))
    (assert (nil? (flush)))))
(assert (= "\n\na 1\"b\" :cd 2\n\"e\" :f\n\n" printed-output))
(assert (= 2 @timed-calls))
(assert (= "" (print-str)))
(assert (= "" (pr-str)))
(assert (= "\n" (println-str)))
(assert (= "\n" (prn-str)))

(def environment
  #?(:native "native"
     :melange "melange"
     :js-of-ocaml "js-of-ocaml"))

(defn sum
  ([^:int value] value)
  ([^:int left ^:int right] (+ left right))
  ([^:int left ^:int right & more]
   (reduce + (sum left right) more)))

(def selected-sum sum)

(def lazy-total (reduce + 0 (take 4 (range 1 10))))

(def stopped
  (reduce
    (fn [acc value]
      (if (= value 3) (reduced acc) (+ acc value)))
    0
    [1 2 3 100]))

(def values (conj (hash-set 1 2) 3))

(def joined (string/join "-" ["a" "b"]))

(def regex-matched
  (boolean (re-matches #"(?:([^/]+)/)?_([^/]+)" "user/_friend")))

(def hash-matched (= -68075478 (clojure.core/m3-fmix 651101558 4)))

(println
  (str environment ":" (selected-sum 1 2 3 4) ":" lazy-total ":" stopped ":"
       (contains? values 3) ":" joined ":" regex-matched ":" hash-matched))
