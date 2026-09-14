; Copyright (c) Rich Hickey. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).

(ns cljs.reader
  (:refer-clojure :exclude [read-string])
  (:require [ocaml.Lg_runtime.Runtime_edn :as runtime]
            [ocaml.Lg_runtime.Runtime_string :as runtime-string]))

(defn- zero-fill-right-and-truncate [^:string source ^int width]
  (cond
    (= width (count source)) source
    (< width (count source)) (subs source 0 width)
    :else
    (zero-fill-right-and-truncate (str source "0") width)))

(defn- divisible? [^int number ^int divisor]
  (zero? (mod number divisor)))

(defn- indivisible? [^int number ^int divisor]
  (not (divisible? number divisor)))

(defn- leap-year? [^int year]
  (and (divisible? year 4)
       (or (indivisible? year 100)
           (divisible? year 400))))

(defn- days-in-month [^int month ^:bool leap?]
  (let [normal [0 31 28 31 30 31 30 31 31 30 31 30 31]
        leap [0 31 29 31 30 31 30 31 31 30 31 30 31]]
    (nth (if leap? leap normal) month)))

(defn- parse-int-or [^:option<string> source ^int fallback]
  (if-some [value source]
    (if-some [parsed (parse-long value)] parsed fallback)
    fallback))

(defn- check [^int low ^int value ^int high ^:string message]
  (when-not (<= low value high)
    (raise
     (Invalid_argument
      (str message " Failed:  " low "<=" value "<=" high))))
  value)

(defn parse-and-validate-timestamp [source]
  (match (runtime-string/timestamp-captures source)
    None
    (raise
     (Invalid_argument
      (str "Unrecognized date/time syntax: " source)))
    (Some captures)
    (let [years (parse-int-or (nth captures 1) 0)
          months (parse-int-or (nth captures 2) 1)
          days (parse-int-or (nth captures 3) 1)
          hours (parse-int-or (nth captures 4) 0)
          minutes (parse-int-or (nth captures 5) 0)
          seconds (parse-int-or (nth captures 6) 0)
          fraction-source (if-some [fraction (nth captures 7)] fraction "")
          fraction (parse-int-or
                    (Some (zero-fill-right-and-truncate fraction-source 3))
                    0)
          offset-sign (if-some [sign (nth captures 8)]
                        (if (= sign "-") -1 1)
                        1)
          offset-hours (parse-int-or (nth captures 9) 0)
          offset-minutes (parse-int-or (nth captures 10) 0)
          offset (* offset-sign (+ (* offset-hours 60) offset-minutes))
          month (check 1 months 12
                       "timestamp month field must be in range 1..12")]
      [years
       month
       (check 1 days (days-in-month month (leap-year? years))
              "timestamp day field must be in range 1..last day in month")
       (check 0 hours 23 "timestamp hour field must be in range 0..23")
       (check 0 minutes 59 "timestamp minute field must be in range 0..59")
       (check 0 seconds (if (= minutes 59) 60 59)
              "timestamp second field must be in range 0..60")
       (check 0 fraction 999
              "timestamp millisecond field must be in range 0..999")
       offset])))

(defn read-string [source]
  (runtime/read-string source))

(defn read
  ([source]
   (read-string source))
  ([^:map<keyword;Lg_edn_backend.t> opts ^:string source]
   (if (= source "")
     (get opts :eof (read-string source))
     (read-string source))))

(defn register-tag-parser! [tag f]
  (runtime/register-tag-parser (str tag) f))

(defn deregister-tag-parser! [tag]
  (runtime/deregister-tag-parser (str tag)))

(defn register-default-tag-parser! [f]
  (runtime/register-default-tag-parser
   (fn [^:string tag value]
     (f (symbol tag) value))))

(defn deregister-default-tag-parser! []
  (runtime/deregister-default-tag-parser))
