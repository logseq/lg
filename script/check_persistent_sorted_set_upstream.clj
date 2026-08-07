#!/usr/bin/env bb

(require '[clojure.java.io :as io]
         '[clojure.set :as set]
         '[clojure.string :as str])

(def definition-heads
  '#{def defonce defn defn- defmacro defprotocol defrecord deftype
     type-alias type-record type-variant})

(def container-heads '#{do})

(defn read-forms [path feature]
  (try
    (with-open [reader (java.io.PushbackReader. (io/reader path))]
      (loop [forms []]
        (let [form (read {:eof ::eof
                          :read-cond :allow
                          :features (hash-set feature)}
                         reader)]
          (if (= ::eof form)
            forms
            (recur (conj forms form))))))
    (catch Exception error
      (throw (ex-info (str "Failed to read " path " for " feature
                           ": " (.getMessage error))
                      {:path path :feature feature}
                      error)))))

(defn collect-names [form accepted-heads]
  (if (seq? form)
    (let [head (first form)]
      (cond
        (contains? accepted-heads head)
        (if (symbol? (second form)) #{(str (second form))} #{})

        (contains? container-heads head)
        (reduce set/union #{} (map #(collect-names % accepted-heads) (rest form)))

        :else #{}))
    #{}))

(defn names-in-file [path feature accepted-heads]
  (reduce set/union #{}
          (map #(collect-names % accepted-heads)
               (read-forms path feature))))

(defn names-in-files [specs accepted-heads]
  (reduce set/union #{}
          (map (fn [[path feature]]
                 (names-in-file path feature accepted-heads))
               specs)))

(defn lexical-names-in-files [paths heads-pattern]
  (reduce
   set/union
   #{}
   (map (fn [path]
          (->> (re-seq (re-pattern
                        (str "(?m)^\\s*\\((?:" heads-pattern
                             ")\\s+(?:\\^\\S+\\s+)*([^\\s\\[\\]()]+)"))
                       (slurp path))
               (map second)
               set))
        paths)))

(defn percentage [matched total]
  (if (zero? total) 100.0 (* 100.0 (/ matched total))))

(defn print-comparison [label upstream actual]
  (let [common (set/intersection upstream actual)
        missing (set/difference upstream actual)]
    (printf "%s: %d/%d (%.1f%%)%n"
            label (count common) (count upstream)
            (percentage (count common) (count upstream)))
    (println "Missing:")
    (doseq [name (sort missing)]
      (println name))))

(let [[upstream-root lg-root] *command-line-args*]
  (when-not (and upstream-root lg-root)
    (binding [*out* *err*]
      (println "usage: check_persistent_sorted_set_upstream.clj UPSTREAM_ROOT LG_ROOT"))
    (System/exit 2))
  (let [upstream-source-root (str upstream-root "/src-clojure/me/tonsky")
        upstream-test-root (str upstream-root "/test-clojure/me/tonsky/persistent_sorted_set/test")
        lg-source-root (str lg-root "/datascript/me/tonsky")
        lg-test-root (str lg-root "/test/datascript/persistent_sorted_set")
        upstream-native-definitions
        (names-in-files
         [[(str upstream-source-root "/persistent_sorted_set.clj") :clj]
          [(str upstream-source-root "/persistent_sorted_set/arrays.cljc") :clj]]
         definition-heads)
        upstream-melange-definitions
        (names-in-files
         [[(str upstream-source-root "/persistent_sorted_set.cljs") :cljs]
          [(str upstream-source-root "/persistent_sorted_set/arrays.cljc") :cljs]
          [(str upstream-source-root "/persistent_sorted_set/protocol.cljs") :cljs]]
         definition-heads)
        lg-definitions
        (lexical-names-in-files
         [(str lg-source-root "/persistent_sorted_set.cljc")
          (str lg-source-root "/persistent_sorted_set/arrays.cljc")
          (str lg-source-root "/persistent_sorted_set/protocol.cljc")]
         "def|defonce|defn|defn-|defmacro|defprotocol|defrecord|deftype|type-alias|type-record|type-variant")
        upstream-native-tests
        (names-in-files
         [[(str upstream-test-root "/core.cljc") :clj]
          [(str upstream-test-root "/small.cljc") :clj]
          [(str upstream-test-root "/storage.clj") :clj]
          [(str upstream-test-root "/stress.cljc") :clj]]
         '#{deftest})
        upstream-melange-tests
        (names-in-files
         [[(str upstream-test-root "/core.cljc") :cljs]
          [(str upstream-test-root "/small.cljc") :cljs]
          [(str upstream-test-root "/stress.cljc") :cljs]]
         '#{deftest})
        lg-tests
        (lexical-names-in-files
         (mapv (fn [name]
                 (str lg-test-root "/" name "_test.cljc"))
               ["arrays" "helpers" "leaf" "path" "storage"])
         "deftest|defn")]
    (println "Definition-name alignment (discovery only; not implementation parity):")
    (print-comparison "Native" upstream-native-definitions lg-definitions)
    (print-comparison "Melange" upstream-melange-definitions lg-definitions)
    (println "Exact upstream test-name alignment:")
    (print-comparison "Native" upstream-native-tests lg-tests)
    (print-comparison "Melange" upstream-melange-tests lg-tests)))
