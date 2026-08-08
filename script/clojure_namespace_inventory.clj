(ns clojure-namespace-inventory
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn- read-ns-form [path]
  (with-open [reader (java.io.PushbackReader. (io/reader path))]
    (loop []
      (let [form (read {:read-cond :allow
                        :features #{:cljs}
                        :eof nil}
                       reader)]
        (cond
          (nil? form) nil
          (and (seq? form) (= 'ns (first form))) form
          :else (recur))))))

(defn- require-specs [ns-form]
  (->> (drop 2 ns-form)
       (filter seq?)
       (filter #(#{:require :require-macros} (first %)))
       (mapcat rest)))

(defn- spec-info [spec]
  (let [spec (if (symbol? spec) [spec] spec)
        namespace (first spec)
        options (apply hash-map (rest spec))]
    (when (symbol? namespace)
      {:namespace (str namespace)
       :alias (some-> (:as options) str)})))

(defn- standard-namespace? [namespace]
  (or (string/starts-with? namespace "clojure.")
      (string/starts-with? namespace "cljs.")))

(def qualified-symbol-pattern
  #"(?<![A-Za-z0-9_.-])([A-Za-z][A-Za-z0-9_.-]*)/([A-Za-z0-9_?!*+<>=.-]+)")

(defn- inspect-file [path]
  (try
    (let [infos (keep spec-info (require-specs (read-ns-form path)))
          aliases (into {} (keep (fn [{:keys [namespace alias]}]
                                   (when alias [alias namespace])))
                        infos)
          source (slurp path)]
      (doseq [{:keys [namespace]} infos
              :when (standard-namespace? namespace)]
        (println "namespace" namespace))
      (doseq [[_ prefix member] (re-seq qualified-symbol-pattern source)
              :let [namespace (or (get aliases prefix)
                                  (when (standard-namespace? prefix) prefix))]
              :when (and namespace (standard-namespace? namespace))]
        (println "qualified-var" (str namespace "/" member))))
    (catch Exception _
      nil)))

(doseq [path *command-line-args*]
  (inspect-file path))
