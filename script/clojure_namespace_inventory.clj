(ns clojure-namespace-inventory
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(def read-options
  {:read-cond :allow
   :features #{:cljs}
   :readers {'js identity}
   :auto-resolve (fn [alias] (symbol (name alias)))
   :default tagged-literal})

(defn- read-forms [path]
  (with-open [reader (java.io.PushbackReader. (io/reader path))]
    (loop [forms []]
      (let [form (read (assoc read-options :eof ::eof) reader)]
        (if (= ::eof form)
          forms
          (recur (conj forms form)))))))

(defn- ns-form [forms]
  (some #(when (and (seq? %) (= 'ns (first %))) %) forms))

(defn- require-specs [ns-form]
  (->> (drop 2 ns-form)
       (filter seq?)
       (filter #(#{:require :require-macros} (first %)))
       (mapcat rest)))

(defn- refer-clojure-exclusions [ns-form]
  (->> (drop 2 ns-form)
       (filter seq?)
       (filter #(= :refer-clojure (first %)))
       (mapcat rest)
       (partition 2)
       (keep (fn [[option value]]
               (when (= :exclude option)
                 value)))
       (mapcat identity)
       (map str)
       set))

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

(def failures (atom 0))

(defn- qualified-symbols [forms]
  (->> forms
       (mapcat #(tree-seq coll? seq %))
       (filter symbol?)
       (keep (fn [symbol]
               (when-let [prefix (namespace symbol)]
                 [prefix (name symbol)])))))

(defn- unqualified-symbols [forms ns-form]
  (->> forms
       (remove #(identical? ns-form %))
       (mapcat #(tree-seq coll? seq %))
       (filter symbol?)
       (keep (fn [symbol]
               (when-not (namespace symbol)
                 (name symbol))))))

(defn- inspect-file [path]
  (try
    (let [forms (read-forms path)
          ns-form (ns-form forms)
          infos (keep spec-info (require-specs ns-form))
          aliases (into {} (keep (fn [{:keys [namespace alias]}]
                                   (when alias [alias namespace])))
                        infos)
          excluded-core (refer-clojure-exclusions ns-form)]
      (doseq [{:keys [namespace]} infos
              :when (standard-namespace? namespace)]
        (println "namespace" namespace))
      (doseq [[prefix member] (qualified-symbols forms)
              :let [namespace (or (get aliases prefix)
                                  (when (standard-namespace? prefix) prefix))]
              :when (and namespace (standard-namespace? namespace))]
        (println "qualified-var" (str namespace "/" member)))
      (doseq [member (unqualified-symbols forms ns-form)
              :when (not (contains? excluded-core member))]
        (println "core-var" (str "clojure.core/" member))))
    (catch Exception error
      (swap! failures inc)
      (binding [*out* *err*]
        (println (str path ": " (ex-message error)))))))

(doseq [path *command-line-args*]
  (inspect-file path))

(when (pos? @failures)
  (System/exit 1))
