#!/usr/bin/env bb

(require '[clojure.string :as string]
         '[clojure.tools.reader :as reader]
         '[clojure.tools.reader.reader-types :as reader-types])

(def ^:private eof (Object.))

(defn- reader-compatible-source [path]
  ;; LG type applications use semicolons inside one token, for example
  ;; :fn<key;key;int>. tools.reader treats those semicolons as comments, but
  ;; this inventory only needs top-level definition names and visibility.
  (string/replace (slurp path) #"(?<=\S);(?=\S)" "_"))

(defn- private-options? [values]
  (some (fn [value]
          (and (map? value) (:private value)))
        (take-while #(or (string? %) (map? %)) values)))

(defn- declaration-private? [form]
  (or (:private (meta form))
      (private-options? (drop 2 form))))

(defn- read-forms [path features]
  (with-open [input (java.io.StringReader. (reader-compatible-source path))]
    (let [source (reader-types/indexing-push-back-reader input)]
      (loop [forms []]
        (let [form (binding [reader/*default-data-reader-fn* (fn [_tag] identity)
                             reader/*alias-map* (delay {'ana 'cljs.analyzer})]
                     (reader/read {:eof eof
                                   :read-cond :allow
                                   :features features}
                                  source))]
          (if (identical? eof form)
            forms
            (recur (conj forms form))))))))

(defn- ordinary-definition [namespace form]
  (when (seq? form)
    (let [operator (some-> form first str)
          definition-name (second form)
          private-operator? (contains? #{"defn-" "core/defn-"} operator)
          private-name? (boolean (:private (meta definition-name)))
          private-declaration? (boolean (declaration-private? form))
          kind (cond
                 (contains? #{"defn" "core/defn"} operator) "function"
                 (contains? #{"defmacro" "core/defmacro"} operator) "macro"
                 :else nil)]
      (when (and kind
                 (symbol? definition-name)
                 (not private-operator?)
                 (not private-name?)
                 (not private-declaration?))
        [[(str namespace "/" (name definition-name)) kind]]))))

(defn- protocol-method-definitions [namespace form]
  (when (seq? form)
    (let [operator (some-> form first str)
          protocol-name (second form)]
      (when (and (contains? #{"defprotocol" "core/defprotocol"} operator)
                 (symbol? protocol-name)
                 (not (:private (meta protocol-name)))
                 (not (declaration-private? form)))
        (->> (drop 2 form)
             (keep (fn [method-form]
                     (when (seq? method-form)
                       (let [method-name (first method-form)]
                         (when (and (symbol? method-name)
                                    (not (:private (meta method-name)))
                                    (not (:private (meta method-form)))
                                    (not (private-options? (rest method-form))))
                           [(str namespace "/" (name method-name))
                            "protocol-method"])))))
             seq)))))

(defn- definitions [namespace form]
  (or (protocol-method-definitions namespace form)
      (ordinary-definition namespace form)))

(defn- top-level-definitions [namespace form]
  (let [operator (when (seq? form) (first form))]
    (cond
      (= 'do operator)
      (mapcat #(top-level-definitions namespace %) (rest form))

      (= 'if operator)
      (mapcat #(top-level-definitions namespace %) (drop 2 form))

      :else
      (or (definitions namespace form) []))))

(let [[namespace & paths] *command-line-args*]
  (when (or (string/blank? namespace) (empty? paths))
    (binding [*out* *err*]
      (println "usage: extract_clojurescript_public_vars.clj NAMESPACE SOURCE..."))
    (System/exit 2))
  (->> paths
       (mapcat (fn [path]
                 (mapcat #(read-forms path %) [#{:cljs} #{:clj}])))
       (mapcat #(top-level-definitions namespace %))
       distinct
       sort
       (run! (fn [[qualified-name kind]]
               (println (str qualified-name "\t" kind))))))
