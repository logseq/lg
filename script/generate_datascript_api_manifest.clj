#!/usr/bin/env bb

(require '[clojure.java.io :as io]
         '[clojure.string :as str]
         '[clojure.tools.reader :as reader]
         '[clojure.tools.reader.reader-types :as reader-types])

(def definition-heads
  '#{def defonce defn defn+ defmacro})

(defn source-file? [file]
  (and (.isFile file)
       (re-find #"\.(clj|cljc|cljs)$" (.getName file))))

(defn sanitize-lg-source [source]
  (-> source
      (str/replace
       #":[^\s\[\](){}\",]+"
       #(if (str/includes? % "<")
          (-> %
              (str/replace ";" "_")
              (str/replace "/" "_"))
          %))
      (str/replace
       #"[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+/[A-Za-z0-9_.!?*+<>=-]+"
       #(str/replace % "/" "__"))))

(defn read-forms-with-features [file lg-source? features]
  (try
    (let [source (str/replace (slurp file) #"#js\s*" "")
          source (if lg-source? (sanitize-lg-source source) source)]
      (with-open [input (reader-types/indexing-push-back-reader source)]
        (binding [*read-eval* false]
          (loop [forms []]
            (let [form (reader/read {:read-cond :allow
                                     :features features
                                     :eof ::eof}
                                    input)]
              (if (= ::eof form)
                forms
                (recur (conj forms form))))))))
    (catch Exception error
      (throw
       (ex-info "failed to read manifest source"
                {:file (.getPath file)
                 :cause (ex-message error)}
                error)))))

(defn read-forms [file lg-source?]
  (let [cljc? (str/ends-with? (.getName file) ".cljc")
        feature-sets (if (and lg-source? cljc?)
                       [#{:clj} #{:cljs}]
                       [#{:cljs}])]
    (mapcat
     #(read-forms-with-features file lg-source? %)
     feature-sets)))

(defn namespace-name [forms]
  (some (fn [form]
          (when (and (seq? form) (= 'ns (first form)))
            (str (second form))))
        forms))

(defn private-definition? [head name form]
  (or (str/ends-with? (str head) "-")
      (:private (meta name))
      (:private (meta form))
      (some #(and (map? %) (:private %)) (take 4 (drop 2 form)))))

(defn argument-lists [tail]
  (let [tail (drop-while #(or (string? %) (map? %)) tail)
        candidate (first tail)]
    (cond
      (vector? candidate) [candidate]
      (and (seq? candidate) (vector? (first candidate)))
      (keep
       #(when (and (seq? %) (vector? (first %))) (first %))
       tail)
      :else [])))

(defn arity-entry [qualified-name arguments]
  (let [amp-index (.indexOf arguments '&)
        variadic? (not= -1 amp-index)
        required (if variadic? amp-index (count arguments))]
    ["arity" qualified-name (str required)
     (if variadic? "variadic" "fixed")]))

(defn metadata-argument-lists [name]
  (let [arglists (:arglists (meta name))
        arglists (if (and (seq? arglists) (= 'quote (first arglists)))
                   (second arglists)
                   arglists)]
    (filter vector? arglists)))

(defn option-keys [form]
  (->> (tree-seq coll? seq form)
       (keep (fn [node]
               (when (map? node)
                 (or (:keys node) (get node 'keys)))))
       (mapcat identity)
       (map #(str ":" %))
       distinct
       sort))

(defn definition-entries [namespace form]
  (when (seq? form)
    (let [head (first form)
          name (second form)]
      (when (and (definition-heads head)
                 (symbol? name)
                 (not (private-definition? head name form)))
        (let [qualified-name (str namespace "/" name)
              tail (drop 2 form)
              arglists (if (#{'defn 'defn+ 'defmacro} head)
                         (argument-lists tail)
                         (metadata-argument-lists name))]
          (concat
           [["var" qualified-name]]
           (map #(arity-entry qualified-name %) arglists)
           (map #(vector "option" qualified-name %) (option-keys form))))))))

(defn protocol-entries [namespace form]
  (when (and (seq? form) (= 'defprotocol (first form)))
    (let [protocol-name (second form)
          qualified-name (str namespace "/" protocol-name)]
      (concat
       [["var" qualified-name]]
       (mapcat
        (fn [method]
          (let [method-name (str qualified-name "/" (first method))
                arglists (filter vector? (rest method))]
            (concat
             [["protocol" qualified-name (str (first method))]]
             (map #(arity-entry method-name %) arglists))))
        (filter seq? (drop 2 form)))))))

(def static-surface
  [["source-form" "query" "map-or-vector"]
   ["source-form" "query-input" "scalar-collection-tuple-relation-source-rules"]
   ["source-form" "pull" "vector"]
   ["source-form" "transaction" "map-operation-vector-datom-function"]
   ["source-form" "schema" "map"]
   ["source-form" "serialization-options" "map"]
   ["arity" "datascript.inline/assoc" "3" "fixed"]
   ["arity" "datascript.inline/update" "3" "fixed"]
   ["arity" "datascript.inline/update" "4" "fixed"]
   ["arity" "datascript.inline/update" "5" "fixed"]
   ["arity" "datascript.inline/update" "6" "fixed"]
   ["arity" "datascript.inline/update" "6" "variadic"]
   ["tagged-reader" "datascript/Datom"]
   ["tagged-reader" "datascript/DB"]])

(def closed-option-surface
  [["datascript.pull-api/parse-opts" [:visitor]]
   ["datascript.db/db-from-reader" [:datoms :schema]]
   ["datascript.db/restore-db"
    [:aevt :avet :eavt :max-eid :max-tx :schema]]
   ["datascript.storage/restore-impl"
    [:aevt :aevt-metadata
     :avet :avet-metadata
     :eavt :eavt-metadata
     :max-addr :max-eid :max-tx :schema]]])

(def closed-option-entries
  (mapcat
   (fn [[qualified-name options]]
     (map #(vector "option" qualified-name (str %)) options))
   closed-option-surface))

(def lg-source-paths
  ["test/datascript/lg/query.cljc"
   "test/datascript/upstream/built_ins.cljc"
   "test/datascript/upstream/conn.cljc"
   "test/datascript/upstream/core.cljc"
   "test/datascript/upstream/db.cljc"
   "test/datascript/upstream/entity.cljc"
   "test/datascript/upstream/inline.cljc"
   "test/datascript/upstream/lru.cljc"
   "test/datascript/upstream/parser.cljc"
   "test/datascript/upstream/pull_api.cljc"
   "test/datascript/upstream/pull_parser.cljc"
   "test/datascript/upstream/schema.cljc"
   "test/datascript/upstream/serialize.cljc"
   "test/datascript/upstream/storage.cljc"
   "test/datascript/upstream/storage_file.cljc"
   "test/datascript/upstream/util.cljc"])

(def lg-namespace-overrides
  {"datascript.lg.query" "datascript.query"
   "datascript.storage-file" "datascript.storage"})

(defn file-entries [file namespace-overrides lg-source?]
  (let [forms (read-forms file lg-source?)
        source-namespace (namespace-name forms)
        namespace (get namespace-overrides source-namespace source-namespace)]
    (when namespace
      (mapcat
       (fn [form]
         (or (seq (protocol-entries namespace form))
             (seq (definition-entries namespace form))
             []))
       forms))))

(defn format-entry [entry]
  (str/join "\t" entry))

(let [[mode root] (if (= "--lg" (first *command-line-args*))
                    [:lg (second *command-line-args*)]
                    [:upstream (first *command-line-args*)])
      root (or root
               (throw
                (ex-info
                 "usage: generate_datascript_api_manifest.clj [--lg] ROOT"
                 {})))
      [files namespace-overrides]
      (if (= :lg mode)
        [(map #(io/file root %) lg-source-paths) lg-namespace-overrides]
        [(->> (file-seq (io/file root "src" "datascript"))
              (filter source-file?)
              (sort-by #(.getPath %)))
         {}])
      missing-files (remove #(.isFile %) files)
      _ (when (seq missing-files)
          (throw
           (ex-info "manifest source file is missing"
                    {:files (mapv #(.getPath %) missing-files)})))
      entries (concat
               (mapcat #(file-entries % namespace-overrides (= :lg mode)) files)
               (when (= :lg mode) closed-option-entries)
               static-surface)]
  (doseq [entry (sort-by format-entry (distinct entries))]
    (println (format-entry entry))))
