#!/usr/bin/env bb

(require '[clojure.edn :as edn]
         '[clojure.java.io :as io])

(defn- classification [status]
  (case status
    (:ported :static-adaptation) "source"
    :special-form "special-form"
    :typed-primitive "typed-primitive"
    :host-primitive "host-boundary"
    (:blocked :blocked-static-typing) "blocked-static-typing"
    :host-boundary "host-boundary"
    :out-of-scope "out-of-scope"
    :deferred "deferred"
    nil))

(defn- reason [entry]
  (name (or (:reason entry) (:status entry) :manifest-source)))

(let [[path] *command-line-args*]
  (when-not path
    (binding [*out* *err*]
      (println "usage: extract_stdlib_manifest_status.clj MANIFEST"))
    (System/exit 2))
  (let [manifest (-> path io/file slurp edn/read-string)]
    (doseq [[namespace entry] (sort-by (comp str key) (:namespaces manifest))]
      (println
       (str "namespace-ownership\t" namespace "\t"
            (if (:implementation entry)
              (if (:primitive-boundary entry)
                "source-with-primitive-boundary"
                "source")
              "manifest-only")
            "\t"
            (if (:implementation entry)
              (if (:primitive-boundary entry)
                "precompiled-lg-source-with-explicit-typed-or-host-boundary"
                "precompiled-lg-source")
              (reason entry))))
      (let [status (or (classification (:status entry))
                       (when (:implementation entry) "source-aggregate"))]
        (when status
          (println (str "namespace\t" namespace "\t" status "\t" (reason entry)))))
      (doseq [[definition-name definition-entry]
              (sort-by (comp str key) (:definitions entry))]
        (when-let [status (classification (:status definition-entry))]
          (println (str "definition\t" namespace "/" definition-name "\t"
                        status "\t" (reason definition-entry))))))))
