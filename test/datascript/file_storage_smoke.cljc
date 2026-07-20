(ns datascript.test.file-storage-smoke
  (:require
   [clojure.edn :as edn]
   [datascript.core :as d]
   [datascript.db :as db]
   [datascript.storage :as storage]))

(defn temporary-directory []
  (let [path (Filename.temp_file "lg-datascript-storage-" "")]
    (Sys.remove path)
    path))

(defn cleanup-directory [backend directory]
  (storage/-delete backend (storage/-list-addresses backend))
  (Unix.rmdir directory))

(let [directory (temporary-directory)
      backend (d/file-storage directory)
      database (db/init-db [(db/datom 1 :name "Ivan")] {} {})
      _stored (d/store database backend)
      restored (d/restore backend)
      reopened (d/file-storage directory)
      restored-after-reopen (d/restore reopened)
      stored-addresses (set (storage/-list-addresses backend))
      stored-filenames (set (array-seq (Sys.readdir directory)))
      missing-before (storage/-restore backend 999999)
      _orphan-stored
      (storage/-store backend [[999999 {:orphan true}]] [])
      orphan-present
      (contains? (set (storage/-list-addresses backend)) 999999)
      _orphan-deleted (storage/-delete backend [999999])
      missing-after (storage/-restore backend 999999)
      roundtrip-ok
      (= "Ivan" (:v (first (db/-search restored [1 :name]))))
      reopen-ok
      (= "Ivan"
         (:v (first (db/-search restored-after-reopen [1 :name]))))
      addresses-ok
      (and (contains? stored-addresses 0)
           (contains? stored-addresses 1)
           (contains? stored-filenames "00000000")
           (contains? stored-filenames "00000001")
           (> (count stored-addresses) 2))
      delete-ok (and orphan-present (nil? missing-after))
      missing-ok (nil? missing-before)
      _cleaned (cleanup-directory backend directory)]
  (println
   (str "native:file-storage:"
        roundtrip-ok ":"
        reopen-ok ":"
        addresses-ok ":"
        delete-ok ":"
        missing-ok)))

(let [directory (temporary-directory)
      freeze-count (atom 0)
      thaw-count (atom 0)
      freeze-value
      (fn [^:dynamic value]
        (swap! freeze-count inc)
        (pr-str value))
      thaw-value
      (fn [^:string contents]
        (swap! thaw-count inc)
        (edn/read-string contents))
      backend
      (d/file-storage
       directory
       {:freeze-fn freeze-value
        :thaw-fn thaw-value})
      database (db/init-db [(db/datom 1 :name "Ivan")] {} {})
      _stored (d/store database backend)
      restored (d/restore backend)
      roundtrip-ok
      (= "Ivan" (:v (first (db/-search restored [1 :name]))))
      freeze-ok (pos? @freeze-count)
      thaw-ok (pos? @thaw-count)
      _cleaned (cleanup-directory backend directory)]
  (println
   (str "native:file-storage-custom:"
        roundtrip-ok ":"
        freeze-ok ":"
        thaw-ok)))

(let [directory (temporary-directory)
      write-count (atom 0)
      read-count (atom 0)
      backend
      (d/file-storage
       directory
       {:addr->filename-fn (fn [^:int address] (str "addr-" address))
        :filename->addr-fn
        (fn [^:string filename]
          (int
           (Stdlib.int_of_string
            (String.sub filename 5 (- (String.length filename) 5)))))
        :write-fn
        (fn [^:out_channel output ^:dynamic value]
          (swap! write-count inc)
          (Stdlib.output_string output (pr-str value)))
        :read-fn
        (fn [^:in_channel input]
          (swap! read-count inc)
          (edn/read-string
           (Stdlib.really_input_string
            input
            (Stdlib.in_channel_length input))))})
      database (db/init-db [(db/datom 1 :name "Ivan")] {} {})
      _stored (d/store database backend)
      restored (d/restore backend)
      filenames (set (array-seq (Sys.readdir directory)))
      listed-addresses (set (storage/-list-addresses backend))
      roundtrip-ok
      (= "Ivan" (:v (first (db/-search restored [1 :name]))))
      callbacks-ok (and (pos? @write-count) (pos? @read-count))
      filenames-ok
      (and (contains? filenames "addr-0")
           (contains? filenames "addr-1")
           (contains? listed-addresses 0)
           (contains? listed-addresses 1))
      _cleaned (cleanup-directory backend directory)]
  (println
   (str "native:file-storage-io:"
        roundtrip-ok ":"
        callbacks-ok ":"
        filenames-ok)))
