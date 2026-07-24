(ns datascript.test.file-storage-smoke
  (:require
   [datascript.db :as db]
   [datascript.storage :as storage]
   [datascript.storage-file :as storage-file]))

(defn temporary-directory []
  (let [path (Filename.temp_file "lg-datascript-storage-" "")]
    (Sys.remove path)
    path))

(defn cleanup-directory
  [^Datascript_runtime.Storage_backend.t backend ^:string directory]
  (storage/-delete backend (storage/-list-addresses backend))
  (Unix.rmdir directory))

(defn has-string?
  [^datascript.db/DB database
   ^:int eid
   ^:keyword attr
   ^:string value]
  (if-some [datom (db/search-ea database eid attr)]
    (Datascript_runtime.Data_value.equal
     (.-v datom)
     (Datascript_runtime.Data_value.String value))
    false))

(let [directory (temporary-directory)
      backend (storage-file/file-storage directory)
      database
      (db/init-db
       (to-array
        [(db/datom
          1 :name (Datascript_runtime.Data_value.String "Ivan"))])
       {})
      _stored (storage/store database backend)
      restored (storage/restore backend)
      reopened (storage-file/file-storage directory)
      restored-after-reopen (storage/restore reopened)
      stored-addresses (set (storage/-list-addresses backend))
      stored-filenames (set (array-seq (Sys.readdir directory)))
      missing-before (storage/-restore backend 999999)
      _orphan-stored
      (storage/-store
       backend
       [(tuple
         999999
         (Datascript_runtime.Storage_value.Stored_tail []))]
       [])
      orphan-present
      (contains? (set (storage/-list-addresses backend)) 999999)
      _orphan-deleted (storage/-delete backend [999999])
      missing-after (storage/-restore backend 999999)
      roundtrip-ok
      (if-some [database restored]
        (has-string? database 1 :name "Ivan")
        false)
      reopen-ok
      (if-some [database restored-after-reopen]
        (has-string? database 1 :name "Ivan")
        false)
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
      write-count (atom 0)
      read-count (atom 0)
      backend
      (storage-file/file-storage
       directory
       (storage-file/options
        (fn [^:out_channel output
             ^:Datascript_runtime.Storage_value.t value]
          (Stdlib.ignore (swap! write-count inc))
          (Marshal.to_channel
           output value (list-of :Marshal.extern_flags)))
        (fn [^:in_channel input]
          (Stdlib.ignore (swap! read-count inc))
          (Marshal.from_channel input))
        (fn [^:int address] (str "addr-" address))
        (fn [^:string filename]
          (Stdlib.int_of_string
           (String.sub filename 5 (- (String.length filename) 5))))))
      database
      (db/init-db
       (to-array
        [(db/datom
          1 :name (Datascript_runtime.Data_value.String "Ivan"))])
       {})
      _stored (storage/store database backend)
      restored (storage/restore backend)
      filenames (set (array-seq (Sys.readdir directory)))
      listed-addresses (set (storage/-list-addresses backend))
      roundtrip-ok
      (if-some [database restored]
        (has-string? database 1 :name "Ivan")
        false)
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
