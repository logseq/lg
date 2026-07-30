(ns datascript.storage-file
  (:require
   [datascript.storage :as storage]))

(type-record file-storage-backend
  (directory :string)
  (write-value :fn<out_channel;Datascript_runtime.Storage_value.t;unit>)
  (read-value :fn<in_channel;Datascript_runtime.Storage_value.t>)
  (address-to-filename :fn<int;string>)
  (filename-to-address :fn<string;int>))

(type-record file-storage-options
  (write-value :option<fn<out_channel;Datascript_runtime.Storage_value.t;unit>>)
  (read-value :option<fn<in_channel;Datascript_runtime.Storage_value.t>>)
  (address-to-filename :option<fn<int;string>>)
  (filename-to-address :option<fn<string;int>>))

(defn ^file-storage-options default-options []
  (record file-storage-options
          (write-value None)
          (read-value None)
          (address-to-filename None)
          (filename-to-address None)))

(defn ^file-storage-options options
  [^:fn<out_channel;Datascript_runtime.Storage_value.t;unit> write-value
   ^:fn<in_channel;Datascript_runtime.Storage_value.t> read-value
   ^:fn<int;string> address-to-filename
   ^:fn<string;int> filename-to-address]
  (record file-storage-options
          (write-value (Some write-value))
          (read-value (Some read-value))
          (address-to-filename (Some address-to-filename))
          (filename-to-address (Some filename-to-address))))

(defn- file-path
  [^file-storage-backend backend ^:int address]
  (Filename.concat
   (:directory backend)
   ((:address-to-filename backend) address)))

(defn- write-file
  [^file-storage-backend backend
   ^:int address
   ^:Datascript_runtime.Storage_value.t value]
  (let [output (Stdlib.open_out_bin (file-path backend address))
        _written ((:write-value backend) output value)]
    (Stdlib.close_out output)))

(defn- read-file
  [^file-storage-backend backend ^:int address]
  (let [path (file-path backend address)]
    (when (Sys.file_exists path)
      (let [input (Stdlib.open_in_bin path)
            value ((:read-value backend) input)
            _closed (Stdlib.close_in input)]
        value))))

(defn- delete-file
  [^file-storage-backend backend ^:int address]
  (let [path (file-path backend address)]
    (when (Sys.file_exists path)
      (Sys.remove path))))

(defn- default-write
  [^:out_channel output
   ^:Datascript_runtime.Storage_value.t value]
  :unit
  (Marshal.to_channel output value (list-of :Marshal.extern_flags)))

(defn- default-read
  [^:in_channel input]
  :Datascript_runtime.Storage_value.t
  (Marshal.from_channel input))

(defn- default-address-to-filename [^:int address] :string
  (Lg_runtime.Runtime_int.format_hex address 8))

(defn- default-filename-to-address [^:string filename] :int
  (Stdlib.int_of_string (str "0x" filename)))

(defn- ensure-directory [^:string directory]
  (when-not (Sys.file_exists directory)
    (Unix.mkdir directory 493))
  directory)

(defn ^:datascript.storage/storage-backend file-storage
  ([^:string directory]
   (file-storage
    directory
    (default-options)))
  ([^:string directory ^file-storage-options opts]
   (let [write-value
         (if-some [write (:write-value opts)]
           write
           default-write)
         read-value
         (if-some [read (:read-value opts)]
           read
           default-read)
         address-to-filename
         (if-some [convert (:address-to-filename opts)]
           convert
           default-address-to-filename)
         filename-to-address
         (if-some [convert (:filename-to-address opts)]
           convert
           default-filename-to-address)]
     (let [backend
           (record file-storage-backend
                   (directory (ensure-directory directory))
                   (write-value write-value)
                   (read-value read-value)
                   (address-to-filename address-to-filename)
                   (filename-to-address filename-to-address))]
       (storage/make-backend
        (fn [address-data delete-addresses]
          (doseq [entry address-data]
            (write-file
             backend
             (tuple-get entry 0)
             (tuple-get entry 1)))
          (doseq [address delete-addresses]
            (delete-file backend address))
          (Stdlib.ignore 0))
        (fn [address]
          (read-file backend address))
        (fn [_ignored]
          (mapv
           (:filename-to-address backend)
           (array-seq (Sys.readdir (:directory backend)))))
        (fn [addresses]
          (doseq [address addresses]
            (delete-file backend address))
          (Stdlib.ignore 0)))))))
