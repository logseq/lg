(ns datascript.storage-file
  (:require
   [clojure.edn :as edn]
   [datascript.storage :as storage]))

(type-record file-storage-backend
  (directory :string)
  (write-value :fn<out_channel;dynamic;unit>)
  (read-value :fn<in_channel;dynamic>)
  (address-to-filename :fn<int;string>)
  (filename-to-address :fn<string;int>))

(defn- file-path
  [^file-storage-backend backend ^:int address]
  (Filename.concat
   (:directory backend)
   ((:address-to-filename backend) address)))

(defn- write-file
  [^file-storage-backend backend ^:int address ^:dynamic value]
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

(extend-type file-storage-backend
  storage/IStorage
  (-store [backend address-data delete-addresses]
    (doseq [[address data] address-data]
      (write-file backend (int address) data))
    (doseq [address delete-addresses]
      (delete-file backend (int address))))
  (-restore [backend address]
    (read-file backend (int address)))
  (-list-addresses [backend]
    (mapv
     (:filename-to-address backend)
     (array-seq (Sys.readdir (:directory backend)))))
  (-delete [backend addresses]
    (doseq [address addresses]
      (delete-file backend (int address)))))

(defn- default-write
  [^:out_channel output ^:dynamic value] :unit
  (Stdlib.output_string output (pr-str value)))

(defn- default-freeze [^:dynamic value] :string
  (pr-str value))

(defn- default-read
  [^:in_channel input] :dynamic
  (edn/read-string
   (Stdlib.really_input_string
    input
    (Stdlib.in_channel_length input))))

(defn- default-thaw [^:string contents] :dynamic
  (edn/read-string contents))

(defn- default-address-to-filename [^:int address] :string
  (Lg_runtime.Runtime_int.format_hex address 8))

(defn- default-filename-to-address [^:string filename] :int
  (Stdlib.int_of_string (str "0x" filename)))

(defn- ensure-directory [^:string directory]
  (when-not (Sys.file_exists directory)
    (Unix.mkdir directory 493))
  directory)

(defn file-storage
  ([^:string directory]
   (file-storage directory {}))
  ([^:string directory ^:dynamic opts]
   (let [write-value
         (if-some [write (:write-fn opts)]
           (__lg_dynamic-narrow default-write write)
           (if-some [freeze (:freeze-fn opts)]
             (let [freeze (__lg_dynamic-narrow default-freeze freeze)]
               (fn [^:out_channel output ^:dynamic value]
                 (Stdlib.output_string output (freeze value))))
             default-write))
         read-value
         (if-some [read (:read-fn opts)]
           (__lg_dynamic-narrow default-read read)
           (if-some [thaw (:thaw-fn opts)]
             (let [thaw (__lg_dynamic-narrow default-thaw thaw)]
               (fn [^:in_channel input]
                 (thaw
                  (Stdlib.really_input_string
                   input
                   (Stdlib.in_channel_length input)))))
             default-read))
         address-to-filename
         (if-some [convert (:addr->filename-fn opts)]
           (__lg_dynamic-narrow default-address-to-filename convert)
           default-address-to-filename)
         filename-to-address
         (if-some [convert (:filename->addr-fn opts)]
           (__lg_dynamic-narrow default-filename-to-address convert)
           default-filename-to-address)]
     (record file-storage-backend
             (directory (ensure-directory directory))
             (write-value write-value)
             (read-value read-value)
             (address-to-filename address-to-filename)
             (filename-to-address filename-to-address)))))
