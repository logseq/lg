(ns datascript.bench.bench)

(defn ^:float warmup-duration []
  (match (Sys.getenv_opt "LG_BENCH_WARMUP_MS")
    (Some value) (Float.of_string value)
    None 2000.0))

(defn ^:float sample-duration []
  (match (Sys.getenv_opt "LG_BENCH_SAMPLE_MS")
    (Some value) (Float.of_string value)
    None 1000.0))

(defn ^:int batch-size []
  (match (Sys.getenv_opt "LG_BENCH_BATCH")
    (Some value) (Stdlib.int_of_string value)
    None 10))

(defn now []
  (* (Sys.time) 1000.0))

#?(:clj
   (defmacro dotime
     "Runs form duration, returns average time (ms) per iteration"
     [duration & body]
     `(let [start-t# (datascript.bench.bench/now)
            end-t#   (+ ~duration start-t#)
            batch#   (datascript.bench.bench/batch-size)]
        (loop [^:int iterations# batch#]
          (dotimes [_# batch#]
            (Stdlib.ignore (do ~@body)))
          (let [now# (datascript.bench.bench/now)]
            (if (< now# end-t#)
              (recur (+ batch# iterations#))
              (Float.div
               (Float.sub now# start-t#)
               (double iterations#))))))))

(defn ^:float median [^:vector<float> xs]
  (nth (sort xs) (quot (count xs) 2)))

(defn to-fixed [^:float n _places]
  (Float.to_string n))

(defn round [^:float n]
  (cond
    (> n 1)    (to-fixed n 1)
    (> n 0.01) (to-fixed n 3)
    :else      (to-fixed n 7)))

(defn left-pad [s l]
  (if (<= (count s) l)
    (str (apply str (repeat (- l (count s)) " ")) s)
    s))

(defn right-pad [s l]
  (if (<= (count s) l)
    (str s (apply str (repeat (- l (count s)) " ")))
    s))

(type-record benchmark-result
  (mean-ms :float))

(defn ^benchmark-result benchmark-result-value [^:float mean-ms]
  (record benchmark-result (mean-ms mean-ms)))

(defn ^benchmark-result bench-fn [^:fn<unit;unit> operation]
  (let [_ (dotime (warmup-duration)
            (operation (Stdlib.ignore 0)))
        times (mapv
               (fn [_]
                 (dotime (sample-duration)
                   (operation (Stdlib.ignore 0))))
               (range 5))]
    (benchmark-result-value (median times))))

(defmacro bench
  "Runs for warmup plus sample durations and returns median milliseconds per operation."
  [& body]
  `(datascript.bench.bench/bench-fn
    (fn []
      (let [_result# (do ~@body)]
        (Stdlib.ignore 0)))))

;; test dbs

(def next-eid (volatile! 0))

(def alias-limit
  (match (Sys.getenv_opt "LG_BENCH_ALIAS_LIMIT")
    (Some value) (Stdlib.int_of_string value)
    None 10))

(defn ^:map<keyword;Datascript_runtime.Data_value.t> random-man []
  (let [name      (rand-nth ["Ivan" "Petr" "Sergei" "Oleg" "Yuri" "Dmitry" "Fedor" "Denis"])
        last-name (rand-nth ["Ivanov" "Petrov" "Sidorov" "Kovalev" "Kuznetsov" "Voronoi"])
        ^:map<keyword;Datascript_runtime.Data_value.t> person {}]
    (assoc
     person
     :db/id     (Datascript_runtime.Data_value.Ref_to
                 (Datascript_runtime.Data_value.Temp_id
                  (str (vswap! next-eid inc))))
     :name      (Datascript_runtime.Data_value.String name)
     :last-name (Datascript_runtime.Data_value.String last-name)
     :full-name (Datascript_runtime.Data_value.String
                 (str name " " last-name))
     :alias     (Datascript_runtime.Data_value.string_vector
                 (vec
                  (repeatedly
                   (if (zero? alias-limit) 0 (rand-int alias-limit))
                   #(rand-nth ["A. C. Q. W." "A. J. Finn" "A.A. Fair" "Aapeli" "Aaron Wolfe" "Abigail Van Buren" "Jeanne Phillips" "Abram Tertz" "Abu Nuwas" "Acton Bell" "Adunis"]))))
     :sex       (Datascript_runtime.Data_value.Keyword
                 (str (rand-nth [:male :female])))
     :age       (Datascript_runtime.Data_value.Int (rand-int 100))
     :salary    (Datascript_runtime.Data_value.Int (rand-int 100000)))))

(defn wide-db [^:int id ^:int depth ^:int width]
  (loop [remaining depth
         ids [id]
         result []]
    (if (zero? remaining)
      (reduce
       (fn [result node-id]
         (conj
          result
          (datascript.db/tx-entity
           (assoc
            (random-man)
            :db/id
            (Datascript_runtime.Data_value.Ref_to
             (Datascript_runtime.Data_value.Temp_id (str node-id)))
            :id (Datascript_runtime.Data_value.Int node-id)
            :follows (Datascript_runtime.Data_value.temp_id_vector [])))))
       result
       ids)
      (let [next-ids (volatile! [])
            result'
            (reduce
             (fn [result node-id]
               (let [children
                     (mapv
                      (fn [offset] (+ (* node-id width) offset))
                      (range width))]
                 (vswap! next-ids into children)
                 (conj
                  result
                  (datascript.db/tx-entity
                   (assoc
                    (random-man)
                    :db/id
                    (Datascript_runtime.Data_value.Ref_to
                     (Datascript_runtime.Data_value.Temp_id (str node-id)))
                    :id (Datascript_runtime.Data_value.Int node-id)
                    :follows
                    (Datascript_runtime.Data_value.temp_id_vector
                     (mapv (fn [^:int child] (str child)) children)))))))
             result
             ids)]
        (recur (dec remaining) @next-ids result')))))

(defn ^:vector<datascript.db/tx-entry> long-db
  "depth = 3 width = 5

   1  4  7  10  13
   ↓  ↓  ↓  ↓   ↓
   2  5  8  11  14
   ↓  ↓  ↓  ↓   ↓
   3  6  9  12  15"
  [^:int depth ^:int width]
  (vec
   (apply concat
          (for [x (range width)
                y (range depth)
                :let [from (+ (* x (inc depth)) y)
                      to   (+ (* x (inc depth)) y 1)]]
            [(datascript.db/tx-entity
              {:db/id
               (Datascript_runtime.Data_value.Int (inc from))
               :name
               (Datascript_runtime.Data_value.String "Ivan")
               :follows
               (Datascript_runtime.Data_value.Int (inc to))})
             (datascript.db/tx-entity
              {:db/id
               (Datascript_runtime.Data_value.Int (inc to))
               :name
               (Datascript_runtime.Data_value.String "Ivan")})]))))

(defn people [^:int count]
  (repeatedly count random-man))

(def people-count
  (match (Sys.getenv_opt "LG_BENCH_PEOPLE")
    (Some value) (Stdlib.int_of_string value)
    None 20000))

(def *people20k
  (delay
    (let [generated (people people-count)]
      (match (Sys.getenv_opt "LG_BENCH_SHUFFLE")
        (Some "0") (vec generated)
        _ (vec (shuffle generated))))))

(defn ^:vector<map<keyword;Datascript_runtime.Data_value.t>> people20k []
  @*people20k)
