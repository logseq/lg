(ns lg.literal)

(defmacro build [constructors form]
  (assert (map? constructors) "literal constructors must be a map")
  (let [bindings (volatile! [])
        constructor (fn [kind]
                      (let [value (case kind
                                    :nil (:nil constructors)
                                    :bool (:bool constructors)
                                    :int (:int constructors)
                                    :float (:float constructors)
                                    :string (:string constructors)
                                    :keyword (:keyword constructors)
                                    :symbol (:symbol constructors)
                                    :char (:char constructors)
                                    :vector (:vector constructors)
                                    :list (:list constructors)
                                    :set (:set constructors)
                                    :map (:map constructors)
                                    nil)]
                        (assert (if (nil? value) false (symbol? value)) (str "unsupported literal kind " kind))
                        value))
        bind (fn [expression]
               (let [name (gensym "literal_")]
                 (IVolatile/-vreset! bindings (conj (conj (deref bindings) name) expression))
                 name))
        emit (fn emit [value]
               (let [scalar (fn [kind payload]
                              (bind (list (constructor kind) payload)))
                     sequence (fn [kind items]
                                (let [children (map emit items)]
                                  (bind (list (constructor kind)
                                              (cons 'list children)))))]
                 (cond
                   (nil? value) (bind (list (constructor :nil)))
                   (string? value) (scalar :string value)
                   (keyword? value) (scalar :keyword (if (namespace value)
                                                     (str (namespace value) "/" (name value))
                                                     (name value)))
                   (symbol? value) (scalar :symbol (str value))
                   (int? value) (scalar :int value)
                   (float? value) (scalar :float value)
                   (char? value) (scalar :char value)
                   (= value true) (scalar :bool value)
                   (= value false) (scalar :bool value)
                   (vector? value) (sequence :vector value)
                   (map? value)
                   (let [entries (map (fn [pair]
                                        (let [key (emit (first pair))
                                              value (emit (second pair))]
                                          (list 'tuple key value)))
                                      value)]
                     (bind (list (constructor :map) (cons 'list entries))))
                   (seq? value)
                   (cond
                     (= 'unquote (first value))
                     (case (count value)
                       2 (bind (second value))
                       3 (scalar (second value) (last value))
                       (assert false "unquote expects an expression or a kind and expression"))
                     (= '__lg_hash-set (first value)) (sequence :set (next value))
                     :else (sequence :list value))
                   :else (assert false "unsupported literal form"))))
        result (emit form)]
    (list 'let (deref bindings) result)))
