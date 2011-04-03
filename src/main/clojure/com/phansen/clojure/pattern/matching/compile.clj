(ns com.phansen.clojure.pattern.matching.compile
  (:use [com.phansen.clojure.pattern.matching.ast])
  (:use [com.phansen.clojure.adt.core]))

(defn- compile-as [ast]
  (when-let [sym (:as ast)]
    [:as sym]))

(defn compile-binding 
  [ast]
  (condp = (:type ast)
    :wildcard (gensym)
    :binding (let [sym (:symbol ast)]
               (symbol sym))
    :var-test (gensym)
    :value-check (gensym)
    :adt-check (compile-binding {:type :map-check :mappings (:mappings ast)})
    :exact-seq-check (let [children (:children ast)]
                       `[~@(map compile-binding children) ~@(compile-as ast)])
    :seq-check (let [{:keys [children rest-binding]} ast]
                 `[~@(map compile-binding children) & ~(compile-binding rest-binding) ~@(compile-as ast)])
    :map-check (let [mappings (:mappings ast)
                     binding-expr (reduce conj {} (map (fn [[k child]] [(compile-binding child) k]) mappings))]
                 (if-let [sym (:as ast)]
                   (assoc binding-expr :as sym)
                   binding-expr))
    ))

(defn- compile-guards 
  "Compiles a list of pattern guards to a list function applications"
  [ast arg]
  (if-let [guards (:when ast)]
    `(let [~(compile-binding ast) ~arg]
       (and ~@guards))
    true))

(defn compile-test 
  "Compiles the test expression for the pattern"
  [ast arg]
  (condp = (:type ast)
    :wildcard (compile-guards ast arg)
    :binding (compile-guards ast arg)
    :var-test (let [sym (:symbol ast)]
                `(and 
                   (= ~sym ~arg) 
                   ~(compile-guards ast arg)))
    :value-check (let [value (:value ast)]
                   `(and 
                      (= ~value ~arg) 
                      ~(compile-guards ast arg)))
    :adt-check (let [{:keys [mappings adt-type adt-enclosing-type]} ast
                     binding-asts (map (fn [[k child]] {:binding-key k :binding-sym (gensym) :ast child}) mappings)]
                 `(and 
                    ~(compile-guards ast arg)
                    (= ~adt-type (type ~arg))
                    (get-in adt-types [~adt-enclosing-type ~adt-type])
                    ~@(for [binding-key (keys mappings)] `(contains? ~arg ~binding-key))
                    (let [~(apply hash-map (flatten (map #(map % [:binding-sym :binding-key]) binding-asts))) ~arg] 
                      (and 
                        ~@(map #(apply compile-test %) (map #(map % [:ast :binding-sym]) binding-asts))))))
    :exact-seq-check (let [{:keys [size children]} ast
                           n (count children)
                           gensyms (for [_ (range n)] (gensym))]
                       `(and 
                          (or (seq? ~arg) (list? ~arg) (vector? ~arg)) 
                          (= ~n (count ~arg))
                          ~(compile-guards ast arg)
                          (let [[~@gensyms] ~arg]
                            (and ~@(map compile-test children gensyms)))))
    :seq-check (let [{:keys [size children rest-binding]} ast
                     n (count children)
                     gensyms (for [_ (range n)] (gensym))
                     gensym-rest (gensym)]
                 `(and 
                    (or (seq? ~arg) (list? ~arg) (vector? ~arg))
                    ~(compile-guards ast arg)
                    (let [[~@gensyms & ~gensym-rest] ~arg]
                      (and 
                        ~@(map compile-test children gensyms)
                        ~(compile-test rest-binding gensym-rest)))))
    :map-check (let [mappings (:mappings ast)
                     binding-asts (map (fn [[k child]] {:binding-key k :binding-sym (gensym) :ast child}) mappings)]
                 `(and 
                    ~(compile-guards ast arg)
                    ~@(for [binding-key (keys mappings)] `(contains? ~arg ~binding-key))
                    (let [~(apply hash-map (flatten (map #(map % [:binding-sym :binding-key]) binding-asts))) ~arg] 
                      (and 
                        ~@(map #(apply compile-test %) (map #(map % [:ast :binding-sym]) binding-asts))))))
    ))

(defn compile-body [ast arg exprs]
  (condp = (:type ast)
    :wildcard `(do 
                 ~@exprs) 
    :binding `(let [~(compile-binding ast) ~arg]
                ~@exprs)
    :var-test `(do
                 ~@exprs)
    :value-check `(do
                    ~@exprs)
    :adt-check `(let [~(compile-binding ast) ~arg]
                  ~@exprs)
    :exact-seq-check `(let [~(compile-binding ast) ~arg]
                        ~@exprs)
    :seq-check `(let [~(compile-binding ast) ~arg]
                  ~@exprs)
    :map-check `(let [~(compile-binding ast) ~arg]
                  ~@exprs)))

(defn compile-ast 
  "Compiles the pattern against arg with exprs"
  [ast arg exprs]
  `(when ~(compile-test ast arg)
     ~(compile-body ast arg exprs)))