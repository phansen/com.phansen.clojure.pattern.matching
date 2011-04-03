(ns com.phansen.clojure.pattern.matching.ast
  (:use [com.phansen.clojure.adt.core]))

(defn wildcard? [sym]
  (= '_ sym))

(defn- binding? [sym]
  (let [str-sym (str sym)]
    (.startsWith str-sym "_")))

(defn- var-test? [sym]
  (and 
    (not (wildcard? sym)) 
    (not (binding? sym)) 
    (symbol? sym)))

(defn- decorated-pattern? [pattern]
  (and (list? pattern)
    (= :# (first pattern))
    (let [[_ ptn & decorations] pattern]
      (every? #{:as :when} (map first (partition 2 decorations))))))

(defn- adt-check? [pattern]
  (and 
    (not (decorated-pattern? pattern))
    (list? pattern)))

(defn- exact-seq-check? [pattern]
  (and 
    (vector? pattern) 
    (not (some #(= '& %) pattern))))

(defn- seq-check? [pattern]
  (and 
    (vector? pattern)
    (let [n (count pattern)]
      (= '& (pattern (- n 2))))))

(defn- map-check? [pattern]
  (map? pattern))

(defn- get-binding-name [sym]
  (symbol (.substring (str sym) 1)))

(defn- merge-decoration [ast decoration]
  (merge-with conj ast (conj {} (apply vector decoration))))

(defn- apply-decoration 
  "Adds the decoration keys :as and :when if applicable.  
   :wildcard, :var-test and :value-check nodes with the :as decoration
   are transformed into :binding nodes."
  [ast decoration]
  (let [[decoration-type decoration-value] decoration]
    (condp = (:type ast)
      :wildcard (condp = decoration-type
                  :as   {:type :binding :symbol decoration-value :when []}
                  :when (merge-decoration ast decoration))
      :binding (condp = decoration-type
                 :as   (assoc ast :symbol decoration-value)
                 :when (merge-decoration ast decoration))
      :var-test (condp = decoration-type
                  :as   {:type :binding :symbol decoration-value :when [(let [arg-gensym (gensym)] 
                                                                          `(fn [~arg-gensym] (= ~arg-gensym ~(:symbol ast))))]}
                  :when (merge-decoration ast decoration))
      :value-check (condp = decoration-type
                     :as   {:type :binding :symbol decoration-value :when [(let [arg-gensym (gensym)] 
                                                                             `(fn [~arg-gensym] (= ~arg-gensym ~(:value ast))))]}
                     :when (merge-decoration ast decoration))
      :exact-seq-check (merge-decoration ast decoration)
      :seq-check (merge-decoration ast decoration)
      :map-check (merge-decoration ast decoration)
      :adt-check (merge-decoration ast decoration)
      )))

(defn build-ast 
  "Given a pattern builds an Abstract Syntax Tree that represents the pattern.
   AST nodes are represented by maps.  Every node will have a :type mapping
   and branches will have a :children mapping"
  [pattern]
  (cond
    (wildcard? pattern)          {:type :wildcard}
    (binding? pattern)           {:type :binding :symbol (get-binding-name pattern)}
    (var-test? pattern)          {:type :var-test :symbol pattern}
    (adt-check? pattern)         (let [adt-type (keyword (first pattern))
                                       adt-enclosing-type (adt-enclosing-type adt-type)
                                       adt-field-order (get-in adt-types [adt-enclosing-type adt-type])]         
                                   {:type :adt-check
                                    :adt-type adt-type
                                    :adt-enclosing-type adt-enclosing-type
                                    :mappings (apply hash-map (interleave adt-field-order (map build-ast (rest pattern))))})
    (decorated-pattern? pattern) (let [[_ ptn & decorations] pattern
                                       ast (build-ast ptn)]
                                   (reduce apply-decoration (assoc ast :when []) (partition 2 decorations)))
    (exact-seq-check? pattern)   {:type :exact-seq-check
                                  :size (count pattern)
                                  :children (map build-ast pattern)}
    (seq-check? pattern)         (let [n (count pattern)]
                                   {:type :seq-check 
                                    :children (map build-ast (take (- n 2) pattern)) 
                                    :rest-binding (build-ast (pattern (- n 1)))})
    (map-check? pattern)         {:type :map-check :mappings (apply hash-map (flatten (map (fn [[k v]] [v (build-ast k)]) pattern)))}
    :else                        {:type :value-check :value pattern}))