(ns com.phansen.clojure.pattern.matching.test.compile
  (:use [com.phansen.clojure.adt.core])
  (:use [com.phansen.clojure.pattern.matching.ast])
  (:use [com.phansen.clojure.pattern.matching.compile])
  (:use [clojure.test])
  (:use [clojure.contrib.pprint]))

(deftest compile-tests
  (testing "compile-binding"
    (is (symbol? (compile-binding (build-ast '_))) 
      ":wildcard")
    (is (= (compile-binding (build-ast '_a))
          'a) 
      ":binding")
    (is (= (compile-binding (build-ast '_a))
          'a) 
      ":binding")
    (is (symbol? (compile-binding (build-ast 'y))) 
      ":var-test")
    (is (symbol? (compile-binding (build-ast 'count))) 
      ":var-test")
    (is (symbol? (compile-binding (build-ast '0))) 
      ":value-check")
    (is (symbol? (compile-binding (build-ast '"hi"))) 
      ":value-check")
    (is (let [[q r s t u v] (compile-binding (build-ast '[_ _a y 0 [] {}]))]
          (and
            (symbol? q)
            (= 'a r)
            (symbol? s)
            (symbol? t)
            (= [] u)
            (= {} v)))
      ":exact-seq-check")
    (is (let [[q r s t u v w] (compile-binding (build-ast '[_ _a y "hi" [] & {}]))]
          (and
            (symbol? q)
            (= 'a r)
            (symbol? s)
            (symbol? t)
            (= [] u)
            (= '& v)
            (= {} w)))
      ":seq-check")
    (is (let [bound (apply hash-map (reduce concat [] (map reverse (compile-binding (build-ast '{_ :a _z :b y :c 0 :d [] :e {} :f})))))
              {a :a b :b c :c d :d e :e f :f} bound]
          (and 
            (symbol? a)
            (= 'z b)
            (symbol? c)
            (symbol? d)
            (= [] e)
            (= {} f)))
      ":map-check")
    )
  (testing "compile-binding with :as"
    (is (= 'a (compile-binding (build-ast '(:# _ :as a)))) 
      ":wildcard")
    (is (= 'b (compile-binding (build-ast '(:# _a :as b)))) 
      ":binding")
    (is (= 'c (compile-binding (build-ast '(:# y :as c)))) 
      ":var-test")
    (is (= 'd (compile-binding (build-ast '(:# 0 :as d)))) 
      ":value-check")
    (is (let [[q r s t u v w x] (compile-binding (build-ast '(:# [_ _a y 0 [] {}] :as z)))]
          (and
            (symbol? q)
            (= 'a r)
            (symbol? s)
            (symbol? t)
            (= [] u)
            (= {} v)
            (= :as w)
            (= 'z x)))
      ":exact-seq-check")
    (is (let [[q r s t u v w x y] (compile-binding (build-ast '(:# [_ _a y "hi" [] & {}] :as z)))]
          (and
            (symbol? q)
            (= 'a r)
            (symbol? s)
            (symbol? t)
            (= [] u)
            (= '& v)
            (= {} w)
            (= :as x)
            (= 'z y)))
      ":seq-check")
    (is (let [bound (apply hash-map (reduce concat [] (map reverse (compile-binding (build-ast '(:# {_ :a _z :b y :c 0 :d [] :e {} :f} :as w))))))
              {a :a b :b c :c d :d e :e f :f g 'w} bound]
          (and 
            (symbol? a)
            (= 'z b)
            (symbol? c)
            (symbol? d)
            (= [] e)
            (= {} f)
            (= :as g)))
      ":map-check")
    ))

(defn inspect-compile-test [pattern arg desc]
  (println desc)
  (println pattern "    " arg)
  (let [ast (build-ast pattern)]
    (println "ast")
    (pprint  ast)
    (println "test")
    (pprint
      (compile-test ast arg))))

(defn inspect-compile-binding [pattern arg desc]
  (println desc)
  (println pattern "    " arg)
  (let [ast (build-ast pattern)]
    (println "ast")
    (pprint  ast)
    (println "test")
    (pprint
      (compile-binding ast))))

(defn test-ns-hook []
  (compile-tests))

(run-tests)

(inspect-compile-test '_ 'z
  ":wildcard with no pattern guards")

(inspect-compile-test '(:# _ :when even?) 'z
  ":wildcard with pattern guards")

(inspect-compile-test '_a 'z
  ":binding with no pattern guards")

(inspect-compile-test '(:# _a :when #(= 10 %)) 'z
  ":binding with pattern guards")

(inspect-compile-test 'x 'z
  ":var-test with no pattern guards")

(inspect-compile-test '(:# x :when odd?) 'z
  ":var-test with pattern guards")

(inspect-compile-test '"abcd" 'z
  ":value-check with no pattern guards")

(inspect-compile-test '(:# "abcd" :when #(= 4 (count %))) 'z
  ":value-check with pattern guards")

(inspect-compile-test '[_ _x y 10 []] 'z
  ":exact-seq-check with no pattern guards")

(inspect-compile-test '(:# [_ _x y 10 []] :when odd?) 'z
  ":exact-seq-check with pattern guards")

(inspect-compile-test '[_ _x y 10 [] & ps] 'z
  ":seq-check with no pattern guards")

(inspect-compile-test '(:# [_ _x y 10 [] & ps] :when even?) 'z
  "seq-check with pattern guards")

(inspect-compile-test '{_ :a _x :b y :c 10 :d} 'z
  ":map-check with no pattern guards")

(inspect-compile-test '(:# {_ :a _x :b y :c 10 :d} :when odd?) 'z
  ":map-check with pattern guards")

(def-adt Tree
  (E)   
  (T left value right))

(inspect-compile-test '(T _l 12 _r) 'z
  ":adt-check with no pattern guards")

(inspect-compile-binding '(T _left 12 (T _ll _v _rr)) 'z
  ":adt-check with complex patterns")

(defn test-ns-hook []
  (compile-tests))