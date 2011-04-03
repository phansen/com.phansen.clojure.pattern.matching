(ns com.phansen.clojure.pattern.matching.test.ast
  (:use [com.phansen.clojure.adt.core])  
  (:use [com.phansen.clojure.pattern.matching.ast])
  (:use [clojure.test])
  (:use [clojure.contrib.pprint]))

(deftest build-ast-tests
  (testing "wildcard"
    (is (= (build-ast '_) 
          {:type :wildcard}) ""))
  (testing "binding"
    (is (= (build-ast '_a) 
          {:type :binding 
           :symbol 'a}) "")
    (is (= (build-ast '_count) 
          {:type :binding 
           :symbol 'count}) ""))
  (testing "var-test"
    (is (= (build-ast 'x) 
          {:type :var-test 
           :symbol 'x}) ""))
  (testing "value-check"
    (is (= (build-ast '0) 
          {:type :value-check 
           :value '0}) ""))
  (testing "exact-seq-check"
    (is (= (build-ast '[]) 
          {:type :exact-seq-check 
           :size 0
           :children '()}) "")
    (is (= (build-ast '[_ _a x 0]) 
          {:type :exact-seq-check 
           :size 4
           :children '({:type :wildcard} 
                       {:type :binding 
                        :symbol a}
                       {:type :var-test 
                        :symbol x}
                       {:type :value-check 
                        :value 0})}) "")
    (is (= (build-ast '[_ _a x 0 []]) 
          {:type :exact-seq-check 
           :size 5
           :children '({:type :wildcard} 
                        {:type :binding 
                         :symbol a}
                        {:type :var-test 
                         :symbol x}
                        {:type :value-check 
                         :value 0}
                        {:type :exact-seq-check
                         :size 0
                         :children ()})}) "")
    (is (= (build-ast '[_ _a x 0 [_ _b]]) 
          {:type :exact-seq-check 
           :size 5
           :children '({:type :wildcard} 
                        {:type :binding 
                         :symbol a}
                        {:type :var-test 
                         :symbol x}
                        {:type :value-check 
                         :value 0}
                        {:type :exact-seq-check
                         :size 2
                         :children ({:type :wildcard}
                                    {:type :binding
                                     :symbol b})})}) ""))
  (testing "seq-check"
    (is (= (build-ast '[& _ps]) 
          {:type :seq-check 
           :rest-binding {:type :binding :symbol 'ps}
           :children '()}) "")
    (is (= (build-ast '[_ _a x 0 & ps]) 
          {:type :seq-check 
           :rest-binding {:type :var-test :symbol 'ps}
           :children '({:type :wildcard} 
                       {:type :binding 
                        :symbol a}
                       {:type :var-test 
                        :symbol x}
                       {:type :value-check 
                        :value 0})}) "")
    (is (= (build-ast '[_ _a x 0 [] & []]) 
          {:type :seq-check 
           :rest-binding {:type :exact-seq-check
                          :size 0
                          :children '()}
           :children '({:type :wildcard} 
                        {:type :binding 
                         :symbol a}
                        {:type :var-test 
                         :symbol x}
                        {:type :value-check 
                         :value 0}
                        {:type :exact-seq-check
                         :size 0
                         :children ()})}) "")
    (is (= (build-ast '[_ _a x 0 [ & _ps]]) 
          {:type :exact-seq-check 
           :size 5
           :children '({:type :wildcard} 
                        {:type :binding 
                         :symbol a}
                        {:type :var-test 
                         :symbol x}
                        {:type :value-check 
                         :value 0}
                        {:type :seq-check
                         :rest-binding {:type :binding
                                        :symbol ps}
                         :children ()})}) ""))
  (testing "map-check"
    (is (= (build-ast '{_ :r _a :A z :z 9 :y [] :w})
          {:type :map-check
           :mappings {:r {:type :wildcard}
                      :A {:type :binding
                          :symbol 'a}
                      :z {:type :var-test
                          :symbol 'z}
                      :y {:type :value-check
                          :value '9}
                      :w {:type :exact-seq-check
                          :size 0
                          :children '()}}})))
  (testing "decoration"
    (is (= (build-ast '(:# _ :when even?))
          {:type :wildcard
           :when ['even?]})))
  (testing "adt-check"
    (do
      (def-adt Tree 
        (E)
        (T left value right))
      (is (= (build-ast '(T _l _v _r))
            {:type :adt-check
             :adt-type :T
             :adt-enclosing-type :Tree
             :mappings {:left {:type :binding
                            :symbol 'l}
                        :value {:type :binding
                            :symbol 'v}
                        :right {:type :binding
                            :symbol 'r}}})))
  ))

(defn test-ns-hook []
  (build-ast-tests))
