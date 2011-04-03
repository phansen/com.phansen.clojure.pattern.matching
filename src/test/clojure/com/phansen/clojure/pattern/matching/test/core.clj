(ns com.phansen.clojure.pattern.matching.test.core
  (:use [com.phansen.clojure.pattern.matching.core])
  (:use [clojure.contrib.pprint])
  (:use [clojure.test]))

(defmacro inspect-compile-when-match [desc value arg pattern & exprs]
  (println desc)
  (pprint
    (macroexpand-1 `(when-match ~arg ~pattern ~@exprs)))
  `(is (= ~value (when-match ~arg ~pattern ~@exprs))))

(defmacro inspect-compile-if-match [desc value arg pattern then-exprs else-exprs]
  (println desc)
  (pprint
    (macroexpand-1 `(if-match ~arg ~pattern ~then-exprs ~else-exprs)))
  `(is (= ~value (if-match ~arg ~pattern ~then-exprs ~else-exprs))))

(defmacro inspect-compile-cond-match [desc value arg & pattern-bodies]
  (println desc)
  (pprint
    (macroexpand-1 `(cond-match ~arg ~@pattern-bodies)))
  `(is (= ~value (cond-match ~arg ~@pattern-bodies))))

(deftest core-tests
  (testing "when-match"
    (inspect-compile-when-match 
      "Wildcards match anything.  _ is used to specify wildcard in a pattern."
      :matched
      10 _
      :matched)
    
    (inspect-compile-when-match 
      "Bindings match anything.  They are used to provide access to the matched
       value as a variable name in the body.  _<var-name> is used to specify a 
       binding in a pattern."
      10
      10 _x
      x)
    
    (inspect-compile-when-match 
      "Value checks match a value using =.  Values can be any clojure value."
      :matched
      10 10
      :matched)
    
    (let [counter 4]
      (inspect-compile-when-match 
        "Var checks match against the value of var using = . The var name itself is used
         to specify a var check in a pattern."
        :matched
        4 counter
        :matched))
    
    (inspect-compile-when-match 
      "Exact sequence checks match a vector of patterns against a sequence.  For a 
       vector of n patterns the matched sequence must have a count of n elements.  Exact
       sequence checks parse patterns recursively."
      "a"
      ["a" 1 []] [_s 1 _]
      s)
    
    (inspect-compile-when-match 
      "Sequence checks match a vector of patterns against a sequence.  Sequence checks
       are different than exact sequence checks by matching a pattern to the rest of
       the matched sequence by using the & <pattern> syntax.  Sequence checks parse 
       patterns recursively."
      2
      ["a" 3 :a :b] [_s 3  & _ps]
      (count ps))
    
    (inspect-compile-when-match 
      "Map checks match a map of patterns against a sequence.  Map checks
       check a pattern against the value of a key.  Any keys specified in the map
       must exist in the matched map.  Map checks parse patterns recursively."
      4
      {:a "?" :b 4 :c :not-checked} {_ :a _b :b }
      b)
    
    (inspect-compile-when-match 
      "Patterns may be decorated with by putting the pattern in a list 
       followed by a decorations.  The :as decoration will bind a variable 
       to the matched value.  The following example shows matching any value
       and binding that value to x.  This is exactly the same as using _x as 
       the pattern."
      10
      10 (:# _ :as x)
      x)
    
    (inspect-compile-when-match 
      "The :when decoration acts as a pattern guard.  It must be an expression
       that produces a boolean value.  If the pattern contains any variable 
       bindings those bindings will be available to the expression.  The 
       following example shows matching any value that is even?" 
      :matched
      10 (:# _x :when (even? x))
      :matched)
    
    (inspect-compile-when-match 
      "More involved pattern guard example." 
      :matched
      [1 3] (:# _ :as v :when (even? (count v)))
      :matched))

  (testing "if-match"

    (inspect-compile-if-match 
      "Testing the then case of if-match"
      :true
      10 (:# _x :when (even? x))
      :true
      :false)
    
    (inspect-compile-if-match 
      "Testing the else case of if-match"
      :false
      10 (:# _x :when (odd? x))
      :true
      :false))
  
  (testing "cond-match"

    (inspect-compile-cond-match
      "Testing expansion of cond-match"
      :red
      10 
      1 (do (println "got 1") :blue)
      2 :green
      10 :red)
    
    (defn fib
      "fibonnaci function written using cond-match"
      [n]
      (cond-match n
        0 0
        1 1
        _ (+ (fib (dec n)) (fib (- n 2)))))
    
    (is (= 0 (fib 0)))
    (is (= 1 (fib 1)))
    (is (= 1 (fib 2)))
    (is (= 2 (fib 3)))
    (is (= 3 (fib 4)))
    
    (defn fib-fast 
      "you can even write fib using recur and loops because all the 
       match macros compile into when, if, or cond statements."
      [n]
      (loop [i n, j 1, sum 0]
        (cond-match i 
          (:# _x :when (zero? x)) sum
          _ (recur (dec i) (+ sum j) j))))
    
    (is (= 0 (fib-fast 0)))
    (is (= 1 (fib-fast 1)))
    (is (= 1 (fib-fast 2)))
    (is (= 2 (fib-fast 3)))
    (is (= 144 (fib-fast 12)))))

(defn test-ns-hook []
  (core-tests))