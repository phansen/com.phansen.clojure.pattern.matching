(ns com.phansen.clojure.pattern.matching.core
  (:use [com.phansen.clojure.pattern.matching.compile])
  (:use [com.phansen.clojure.pattern.matching.ast]))

(defmacro when-match [arg pattern & exprs]
  (let [ast (build-ast pattern)]
    `(when ~(compile-test ast arg)
       ~(compile-body ast arg exprs))))

(defmacro if-match [arg pattern then-exprs else-exprs]
  (let [ast (build-ast pattern)]
    `(if ~(compile-test ast arg)
       ~(compile-body ast arg [then-exprs])
       ~(compile-body ast arg [else-exprs]))))

(defmacro cond-match [arg & pattern-bodies]
  (let [cond-exprs (for [[pattern body] (partition 2 pattern-bodies)] 
                     (let [ast (build-ast pattern)]
                       [(compile-test ast arg) (compile-body ast arg [body])]))]
    `(cond 
       ~@(reduce concat [] cond-exprs))))