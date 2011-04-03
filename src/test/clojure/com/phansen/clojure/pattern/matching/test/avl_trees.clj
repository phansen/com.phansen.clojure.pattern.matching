(ns com.phansen.clojure.pattern.matching.test.avl-trees
  (:use [com.phansen.clojure.pattern.matching.core])
  (:use [com.phansen.clojure.adt.core])
  (:use [clojure.contrib.pprint])
  (:use [clojure.test]))

(def-adt BTree
  (E)
  (Node height left value right))

(defn- height [tree]
  (cond-match tree
    (E) 0
    (Node _h _ _ _) h))

(defn- balance-num [tree]
  (if-match tree (Node _ _l _ _r)
    (- (height l) (height r))
    0))

(defn- mkNode [l x r]
  (let [h (+ 1 (max (height l) (height r)))]
    (Node h l x r)))

(defn- rotate-right [tree] 
  (cond-match tree
    (Node _ (Node _ _ll _lx _lr) _x _r) (let [nr (mkNode lr x r)]
                                    (mkNode ll lx nr))
    _ tree))

(defn- rotate-left [tree]
  (cond-match tree
    (Node _ _l _x (Node _ _rl _rx _rr)) (let [nl (mkNode l x rl)]
                                    (mkNode nl rx rr))
    _ tree))

(defn- double-rotate-right [tree]
  (cond-match tree
    (Node _ _l _x _r) (let [nl (rotate-left l)
                         node (mkNode nl x r)]
                     (rotate-right node))
    _ tree))

(defn- double-rotate-left [tree]
  (cond-match tree
    (Node _ _l _x _r) (let [nr (rotate-right r)
                         node (mkNode l x nr)]
                     (rotate-left node))
    _ tree))


(defn- balance [tree]
  (cond-match tree 
    (:# (Node _h _l _x _r) :when (>= (balance-num tree) 2)) (if (>= (balance-num l) 1)
                                                           (rotate-right tree)
                                                           (double-rotate-right tree))
    (:# (Node _h _l _x _r) :when (<= (balance-num tree) -2))  (if (>= (balance-num r) 1)
                                                             (rotate-left tree)
                                                             (double-rotate-left tree))                                                     
    _ tree))

(defn insert [tree v]
  (cond-match tree
    (E) (Node 1 (E) v (E))
    (:# (Node _ _l _x _r) :when (< v x)) (let [[nl nr] [(insert l v) r]] 
                                        (balance (mkNode nl x nr)))
    (:# (Node _ _l _x _r) :when (> v x)) (let [[nl nr] [l (insert r v)]] 
                                        (balance (mkNode nl x nr)))
    _ tree))

(defn member? [tree x]
  (cond-match tree
    (E) nil
    (:# (Node _ _l _v _r) :when (< x v)) (recur l x)
    (:# (Node _ _l _v _r) :when (> x v)) (recur r x)
    _ x))


(deftest avl-tree-tests
  (testing "avl-tree insert"
    (is (= (Node 1 (E) 1 (E))
           (insert (E) 1)))
    (is (= (Node 2 (E) 1 (Node 1 (E) 2 (E)))
           (-> (E) 
             (insert 1)
             (insert 2))))
    (is (= (Node 2 (Node 1 (E) 1 (E)) 2 (Node 1 (E) 3 (E)))
           (-> (E) 
             (insert 1)
             (insert 2)
             (insert 3)))))
  (testing "avl-tree lookup"))

(defn test-ns-hook []
  (avl-tree-tests))