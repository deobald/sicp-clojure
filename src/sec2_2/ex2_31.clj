(ns sec2_2.ex2_30
  (:use clojure.test))

(defn square [x]
  (* x x))

(defn tree-map [f tree]
  (map (fn [sub-tree] (if (seq? sub-tree)
                    (tree-map f sub-tree)
                    (f sub-tree)))
    tree))

(defn square-tree [tree]
  (tree-map square tree))

(deftest should-square-all-leaves-in-the-tree
  (let [expected '(1 (4 (9 16) 25) (36 49))
        input    '(1 (2 (3  4)  5) (6   7))]
  (is (= expected (square-tree input)))))

(run-all-tests #"ex.*")
