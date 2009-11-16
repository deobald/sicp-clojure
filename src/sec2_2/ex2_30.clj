(ns sec2_2.ex2_30
  (:use clojure.test))

(defn square [x]
  (* x x))

(defn square-tree-ugly [x]
  (cond (nil? x) nil
        (not (seq? x)) (square x)
        (empty? x) '()
        :default (cons (square-tree-ugly (first x))
                       (square-tree-ugly (rest x)))))

(defn square-tree-map [x]
  (map (fn [sub-x] (if (seq? sub-x)
                    (square-tree-map sub-x)
                    (square sub-x)))
    x))

(deftest should-square-all-leaves-in-the-tree-the-ugly-way
  (let [expected '(1 (4 (9 16) 25) (36 49))
        input    '(1 (2 (3  4)  5) (6   7))]
  (is (= expected (square-tree-ugly input)))))

(deftest should-square-all-leaves-in-the-tree-with-map
  (let [expected '(1 (4 (9 16) 25) (36 49))
        input    '(1 (2 (3  4)  5) (6   7))]
  (is (= expected (square-tree-map input)))))

(run-all-tests #"ex.*")
