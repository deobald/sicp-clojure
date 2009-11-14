(ns sec2_2.ex2_21
  (:use clojure.test))

(defn square [number]
  (* number number))

(defn square-list-1 [items]
  (if (empty? items)
    nil
    (cons (square (first items))
          (square-list-1 (rest items)))))

(defn square-list-2 [items]
  (map square items))

(deftest should-square-each-item-in-the-list-1
  (is (= '(1 4 9 16) (square-list-1 '(1 2 3 4)))))

(deftest should-square-each-item-in-the-list-2
  (is (= '(1 4 9 16) (square-list-2 '(1 2 3 4)))))

(run-all-tests #"ex.*")

