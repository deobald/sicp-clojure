(ns sec2_2.ex2_28
  (:use clojure.test))

(def x '((1 2) (3 4)))

(defn append [list1 list2]
 (if (empty? list1)
     list2
     (cons (first list1) (append (rest list1) list2))))

(defn fringe [x]
  (cond (nil? x) nil
        (not (seq? x)) (list x)
        (empty? x) '()
        :default (append (fringe (first x)) (fringe (rest x)))))

(deftest should-create-a-list-containing-all-leaves-from-left-to-right
  (is (= '(1 2 3 4) (fringe x))))

(run-all-tests #"ex.*")
