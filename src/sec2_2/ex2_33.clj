(ns sec2_2.ex2_33
  (:use clojure.test))

; need an accumulate like the book -- reduce won't work.
(defn accumulate [op initial sq]
 (if (empty? sq)
     initial
     (op (first sq)
         (accumulate op initial (rest sq)))))

(defn mymap [p seq]
  (accumulate (fn [x y] (cons (p x) y)) nil seq))

(deftest should-perform-a-function-on-each-element-of-a-list
  (is (= '(2 4 6 8) (mymap #(* % 2) '(1 2 3 4)))))

(defn myappend [seq1 seq2]
  (accumulate cons seq2 seq1))

(deftest should-add-one-sequence-to-the-end-of-the-other
  (is (= '(1 2 3 4) (myappend '(1 2) '(3 4)))))

(defn mylength [seq]
  (accumulate (fn [x y] (inc y)) 0 seq))

(deftest should-count-the-items-in-a-sequence
  (is (= 5 (mylength '(:a :b :c :d :e)))))

(run-tests)