(ns sec2_2.ex2_39
  (:use clojure.test))

(defn fold-right [op initial sq]
 (if (empty? sq)
     initial
     (op (first sq)
         (fold-right op initial (rest sq)))))

(defn fold-left [op initial sq]
  (loop [result initial
         remaining sq]
    (if (empty? remaining)
        result
        (recur (op result (first remaining)) (rest remaining)))))

; from 2.2.1
(defn append [list1 list2]
 (if (empty? list1)
     list2
     (cons (first list1) (append (rest list1) list2))))

; feels backwards because fold-right actually works inside-out, starting with 3.
(defn reverse-right [s]
  (fold-right (fn [x acc] (append acc (list x))) nil s))

(deftest should-reverse-a-list-using-fold-right
  (is (= '(3 2 1) (reverse-right '(1 2 3)))))

; maybe these should be called 'fold-from-left' and 'fold-from-right'?
(defn reverse-left [s]
  (fold-left (fn [acc x] (println x) (cons x acc)) '() s))

(deftest should-reverse-a-list-using-fold-left
  (is (= '(3 2 1) (reverse-left '(1 2 3)))))

(run-tests)