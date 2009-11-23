(ns sec2_2.ex2_36
  (:use clojure.test))

(defn accumulate [op initial sq]
 (if (empty? sq)
     initial
     (op (first sq)
         (accumulate op initial (rest sq)))))

(defn condense [x]
  (cond (not (seq? x)) 1
        (empty? x) 0
        :default (+ (condense (first x)) (condense (rest x)))))

(defn count-leaves [t]
  (accumulate +
              0
              (map condense t)))

(deftest should-count-all-leaf-nodes
  (is (= 5 (count-leaves '((1 2) (3 4) 5)))))

(run-tests)