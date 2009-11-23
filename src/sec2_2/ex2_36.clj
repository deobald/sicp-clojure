(ns sec2_2.ex2_36
  (:use clojure.test))

(defn accumulate [op initial sq]
 (if (empty? sq)
     initial
     (op (first sq)
         (accumulate op initial (rest sq)))))

(defn accumulate-n [op init seqs]
  (if (empty? (first seqs))
      nil
      (cons (accumulate op init (map first seqs))
            (accumulate-n op init (map rest seqs)))))

(deftest should-accumulate-across-multiple-seqs
  (let [s '((1 2 3) (4 5 6) (7 8 9) (10 11 12))]
    (is (= '(22 26 30) (accumulate-n + 0 s)))))

(run-tests)