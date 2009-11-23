(ns sec2_2.ex2_37
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

(def matrix '((1 2 3 4) (4 5 6 6) (6 7 8 9)))

(defn matrix-*-vector [m v]
  (map (fn [s a] (* a (accumulate + 0 s)))
    m
    v))

(deftest should-multiply-a-matrix-and-vector-by-multiplying-each-row-of-the-matrix-with-the-vector-then-summing
  (let [v '(1 2 0)]
    (is (= '(10 42 0) (matrix-*-vector matrix v)))))

(defn transpose [m]
  (accumulate-n cons nil m))

(deftest test-transpose
  (is (= '((1 4 6) (2 5 7) (3 6 8) (4 6 9)) (transpose matrix))))

(defn matrix-*-matrix [mat1 mat2]
  (let [cols (transpose mat2)]
    (map (fn [row]
      (map (fn [column] (accumulate + 0 (map * row column)))
           cols))
           mat1)))
 
(def expected '((30 56 80) (56 113 161) (80 161 230)))
(deftest test-matrix-*-matrix
  (is (= expected
    (matrix-*-matrix matrix (transpose matrix)))))


(run-tests)