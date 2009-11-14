(ns sec2_2.ex2_25)

(def example1 '(1 3 (5 7) 9))

(def example2 '((7)))

(def example3 '(1 (2 (3 (4 (5 (6 7)))))))

(println
  (first (rest (first (rest (rest example1))))))

(println
  (first (first example2)))

(println
  (first (rest (first (rest (first (rest (first (rest (first (rest (first (rest example3)))))))))))))

