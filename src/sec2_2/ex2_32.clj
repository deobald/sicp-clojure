(ns sec2_2.ex2_32
  (:use clojure.test))

; from 2.2.1
(defn append [list1 list2]
 (if (empty? list1)
     list2
     (cons (first list1) (append (rest list1) list2))))

(defn subsets [s]
  (if (empty? s)
    (list ())
    (let [remaining (subsets (rest s))
          with-head (map #(cons (first s) %) remaining)]
      (append remaining with-head))))

(deftest should-find-all-subets-of-a-simple-set
  (let [expected [[] [2] [1] [1 2]]
        input    [1 2]]
    (is (= expected (subsets input)))))

(deftest should-find-all-possible-subsets-of-a-set
  (let [expected '(() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))
        input    '(1 2 3)]
  (is (= expected (subsets input)))))

; this was my attempt to solve 2.32 using Vectors. sadly, 'cons' poses similar problems for vectors as 'append' poses
; for lists. upon realizing this, I gave up on the effort because of the structure of the exercise.
(defn subsets-vec [s]
  (if (empty? s)
    (vector [])
    (let [remaining (subsets-vec (rest s))
          with-head (map #(cons (first s) %) remaining)]
      (conj remaining with-head))))

(run-all-tests #"ex.*")
