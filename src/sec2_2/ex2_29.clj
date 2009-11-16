(ns sec2_2.ex2_29
  (:use clojure.test))

(defn make-mobile [left right]
  (list left right))

(defn make-branch [length structure]
  (list length structure))

; a. Write the corresponding selectors left-branch and right-branch, which return the branches of the mobile, and
;    branch-length and branch-structure which return the components of a branch.

(defn left-branch [mobile]
  (first mobile))

(defn right-branch [mobile]
  (first
    (rest mobile)))

(defn branch-length [branch]
  (first branch))

(defn branch-structure [branch]
  (first
    (rest branch)))

; b. Using those selectors, define a procedure total-weight that returns the total weight of a mobile.

(def total-weight)

(defn branch-weight [branch]
  (let [structure (branch-structure branch)]
    (if (number? structure)
      structure
      (total-weight structure))))

(defn total-weight [mobile]
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

(deftest should-weigh-a-simple-mobile
  (let [left (make-branch 2 3)
        right (make-branch 2 5)]
    (is (= 8 (total-weight (make-mobile left right))))))

(deftest should-weigh-a-complex-mobile
  (let [left-1 (make-branch 2 3)
        left-2 (make-branch 2 5)
        left-mobile (make-mobile left-1 left-2)
        left  (make-branch 2 left-mobile)
        right (make-branch 2 9)]
    (is (= 17 (total-weight (make-mobile left right))))))

(run-all-tests #"ex.*")


