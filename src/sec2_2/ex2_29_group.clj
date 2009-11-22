(ns sec2_2.ex2_29_group
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

(defn total-weight [mobile]
  (if (seq? (fnext mobile)) (+ (total-weight first) (total-weight fnext))
    (fnext mobile)))

(deftest should-weigh-an-ultra-simple-mobile
  (is (= 1 (total-weight '(3 1)))))

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

; c. Design a predicate that tests whether a binary mobile is balanced.



;(deftest a-mobile-is-balanced-if-both-branches-have-equal-torque
;  (let [left (make-branch 2 3)
;        right (make-branch 2 3)
;        mobile (make-mobile left right)]
;    (is (balanced? mobile))))
;
;(deftest a-mobile-is-not-balanced-if-one-branch-has-more-torque-than-the-other
;  (let [left (make-branch 2 3)
;        right (make-branch 2 4)
;        mobile (make-mobile left right)]
;    (is (not (balanced? mobile)))))

; d. Suppose we change the constructors to use 'cons' (scheme). How much do you need to change the programs
;    to convert to the new representation?

; answer: only the selectors need to change, given a scheme-style cons. (car/cdr vs. car/cadr)

(run-all-tests #"ex.*")


