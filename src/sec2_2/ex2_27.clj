(ns sec2_2.ex2_27
  (:use clojure.test))

(defn deep-reverse [x]
  (cond (nil? x) nil
        (not (seq? x)) x
        :default (map deep-reverse (reverse x))))

(deftest should-reverse-items-in-sub-lists-as-well-as-the-list-itself
  (is (= '((4 3) (2 1)) (deep-reverse '((1 2) (3 4))))))

(run-all-tests #"ex.*")
