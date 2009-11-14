(ns sec2_2.ex2_20
  (:use clojure.test))

(defn same-parity [& nums]
  (if (even? (first nums))
    (filter even? nums)
    (filter odd? nums)))

(deftest should-return-all-odd-numbers-given-when-list-begins-with-an-odd-number
  (is (= '(1 3 5 7) (same-parity 1 2 3 4 5 6 7))))

(deftest should-return-all-even-numbers-given-when-list-begins-with-an-even-number
  (is (= '(2 4 6) (same-parity 2 3 4 5 6 7))))

(run-all-tests #"ex.*")

