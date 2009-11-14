(ns sec2_2.ex2_18
  (:use clojure.test))

(defn ireverse [l]
  (let [shift (fn [original accum]
                (if (empty? original)
                  accum
                  (recur (rest original) (conj accum (first original)))))]
    (shift l '())))

(deftest should-return-a-list-in-reverse-order
  (is (= (list 25 16 9 4 1) (ireverse (list 1 4 9 16 25)))))

(run-all-tests #"ex.*")
