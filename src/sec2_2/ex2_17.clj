(ns sec2_2.ex2_17
  (:use clojure.test))

(defn last-pair [l]
  (if (= (count l) 1)
    l
    (recur (rest l))))

(deftest should-retrieve-34-as-last-item-in-list
  (is (= '(34) (last-pair '(23 72 149 34)))))

(run-all-tests #"ex.*")