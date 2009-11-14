(ns sec2_2.ex2_19
  (:use clojure.test))

(defn first-denomination [x]
 (first x))

(defn except-first-denomination [x]
 (rest x))

(defn no-more? [x]
 (empty? x))

(defn cc [amount coin-values]
 (cond (= amount 0) 1
       (or (< amount 0) (no-more? coin-values)) 0
       :else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount (first-denomination coin-values))
                coin-values))))

(def us-coins (list 50 25 10 5 1))
(def uk-coins (list 100 50 20 10 5 2 1 0.5))

(deftest should-find-100-cents-can-be-counted-292-ways-with-us-coins
  (is (= 292 (cc 100 us-coins))))

(deftest should-find-50-pence-can-be-counted-6149-ways-with-uk-coins
  (is (= 6149 (cc 50 uk-coins))))

(run-all-tests #"ex.*")
