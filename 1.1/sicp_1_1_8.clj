(ns sicp-1-1-8)

(defn sqrt [x]
  (let [square (fn [side] (* side side))
        average (fn [a b] (/ (+ a b) 2))
        good-enough? (fn [guess] (< (Math/abs (- (square guess) x)) 0.001))
        improve (fn [guess] (average guess (/ x guess)))
        sqrt-iter (fn sqrt-iter [guess] (if (good-enough? guess) guess (sqrt-iter (improve guess))))]
  (sqrt-iter 1.0)))

