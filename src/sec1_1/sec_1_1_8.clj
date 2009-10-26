(ns sec1-1.sec-1-1-8)

;; from: http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-10.html#%_sec_1.1.8
;;       "Internal definitions and block structure"

(defn sqrt [x]
  (let [square (fn [side] (* side side))
        average (fn [a b] (/ (+ a b) 2))
        good-enough? (fn [guess] (< (Math/abs (- (square guess) x)) 0.001))
        improve (fn [guess] (average guess (/ x guess)))
        sqrt-iter (fn sqrt-iter [guess] (if (good-enough? guess) guess (sqrt-iter (improve guess))))]
  (sqrt-iter 1.0)))

