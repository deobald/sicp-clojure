(ns sec2_2.ex2_22)

; the first attempt removes items front-to-back, but adds them to the answer back-to-front
; first attempt (via jake):

(defn square [x] (* x x))

(defn square-list [items]
 (letfn [(iter [things answer]
          (if (empty? things)
              answer
              (recur (rest things)
                     (cons (square (first things))
                           answer))))]
  (iter items nil)))

(square-list (list 1 2 3 4))


; the second attempt creates a nested list of lists
; the clojure behavioural equivalent of scheme 'cons' is 'list'
; second attempt (via jake):

(defn square-list [items]
 (letfn [(iter [things answer]
          (if (empty? things)
              answer
              (recur (rest things)
                     (list answer (square (first things))))))]
  (iter items nil)))

(square-list (list 1 2 3 4))
