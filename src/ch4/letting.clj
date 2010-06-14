
(ns ch4.letting
    (:use ch4.predicates))

(defn let? [exp]
  (tagged-list? exp 'let))

(defn named-let? [exp]
  (symbol? (second exp)))

(defn let-body [exp]
  (if (named-let? exp)
    (nth exp 3)
    (nth exp 2)))

(defn make-let [clauses body]
  (list 'let clauses body))

(defn let-variables [exp]
  (if (named-let? exp)
    (map first (nth exp 2))
    (map first (second exp))))

(defn let-values [exp]
  (if (named-let? exp)
    (map second (nth exp 2))
    (map second (second exp))))

(defn let-name [exp]
  (second exp))

(defn let*? [exp]
  (tagged-list? exp 'let*))

(defn let*->nested-lets [exp]
  (let [let-clauses (reverse (second exp))
        body (let-body exp)]
    (reduce #(make-let (list %2) %1) body let-clauses)))
