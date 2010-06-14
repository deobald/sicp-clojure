
(ns ch4.predicates
  (:use ch4.scheme-helpers))


(defn first-exp [xs] (car xs))

(defn rest-exps [xs] (cdr xs))

(defn tagged-list? [exp tag]
  (if (seq? exp)
    (= (first exp) tag)
    false))

(defn last-exp? [xs] (null? (cdr xs)))

(declare last-exp? first-exp rest-exps)

(defn begin? [exp] (tagged-list? exp 'begin))

(defn quoted? [exp]
  (tagged-list? exp 'quote))

(defn pair? [x] (seq? x))

(defn application? [exp] (pair? exp))

(defn definition? [exp]
  (tagged-list? exp 'define))

(defn assignment? [exp]
  (tagged-list? exp 'set!))

(defn variable? [exp]
  (or (symbol? exp)
      (= 'true exp)
      (= 'false exp)))

(defn if? [exp] (tagged-list? exp 'if))

(defn lambda? [exp] (tagged-list? exp 'lambda))

(defn self-evaluating? [exp]
  (or (number? exp)
      (string? exp)
      (and (seq? exp) (self-evaluating? (first exp)))))
