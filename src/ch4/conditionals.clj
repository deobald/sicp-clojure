
(ns ch4.conditionals
  (:use ch4.scheme-helpers
        ch4.predicates
        ch4.declarations))

(defn if-predicate [exp] (cadr exp))

(defn if-consequent [exp] (caddr exp))

(defn if-alternative [exp]
  (if (not (nil? (cdddr exp)))
    (cadddr exp)
    'false))

(defn make-if [predicate consequent alternative]
  (list 'if predicate consequent alternative))

(defn extended-cond? [clause]
  (and (list? clause)
       (> (count clause) 2)
       (= (second clause) '=>)))

(defn extended-cond-test [clause]
  (first clause))

(defn extended-cond-recipient [clause]
  (nth clause 2))

(defn cond? [exp] (tagged-list? exp 'cond))

(defn cond-clauses [exp] (cdr exp))

(defn cond-predicate [clause] (car clause))

(defn cond-else-clause? [clause]
  (= (cond-predicate clause) 'else))

(defn cond-actions [clause] (cdr clause))

(declare expand-clauses)

(defn cond->if [exp]
  (expand-clauses (cond-clauses exp)))

(defn expand-clauses [clauses]
  (if (null? clauses)
    'false
    (let [first-clause (car clauses)
          rest-clauses (cdr clauses)]
      (cond (cond-else-clause? first-clause)
            (if (null? rest-clauses)
              (sequence->exp (cond-actions first-clause))
              (Error. (str "ELSE clause isn't last -- COND->IF"
                            clauses)))
            (extended-cond? first-clause)
            (make-if (extended-cond-test first-clause)
                     (list
                      (extended-cond-recipient first-clause)
                      (extended-cond-test first-clause))
                     (expand-clauses rest-clauses))
            :else
            (make-if (cond-predicate first-clause)
                     (sequence->exp (cond-actions first-clause))
                     (expand-clauses rest-clauses))))))
