
(ns ch4.scheme-test
  (:use clojure.test
        ch4.scheme))

(use-fixtures :each (fn [f] (reset-global-environment) (f)))

(deftest test-self-eval
  (is (= 5 (interpret 5)))
  (is (= "hey" (interpret "hey"))))

 (deftest test-expressions
   (are [x y] (= x y)
        3 (interpret '(+ 1 2))
        -14 (interpret '(* (- 10 3) (- 4 6)))
        8 (interpret '(* (/ 4 2) (- 6 4) (+ 1 1)))))

 (deftest test-quoted
   (are [x y] (= x y)
        2 (interpret '(quote 2))
        'howdy (interpret '(quote howdy))
        '(jake jim 2) (interpret '(quote (jake jim 2)))))

 (deftest test-if
   (are [x] (true? (interpret x))
        '(if (= 1 1) true false)
        '(if (= 1 0) false true)
        '(if 1 true false)
                                        ; '(if nil false true)
        ))

 (deftest test-cond
   (are [x] (true? (interpret x))
        '(cond ((= 1 2) false)
               ((= 2 2) true)
               ((= 2 3) false))
        '(cond ((= 1 2) false)
               ((= 2 3) false)
               (else true))))

; For exercise 4.5
 (deftest test-different-cond-format
   (is (= 2 (interpret '(cond ((1 2 3) => cadr)
                              (else false))))))

 (deftest test-or
   (is (interpret '(or 5 4 3)))
   (is (false? (interpret '(or false false)))))

 (deftest test-and
   (is (true? (interpret '(and true true))))
   (is (false? (interpret '(and false true)))))

 (deftest test-vars
   (interpret '(define twelve 12))
   (is (= 12 (interpret 'twelve)))
   (is (= 14 (interpret '(+ twelve 2))))
   (interpret '(define two 2))
   (is (= 2 (interpret 'two)))
   (is (= 14 (interpret '(+ two twelve))))
   (interpret '(set! twelve 9))
   (is (= 9 (interpret 'twelve))))

 (deftest test-define
   (interpret
    '(define (ident a) a))
   (interpret '(define (sum a b) (+ a b)))
   (is (= 5 (interpret '(ident 5))))
   (is (= 10 (interpret '(sum 4 6))))
   (is (= 11 (interpret '(sum (ident 5) 6)))))

 (deftest test-lambdas
   (is (= 10 (interpret '((lambda (a b) (+ a b)) 7 3)))))

 (deftest test-recursive-function
   (interpret
    '(define (exp x y)
       (if (= y 1)
         x
         (exp (* x x) (- y 1)))))
   (is (= 25 (interpret '(exp 5 2)))))

; Exercise 4.6
 (deftest basic-let-form-works
   (is (= 2
          (interpret '(let ((a 2))
                        a))))
   (is (= 42
          (interpret '(let ((a 2) (b 40))
                        (+ a b))))))

; Exercise 4.7
 (deftest let*-works
   (is (= 42
          (interpret '(let* ((a 2)
                             (b (+ a 40)))
                            b))))
   (is (= 39
          (interpret '(let* ((x 3)
                             (y (+ x 2))
                             (z (+ x y 5)))
                            (* x z))))))

; Exercise 4.8
 (deftest let-supports-named-let
   (interpret '(define (fib n)
                 (let fib-iter ((a 1)
                                (b 0)
                                (count n))
                      (if (= count 0)
                        b
                        (fib-iter (+ a b) a (- count 1))))))
   (is (= 3 (interpret '(fib 4)))))

; Exercise 4.13
(deftest can-remove-binding-from-environment
  (interpret '(define a 1))
  (is (= 1 (interpret 'a)))
  (interpret '(make-unbound! a))
  (is (= (type (Error.)) (type (interpret 'a)))))

; Exercise 4.16
(deftest scans-out-internal-definitions
  (is (=
       '(lambda jake
                (let ((u '*unassigned*)
                      (v '*unassigned*))
                  (set! u e1)
                  (set! v e2)
                  e3))

       (scan-out-defines
        '(lambda jake
                 (define u e1)
                 (define v e2)
                 e3)))))

(deftest scan-out-defines-returns-original-when-no-internal-defines
  (let [statement '(lambda (a b c d)
                           (+ a (- b (+ c d))))]
    (is (= statement
           (scan-out-defines statement)))))

; Exercise 4.20
(deftest letrec-works-by-transforming-into-let-set!-combo
  (is (= '(let ((increment '*unassigned*))
            (begin
             (set! increment (lambda (n)
                                     (+ 1 n)))
             (increment 1)))

         (letrec->let '(letrec ((increment
                                 (lambda (n)
                                         (+ 1 n))))
                               (increment 1))))))

(deftest letrec-can-be-evalulated
  (is (= 2
         (interpret '(letrec ((increment
                                 (lambda (n)
                                         (+ 1 n))))
                               (increment 1))))))
