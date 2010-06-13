
(ns ch4.environment-test
  (:use clojure.test
        ch4.environment))

(deftest can-create-frame
  (let [f (make-frame '(a b c d) '(1 2 3 4))]
    (is (= '(a b c d) (sort (frame-variables f))))
    (is (= '(1 2 3 4) (sort (frame-values f))))))

(deftest can-add-vars-to-frame
  (let [f (make-frame '() '())]
    (add-binding-to-frame! 'a 1 f)
    (is (= '(a) (frame-variables f)))
    (is (= '(1) (frame-values f)))))

(deftest can-lookup-variable-in-environment
  (let [e (extend-environment '(a b c d)
                              '(1 2 3 4)
                              the-empty-environment)]
    (is (= 1 (lookup-variable-value 'a e)))))

(deftest can-set-variable-in-environment
  (let [e (extend-environment '(a b c d)
                              '(1 2 3 4)
                              the-empty-environment)]
    (set-variable-value! 'a 10 e)
    (is (= 10 (lookup-variable-value 'a e)))))

(deftest can-define-new-variable
  (let [e (extend-environment '(a b c d)
                              '(1 2 3 4)
                              the-empty-environment)]
    (define-variable! 'e 5 e)
    (is (= 5 (lookup-variable-value 'e e)))))

(deftest can-define-variable-which-already-exists
  (let [e (extend-environment '(a b c d)
                              '(1 2 3 4)
                              the-empty-environment)]
    (define-variable! 'b 11 e)
    (is (= 11 (lookup-variable-value 'b e)))))

(deftest can-lookup-variables-which-have-false-value
  (let [e (extend-environment '(a b c d)
                              '(1 2 3 4)
                              the-empty-environment)]
    (define-variable! 'g false e)
    (define-variable! 'f nil e)
    (is (= false (lookup-variable-value 'g e)))
    (is (= nil (lookup-variable-value 'f e)))))

(deftest need-to-make-copy-of-environment
  (let [e (extend-environment '(a)
                              '(1)
                              the-empty-environment)
        e2 (copy-environment e)]
    (is (environments-equal? e e2))
    (define-variable! 'b 2 e2)
    (is (not (environments-equal? e e2)))))

(deftest can-unbind-variable-from-frame
  (let [e (extend-environment '(a b c d)
                              '(1 2 3 4)
                              the-empty-environment)]
    (is (= 1 (lookup-variable-value 'a e)))
    (unbind-variable! 'a e)
    (is (= (type (Error.)) (type (lookup-variable-value 'a e))))))

; Exercise 4.16(a)
(deftest unassigned-variable-gets-error
  (let [e (extend-environment '(a)
                              '(1)
                              the-empty-environment)]
    (set-variable-value! 'e '*unassigned* e)
    (is (= (type (Error.)) (type (lookup-variable-value 'e e))))))
