
(ns sec4-1.scheme)

(defn scheme-eval [exp env]
  (cond (self-evaluating? exp) exp
        (variable? exp) (lookup-variable-value exp env)
        (quoted? exp) (text-of-quotation exp)
        (assignment? exp) (eval-assignment exp env)
        (definition? exp) (eval-definition exp env)
        (if? exp) (eval-if exp env)
        (lambda? exp)
          (make-procedure (lambda-parameters exp)
                          (lambda-body exp)
                          env)
        (begin? exp) (begin-actions exp) env)
        (cond? exp) (scheme-eval (cond->if exp) env)
        (application? exp)
          (apply (scheme-eval (operator exp) env)
                 (list-of-values (operands exp) env))
        :else
          (error "Unknown expression type -- EVAL" exp)))
          
(defn scheme-apply [procedure arguments]
  (cond (primitive-procedure? procedure)
          (apply-primitive-procedure procedure arguments)
        (compound-procedure? procedure)
          (eval-sequence
            (procedure-body procedure)
            (extend-environment
              (procedure-parameters procedure)
              arguments
              (procedure-environment procedure)))
        :else
          (error "Unknown procedure type -- APPLY" procedure)))
          
(defn list-of-values [exps env]
  (if (no-operands? exps)
    '()
    (cons (scheme-eval (first-operand exps) env)
          (list-of-values (rest-operands exps) env))))

(defn eval-if [exp env]
  (if (true? (scheme-eval (if-predicate exp) env))
      (scheme-eval (if-consequent exp) env)
      (scheme-eval (if-alternative exp) env)))

(defn eval-sequence [exps env]
  (cond (last-exp? exps) (scheme-eval (first-exp exps) env)
        :else (scheme-eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env)))

(defn eval-assignment [exp env]
  (set-variable-value! (assignment-variable exp)
                       (scheme-eval (assignment-value exp) env)
                       env)
  :ok)

(defn eval-definition [exp env]
  (define-variable! (definition-variable exp)
                    (scheme-eval (definition-value exp) env)
                    env)
  :ok)


