#lang eopl
; In this lab, you will define new arithmetic functionalities to LET language

; 1- Extend the LET language by adding a new operator minus that takes one argument, n,and returns -n.
; For example, the value of minus(-(minus(5),9)) should be 14.

; 2- Extend the LET language by adding operators for addition, multiplication, and integer quotient.
; Add the following rules to the grammar:
; Expression ::= +(Expression,Expression)
; Expression ::= *(Expression,Expression)
; Expression ::= /(Expression,Expression)

; 3- Add a numeric equality predicate equal? and numeric order predicates greater? and less? to the set of operations in the LET language.
; Add the following rules to the grammar:
; Expression ::= equal?(Expression,Expression)
; Expression ::= greater?(Expression,Expression)
; Expression ::= less?(Expression,Expression)

; 4- Add test cases to test your code.

; Good luck.

(define-datatype program program?
  (a-program
   (exp1 expression?)))

; -b- syntax definition
(define-datatype expression expression?
  (const-exp
   (num number?))
  (diff-exp
   (exp1 expression?)
   (exp2 expression?))
  (zero?-exp
   (exp1 expression?))
  (if-exp
   (exp1 expression?)
   (exp2 expression?)
   (exp3 expression?))
  (var-exp
   (var symbol?))
  (let-exp
   (var symbol?)
   (exp1 expression?)
   (body expression?)))

(define-datatype expval expval?
  (num-val
   (num number?))
  (bool-val
   (bool boolean?)))

(define expval->num
  (lambda (val)
    (cases expval val
      (num-val (num) num)
      (else (eopl:printf "Expression value extraction error: num")))))

(define expval->bool
  (lambda (val)
    (cases expval val
      (bool-val (bool) bool)
      (else (eopl:printf "Expression value extraction error: bool")))))

(define scanner-spec-let
  '((comment ("%" (arbno (not #\newline))) skip)
    (symbol (letter (arbno (or letter digit))) symbol)
    (number (digit (arbno digit)) number)))

; -a- grammar definition
(define grammar-let
  '((program
     (expression)
     a-program)
    (expression
     (number)
     const-exp)
    (expression
     ("-(" expression "," expression ")")
     diff-exp)
    (expression
     ("zero?(" expression ")")
     zero?-exp)
    (expression
     ("if " expression " then " expression " else " expression)
     if-exp)
    (expression
     (symbol)
     var-exp)
    (expression
     ("let " symbol " = " expression " in " expression)
     let-exp)))

(define scan&parse
  (sllgen:make-string-parser scanner-spec-let grammar-let))

(define empty-env
  (lambda () (list 'empty-env)))

(define extend-env
  (lambda (var val env)
    (list 'extend-env var val env)))

(define apply-env
  (lambda (env search-var)
    (cond
      ((eqv? (car env) 'empty-env)
       (eopl:printf "No binding found for ~s" search-var))
      ((eqv? (car env) 'extend-env)
       (let ((saved-var (cadr env))
             (saved-val (caddr env))
             (saved-env (cadddr env)))
         (if (eqv? search-var saved-var)
             saved-val
             (apply-env saved-env search-var))))
      (else
       (eopl:printf "Invalid environment!")))))

(define run
  (lambda (string)
    (value-of-program (scan&parse string))))

(define value-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
                 (value-of exp1 (empty-env))))))

; -c- behavior definition
(define value-of 
  (lambda (exp env)
    (cases expression exp
      (const-exp (num) (num-val num))
      (var-exp (var) (apply-env env var))
      (diff-exp (exp1 exp2)
                (let ((val1 (value-of exp1 env))
                      (val2 (value-of exp2 env)))
                  (let ((num1 (expval->num val1))
                        (num2 (expval->num val2)))
                    (num-val
                     (- num1 num2)))))
      (zero?-exp (exp1)
                 (let ((val1 (value-of exp1 env)))
                   (let ((num1 (expval->num val1)))
                     (if (zero? num1)
                         (bool-val #t)
                         (bool-val #f)))))
      (if-exp (exp1 exp2 exp3)
              (let ((val1 (value-of exp1 env)))
                (if (expval->bool val1)
                    (value-of exp2 env)
                    (value-of exp3 env))))
      (let-exp (var exp1 body)
               (let ((val1 (value-of exp1 env)))
                 (value-of body
                  (extend-env var val1 env)))))))

;; - d- test cases
(eopl:printf "Trying to make some calculations...\n")
(eopl:printf "~a\n" (number->string (expval->num (run "-(8,2)")))) ; 6
(eopl:printf "~a\n" (number->string (expval->num (run "minus(-(minus(5),9))")))) ; 14



