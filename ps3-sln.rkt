(module ps1 mzscheme
  
  ;;;;;;;;;;;;;;;;;;; PROBLEMS ;;;;;;;;;;;;;;;;;;;;
  ;; PROBLEM 1 Part B
  ;; Unary
  (define create-u 
    (lambda (x) 
      (cond
        ((eqv? x 0) '())
        (else
         (cons #t (create-u (- x 1)))))))
                
  (define is-zero-u? (lambda (x) (null? x)))
    
  (define predecessor-u (lambda (x) (if (null? x) 'error-only-positive-numbers (cdr x))))

  ;; BigNum
  (define create-b
    (lambda (x base)
      (cond
        ((eqv? x 0) '())
        (else
         (cons (remainder x base) (create-b (quotient x base) base))))))
        
  (define is-zero-b? (lambda (expr) (eqv? expr '())))
    
;  (define predecessor-b
;    (lambda (expr base)
;      (cond
;        ((>= (- (car expr) 1) 0) (cons (- (car expr) 1) (cdr expr)))
;        (else
;         (if (null? (cdr expr))
;             'error-only-positive-numbers
;             (cons (- base 1) (predecessor-b (cdr expr) base)))))))

   (define predecessor-b
    (lambda (expr base)
      (if (null? expr)
          'error-only-positive-numbers
          (cond
            ((>= (- (car expr) 1) 0) (cons (- (car expr) 1) (cdr expr)))
            (else
                 (cons (- base 1) (predecessor-b (cdr expr) base)))))))

  ;; PROBLEM 2
  (define count-free-occurrences
    (lambda (var exp)
      (cond
        ((symbol? exp) (if (eqv? var exp) 1 0))
        ((eqv? (car exp) 'lambda)
         (if (eqv? var (car (cadr exp)))
             0
             (count-free-occurrences var (caddr exp))))
        (else
         (+
          (count-free-occurrences var (car exp))
          (count-free-occurrences var (cadr exp)))))))

  ;; Problem 2 Part B (Optional)
  (define product
  (lambda (sos1 sos2)
    (prod-help sos1 sos2 sos2)))

  ;Function:Prod-help list x list x list -> list
  (define prod-help
    (lambda (sos1 sos2 temp)
      (cond
        ((null? sos1) '()) ; if sos1 is finished we are done.
        ((null? temp) (prod-help (cdr sos1) sos2 sos2)) ; since we start with sos2 as temp, if this is null we start all over with the rest of sos1
        (else (cons (list (car sos1) (car temp)) ; first we use sos1 using car
                    (prod-help sos1 sos2 (cdr temp))))))) ; and then on the same sos1 we continue adding the rest of tmp
  ; So basically how it works is, fırst we exhaust sos2 on fırst element of sos1, then we repeat the same with rest of sos1 and so on...
  
  ;;;;;;;;;;;;;;;;;;; TESTS ;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; Don't worry about the below function, we included it to test your implemented functions and display the result in the console
  ;; As you implement your functions you can Run (the button on the top right corner) to test your implementations
  (define-syntax equal??
    (syntax-rules ()
      ((_ test-exp correct-ans)
       (let ((observed-ans test-exp))
         (if (not (equal? observed-ans correct-ans))
           (printf "Oops! ~s returned ~s, should have returned ~s~%" 'test-exp observed-ans correct-ans)
           (printf "Correct! ~s => ~s~%" 'test-exp correct-ans))))))
  
  ;;; If you don't implement the functions in order and want to test as you go, you can comment out the corresponding tests,
  ;;; otherwise, DrRacket will raise errors.
  ;; PROBLEM 1 TESTS
  ;;; For unary representation
  (display "Unary Tests\n")
  (equal?? (create-u 4) '(#t #t #t #t)) ; should return '(#t #t #t #t)
  (equal?? (is-zero-u? '(#t #t #t)) #f) ; should return #f
  (equal?? (is-zero-u? '()) #t) ; should return #t
  (equal?? (predecessor-u '(#t #t #t)) '(#t #t)) ; should return '(#t #t)
  (equal?? (predecessor-u '()) 'error-only-positive-numbers)
  (newline)

  ;;; For BigNum representation
  (display "\nBigNum Tests\n")
  (equal?? (create-b 15 4) '(3 3)) ; should return '(3 3)
  (equal?? (is-zero-b? (create-b 0 4)) #t) ; should return #t
  (equal?? (is-zero-b? (create-b 5 4)) #f) ; should return #f
  (equal?? (predecessor-b (create-b 31 4) 4) '(2 3 1)) ; should return '(2 3 1)
  (equal?? (predecessor-b (create-b 64 4) 4) '(3 3 3 0)) ; should return '(3 3 3 0)
  (equal?? (predecessor-b (create-b 0 4) 4) 'error-only-positive-numbers) ; should return error

  (newline)

  ;; PROBLEM 2 Part A TESTS
  (display "\nCount Free Occurences Tests\n")
  (equal?? (count-free-occurrences 'x 'x) 1) ;1
  (equal?? (count-free-occurrences 'x 'y) 0) ;0
  (equal?? (count-free-occurrences 'x '(lambda (x) (x y))) 0) ;0
  (equal?? (count-free-occurrences 'x '(lambda (y) (x x))) 2) ;2
  (equal?? (count-free-occurrences 'x '((lambda (xx) x) (x y))) 2) ;2
  (equal?? (count-free-occurrences 'x '((lambda (x) (y x)) (lambda (y) (x (lambda (z) x))))) 2) ;2

  ;; PROBLEM 2 Part B TESTS (Optional)
  (display "\nCartesian Product Tests\n")
  (equal?? (product '(x y) '(a b)) '((x a) (x b) (y a) (y b)))
  (equal?? (product '() '(a b)) '())
)