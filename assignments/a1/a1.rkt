#lang racket 
#| * CSC324H5 Fall 2024: Assignment 1 * |#
#|
Module:        a1
Description:   Assignment 1: Syntactic Sugar
Copyright: (c) University of Toronto Mississsauga
               CSC324 Principles of Programming Languages, Fall 2024
|#

; This specifies which functions this module exports. Don't change this!
(provide desugar)

; Import the testing library
(module+ test
  (require rackunit))

#|
(desugar prog) -> (and/or number? symbol? boolean? list?) 
  prog: (and/or number? symbol? boolean? list?) 
    A program following the grammar of a MandarinSugarProgram from the handout
  Returns a desugared version of prog, following the grammar of a
  MandarinBasicProgram from the handout,
|#
(define (desugar prog)
  (match prog
    [(? (lambda (p) (or (number? p)
                        (boolean? p)
                        (null? p))))
     prog]
    
    [(? symbol?) prog]
    
    ; n-ary Addition Case
    [(list '+ args ...)
     (cond
       [(= (length args) 1)
        (desugar (first args))]
       [(= (length args) 2)
        `(+ ,(desugar (first args)) ,(desugar (second args)))]
       [else
        `(+ ,(desugar (first args)) 
            ,(desugar `(+ ,@(rest args))))])]
    
    ; nary Lambda Case
    [(list 'lambda (list params ...) body)
     (desugar-lambda params body)]
    
    ; match Expression Case
    [(list 'match expr patterns ...)
     (desugar-match expr patterns)]
    
    [(list first-elem args ...)
     (if (special? first-elem)
         (desugar-special prog)
         (desugar-function-call first-elem args))]
    
    ; catch-all
    [_ prog]))

; helper function to check if something is a special form
(define (special? symbol)
  (and (symbol? symbol)
       (member symbol '(if let cons car cdr pair? =))))

; helper function to handle n-ary lambdas
(define (desugar-lambda params body)
  (cond
    [(= (length params) 1)
     `(lambda (,(first params)) ,(desugar body))]
    [else
     `(lambda (,(first params)) 
        ,(desugar `(lambda ,(rest params) ,body)))]))

; helper function to handle match expressions  
(define (desugar-match expr patterns)
  (cond
    [(and (= (length patterns) 1)
          (wildcard-pattern? (first patterns)))
     (desugar (second (first patterns)))]
    
    [else
     (let ([first-pattern (first patterns)]
           [rest-patterns (rest patterns)])
       (process-pattern expr first-pattern rest-patterns))]))

; helper function to process a single pattern
(define (process-pattern expr pattern rest-patterns)
  (match pattern
    [(list (? value-pattern? p) result)
     `(if (= ,(desugar expr) ,p)
          ,(desugar result)
          ,(desugar-match expr rest-patterns))]
    
    [(list (? symbol? var) result)
     (if (eq? var '_)
         (desugar result)
         `(if #t
              (let ((,var ,(desugar expr)))
                ,(desugar result))
              ,(desugar-match expr rest-patterns)))]
    
    [(list (list 'cons var1 var2) result)
     `(if (pair? ,(desugar expr))
          (let ((,var1 (car ,(desugar expr)))
                (,var2 (cdr ,(desugar expr))))
            ,(desugar result))
          ,(desugar-match expr rest-patterns))]
    
    [_ (error "Unknown pattern: ~a" pattern)]))

(define (value-pattern? p)
  (or (number? p) (boolean? p) (null? p)))

; helper function to check if pattern is wildcard
(define (wildcard-pattern? pattern)
  (match pattern
    [(list '_ result) #t]
    [_ #f]))

;  function to handle n-ary function calls
(define (desugar-function-call func args)
  (cond
    [(null? args) (desugar func)]
    
    [(= (length args) 1)
     `(,(desugar func) ,(desugar (first args)))]
    
    [else
     (desugar-function-call `(,(desugar func) ,(desugar (first args)))
                           (rest args))]))

(define (desugar-special expr)
  (match expr
    [(list 'if cond then-expr else-expr)
     `(if ,(desugar cond) ,(desugar then-expr) ,(desugar else-expr))]

    [(list 'let bindings body)
     `(let ,(map (lambda (binding)
                   `(,(first binding) ,(desugar (second binding))))
                 bindings)
        ,(desugar body))]

    [(list 'cons e1 e2)
     `(cons ,(desugar e1) ,(desugar e2))]

    [(list '= e1 e2)
     `(= ,(desugar e1) ,(desugar e2))]

    [(list 'car e)
     `(car ,(desugar e))]

    [(list 'cdr e)
     `(cdr ,(desugar e))]

    [(list 'pair? e)
     `(pair? ,(desugar e))]
    [_ (error "Unknown special form: ~a" expr)]))

;  function to check if something is a base value
(define (base-value? x)
  (or (number? x)
      (symbol? x)
      (boolean? x)
      (null? x)))

(module+ test
  ; We use rackunit's test-equal? to define some simple tests.
  (test-equal? "Desugaring a constant" ; Test label
               (desugar 3)             ; Actual value
               3)                      ; Expected value
  
  (test-equal? "Desugaring binary addition"
               (desugar '(+ 3 3))
               '(+ 3 3))
  
  (test-equal? "Desugaring n-ary addition"
               (desugar '(+ 1 2 3))
               '(+ 1 (+ 2 3)))
  
  (test-equal? "Desugaring n-ary function calls, with nested MandarinSugar structures in the function body"
               (desugar '((lambda (a b c) (+ a b c)) 1 2 3))
               '((((lambda (a) (lambda (b) (lambda (c) (+ a (+ b c))))) 1) 2) 3))
  
  (test-equal? "Desugaring a match expression with a literal (value) pattern"
               (desugar '(match x (1 2) (_ 4)))
               '(if (= x 1) 2 4))
  
  ; Test special forms
  (test-equal? "If with constants"
               (desugar '(if #t 1 2))
               '(if #t 1 2))
  
  (test-equal? "If with nested addition"
               (desugar '(if #t (+ 1 2 3) 4))
               '(if #t (+ 1 (+ 2 3)) 4))
  
  (test-equal? "Cons with constants"
               (desugar '(cons 1 2))
               '(cons 1 2))
  
  (test-equal? "Cons with nested addition"
               (desugar '(cons (+ 1 2 3) 4))
               '(cons (+ 1 (+ 2 3)) 4))
  
  (test-equal? "Car with expression"
               (desugar '(car (cons (+ 1 2) 3)))
               '(car (cons (+ 1 2) 3)))
  
  ; More match tests
  (test-equal? "Match with identifier pattern"
               (desugar '(match x (y (+ y 1)) (_ 0)))
               '(if #t (let ((y x)) (+ y 1)) 0))
  
  (test-equal? "Match with cons pattern"
               (desugar '(match lst ((cons a b) a) (_ 0)))
               '(if (pair? lst) (let ((a (car lst)) (b (cdr lst))) a) 0))
  
  (test-equal? "Match with multiple patterns"
               (desugar '(match x (1 "one") (2 "two") (_ "other")))
               '(if (= x 1) "one" (if (= x 2) "two" "other")))
  

  )