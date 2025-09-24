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
    ; Base Case: VALUES - numbers, booleans, empty list
    [(? (lambda (p) (or (number? p)
                        (boolean? p)
                        (null? p))))
     prog]
    
    ; Base Case: IDENTIFIER (symbols that aren't reserved keywords)
    [(? symbol?) prog]
    
    ; N-ary Addition Case
    [(list '+ args ...)
     (cond
       [(= (length args) 1)
        (desugar (first args))]
       [(= (length args) 2)
        `(+ ,(desugar (first args)) ,(desugar (second args)))]
       [else
        `(+ ,(desugar (first args)) 
            ,(desugar `(+ ,@(rest args))))])]
    
    [(list first-elem args ...)
     (if (special-form? first-elem)
         (desugar-special prog)
         (desugar-))] ; TODO: Handle function calls next

    
    ; Catch-all
    [_ prog]))

; Helper function to check if something is a special form
(define (special-form? symbol)
  (and (symbol? symbol)
       (member symbol '(if let cons car cdr pair? =))))

; helper function to handle n-ary function calls
(define (desugar-function-call func args)
  (cond
    ; Base case: no arguments - just desugar the function
    [(null? args) (desugar func)]
    
    ; Base case: one argument - (func arg)
    [(= (length args) 1)
     `(,(desugar func) ,(desugar (first args)))]
    
    ; Recursive case: multiple arguments - ((func arg1) arg2 ...)
    [else
     (desugar-function-call `(,(desugar func) ,(desugar (first args)))
                           (rest args))]))

; Helper function to handle special forms
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

; Helper function to check if something is a base value
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
  
  )