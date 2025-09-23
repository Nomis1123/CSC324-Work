#lang racket #| * CSC324H5 Fall 2024: Assignment 1 * |#
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
(define/match (desugar prog)
  [(prog) prog]
  )

; You can write helper functions freely
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

  ; TODO: Write more tests. Testing is an important part of programming,
  ; so you and your partner must write your own tests. Do not share your
  ; tests with anyone else.
  )

