#lang racket #| * CSC324H5 Fall 2024: Week 2 Lab * |#
#|
Module:        w02lab
Description:   Week 2 Lab: Pattern Matching and Recursion
Copyright: (c) University of Toronto Mississsauga
               CSC324 Principles of Programming Languages, Fall 2024
|#

; This specifies which functions this module exports. Don't change this!
(provide replace-all
         replace-all-helper
         subst)

; NOTE: As is common to testing frameworks, by default DrRacket only displays
; output for *failing* tests. If you run the module with the tests uncommented
; but don't see any output, that's good---the tests all passed! (If you want
; to double-check this, you can try breaking test cases and seeing the "fail"
; output yourself.)
(module+ test
  ; Import the testing library
  (require rackunit))

;-------------------------------------------------------------------------------
; Task 1

#|
(replace-all lst id with) -> list?
  lst: list? 
    A list of values
  id: symbol? 
    A value to be replaced in the list
  with: symbol? 
    A value that will take the place of `id` in `lst`

  Returns a list of symbols, almost identical to `lst`, but with every
  instance of `id` replaced with `with`
|#
(define/match (replace-all lst id with)
  ; the base case
  [('() id with) ; What pattern should go in TODO?
   '()]
  ; the recursive case
   [((cons f r) id with)
   (if (equal? f id)
       (cons with (replace-all r id with))
       (cons f (replace-all r id with)))])
#|
(replace-all-tail lst id with) -> list?
  lst: list? 
    A list of values
  id: symbol? 
    A value to be replaced in the list
  with: symbol? 
    A value that will take the place of `id` in `lst`

  Returns a list of symbols, almost identical to `lst`, but with every
  instance of `id` replaced with `with`. This function should be
  tail recursive.

  This function is written for you
|#
(define (replace-all-tail lst id with)
    (replace-all-helper lst id with '()))

#| 
(replace-all-helper lst id with acc) -> list?
  lst: list? 
    A list of values
  id: symbol? 
    A value to be replaced in the list
  with: symbol? 
    A value that will take the place of `id` in `lst`
  acc: list?
    An accumulator

  Helper function to replace-all-tail.
|#

(define/match (replace-all-helper lst id with acc)
  [('() id with acc)
   (reverse acc)]
  [((cons f r) id with acc)
   (if (equal? f id)
       (replace-all-helper r id with (cons with acc))
       (replace-all-helper r id with (cons f acc)))])


(module+ test
  (test-equal? "replace-all: empty list"
               (replace-all '() 'a 'b)
               '())
  (test-equal? "replace-all: some replacement"
               (replace-all '(b a c a) 'a 'b)
               '(b b c b))
  (test-equal? "replace-all: no replacement"
               (replace-all '(x y z) 'a 'b)
               '(x y z))

  ; Tests for the tail-recursive version
  (test-equal? "replace-all-tail: empty list"
               (replace-all-tail '() 'a 'b)
               '())
  (test-equal? "replace-all-tail: some replacement"
               (replace-all-tail '(b a c a) 'a 'b)
               '(b b c b))
  (test-equal? "replace-all-tail: no replacement"
               (replace-all-tail '(x y z) 'a 'b)
               '(x y z)))

;-------------------------------------------------------------------------------
; Task 2: Capture Avoiding Substitutions

#|
(subst expr id with) -> list?
  expr: list?
    An expression matching the grammar
  id: symbol?
    An identifier to replace
  with: symbol?
    The expression to replace the identifier with

  Returns a new expression where all non-shadowed instances of `id` in
  `expr` have been replaced by `with`.
|#
(define (subst expr id with)
  (match expr
    ; Case 1: Lambda Expression -- (lambda (param) body)
    ; This pattern is the most specific and must come first.
    [(list 'lambda (list param) body)
     ; Check for variable shadowing.
     (if (equal? param id)
         ; If the lambda's parameter shadows the `id`, return the original expression.
         expr
         ; Otherwise, it's safe to substitute in the body.
         (list 'lambda (list param) (subst body id with)))]

    ; Case 2: Function Call -- (function-expression argument-expression)
    ; This pattern matches a list of exactly two items.
    [(list func-expr arg-expr)
     ; Recursively call subst on both parts and build a new list.
     (list (subst func-expr id with)
           (subst arg-expr id with))]

    ; Case 3: Symbol (or Identifier)
    ; This is the base case. It matches any expression that wasn't a list
    ; matching the previous patterns.
    [sym-expr
     (if (equal? sym-expr id)
         ; If the symbol is the one we're looking for, replace it.
         with
         ; Otherwise, return the symbol unchanged.
         sym-expr)]))


(module+ test
  (test-equal? "subst: simple symbol replacement"
               (subst 'a 'a 'c)
               'c)
  (test-equal? "subst: different symbol"
               (subst 'b 'a 'c)
               'b)
  (test-equal? "subst: function call"
               (subst '(f a) 'a 'c)
               '(f c))
  (test-equal? "subst: nested function call"
               (subst '(f (g a)) 'a 'c)
               '(f (g c)))
  
  ; --- Lambda Test Cases ---
  (test-equal? "subst: lambda with different param (no shadowing)"
               (subst '(lambda (x) (f a)) 'a 'c)
               '(lambda (x) (f c)))
  
  (test-equal? "subst: lambda with same param (shadowing)"
               (subst '(lambda (a) (f a)) 'a 'c)
               '(lambda (a) (f a))) ; Should be unchanged
               
  ; --- Complex Test Cases from Handout ---
  (test-equal? "subst: handout example 1"
                (subst '(f ((g a) (h a))) 'a '(b x))
                '(f ((g (b x)) (h (b x)))))

  (test-equal? "subst: handout example 2 (shadowing)"
                (subst '((lambda (a) (f a)) a) 'a 'b)
                '((lambda (a) (f a)) b))

  (test-equal? "subst: handout example 3 (no shadowing)"
                (subst '((lambda (x) (f a)) a) 'a 'b)
                '((lambda (x) (f b)) b)))