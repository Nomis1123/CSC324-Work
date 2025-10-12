#lang racket

(define list '(1 2 3 ))


(define (add ls)
  (if (empty? ls)
      0
      (+
       (first ls)
       (add (rest ls)))))

(add list)


(define (funny x y)
  'hello)

(funny 10 (/ 1 0))