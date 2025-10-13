#lang racket

(define list '(1 2 3 5))


(define (add ls)
  (if (empty? ls)
      0
      (+
       (first ls)
       (add (rest ls)))))

(add list)


(define (funny x y)
  'hello)

(funny 10 (/ 1 1))


(define (product lst)
  (if (empty? lst)
      1
      (*
       (first lst)
       (product (rest lst)))))

(product list)