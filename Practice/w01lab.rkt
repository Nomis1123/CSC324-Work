#lang racket

(define (funcc lst)
  (if (empty? lst)
      1
      (* (first lst)
         (funcc (rest lst)))))

(funcc '(10 20 30))