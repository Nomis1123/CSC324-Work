#lang racket

(define (stringify-sym sym)
  (string-append "\"" (symbol->string sym) "\""))

#|
<VALUE> ::= #t | #f | <number> | '() | (<Value> <Value>)
|#

(define/match (valueify value)
  [(#t) "T"]
  [(#f) "F"]
  [('()) "Empty"]
  [((cons x y)) (string-append "(Pair " (valueify x) " " (valueify y) ")")]
  [(x) (string-append "(Num " (number->string x) ")")]
  )

#|
<expr> ::= <VALUE>
         | (literal <VALUE>)     ;; since (1 2) is technically a value as well as an expression, this can be used to disambiguate
         | IDENTIFIER
         | (+ <expr> <expr>)
         | (* <expr> <expr>)
         | (equal? <expr> <expr>)
         | (cons <expr> <expr>)
         | (car <expr>)
         | (cdr <expr>)
         | (if <expr> <expr> <expr>)
         | (lambda (IDENTIFIER ...) <expr>)
         | (<expr> <expr> ...)
         | (shift IDENTIFIER <expr>)
         | (reset <expr>)
|#

(define/match (haskellify expr)
  [(`(literal ,x)) (string-append "(Literal " (valueify x) ")")]
  [(`(+ ,e1 ,e2)) (void)]
  [(`(* ,e1 ,e2)) (void)]
  [(`(equal? ,e1 ,e2)) (string-append "(Equal " (haskellify e1) " " (haskellify e2) ")")]
  [(`(cons ,e1 ,e2)) (void)]`
  [(`(car ,e1)) (void)]
  [(`(cdr ,e1)) (void)]
  [(`(if ,e1 ,e2 ,e3)) (void)]
  [(`(shift ,x ,e1)) (void)]
  [(`(reset ,e1)) (void)]
  [(`(lambda ,xs ,expr)) (string-append "(Lambda [" (string-join (map stringify-sym xs) ",") "] " (haskellify expr) ")")]
  [(`(,f . ,xs)) (void)]
  [(x) #:when (symbol? x)
       (string-append "(Var " (stringify-sym x) ")")]
  [(x) (string-append "(Literal " (valueify x) ")")]
   )