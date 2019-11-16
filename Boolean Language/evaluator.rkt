#lang racket

(require "abstractSyntaxTree.rkt"
         "parser.rkt"
         )

(define (eval expression)
  (match expression
    [(boolean-value v) v]
    [(not-expr e) (not (eval e))]
    [(and-expr e1 e2) (and (eval e1) (eval e2))]
    [(or-expr e1 e2) (or (eval e1) (eval e2))]
    ))

