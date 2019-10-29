#lang racket



(provide (all-defined-out))


(struct identifier (name) #:transparent)
(struct lamba (name expr) #:transparent)
(struct app (expr1 expr2) #:transparent)