#lang racket


(provide (all-defined-out))


(struct boolean-value (value) #:transparent)
(struct not-expr (value) #:transparent)
(struct or-expr (argOne argTwo) #:transparent)
(struct and-expr (argOne argTwo) #:transparent)