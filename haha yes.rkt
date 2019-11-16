#lang racket
;;; How to build our own version of an if statement
(define-syntax myIf
  (syntax-rules (then else)
    [(myIf e1 then e2 else e3)
     (if e1 e2 e3)]))


;;; Hygenic macro system
;;; Scoped properly, if another macro uses "then" and "else", they are DIFFERENT "then" and "else"s
;;; Not like in C style macros, #define (add x y) can be redefined and changed
;;; variables and stuff don't leak out either



(define-syntax comment
  (syntax-rules ()
    [(comment e1 e2) e2]))

(define (fact n)
  (if (zero? n)
      1
      (* n *fact (- n 1))))


(define-syntax myAnd
  (syntax-rules ()
    [(e1 myAnd e2) (and e1 e2)]))