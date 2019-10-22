#lang racket


(define (streammaker func arg)
  (letrec ([f (lambda (x)
                 (cons x (lambda () (f (func x arg)))))])
    (lambda () (f arg))))

(define ones (streammaker (lambda (a b) 1) 1))
(define nats (streammaker + 1))
(define powers (streammaker (lambda (a b) (* a 2)) 1))


(define (streamUntil stream predicate)
  (letrec ([f (lambda (stream answer)
                (let ([pr (stream)])
                  (if (predicate (car pr))
                      answer
                      (f (cdr pr) (append answer (list (car pr)))))))])
    (f stream '() )))



(define (next-k-items s k)
  (if (= k 1)
      (car (s))
      
                      