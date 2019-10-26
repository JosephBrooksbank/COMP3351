#lang racket

;;; Wrapping arguments in functions, evaluation is delayed until they're called
;;; eg myIf false (lamba() (/ 1 0)) (lambda() "hello")) -> "hello", no error 
(define (myIf a b c)
  (if a (b) (c) ))


(define (myForce thunk)
  (cond
    [(mcar thunk) (mcdr thunk)]
    [else
     (set-mcar! thunk #t)
     (set-mcdr! thunk ((mcdr thunk)))
     (mcdr thunk)]))


;;; STREAMS
(define ones (lambda() (cons 1 ones)))


;;; nat numbers
;;;(define nats
;;;  (letrec (if (lamda (x)
  ;;;                (cons x (lamda ()
   ;;;                              (f (+ x 1))))))
    ;;;(lamda () (f 0))))

;;; (car (nats)) = 0 
;;; (car ((crd (nats)))) = 1
;;; (car ((cdr ((cdr (nats))))))) = 2


(define (streamUntil stream predicate)
  (letrec ([f (lambda (stream answer)
                (let ([pr (stream)])
                  (if (predicate (car pr))
                      answer
                      (f (cdr pr) (append answer (list (car pr)))))))])
    (f stream 1)))



(define (streammaker func arg)
  (letrec ([f (lambda (x)
                (cons x (lambda () (f (func x arg)))))])
    (lambda () (f arg))))


(define ones* (streammaker (lambda (a b) 1) 1))
(define nats* (streammaker (lambda (a b) (+ a 1)) 1))
(define nats** (streammaker + 1))
(define powers* (streamaker * 2))
