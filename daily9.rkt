#lang racket

(require test-engine/racket-tests)



;;; streammaker: Defined in class, creates a stream from function
;;; func : the function which creates the rules for the stream
;;; arg : the first value to apply the function to for the stream
;;; returns: a stream based on the rules of from the function 
(define (streammaker func arg)
  (letrec ([f (lambda (x)
                 (cons x (lambda () (f (func x arg)))))])
    (lambda () (f arg))))


;;; next-k-items: Produces a list containing the next k items
;;; s: the stream to get values from
;;; k: the number of values to get from the stream
;;; returns: a list containing k items from the stream 
(define (next-k-items s k)
  (if (= k 1)
      (list (car (s)))
      (append (list (car (s))) (next-k-items  (cdr (s)) (- k 1)))))


;;; kth-item: Gets the kth item from the stream
;;; s: the stream to get values from
;;; k: the the index of the value to return
;;; returns: the kth value 
(define (kth-item s k)
  (if (= k 1)
      (car (s))
      (kth-item (cdr (s)) (- k 1))))


;;; Streams defined in class
(define ones* (streammaker (lambda (a b) 1) 1))
(define nats* (streammaker (lambda (a b) (+ a b)) 1))
(define powers* (streammaker (lambda (a b) (* a 2)) 1))


;;; Test cases created by Troy in the 'pl-test-cases' slack 
(check-expect (next-k-items ones* 1) '(1))
(check-expect (next-k-items ones* 5) '(1 1 1 1 1))
(check-expect (next-k-items nats* 1) '(1))
(check-expect (next-k-items nats* 5) '(1 2 3 4 5))
(check-expect (next-k-items powers* 1) '(1))
(check-expect (next-k-items powers* 5) '(1 2 4 8 16))

(check-expect (kth-item ones* 1) 1)
(check-expect (kth-item ones* 5) 1)
(check-expect (kth-item nats* 1) 1)
(check-expect (kth-item nats* 5) 5)
(check-expect (kth-item powers* 1) 1)
(check-expect (kth-item powers* 5) 16)

(test)