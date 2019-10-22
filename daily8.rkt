#lang racket
(require test-engine/racket-tests)

;;; squareSum -- adds the squares of each integer from 0 to n
;;; n -- a nonnegative integer, the max square to add
;;; returns a sum of the squares of each integer from 0 to n 
(define (squareSum n)
  (if (= n 0)
      0
      (+ (* n n) (squareSum (- n 1)))))

;;; cycleOne -- moves the head of the list to the tail
;;; list -- list to modify
;;; returns @list, but the first item is now the last item 
(define (cycleOne list)
  (if (empty? list)
      '()
      (append (rest list) (cons (first list) empty))))

;;; cycleN -- moves the head of the list to the tail, N times
;;; count -- the number of times to cycle the list
;;; list -- the list to cycle
;;; returns a list that has been cycled in a fashion like @cycleOne, @count times 
(define (cycleN count list)
  (if (= count 0)
      list
      (cycleN (- count 1) (cycleOne list))))

;;; memberOf? -- finds if @value is in @list
;;; value -- the value to check for in the list
;;; list -- the list to check for the value
;;; returns true if value found, false otherwise
(define (memberOf? value list)
  (if (empty? list)
      #f
      (if (equal? value (first list))
          #t
          (memberOf? value (rest list)))))

;;; intersection -- finds the intersection of the two lists
;;; list1 -- the first list to intersect
;;; list2 -- the second list to intersect
;;; returns a list containing the values present in both lists, aka the intersection 
(define (intersection list1 list2)
  (if (empty? list1)
      '()
      (if (memberOf? (first list1) list2)
          (append (cons (first list1) empty) (intersection (rest list1) list2))
          (intersection (rest list1) list2))))


;;; Test cases, Taken from the 'pl-test-cases' slack channel-- Credit to Dalton Crutchfield for all. 
(check-expect (squareSum 0) 0)
(check-expect (squareSum 1) 1)
(check-expect (squareSum 2) 5)
(check-expect (squareSum 3) 14)
(check-expect (squareSum 4) 30)
(check-expect (squareSum 5) 55)
(check-expect (cycleOne '()) '())
(check-expect (cycleOne '(0)) '(0))
(check-expect (cycleOne '(1 2)) '(2 1))
(check-expect (cycleOne '(1 2 3 4)) '(2 3 4 1))
(check-expect (cycleOne '(a b c d w)) '(b c d w a))
(check-expect (cycleOne '("this" "is" "fun")) '("is" "fun" "this"))

(check-expect (cycleN 0 '()) '())
(check-expect (cycleN 100 '(a)) '(a))
(check-expect (cycleN 0 '(1 2 3 4)) '(1 2 3 4))
(check-expect (cycleN 1 '(1 2 3 4)) '(2 3 4 1))
(check-expect (cycleN 2 '(1 2 3 4)) '(3 4 1 2))
(check-expect (cycleN 5 '(a b c d e)) '(a b c d e))

(check-expect (memberOf? 0 '()) #f)
(check-expect (memberOf? 0 '(0)) #t)
(check-expect (memberOf? 0 '(0 1 2 3 4)) #t)
(check-expect (memberOf? 0 '(1 2 3 4 5)) #f)
(check-expect (memberOf? 0 '(2 3 4 5 6 0)) #t)
(check-expect (memberOf? "abc" '("def" "hij" "kmn" "abc")) #t)

(check-expect (intersection '() '()) '())
(check-expect (intersection '(1 2 3 4) '()) '())
(check-expect (intersection '() '(1 2 3 4)) '())
(check-expect (intersection '(1 2 3 4) '(1 2 3 4)) '(1 2 3 4))
(check-expect (intersection '(1 2 3 4 5 6) '(4 5 6 7 8 9)) '(4 5 6))
(check-expect (intersection '("A" "a" "B" "b" "C" "c") '("A" "B" "C" "D" "E")) '("A" "B" "C"))

(test)