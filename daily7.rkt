#lang racket 
(require test-engine/racket-tests)


;;; allTrue: takes list of booleans returns whether
;;; they are all true or not. If empty, an argument could be made
;;; that there are no false answers, and thus the list is true.
;;; (all operations on an empty set are true) 
(define (allTrue listItem)
  (if (= 0 (length listItem)) #t
  (if (not (first listItem))
      #f
      (allTrue (rest listItem)))))


;;; countIncreases: returns the number of times the values in a list go up the next item
;;; numList: the list to check for increases
;; returns: the number of times of increase, as an int 
(define (countIncreases numList)
  (if (= (length numList) 1) 0
  (if (< (first numList) (second numList))
     (+ 1 (countIncreases (rest numList)))
     (countIncreases (rest numList)))))

;;; downSeries: returns the list of numbers from high to low, with distance between each value as step 
(define (downSeries step high low)
  (if (< high low) '()
      (append (list high) (downSeries step (- high step) low))))



;;; TEST CASES FOR ALL METHODS 
(check-expect (allTrue '(#t #f #f)) #f)
(check-expect (allTrue '(#f #f #t)) #f)
(check-expect (allTrue '(#t #t #t)) true)

(check-expect (countIncreases '(1 2 3 2 3)) 3)
(check-expect (countIncreases '(1 3 2 4 5 1 2 3 4 5 6)) 8)
(check-expect (countIncreases '(8 7 6 5 4 3 2 1)) 0)

(check-expect (downSeries 3 12 3) '(12 9 6 3))
(check-expect (downSeries 3 8 3) '(8 5))
(check-expect (downSeries 1 5 0) '(5 4 3 2 1 0))

(test)