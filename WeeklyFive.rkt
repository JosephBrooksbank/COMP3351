#lang racket

(require parser-tools/yacc
         (prefix-in lex: parser-tools/lex)
         "JSONLexer.rkt"
         ;;; This is what my abstract syntax tree is called, the official one might be different?
         "JSONAbstractSyntaxTree.rkt"
         "JSONparser.rkt")


;;; helper function to save redundant code
;;; takes an ObjVal and converts it into the 'raw' list it contains 
(define (objValToString json)
   (match json
                [(ObjVal '()) (ObjVal '())]
                [(ObjVal li) li]))



;;; helper function for objectContains?
;;; recursively checks each StrJSONPair in the Object to see if it contains a given key 
(define (objectContainsHelper list strVal)
   ;;; Getting the first entry from the list of STrJSONPairs
    (if (empty? list)
        #f
        (letrec ([firstEntry (first list)])
          ;;; Getting the 'string' entry from the StrJSONPair
          (letrec ([name (match firstEntry [(StrJSONPair st js) st])])
            (if (equal? strVal name)
                #t
                (objectContainsHelper (rest list) strVal))))))

;;; objectContains?: returns whether or not a JSON Object contains the given field name
;;; @json: the json object to check
;;; @strVal: the field name to check for
;;; @ret: whether or not the name was found 
(define (objectContains? json strVal)
  ;;; Getting the list of StrJSONPairs from the ObjVal
  (let ([list (objValToString json)])
    ;;; recursively check each member of the list 
    (objectContainsHelper list strVal)))


;;; Helper function for @getField
;;; recursively checks each object in ObjVal list to see if it matches the name of the field requested
;;; @ret:: the field by the name requested 
(define (getFieldHelp alist str)
  (if (empty? alist)
      (ObjVal '())
      (letrec ([firstEntry (first alist)])
          ;;; Getting the 'string' entry from the StrJSONPair
          (letrec ([name (match firstEntry [(StrJSONPair st js) st])])
            (letrec ([value (match firstEntry [(StrJSONPair st jsonVal) jsonVal])])
              (if (equal? str name)
                value
                (getFieldHelp (rest alist) str)))))))

;;; getField: consumes a JSON Object and field name and produces the JSON value corresponding to that field name
;;; @json: the JSON Object value to search through
;;; @strVal: the name of the field to return
;;; @ret: the field of name @strVal 
(define (getField json strVal)
  (let ([alist (objValToString json)])
    (ObjVal (getFieldHelp alist strVal))))



;;; helper function for @filterKeys
;;; recursively applies a function to each StrJSONPair and returns a list of the ones that return true 
(define (filterKeysHelp func alist)
  (if (empty? alist)
      '()
      (letrec ([name (match (first alist) [(StrJSONPair st js) st])])
      ;;; In the instructions, the function was described as type (String -> Bool), and then in the next sentence was described as "apply it to each (key, value) pair", which I would personally think meant the function
        ;;; was of type (StrJSONPair -> Bool). However, I went with the assumption that the described typing was correct, and thus the function to be passed here is acting on the "string" parameter of the StrJSONPair. 
      (if (func (name))
          (append (filterKeysHelp func (rest alist)) (first alist))
          (filterKeysHelp func (rest alist))))))

;;; filterKeys: consumes a function and a JSON Object and produces a JSON object containing the pairs that returned true
;;; @funct: the function to apply to each string of the StrJSONPairs
;;; @json: the JSON object containing the values to apply the function to
;;; @ret: a list of filtered keyvalue pairs in an ObjVal object
(define (filterKeys funct json)
  (let ([alist (objValToString json)])
    (ObjVal (filterKeysHelp funct alist))))

;;; keyCount: returns the number of keys in a JSON object 
(define (keyCount json)
  (match json
                [(ObjVal '()) 0]
                [(ObjVal li) (length li)]
                ))


;;; helper function for @keyList
;;; returns a list of keys found in the JSON Object 
(define (keyListHelper alist)
  (if (empty? alist)
      '()
      (letrec ([name (match (first alist) [(StrJSONPair st js) st])])
        (append (list name) (keyListHelper (rest alist)) ))))
        
;;; keyList: consumes a JSON Object and produces a list of all keys in the JSON structure
;;; @json: the json object to get the keys from
;;; @ret: a list of all keys in @json
(define (keyList json)
  (let ([list (objValToString json)])
    (keyListHelper list)))


;;; arrayLength: consumes a JSON array and returns number of elements contained in array 
(define (arrayLength jsonArray)
  (let ([alist (match jsonArray
                [(Array li) li])])
    (length alist)))


;;; helper function for @filterRange
;;; recursively finds the values in the list from low to high
(define (filterRangeHelp low high jlist cur)
  (if (or (empty? jlist) (> cur high))
      '()
      (if (>= cur low)
          (append (list (first jlist)) (filterRangeHelp low high (rest jlist) (+ 1 cur)))
          (filterRangeHelp low high (rest jlist) (+ 1 cur)))))
      

;;; filterRange: takes a low and high value and returns a JSON array containing the values
;;; in the indexes from low to high
;;; @low: the low side of the indexes
;;; @high: the max index to get
;;; @jsonArray: the JSON array object to filter
;;; @return: a JSON array containing the filtered data 
(define (filterRange low high jsonArray)
  (match jsonArray
                [(Array li) (Array (filterRangeHelp low high li 0))]))


;;; helper method for @filterArray
;;; recursively applies @funct to every value in @alist
;;; returns list containing values where funct returned true
(define (filterArrayHelp funct alist)
  (if (empty? alist)
      '()
      (if (funct (first alist))
          (append (first alist) (filterArrayHelp funct (rest alist)))
          (filterArrayHelp funct (rest alist)))))

;;;filterArray: consumes a function and an array, returns array containing values which the function returned true for
;;; @funct: the function to apply to each JSON value
;;; @jsonArray: the array to apply the function to
(define (filterArray funct jsonArray)
  (match jsonArray
    [(Array li) (Array (filterArrayHelp funct li))]))


 ;;; helper function for @extractElements
 ;;; recursively gets the values from each index given 
(define (extractElementsHelp aList indList cur)
  (if (empty? aList)
      '()
      (if (member cur indList)
          (append (list (first aList)) (extractElementsHelp (rest aList) indList (+ 1 cur)))
          (extractElementsHelp (rest aList) indList (+ 1 cur)))))
                  
;;; extractElements: takes a JSON array and list of indicies, and returns a new arrray containing only those indicies
;;; jsonArray: the array to extract from
;;; indList: the list of indicies to extract 
(define (extractElements jsonArray indList)
  (match jsonArray
    [(Array li) (Array (extractElementsHelp li indList 0))]))



;;; JSON order
;;; MMWR year   MMWR week   Current week   current week,flag    cum 2018    cum 2018,flag     5-year weekly average    5-year,flag    totalcases 2017    2017,flag    totalcases 2016     2016,flag    2015     2015,flag    2014     2014,flag    2013    2013,flag    statescurrentweek 


;;; Helper function: takes a StrVal, converts it to a string without the escaped quotes 
(define (strvalToString strv)
  (match strv
    [(StrVal str) (substring str 1 (- (string-length str) 1))]
    ;;; Assuming null means 0 cases
    [(NullVal val) "0"]))

;;; Helper function: takes a StrVal and converts it to an int 
(define (strvalToNum numv)
  (match numv
    [(NullVal val) 0]
    [(NumVal num) num]
    [(StrVal str) (string->number (substring str 1 (- (string-length str) 1)))]))

;;; Helper function for @increasingIncidentsl
;;; given the raw list of data contained in the array in the object value with key "data", recursively check each entry to see if there were more cases in 2017 than 2013
(define (increasingIncidentsHelp alist)
  (if (empty? alist)
      '()
      ;;; Extracting the relevant data from the cdc array: [8:name, 10:week, 17:2017 cases, 25:2013 cases]
      (letrec ([curDiseaseData (match (extractElements (first alist) (list 8 10 17 25))
                                 [(Array li) li])])
        ;;; if the number of cases in 2017 is larger than the number in 2013, AND the data is from week 15 then its valid
        ;;; I'm only using data from week 15 because it is the most recent week, and the data is most updated. From what the file looks like, it seems that as weeks increase, more and more data is inputted for 2017,
        ;;; all the way until week 15. 
        (if (and (> (- (strvalToNum (list-ref curDiseaseData 2)) (strvalToNum (list-ref curDiseaseData 3))) 0) (= 15 (strvalToNum (list-ref curDiseaseData 1))))
            (append (list (string-append (strvalToString (list-ref curDiseaseData 0)) ":" (strvalToString (list-ref curDiseaseData 3)) " cases in 2013, " (strvalToString (list-ref curDiseaseData 2)) " cases in 2017")) (increasingIncidentsHelp (rest alist)))
            (increasingIncidentsHelp (rest alist))))))


;;; increasingIncidents: parses the cdc JSON file and finds all diseases that have an increasing number of cases from 2013 to 2017
;;; filename: the name of the file to open
(define (increasingIncidents filename)
   (let ([in (open-input-file filename)])
    (let ([parsed (parse in)])
      (let ([data (match (getField parsed "\"data\"")
                    [(ObjVal val) val])])
        (let ([dataList (match data
                          [(Array aList) aList])])
          (increasingIncidentsHelp dataList))))))



    
    

    
                
