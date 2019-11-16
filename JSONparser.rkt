#lang racket

(require parser-tools/yacc
         (prefix-in lex: parser-tools/lex)
         "JSONLexer.rkt"
         ;;; This is what my abstract syntax tree is called, the official one might be different?
         "JSONAbstractSyntaxTree.rkt")

(provide (all-defined-out))



(define myparser
  (parser
      (start value)
      (end EOF)
      (tokens alphas-numbers
              bool-values
              parens
              null-def
              misc
              end-of-file)
      
      (error (lambda (tok-ok? tok-name tok-value)
               (printf "Parser error: token ~a value ~a"
                           tok-name
                           tok-value)))

      ;;; Grammar begins, this is basically all of the logic of the parser 
      (grammar

           ;;; Values can be any of the basic values, or objects or arrays 
          (value [(TRUE) (TrueVal true)]
                [(FALSE) (FalseVal false)]
                [(NULL) (NullVal null)]
                [(STRING) (StrVal $1)]
                [(NUMBER) (NumVal $1)]

                ;;; Objects are either empty or start with a curly and contain more data
                [(LEFTCURLY RIGHTCURLY) (ObjVal '())]
                [(LEFTCURLY objType) (ObjVal $2)]

                ;;; Basically same with arrays 
                [(LEFTSQUARE arrayType) (Array $2)]
                [(LEFTSQUARE RIGHTSQUARE) (Array '())]
          )

          ;;; Since objects are composed of ONLY string json pairs, this is its own type
          (StrJSONPairType[(STRING COLON value) (StrJSONPair $1 $3)])


          ;;; objects contain either the last item or an item in the middle of the list 
           (objType 
                   [(StrJSONPairType RIGHTCURLY) ( cons $1 empty)]
                   [(StrJSONPairType COMMA objType) (append $3 (cons $1 empty))]
           )

           ;;; arrays are much the same 
           (arrayType         
            [(value RIGHTSQUARE) (cons $1 empty)]
            [(value COMMA arrayType) (append $3 (cons $1 empty))]
            )
          )))



(define (parse in)
  (myparser (get-tokenizer in)))

(define (parsestr str)
  (let ([in (open-input-string str)])
    (parse in)))

(define (parsefile filename)
  (let ([in (open-input-file filename)])
    (parse in)))