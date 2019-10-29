#lang racket


(require parser-tools/yacc
         (prefix-in lex: parser-tools/lex)
          "lexer.rkt"
          "abstractSyntaxTree.rkt")

(provide (all-defined-out))


(define myparser
  (parser
   (start expr)
   (end EOF)
   (tokens
           names-and-values
           parens
           lambda-def
           end-of-file)
   
   (error (lambda(tok-ok? tok-name tok-value)
            (printf "Parser error: token ~a value ~a"
                   tok-name
                   tok-value)))
   (grammar
    (expr [(IDENTIFIER) (identifier $1) ]
          [(NUMBER) (number false)]
          [(LEFTPAREN LAMBDA IDENTIFIER DOT expr RIGHTPAREN) (lambda $3 $5)]
          [(LEFTPAREN expr expr RIGHTPAREN) (app $2 $3)]
          
          ))))


(define (parse in)
  (myparser (get-tokenizer in)))

(define (parsestr str)
  (let ([in (open-input-string str)])
    (parse in)))

(define (parsefile filename)
   (let ([input (open-input-file filename)])
     (parse input)))
          
    
   