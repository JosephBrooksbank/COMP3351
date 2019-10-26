#lang racket

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))
          ;;; prefix with : means its from these tools


(provide (all-defined-out))

;;; Defining empty tokens
;;; These COULD all be in one list, however its better style to keep them apart for readability 

(define-empty-tokens parens (LEFTPAREN RIGHTPAREN))
(define-empty-tokens lambda-def (λ DOT))
(define-empty-tokens end-of-file (EOF))

;;; Variables, NOT empty token 
(define-tokens names (IDENTIFIER))


;;; Words allowed in language 
(define myLexer
  (lexer
   ;;; single chars: #\ char 
   [#\(                          (token-LEFTPAREN)]
   [#\)                          (token-RIGHTPAREN)]
   [#\.                          (token-DOT)]
   [(:or #\λ "lambda")  (token-λ)]
   ;;; + is like (alpha) + (alpha)*
   ;;; basically 1 or more, rather than * which is 0 or more               ;;; lexeme is the word being parsed 
   [(:: (:+ alphabetic) (:* (:or numeric alphabetic)))  (token-IDENTIFIER lexeme)]
   ;;; whitespace and input-port: both from tools, input-port is basically where the characters are coming from (skip whitespace)
   [whitespace   (myLexer input-port)]
   [(eof)        (token-EOF)]
   ))


(define (get-tokenizer in)
  (lambda () (myLexer in)))

;;; Our lex function, repeatedly call lexer 
(define (lex in)
  (let ([tokenizer (get-tokenizer in)])
    (define (lex-function)
      (let ([tok (tokenizer)])
      (cond
        [(eq? tok (token-EOF)) null]
        [else (cons tok (lex-function))])))
      (lex-function)))


;;; For testing purposes: lexes through string and finds if we typed valid words 
(define (lexstr str)
  ;;; open input string: takes string of characters, opens reader that reads in one character at a time (cursor)
  (lex (open-input-string str)))


;;; Lexer that we didn't write does the pattern matching on "not" and "true", things that aren't one character 
(define example (open-input-string "(not true)"))


;;; ex output
;;; (lexstr "(not true)")
;;; '(LEFTPAREN NOT TRUE RIGHTPAREN)

;;; (myLexer example) -- Repeatedly call, gives one thing at a time


   

