#lang racket
(require test-engine/racket-tests)
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))
          ;;; prefix with : means its from these tools
(provide (all-defined-out))




;;; Defining empty tokens
;;; These COULD all be in one list, however its better style to keep them apart for readability 

(define-empty-tokens bool-values (TRUE FALSE))
(define-empty-tokens null-def (NULL))
(define-empty-tokens parens (LEFTSQUARE RIGHTSQUARE LEFTCURLY RIGHTCURLY))
(define-empty-tokens misc (QUOTE DOT DASH COMMA COLON))
(define-empty-tokens end-of-file (EOF))

;;; Variables, NOT empty token 
(define-tokens alphas-numbers (STRING NUMBER))


;;; Words allowed in language 
(define myLexer
  (lexer
   ;;; single chars: #\ char 
   [#\{                          (token-LEFTCURLY)]
   [#\}                         (token-RIGHTCURLY)]
   [#\[                          (token-LEFTSQUARE)]
   [#\]                         (token-RIGHTSQUARE)]
   [#\"                          (token-QUOTE)]
   [#\-                         (token-DASH)]
   [#\,                         (token-COMMA)]
   [#\:                         (token-COLON)]
   [#\.                          (token-DOT)]
   [(:or "true" "True")          (token-TRUE)]
   [(:or "false" "False")        (token-FALSE)]
   ["null"                      (token-NULL)]
   ;;; I think this is technically minor parsing (no leading zeros, basically), however
   ;;; since its doable with regex in the lexer I figured might as well and Jeff said the same 
   [(:: (:? #\-) (:or #\0 (:: (char-range #\1 #\9) (:* numeric))) (:? (:: #\. (:+ numeric)))) (token-NUMBER lexeme)]
   ;;;A string is: any character except for quotes (but including escaped quotes) that is surrounded by two double quotes. 
   [(:: #\" (:* (union (intersection any-char (:~ #\")) "\\\"") )#\") (token-STRING lexeme)]
  
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

;;; lexfile: opens a file and lexes it
;;; filename: the name of the file to lex
;;; returns: a list of tokens 
(define (lexfile filename)
  (let ([input (open-input-file filename)])
    (lex input)))


;;; Lexer that we didn't write does the pattern matching on "not" and "true", things that aren't one character 
(define example (open-input-string "(not true)"))


   

