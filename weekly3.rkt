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
(define-empty-tokens misc (QUOTE APOS DOT DASH COMMA COLON))
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
   [#\'                         (token-APOS)]
   [#\-                         (token-DASH)]
   [#\,                         (token-COMMA)]
   [#\:                         (token-COLON)]
   [#\.                          (token-DOT)]
   [(:or "true" "True")          (token-TRUE)]
   [(:or "false" "False")        (token-FALSE)]
   ["null"                      (token-NULL)]
   ;;; I think what I actually wrote was parsing, as it filtered out invalid integers. I'm leaving it here as a comment in case this is actually what we want for the lexer, replace line 44 with line 43 if this is the case. 
   ;;;;[(:: (:? #\-) (:or #\0 (:: (char-range #\1 #\9) (:* numeric)) (:: (:+ numeric) #\. (:+ numeric)))) (token-NUMBER lexeme)]
   [(:: (:? #\-) (:or (:+ numeric) (:: (:+ numeric) #\. (:+ numeric)))) (token-NUMBER lexeme)]
   [(:+ (:or alphabetic numeric symbolic (intersection punctuation (:~ #\" #\: #\,)) "\\\"")) (token-STRING lexeme)]
  
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




   

