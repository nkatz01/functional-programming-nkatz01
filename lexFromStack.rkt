#lang racket
(require parser-tools/yacc
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre))
  (define-tokens value-tokens (NUM VAR))
(define-empty-tokens op-tokens (newline = RPAR LPAR  ASGN  + - * /   && || == != < > >= <=   EOF ))

(define vars (make-hash))
;(require (prefix-in : parser-tools/lex-sre))

(define-lex-abbrevs
 (lower-letter (:/ "a" "z"))

 (upper-letter (:/ #\A #\Z))

 ;; (:/ 0 9) would not work because the lexer does not understand numbers.  (:/ #\0 #\9) is ok too.
 (digit (:/ "0" "9")))

(define ASGN =)
(define and "&&")

(define or "||") 

(define calcl
  (lexer-src-pos
   [(eof) 'EOF]
   ;; recursively call the lexer on the remaining input after a tab or space.  Returning the
   ;; result of that operation.  This effectively skips all whitespace.
   [(:or #\tab #\space #\return #\newline)   (return-without-pos (calcl input-port))]
   [(:or #\+  #\- #\* #\/ #\% "&&" "||"  "==" "!=" ">=" "<=" ">" "<" #\= )  (string->symbol lexeme)]

 
   
   ["(" 'LPAR]
   [")" 'RPAR]
   
   [(:+ (:or lower-letter upper-letter)) (token-VAR (string->symbol lexeme))]
   [(:+ digit) (token-NUM (string->number lexeme))]
   [(:: (:+ digit) #\. (:* digit)) (token-NUM (string->number lexeme))]))

(define (string->tokens s)
  (port->tokens (open-input-string s)))

(define (port->tokens in)
  (define token (calcl in))
  (if (eq? (position-token-token token) 'EOF)
      '()
      (cons token (port->tokens in))))

(map position-token-token (string->tokens "123*45/3"))