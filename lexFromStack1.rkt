#lang racket
(require parser-tools/yacc
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre))
;https://stackoverflow.com/questions/32928656/racket-define-one-character-with-token-char
 (define-tokens value-tokens (NUM ID  ))
(define-empty-tokens op-tokens ( newline  = OC CC DEL PRINT WHILE IF S1 S2  OP CP + - * / || %   or && == != >= <= > <  EOF ))
(define || (lambda (a b) (or a b)))
(define vars (make-hash))

(define-lex-abbrevs
 (lower-letter (:/ "a" "z"))

 (upper-letter (:/ #\A #\Z))
 (digit (:/ "0" "9")))
 
(define calcl
  (lexer 
   [(eof) 'EOF]
   ;; recursively call the lexer on the remaining input after a tab or space.  Returning the
   ;; result of that operation.  This effectively skips all whitespace.
   [(:or #\tab #\space  #\return )   (calcl input-port)  ]
   ;; (token-newline) returns 'newline
    
      [    #\newline (token-newline)]
   ;; Since (token-=) returns '=, just return the symbol directly
       [ (:= 2  #\|) (string->symbol lexeme)];  (token-||)]
   [(:or "=" "+" "-" "*" "/" "%" "&&"      "==" "!=" ">=" "<=" ">" "<") (string->symbol lexeme)]
  
   ["(" 'OP]
   [")" 'CP]
   ["{" 'OC]
   ["}" 'CC] 
   [(:+ (:or lower-letter upper-letter)) (token-ID (string->symbol lexeme))]
   ["," 'DEL]
   [(:+ digit) (token-NUM (string->number lexeme))]
   [(:: (:+ digit) #\. (:* digit)) (token-NUM (string->number lexeme))]))

(define (string->tokens s)
  (port->tokens (open-input-file s)))

(define (port->tokens in)
  (define token (calcl in))
  (if (eq? token  'EOF)
      '()
      (cons token (port->tokens in))))

 ; (map position-token-token (string->tokens  "cmmExamples/example5.cmm")) 
  (string->tokens "cmmExamples/example5.cmm")