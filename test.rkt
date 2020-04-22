 
#lang racket
 

;; An interactive calculator inspired by the calculator example in the bison manual.


;; Import the parser and lexer generators.
(require parser-tools/yacc
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(define-tokens value-tokens (NUM VAR  ))
(define-empty-tokens op-tokens (newline  = OP CP + - * / || %   or && == != >= <= > <  EOF ))
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
   [(:or #\tab #\space #\newline ) (calcl input-port)]
   ;; (token-newline) returns 'newline
     [#\newline (token-newline)]
   ;; Since (token-=) returns '=, just return the symbol directly
       [ (:= 2  #\|)   (token-||)]
   [(:or "=" "+" "-" "*" "/" "%" "&&"      "==" "!=" ">=" "<=" ">" "<") (string->symbol lexeme)]
  
   ["(" 'OP]
   [")" 'CP]
    
   [(:+ (:or lower-letter upper-letter)) (token-VAR (string->symbol lexeme))]
   [(:+ digit) (token-NUM (string->number lexeme))]
   [(:: (:+ digit) #\. (:* digit)) (token-NUM (string->number lexeme))]))
   
(define (string->char s)  (car (string->list s)))

(define calcp
  (parser

   (start  start)
   (end   newline    EOF)
   (tokens value-tokens op-tokens)
   (error (lambda (a b c) (void)))

   (precs (right =)


          (left  ||)
          (left &&)
          (left == !=)
          (left <= >= < >)
          (left - +)
          (left * / %)
          
         )
   
   (grammar
    
    (start [() #f]
           ;; If there is an error, ignore everything before the error
           ;; and try to start over right after the error
           [(error start) $2]
           [(exp) $1])
    
    (exp [(NUM) $1]
         [(VAR) (hash-ref vars $1 (lambda () 0))]
         [(VAR = exp) (begin (hash-set! vars $1 $3)
                             $3)]
         [(exp || exp) (or  $1 $3 )]
         [(exp && exp) (and $1 $3)]
         [(exp == exp) (equal? $1 $3)]
         [(exp != exp) (not(equal? $1 $3))]
         [(exp < exp) (< $1 $3)]
         [(exp > exp) (> $1 $3)]
         [(exp >= exp) (>= $1 $3)]
         [(exp <= exp) (<= $1 $3)]
         
          
         [(exp + exp) (+ $1 $3)]
         [(exp - exp) (- $1 $3)]
         [(exp * exp) (* $1 $3)]
         [(exp / exp) (/ $1 $3)]
         [(exp % exp) (remainder $1 $3)]
       
         
         [(OP exp CP) $2]))))
           
;; run the calculator on the given input-port       
(define (calc ip)
  (port-count-lines! ip)
  (letrec ((one-line
	    (lambda ()
              (let ((result (calcp (lambda () (calcl ip))  )))
                (when result (printf "~a\n" result)  (one-line))
                )
                 ) ))
    (one-line))
  )

(calc   (open-input-string "0 || (0 || 1)"))